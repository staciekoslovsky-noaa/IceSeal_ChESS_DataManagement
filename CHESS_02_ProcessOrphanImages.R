# CHESS: Process orphan images to identify the correct parent image
# S. Hardy, 23MAR2017

# Set variables!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
dir <- "CHESS2016_N94S_FL1_C_20160407_232653"
filt_image_prefix <- "CHESS_FL1_C"
#process_type <- "retroactive"
process_type <- ""

# Create functions -----------------------------------------------
# Function to install packages needed
install_pkg <- function(x)
{
  if (!require(x,character.only = TRUE))
  {
    install.packages(x,dep=TRUE)
    if(!require(x,character.only = TRUE)) stop("Package not found")
  }
}

# Function to compare rasters
match_orphan2parent <- function(df) {
  df$match_max <- ""
  df$match_min <- ""
  for (i in 1:nrow(df)){
    x <- raster::raster(df$filt_image_file[i])
    y <- raster::raster(df$effort_image_file[i])
    z <- x-y
    df$match_max[i] <- maxValue(z)
    df$match_min[i] <- minValue(z)
    removeTmpFiles(h=0)
  }
  return(df)
}

# Function to merge data frames after comparing rasters
process_merge_df <- function(orphan_m_a, orphan_b_b){
  orphan_m <- merge(orphan_m_a, orphan_b_b, by = "filt_image_file", all = TRUE)
  orphan_m <- unique(orphan_m[which(is.na(orphan_m$min_diff.y)), c(1:6)])
  colnames(orphan_m)[6] <- "min_diff"
  orphan_m$min_diff <- orphan_m$min_diff + 1
  orphan_m <- orphan_m[, c(1, 6)]
  
  orphan_m <- merge(orphan_m, orphan_e, by = "filt_image_file")
  orphan_m <- unique(orphan_m[which(orphan_m$min_diff == orphan_m$diff), c(3, 1, 4, 5, 6, 2)])
  return(orphan_m)
}

# Install libraries ----------------------------------------------
install_pkg("RPostgreSQL")
install_pkg("raster")

# Run code -------------------------------------------------------
# Extract data from DB ------------------------------------------------------------------
con <- RPostgreSQL::dbConnect(PostgreSQL(), 
                              dbname = Sys.getenv("pep_db"), 
                              host = Sys.getenv("pep_ip"), 
                              user = Sys.getenv("pep_user"), 
                              rstudioapi::askForPassword(paste("Enter your DB password for user account: ", Sys.getenv("pep_user"), sep = "")))

orphan <- RPostgreSQL::dbGetQuery(con, paste("SELECT filt_image_dir, filt_image, filt_dt, effort_image_dir FROM surv_chess.qa_orphan WHERE effort_image_dir LIKE \'%", dir, "%\' AND filt_image IS NOT NULL", sep = ""))
print(paste(nrow(orphan), "orphan records...", sep = " "))
if (nrow(orphan) == 0) {stop("no orphan records to match!")}
orphan$filt_image_file <- paste(orphan$filt_image_dir, orphan$filt_image, sep = "/")
orphan <- orphan[, c(4, 3, 5)]
effort <- RPostgreSQL::dbGetQuery(con, paste("SELECT DISTINCT effort_image_dir, effort_image_file, effort_dt FROM surv_chess.tbl_effort WHERE effort_image_dir LIKE \'%", dir, "%\'", sep = ""))

# Compare orphan image to possible parents (round 1) -------------------------------------
# Process file names
orphan_e <- unique(merge(orphan, effort, by = "effort_image_dir"))
dt_count <- unlist(gregexpr(pattern = "[0-9]", orphan_e$effort_dt[1]))
orphan_e$diff <- 0
for (i in 1:length(dt_count)){
  count <- as.numeric(dt_count[i])
  orphan_e$diff <- ifelse(as.numeric(substr(orphan_e$filt_dt, count, count)) - as.numeric(substr(orphan_e$effort_dt, count, count)) == 0, orphan_e$diff, orphan_e$diff + 1)
}
rm(i, count, dt_count, effort)

orphan_m1 <- data.frame(tapply(orphan_e$diff, orphan_e$filt_image_file, min))
orphan_m1$filt_image_file <- row.names(orphan_m1)
row.names(orphan_m1) <- NULL
colnames(orphan_m1)[1] <- c("min_diff")

orphan_m1 <- merge(orphan_m1, orphan_e, by = "filt_image_file", all = TRUE)
orphan_m1 <- unique(orphan_m1[which(orphan_m1$min_diff == orphan_m1$diff), c(3, 1, 4, 5, 6, 2)])

# Process images
orphan_m1 <- match_orphan2parent(orphan_m1)

# Identify best matches
orphan_best <- orphan_m1[which(abs(as.numeric(orphan_m1$match_max)) < 15 & abs(as.numeric(orphan_m1$match_min)) < 15), ]

# Compare remaining orphan images to possible parents ------------------------------------
if (nrow(orphan) == nrow(orphan_best)){
  print("All records successfully processed during round 1")
} else {
  # Process file names
  orphan_b2 <- orphan_best[, c(2, 6)]
  orphan_m2 <- process_merge_df(orphan_m1, orphan_b2)
  
  # Process images
  orphan_m2 <- match_orphan2parent(orphan_m2)
  
  # Identify best matches
  orphan_b2 <- orphan_m2[which(abs(as.numeric(orphan_m2$match_max)) < 15 & abs(as.numeric(orphan_m2$match_min)) < 15), ]
  orphan_best <- rbind(orphan_best, orphan_b2)
  
  if (nrow(orphan) == nrow(orphan_best)){
    print("All records successfully processed during round 2")
  } else {
    # Process file names
    orphan_b3 <- orphan_b2[, c(2, 6)]
    orphan_m3 <- process_merge_df(orphan_m2, orphan_b3)
    
    # Process images
    orphan_m3 <- match_orphan2parent(orphan_m3)
    
    # Identify best matches
    orphan_b3 <- orphan_m3[which(abs(as.numeric(orphan_m3$match_max)) < 15 & abs(as.numeric(orphan_m3$match_min)) < 15), ]
    orphan_best <- rbind(orphan_best, orphan_b3)
  }
}

if (nrow(orphan) != nrow(orphan_best)) {stop("still outstanding orphan records to match!")}
## UNCOMMENT SECTION IF MANUAL PROCESSING NEEDED
# # Process file names
# orphan_b4 <- unique(orphan_m3[, c(2, 6)])
# orphan_b4$min_diff <- orphan_b4$min_diff + 1
# orphan_m4 <- merge(orphan_b4, orphan_e, by = "filt_image_file")
# orphan_m4 <- unique(orphan_m4[which(orphan_m4$min_diff == orphan_m4$diff), c(3, 1, 4, 5, 6, 2)])
# orphan_m4 <- orphan_m4[(row.names(orphan_m4) == 12222 | row.names(orphan_m4) == 50317), ]
# 
# # Process images
# orphan_m4 <- match_orphan2parent(orphan_m4)
# 
# # Identify best matches
# orphan_b4 <- orphan_m4[which(abs(as.numeric(orphan_m4$match_max)) < 15 & abs(as.numeric(orphan_m4$match_min)) < 15), ]
# orphan_best <- rbind(orphan_best, orphan_b4)
# rm(orphan_b4, orphan_m4)

## UNCOMMENT SECTION IF MANUAL PROCESSING NEEDED
# # Process file names
# orphan_b5 <- unique(orphan_m4[, c(2, 6)])
# orphan_b5$min_diff <- orphan_b5$min_diff + 1
# orphan_m5 <- merge(orphan_b5, orphan_e, by = "filt_image_file")
# orphan_m5 <- unique(orphan_m5[which(orphan_m5$min_diff == orphan_m5$diff), c(3, 1, 4, 5, 6, 2)])
# orphan_m5 <- orphan_m5[(row.names(orphan_m5) == 12222 | row.names(orphan_m5) == 50317), ]
# 
# # Process images
# orphan_m5 <- match_orphan2parent(orphan_m5)
# 
# # Identify best matches
# orphan_b5 <- orphan_m5[which(abs(as.numeric(orphan_m5$match_max)) < 15 & abs(as.numeric(orphan_m5$match_min)) < 15), ]
# orphan_best <- rbind(orphan_best, orphan_b5)
# rm(orphan_b5, orphan_m5)

rm(orphan_e, orphan_m2, orphan_m1, orphan_b2, orphan_b3, orphan_m3)

# Correct image file name and detection CSV ---------------------------------------------
# Create new file names
orphan_best$filt_dir <- dirname(orphan_best$filt_image_file)
orphan_best$old_image_file_c <- basename(orphan_best$filt_image_file)
orphan_best$old_image_file_t_png <- gsub("COLOR-8-BIT.JPG", "THERM-16BIT.PNG", basename(orphan_best$filt_image_file))
orphan_best$old_image_file_t_jpg <- gsub("C???OLOR-8-BIT.JPG", "THERM-8-BIT.JPG", basename(orphan_best$filt_image_file))
orphan_best$new_image_file_c <- paste(filt_image_prefix, substr(sapply(strsplit(orphan_best$effort_dt, "-"),'[',2), 3, 8), sapply(strsplit(orphan_best$effort_dt, "-"),'[',3), "COLOR-8-BIT.JPG", sep = "_")
orphan_best$new_image_file_t_png <- paste(filt_image_prefix, substr(sapply(strsplit(orphan_best$effort_dt, "-"),'[',2), 3, 8), sapply(strsplit(orphan_best$effort_dt, "-"),'[',3), "THERM-16BIT.PNG", sep = "_")
orphan_best$new_image_file_t_jpg <- paste(filt_image_prefix, substr(sapply(strsplit(orphan_best$effort_dt, "-"),'[',2), 3, 8), sapply(strsplit(orphan_best$effort_dt, "-"),'[',3), "THERM-8-BIT.JPG", sep = "_")
orphan_best$new_timestamp <- paste(sapply(strsplit(orphan_best$effort_dt, "-"),'[',2), sapply(strsplit(orphan_best$effort_dt, "-"),'[',3), "GMT", sep = "") 
orphan_best$filt_dir_thumb <- paste(dirname(orphan_best$filt_dir), "THUMBNAILS", sep = "/")

# Update detection CSV
detect_file <- paste(dirname(dirname(orphan_best$filt_image_file[1])), '/', basename(dirname(dirname(orphan_best$filt_image_file[1]))), '.csv', sep = "")
detect <- read.csv(detect_file, stringsAsFactors = FALSE)
detect <- merge(detect, orphan_best, by.x = "color_image_name", by.y = "old_image_file_c", all.x = TRUE)
detect$new_thumbnail <- ifelse(is.na(detect$new_image_file_c), NA,
                               paste(paste(filt_image_prefix, substr(sapply(strsplit(detect$effort_dt, "-"),'[',2), 3, 8), sapply(strsplit(detect$effort_dt, "-"),'[',3), sep = "_"), "COLOR-8-BIT-", detect$hotspot_id, ".JPG", sep = ""))
detect <- detect[, c(2:5, 1, 6:37)]

for_v_p <- unique(detect[which(!is.na(detect$new_image_file_c)), c(2, 6:7, 32:37)])

detect$timestamp <- ifelse(!is.na(detect$new_timestamp), detect$new_timestamp, detect$timestamp)
detect$thermal_image_name <- ifelse(!is.na(detect$new_image_file_t_png), detect$new_image_file_t_png, detect$thermal_image_name)
detect$color_image_name <- ifelse(!is.na(detect$new_image_file_c), detect$new_image_file_c, detect$color_image_name)
detect <- detect[, c(1:20)]

detect_old <- paste(dirname(dirname(orphan_best$filt_image_file[1])), '/', basename(dirname(dirname(orphan_best$filt_image_file[1]))), '.old_FromOrphan', sep = "")
file.rename(detect_file, detect_old)
write.csv(detect, detect_file, row.names = FALSE, quote = FALSE)

# Change image file names
for (i in 1:nrow(orphan_best)){
  color_from <- paste(orphan_best$filt_dir[i], orphan_best$old_image_file_c[i], sep = "/")
  color_to <- paste(orphan_best$filt_dir[i], orphan_best$new_image_file_c[i], sep = "/")
  therm_png_from <- paste(orphan_best$filt_dir[i], orphan_best$old_image_file_t_png[i], sep = "/")
  therm_png_to <- paste(orphan_best$filt_dir[i], orphan_best$new_image_file_t_png[i], sep = "/")
  therm_jpg_from <- paste(orphan_best$filt_dir[i], orphan_best$old_image_file_t_jpg[i], sep = "/")
  therm_jpg_to <- paste(orphan_best$filt_dir[i], orphan_best$new_image_file_t_jpg[i], sep = "/")

  file.rename(color_from, color_to)
  file.rename(therm_png_from, therm_png_to)
  file.rename(therm_jpg_from, therm_jpg_to)
}
rm(i, detect_file, detect_old)

write.csv(orphan_best, paste(dirname(dirname(orphan_best$filt_image_file[1])), '/', filt_image_prefix, '_orphanCorrections.csv', sep = ""), row.names = FALSE, quote = FALSE)

# Process retroactive data ---------------------------------------------------------------
if (process_type == "retroactive"){
  # Update valid CSV (thermal, color, timestamp)
  valid_file <- as.character(dbGetQuery(con, paste("SELECT DISTINCT valid_file FROM surv_chess.tbl_valid WHERE valid_file LIKE \'%", dir, "%\'", sep = "")))
  valid <- read.csv(valid_file, stringsAsFactors = FALSE)
  valid <- merge(valid, for_v_p, by = c("frame_index_ind", "x_pos", "y_pos"), all.x = TRUE)
  
  # Continue processing if any orphans match up to valid data
  if (sum(!is.na(valid$new_image_file_c)) != 0) {
    valid$timestamp <- ifelse(!is.na(valid$new_timestamp), valid$new_timestamp, valid$timestamp)
    valid$thermal_image_name <- ifelse(!is.na(valid$new_image_file_t_png), valid$new_image_file_t_png, valid$thermal_image_name)
    valid$color_image_name <- ifelse(!is.na(valid$new_image_file_c), valid$new_image_file_c, valid$color_image_name)
    valid <- valid[, c(4, 1, 5:7, 2:3, 8:20)]
    
    valid_old <- gsub("CSV|csv", "old_FromOrphan", valid_file)
    file.rename(valid_file, valid_old)
    
    write.csv(valid, valid_file, row.names = FALSE, quote = FALSE)
    
    # Update process CSV (thermal, color, timestamp, thumbnail name)
    process_file <- as.character(dbGetQuery(con, paste("SELECT DISTINCT process_file FROM surv_chess.tbl_process WHERE process_file LIKE \'%", dir, "%\'", sep = "")))
    process <- read.csv(process_file, stringsAsFactors = FALSE)
    process <- merge(process, for_v_p, by = c("frame_index_ind", "x_pos", "y_pos"), all.x = TRUE)
    thumbnail <- process[which(!is.na(process$new_thumbnail)), ]

    process$timestamp <- ifelse(!is.na(process$new_timestamp), process$new_timestamp, process$timestamp)
    process$thermal_image_name <- ifelse(!is.na(process$new_image_file_t_png), process$new_image_file_t_png, process$thermal_image_name)
    process$color_image_name <- ifelse(!is.na(process$new_image_file_c), ifelse(process$MATCH_UNCERTAIN == "", process$new_image_file_c, process$color_image_name), process$color_image_name)
    process$THUMB_NAME <- ifelse(!is.na(process$new_thumbnail), ifelse(process$MATCH_UNCERTAIN == "", process$new_thumbnail, process$color_image_name), process$THUMB_NAME)
    process <- process[, c(4, 1, 5:7, 2:3, 8:37)]

    process_old <- gsub("CSV|csv", "old_FromOrphan", process_file)
    
    file.rename(process_file, process_old)

    write.csv(process, process_file, row.names = FALSE, quote = FALSE)
    
    # Change image file names
    
    for (i in 1:nrow(thumbnail)){
      thumb_from <- paste(thumbnail$filt_dir_thumb[i], thumbnail$THUMB_NAME[i], sep = "/")
      thumb_to <- paste(thumbnail$filt_dir_thumb[i], thumbnail$new_thumbnail[i], sep = "/")
      
      file.rename(thumb_from, thumb_to)
    }
    rm(i, thumb_from, thumb_to)
  } else {
    print("No processing of valid and process CSVs required")
  }
}

# Disconnect for database and delete unnecessary variables ------------------------------
RPostgreSQL::dbDisconnect(con)
rm(con, color_from, color_to, therm_jpg_from, therm_jpg_to, therm_png_from, therm_png_to, for_v_p, valid, process, detect)
