# CHESS: Randomly select seals for species identification
# S. Hardy, 23MAR2017

## Set variables !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
process_file <- "//nmfs/akc-nmml/NMML_CHESS_Imagery/FL23/left/CHESS2016_N94S_FL23_S_20160517_203944/UNFILTERED/detections_CHESS_FL23_S_6-300_200_110_70_R73/ELR_InProgress_valid_CHESS_FL23_S_set1of2.CSV"

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

# Function to filtered date/time
process_dt_filt <- function(df, img, dt) {
  df$fl <- gsub("_", "", substr(df[[img]], 7, 12))
  df[[dt]] <- ifelse(substr(substr(df[[img]], 13, 29), 1, 1) == 1, 
                     substr(df[[img]], 13, 29), 
                     substr(df[[img]], 14, 30))
  df[[dt]] <-  as.character(paste(df$fl, paste("20", substr(df[[dt]], 1, 6), sep = ""), substr(df[[dt]], 8, 17), sep = "-"))
  df <- subset(df, select = -c(fl))
  return(df)
}

# Install libraries ----------------------------------------------
install_pkg("RPostgreSQL")

# Run code -------------------------------------------------------
# Extract data from DB and CSV -----------------------------------------------------------
con <- RPostgreSQL::dbConnect(PostgreSQL(), 
                              dbname = Sys.getenv("pep_db"), 
                              host = Sys.getenv("pep_ip"), 
                              user = Sys.getenv("pep_user"), 
                              rstudioapi::askForPassword(paste("Enter your DB password for user account: ", Sys.getenv("pep_user"), sep = "")))

unfilt <- RPostgreSQL::dbGetQuery(con, "SELECT * FROM surv_chess.tbl_unfilt WHERE unfilt_image LIKE \'%COLOR%\'")
unfilt$id_row <- row.names(unfilt)
unfilt <- unfilt[, c(6, 5, 2, 4)]

dbDisconnect(con)
rm(con)

process <- read.csv(process_file, stringsAsFactors = FALSE)
process <- process_dt_filt(process, "color_image_name", "process_dt_c")

# Get neighboring images for processing -------------------------------------------------
processed <- process[which(process$MATCH_UNCERTAIN == "Yes-hot spot low" | process$MATCH_UNCERTAIN == "Yes-hot spot high"), c(1, 5, 33, 38, 28)]
processed <- merge (processed, unfilt, by.x = "process_dt_c", by.y = "unfilt_dt")
processed$id_row <- ifelse(processed$MATCH_UNCERTAIN == "Yes-hot spot low", as.integer(processed$id_row) - 1, as.integer(processed$id_row) + 1)
processed <- processed[, c(1:4, 6)]
processed <- merge(processed, unfilt, by = "id_row")
processed$new_dt <- sub("-", "_", substr(processed$unfilt_dt, nchar(processed$unfilt_dt)-17+1, nchar(processed$unfilt_dt)))
for (i in 1:nrow(processed)){
  processed$new_color_image[i] <- gsub("[0-9][0-9][0-9][0-9][0-9][0-9]_[0-9][0-9][0-9][0-9][0-9][0-9].[0-9][0-9][0-9]", processed$new_dt[i], processed$color_image_name[i])
  processed$new_thumbnail[i] <- gsub("[0-9][0-9][0-9][0-9][0-9][0-9]_[0-9][0-9][0-9][0-9][0-9][0-9].[0-9][0-9][0-9]", processed$new_dt[i], processed$THUMB_NAME[i])
}

# Copy image from unfiltered and rename -------------------------------------------------
new_path_images <- paste(dirname(process_file), "/FILTERED", sep = "")
for (i in 1:nrow(processed)) {
  file.copy(paste(processed$unfilt_image_dir[i], processed$unfilt_image[i], sep = "/"), paste(new_path_images, processed$new_color_image[i], sep = "/"), overwrite = TRUE)
}

# Update information in processed CSV and re-export -------------------------------------
processed <- processed[, c(3, 10, 11)]
process <- merge(process, processed, by = "hotspot_id", all.x = TRUE)
process$color_image_name <- ifelse(is.na(process$new_color_image), process$color_image_name, process$new_color_image)
process$THUMB_NAME <- ifelse(is.na(process$new_thumbnail), process$THUMB_NAME, process$new_thumbnail)
process <- process[, c(1:37)]

process_old <- gsub(".csv|.CSV", ".old_FromHiLo", process_file)

file.rename(process_file, process_old)
write.csv(process, process_file, row.names = FALSE, quote = FALSE)

# Delete unnecessary variables ------------------------------
rm(unfilt, i, new_path_images, process_file, process_old)