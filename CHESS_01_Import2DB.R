# CHESS: Process CSV files into single tables
# S. Hardy, 01DEC2017

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

# Install libraries ----------------------------------------------
install_pkg("RPostgreSQL")
install_pkg("lubridate")
install_pkg("dplyr")

options(digits.secs=6)

# Run code -------------------------------------------------------
# Set initial working directory
wd <- "//nmfs/akc-nmml/NMML_CHESS_Imagery"
setwd(wd)

# Create list of camera folders within which data need to be processed ----------------
dir <- list.dirs(wd, full.names = FALSE, recursive = FALSE)
dir <- data.frame(path = dir[grep("FL", dir)], stringsAsFactors = FALSE)
dir <- merge(dir, c("left", "middle", "right"), ALL = true)
colnames(dir) <- c("path", "cameraLoc")
dir$camera_dir <- paste(wd, dir$path, dir$cameraLoc, sep = "/")

# Create table of effort file names ---------------------------------------------------
effortDir <- data.frame(effort_dir = "", stringsAsFactors = FALSE)
for (i in 1:nrow(dir)){
  wd <- dir$camera_dir[[i]]
  df <- data.frame(effort_dir = list.dirs(wd, full.names = TRUE, recursive = FALSE), stringsAsFactors = FALSE)
  effortDir <- rbind(effortDir, df)
}
rm(df, i, dir, wd)
effortDir$effort_csv <- paste(effortDir$effort_dir, "/", basename(effortDir$effort_dir), ".csv", sep = "")
effortDir <- effortDir[!(effortDir$effort_dir == "" |
                             grepl("target", effortDir$effort_dir) |
                             grepl("test", effortDir$effort_dir)),]

# Read and append effort CSV ---------------------------------------------------------
effort <- read.table(effortDir$effort_csv[[1]], sep = ",", header = TRUE, stringsAsFactors = FALSE)
effort$effort_csv <- effortDir$effort_csv[[1]]
for (i in 2:nrow(effortDir)){
  df <- read.table(effortDir$effort_csv[[i]], sep = ",", header = TRUE, stringsAsFactors = FALSE)
  df$effort_csv <- effortDir$effort_csv[[i]]
  effort <- rbind(effort, df)
  }
rm(df, i)
effort$hotspot_id <- as.character(effort$hotspot_id)

# Create list of images within unfiltered folders -------------------------------------
unfilt <- data.frame(unfilt_image = "", stringsAsFactors = FALSE)
for (i in 1:nrow(effortDir)){
  wd <- paste(effortDir$effort_dir[[i]], "/UNFILTERED", sep = "")
  df <- data.frame(unfilt_image = list.files(wd, pattern = "jpg|png", full.names = TRUE, ignore.case = TRUE, recursive = FALSE), stringsAsFactors = FALSE)
  unfilt <- rbind(unfilt, df)
}
rm(df, i, wd)
unfilt <- data.frame(unfilt_image = unfilt[which(unfilt$unfilt_image != ""),], stringsAsFactors = FALSE)
unfilt$image_type <- ifelse(grepl("COLOR", unfilt$unfilt_image) == TRUE, "COLOR", "THERMAL")

# Create list of detection folders within which data need to be processed -------------
detectDir <- data.frame(detect_dir = "", stringsAsFactors = FALSE)
for (i in 1:nrow(effortDir)){
  wd <- paste(effortDir$effort_dir[[i]], "/UNFILTERED", sep = "")
  if (identical(dir(wd, pattern = "^detection", recursive = FALSE), character(0)) == FALSE) {
    df <- data.frame(detect_dir = dir(wd, pattern = "^detection", full.names = TRUE, recursive = FALSE), stringsAsFactors = FALSE)
    detectDir <- rbind(detectDir, df)
  }
}
rm(df, i)
detectDir <- data.frame(detect_dir = detectDir[!(detectDir$detect_dir == ""),], stringsAsFactors = FALSE)

# Get CSV files listed under UNFILTERED/detections... folders
detectFiles <- data.frame(detect_csv = "", stringsAsFactors = FALSE)
for (i in 1:nrow(detectDir)){
  wd <- detectDir$detect_dir[[i]]
  if (identical(list.files(wd, pattern = "csv", ignore.case = TRUE, recursive = FALSE), character(0)) == FALSE) {
    df <- data.frame(detect_csv = list.files(wd, pattern = "csv", full.names = TRUE, ignore.case = TRUE, recursive = FALSE), stringsAsFactors = FALSE)
    detectFiles <- rbind(detectFiles, df)
  }
}
rm(df, i, wd)
detectFiles <- data.frame(detect_csv = detectFiles[!(detectFiles$detect_csv == ""),], stringsAsFactors = FALSE)
detectFiles$file_type <- ifelse(grepl("^detections", basename(detectFiles$detect_csv)) == TRUE, "detection",
                                ifelse(grepl("^valid", basename(detectFiles$detect_csv)) == TRUE, "valid",
                                       ifelse(grepl("^Processed", basename(detectFiles$detect_csv)) == TRUE, "processed", "unknown")))
detectFiles$file_type <- ifelse(grepl("InProg", basename(detectFiles$detect_csv)) == TRUE, "inProgress", detectFiles$file_type)

# Read and append detection CSV -------------------------------------------------------
dfDet <- detectFiles[which(detectFiles$file_type == "detection"),]
detect <- read.table(dfDet$detect_csv[[1]], sep = ",", header = TRUE, stringsAsFactors = FALSE)
detect$detect_csv <- dfDet$detect_csv[[1]]
for (i in 2:nrow(dfDet)){
  df <- read.table(dfDet$detect_csv[[i]], sep = ",", header = TRUE, stringsAsFactors = FALSE)
  df$detect_csv <- dfDet$detect_csv[[i]]
  detect <- rbind(detect, df)
}
rm(df, i, dfDet)
detect$hotspot_id <- as.character(detect$hotspot_id)

# Read and append valid CSV -----------------------------------------------------------
dfVal <- detectFiles[which(detectFiles$file_type == "valid"),]
valid <- read.table(dfVal$detect_csv[[1]], sep = ",", header = TRUE, stringsAsFactors = FALSE)
valid$valid_file <- dfVal$detect_csv[[1]]
for (i in 2:nrow(dfVal)){
  df <- read.table(dfVal$detect_csv[[i]], sep = ",", header = TRUE, stringsAsFactors = FALSE)
  df <- df[, c(1:20)]
  df$valid_file <- dfVal$detect_csv[[i]]
  valid <- rbind(valid, df)
}
rm(df, i, dfVal)
valid$hotspot_id <- as.character(valid$hotspot_id)

# Read and append processed CSV -------------------------------------------------------
dfProc <- detectFiles[which(detectFiles$file_type == "processed"),]
process <- read.table(dfProc$detect_csv[[1]], sep = ",", header = TRUE, stringsAsFactors = FALSE)
process$process_file <- dfProc$detect_csv[[1]]
for (i in 2:nrow(dfProc)){
  df <- read.table(dfProc$detect_csv[[i]], sep = ",", header = TRUE, stringsAsFactors = FALSE)
  df <- df[, c(1:37)]
  df$process_file <- dfProc$detect_csv[[i]]
  process <- rbind(process, df)
}
rm(df, i, dfProc)
process$reviewer <- toupper(substr(basename(process$process_file), 11, 13))
process$hotspot_id <- as.character(process$hotspot_id)

# Create list of images within filtered folders ---------------------------------------
detectDir$filt_dir <- paste(detectDir$detect_dir, "/FILTERED", sep = "")
filt <- data.frame(filt_image = "", stringsAsFactors = FALSE)
for (i in 1:nrow(detectDir)){
  wd <- detectDir$filt_dir[[i]]
  df <- data.frame(filt_image = list.files(wd, pattern = "jpg|png", ignore.case = TRUE, full.names = TRUE, recursive = FALSE), stringsAsFactors = FALSE)
  filt <- rbind(filt, df)
}
rm(df, i, wd)
filt <- data.frame(filt_image = filt[which(filt$filt_image != ""),], stringsAsFactors = FALSE)
filt$image_type <- ifelse(grepl("COLOR", filt$filt_image) == TRUE, "COLOR", "THERMAL")

rm(detectFiles, detectDir, effortDir)

# Process datasets to simplify DB queries and QA/QC -----------------------------------
# Create functions
process_dt_unfilt <- function(df, img, dt) {
  df$fl <- gsub("_", "", substr(df[[img]], 16, 21))
  df[[dt]] <- ifelse(substr(substr(df[[img]], 23, 40), 1, 1) == 2, 
                      substr(df[[img]], 23, 40), 
                      substr(df[[img]], 24, 41))
  df[[dt]] <- as.character(paste(df$fl, substr(df[[dt]], 1, 8), substr(df[[dt]], 9, 18), sep = "-"))
  df <- subset(df, select = -c(fl))
  return(df)
}
process_dt_filt <- function(df, img, dt) {
  df$fl <- gsub("_", "", substr(df[[img]], 7, 12))
  df[[dt]] <- ifelse(substr(substr(df[[img]], 13, 29), 1, 1) == 1, 
                          substr(df[[img]], 13, 29), 
                          substr(df[[img]], 14, 30))
  df[[dt]] <-  as.character(paste(df$fl, paste("20", substr(df[[dt]], 1, 6), sep = ""), substr(df[[dt]], 8, 17), sep = "-"))
  df <- subset(df, select = -c(fl))
  return(df)
}
process_lat_long <- function(df, lat_old, lat_new, long_old, long_new, long_dir) {
  df[[lat_old]][which(is.na(df[[lat_old]]))] <- 0
  df[[lat_new]] <- as.numeric(substr(df[[lat_old]], 1, 2)) + as.numeric(substr(df[[lat_old]], 3, 8))/60
  df[[lat_new]][which(is.na(df[[lat_new]]))] <- 0
  
  df[[long_old]][which(is.na(df[[long_old]]))] <- 0
  df[[long_new]] <- as.numeric(substr(df[[long_old]], 1, 3)) + as.numeric(substr(df[[long_old]], 4, 9))/60
  df[[long_new]] <- ifelse(df[[long_dir]] == "E", df[[long_new]], df[[long_new]] * (-1))
  df[[long_new]][which(is.na(df[[long_new]]))] <- 0
  return(df)
}

# Process data
effort$id <- row.names(effort)
effort$effort_image <- effort$image_name
effort$effort_image_file <- paste(dirname(effort$effort_csv), "UNFILTERED", effort$image, sep = "/")
effort$effort_image_dir <- dirname(effort$effort_csv)
effort <- process_dt_unfilt(effort, "effort_image", "effort_dt")
effort$flight_num <- gsub('FL', '', basename(dirname(dirname(effort$effort_image_dir))))
effort$camera_loc <- substr(effort$effort_dt, nchar(effort$effort_dt)-21+1, nchar(effort$effort_dt)-20)
effort <- effort[, c(18, 23:24, 1:17, 19:22)]
effort <- process_lat_long(effort, "RMC_LAT", "latitude", "RMC_LON", "longitude", "RMC_EW")
colnames(effort) <- tolower(colnames(effort))
effort[effort == ''] <- NA
effort[effort == 'NULL'] <- NA

detect$id <- row.names(detect)
detect <- detect[, c(22, 1:21)]
detect$detect_image_c <- detect$color_image_name
detect$detect_image_t <- detect$thermal_image_name
detect$detect_image_file <- paste(dirname(detect$detect_csv), "FILTERED", detect$image, sep = "/")
detect$detect_image_dir <- dirname(detect$detect_csv)
detect <- process_dt_filt(detect, "detect_image_c", "detect_dt_c")
detect <- process_dt_filt(detect, "detect_image_t", "detect_dt_t")
detect <- process_lat_long(detect, "RMC_LAT", "latitude", "RMC_LON", "longitude", "RMC_EW")
colnames(detect) <- tolower(colnames(detect))
detect[detect == ''] <- NA
detect[detect == 'NULL'] <- NA

valid$id <- row.names(valid)
valid <- valid[, c(22, 1:21)]
valid$valid_image_c <- valid$color_image_name
valid$valid_image_t <- valid$thermal_image_name
valid <- process_dt_filt(valid, "valid_image_c", "valid_dt_c")
valid <- process_dt_filt(valid, "valid_image_t", "valid_dt_t")
valid <- process_lat_long(valid, "RMC_LAT", "latitude", "RMC_LON", "longitude", "RMC_EW")
colnames(valid) <- tolower(colnames(valid))
valid[valid == ''] <- NA
valid[valid == 'NULL'] <- NA

process$id <- row.names(process)
process <- process[, c(40, 1:39)]
process$process_image_c <- process$color_image_name
process$process_image_t <- process$thermal_image_name
process <- process_dt_filt(process, "process_image_c", "process_dt_c")
process <- process_dt_filt(process, "process_image_t", "process_dt_t")
process <- process_lat_long(process, "RMC_LAT", "latitude", "RMC_LON", "longitude", "RMC_EW")
colnames(process) <- tolower(colnames(process))
process[process == ''] <- NA
process[process == 'NULL'] <- NA
process$fog <- ifelse(is.na(process$fog), 'No', process$fog)
process$species_id <- ifelse(process$species_id == 'Unknown animal', 'UNK Animal', process$species_id)
process$calculated_yaw_angle <- ifelse(process$calculated_yaw_angle > 0, 0, process$calculated_yaw_angle)

filt$id <- row.names(filt)
filt <- filt[, c(3, 1:2)]
filt$filt_image_dir <- dirname(filt$filt_image)
filt$filt_image <- basename(filt$filt_image)
filt <- process_dt_filt(filt, "filt_image", "filt_dt")
colnames(filt) <- tolower(colnames(filt))
filt[filt == ''] <- NA
filt[filt == 'NULL'] <- NA

unfilt$id <- row.names(unfilt)
unfilt <- unfilt[, c(3, 1:2)]
unfilt$unfilt_image_dir <- dirname(unfilt$unfilt_image)
unfilt$unfilt_image <- basename(unfilt$unfilt_image)
unfilt <- process_dt_unfilt(unfilt, "unfilt_image", "unfilt_dt")
colnames(unfilt) <- tolower(colnames(unfilt))
unfilt[unfilt == ''] <- NA
unfilt[unfilt == 'NULL'] <- NA

# Add correct_dt field to effort data, so data sort and merge with covariates properly
effort$id <- as.numeric(effort$id)
effort <- effort[order(effort$effort_csv, effort$id), ]
effort$id_csv <- as.numeric(ave(effort$id, effort$effort_csv, FUN = seq_along))
effort <- effort[order(effort$effort_csv, effort$image_name), ]
effort$id_dt <- as.numeric(ave(effort$image_name, effort$effort_csv, FUN = seq_along))
effort$diff <- as.numeric(effort$id_csv) - as.numeric(effort$id_dt)
effort <- effort[order(effort$effort_csv, effort$id_csv),]

effort$date <- as.Date(sapply(strsplit(effort$effort_dt, split='-'),'[', 2), "%Y%m%d")
effort$time <- sapply(strsplit(effort$effort_dt, split='-'),'[', 3)
effort$time <- paste(substr(effort$time, 1, 2), ":", substr(effort$time, 3, 4), ":", substr(effort$time, 5, 10), sep = "")
effort$correct_dt <- as.POSIXct(paste(effort$date, effort$time), format="%Y-%m-%d %H:%M:%OS")

effort <- mutate(effort, last_dt = lag(correct_dt))
effort$last_dt <- as.POSIXct(effort$last_dt, format="%Y-%m-%d %H:%M:%OS", timezone = "GMT")
effort <- transform(effort, next_dt = c(correct_dt[-1], NA))
effort$next_dt <- as.POSIXct(effort$next_dt, format="%Y-%m-%d %H:%M:%OS", timezone = "GMT")

effort$new_dt <- (effort$next_dt - effort$last_dt)/2 + effort$last_dt
effort$correct_dt <- ifelse(effort$diff > 0, effort$new_dt, effort$correct_dt)
effort$correct_dt <- as.POSIXct(effort$correct_dt, origin = '1970-01-01', timezone = 'Alaska/Anchorage')
effort <- effort[, c(1:26, 32)]

# Add log and fast ice information to effort data
con <- RPostgreSQL::dbConnect(PostgreSQL(), 
                              dbname = Sys.getenv("pep_db"), 
                              host = Sys.getenv("pep_ip"), 
                              user = Sys.getenv("pep_admin"), 
                              rstudioapi::askForPassword(paste("Enter your DB password for user account: ", Sys.getenv("pep_admin"), sep = "")))

log <- RPostgreSQL::dbGetQuery(con, "SELECT flight_num, camera_loc, start_image_c, end_image_c, effort_type, weather FROM surv_chess.tbl_effort_log")
log$start_image_c <- gsub("__", "_", log$start_image_c)
log$start_dt <- sapply(strsplit(log$start_image_c, split='_'),'[', 5)
log$start_dt <- gsub("GMT", "", log$start_dt)
log$start_dt <- as.POSIXct(log$start_dt, format="%Y%m%d%H%M%OS", timezone = "GMT")
log$end_image_c <- gsub("__", "_", log$end_image_c)
log$end_dt <- sapply(strsplit(log$end_image_c, split='_'),'[', 5)
log$end_dt <- gsub("GMT", "", log$end_dt)
log$end_dt <- as.POSIXct(log$end_dt, format="%Y%m%d%H%M%OS", timezone = "GMT")

effort_log <- merge(effort, log, by = c("flight_num", "camera_loc"))
effort_log <- unique(effort_log[, c("effort_image", "correct_dt", "start_dt", "end_dt", "effort_type", "weather")])
effort_log$status <- ifelse(effort_log$correct_dt >= effort_log$start_dt, 
                            ifelse(effort_log$correct_dt <= effort_log$end_dt, "keep", "delete"), "delete")
effort_log <- effort_log[which(effort_log$status == "keep"), c(1, 5:6)]
effort_log <- unique(effort_log)

fast <- RPostgreSQL::dbGetQuery(con, "SELECT flight_num, camera_loc, start_image_c, end_image_c, ice_type FROM surv_chess.tbl_fast_ice")
fast$start_image_c <- gsub("__", "_", fast$start_image_c)
fast$start_dt <- sapply(strsplit(fast$start_image_c, split='_'),'[', 5)
fast$start_dt <- gsub("GMT", "", fast$start_dt)
fast$start_dt <- as.POSIXct(fast$start_dt, format="%Y%m%d%H%M%OS", timezone = "GMT")
fast$end_image_c <- gsub("__", "_", fast$end_image_c)
fast$end_dt <- sapply(strsplit(fast$end_image_c, split='_'),'[', 5)
fast$end_dt <- gsub("GMT", "", fast$end_dt)
fast$end_dt <- as.POSIXct(fast$end_dt, format="%Y%m%d%H%M%OS", timezone = "GMT")

effort_fast <- merge(effort, fast, by = c("flight_num", "camera_loc"))
effort_fast <- unique(effort_fast[, c("effort_image", "correct_dt", "start_dt", "end_dt", "ice_type")])
effort_fast$status <- ifelse(effort_fast$correct_dt >= effort_fast$start_dt, 
                             ifelse(effort_fast$correct_dt <= effort_fast$end_dt, "keep", "delete"), "delete")
effort_fast <- effort_fast[which(effort_fast$status == "keep"), c(1, 5)]
effort_fast <- unique(effort_fast)

effort <- merge(effort, effort_log, by = "effort_image", all = TRUE)
effort <- merge(effort, effort_fast, by = "effort_image", all = TRUE)
effort <- effort[, c(2:23, 1, 24:30)]
#effort[effort == ""] <- NA
#effort[effort == 'NULL'] <- NA
rm(effort_fast, effort_log, log, fast)
effort$effort_type <- ifelse(is.na(effort$effort_type), "OFF", effort$effort_type)
effort$weather <- ifelse(is.na(effort$weather) | effort$weather == "", "NOT SPECIFIED", effort$weather)
effort$ice_type <- ifelse(is.na(effort$ice_type), "NOT SPECIFIED", effort$ice_type)

# Export data to PostgreSQL -----------------------------------------------------------
df <- list(effort, detect, valid, process, unfilt, filt)
dat <- c("tbl_effort_raw", "tbl_detect", "tbl_valid", "tbl_process", "tbl_unfilt", "tbl_filt")

# Identify and delete dependencies for each table
for (i in 1:length(dat)){
  sql <- paste("SELECT fxn_deps_save_and_drop_dependencies(\'surv_chess\', \'", dat[i], "\')", sep = "")
  RPostgreSQL::dbSendQuery(con, sql)
  RPostgreSQL::dbClearResult(dbListResults(con)[[1]])
}
RPostgreSQL::dbSendQuery(con, "DELETE FROM deps_saved_ddl WHERE deps_ddl_to_run NOT LIKE \'%CREATE VIEW%\'")

# Push data to pepgeo database and process data to spatial datasets where appropriate
for (i in 1:length(dat)){
  RPostgreSQL::dbWriteTable(con, c("surv_chess", dat[i]), data.frame(df[i]), overwrite = TRUE, row.names = FALSE)
  if (i <= 4) {
    sql1 <- paste("ALTER TABLE surv_chess.", dat[i], " ADD COLUMN geom geometry(POINT, 4326)", sep = "")
    sql2 <- paste("UPDATE surv_chess.", dat[i], " SET geom = ST_SetSRID(ST_MakePoint(longitude, latitude), 4326)", sep = "")
    RPostgreSQL::dbSendQuery(con, sql1)
    RPostgreSQL::dbSendQuery(con, sql2)
  }
}

# Recreate table dependencies
for (i in length(dat):1) {
  sql <- paste("SELECT fxn_deps_restore_dependencies(\'surv_chess\', \'", dat[i], "\')", sep = "")
  RPostgreSQL::dbSendQuery(con, sql)
  RPostgreSQL::dbClearResult(dbListResults(con)[[1]])
}

# Disconnect for database and delete unnecessary variables ----------------------------
RPostgreSQL::dbDisconnect(con)
rm(con, df, dat, i, sql, sql1, sql2)