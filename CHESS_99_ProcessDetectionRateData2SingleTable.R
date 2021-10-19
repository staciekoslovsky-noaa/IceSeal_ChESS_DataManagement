# CHESS: Process data for detection rate to DB
# S. Hardy, 14SEP2017

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

# Run code -------------------------------------------------------
# Read in image list
images <- read.table("//akc0SS-N086/NMML_Users/Stacie.Hardy/Work/Projects/AS_CHESS/RFI/201707_Richmond_HotspotSubset/selected-log 15-06-36_UncooledDetectionSample.txt", header = FALSE, stringsAsFactors = FALSE)
colnames(images) <- "path"
images$unfilt_image <- basename(images$path)
images <- images[which(grepl('COLOR', images$unfilt_image)), ]

process_dt_unfilt <- function(df, img, dt) {
  df$fl <- gsub("_", "", substr(df[[img]], 16, 21))
  df[[dt]] <- ifelse(substr(substr(df[[img]], 23, 40), 1, 1) == 2, 
                     substr(df[[img]], 23, 40), 
                     substr(df[[img]], 24, 41))
  df[[dt]] <- as.character(paste(df$fl, substr(df[[dt]], 1, 8), substr(df[[dt]], 9, 18), sep = "-"))
  df <- subset(df, select = -c(fl))
  return(df)
}

images <- process_dt_unfilt(images, "unfilt_image", "unfilt_dt")
images <- images[, c("unfilt_image", "unfilt_dt")]

# Read in image list where bear tracks detected
bear <- read.csv("//akc0SS-N086/NMML_Users/Stacie.Hardy/Work/Projects/AS_CHESS/Data/DetectionRate_Carter/bearTracks4import_carter.csv", stringsAsFactors = FALSE)
bear$unfilt_image <- paste(bear$unfilt_image, ".JPG", sep = "")
bear$detect_bear_track <- 'Y'
bear <- bear[, c("unfilt_image", "detect_bear_track", "bear_track_comments")]
bear[bear == ""] <- NA

# Read in image list where seals detected
seal <- read.csv("//akc0SS-N086/NMML_Users/Stacie.Hardy/Work/Projects/AS_CHESS/Data/DetectionRate_Carter/seals4import_carter.csv", stringsAsFactors = FALSE)
seal$detect_seal <- 'Y'
seal <- seal[, c("unfilt_image", "detect_seal", "seal_id", "seal_sighting_comments")]
seal[seal == ""] <- NA

# Push data to DB
con <- RPostgreSQL::dbConnect(PostgreSQL(), 
                              dbname = Sys.getenv("pep_db"), 
                              host = Sys.getenv("pep_ip"), 
                              user = Sys.getenv("pep_admin"), 
                              rstudioapi::askForPassword(paste("Enter your DB password for user account: ", Sys.getenv("pep_admin"), sep = "")))
RPostgreSQL::dbWriteTable(con, c("surv_chess", "tbl_unfilt_detect_rate_images"), images, overwrite = TRUE, row.names = FALSE)
RPostgreSQL::dbWriteTable(con, c("surv_chess", "tbl_unfilt_detect_rate_beartrk"), bear, overwrite = TRUE, row.names = FALSE)
RPostgreSQL::dbWriteTable(con, c("surv_chess", "tbl_unfilt_detect_rate_seal"), seal, overwrite = TRUE, row.names = FALSE)
