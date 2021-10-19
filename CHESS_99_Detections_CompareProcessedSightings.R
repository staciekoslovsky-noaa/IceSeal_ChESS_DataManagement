# VIAME Detections: Process sightings and compare to DB
# S. Hardy, 1APR2019

# Variables ------------------------------------------------------
process_yolo <- "//AKC0SS-N086/NMML_Users/Stacie.Hardy/Work/Projects/AS_Detections/Data/chess12S_yolo_ir20_rgb80_20190401_processed.csv"

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

# Function to create similar date-time field between two files...that matches ChESS DB
process_dt_unfilt <- function(df, img, dt) {
  df$fl <- gsub("_", "", substr(df[[img]], 16, 21))
  df[[dt]] <- ifelse(substr(substr(df[[img]], 23, 40), 1, 1) == 2, 
                     substr(df[[img]], 23, 40), 
                     substr(df[[img]], 24, 41))
  df[[dt]] <- as.character(paste(df$fl, substr(df[[dt]], 1, 8), substr(df[[dt]], 9, 18), sep = "-"))
  df <- subset(df, select = -c(fl))
  return(df)
}

# Install libraries ----------------------------------------------
install_pkg("RPostgreSQL")

# Process data --------------------------------------------------
# Read 1st processed file
yolo <- read.csv(process_yolo, header = FALSE, stringsAsFactors = FALSE)
colnames(yolo) <- c("hotspot", "frame_rgb", "xmin", "ymin", "xmax", "ymax", "not_sure", "confidence", "unsure", "type", "confidence2")
yolo <- process_dt_unfilt(yolo, "frame_rgb", "unfilt_dt")
yolo <- yolo[which(yolo$type == "ringed_seal" | yolo$type == "beareded_seal"), ]
yolo_count <- data.frame(table(yolo$unfilt_dt))
colnames(yolo_count) <- c("unfilt_dt", "num_seals_yolo")

# Get sightings from DB
con <- RPostgreSQL::dbConnect(PostgreSQL(),
                              dbname = Sys.getenv("pep_db"),
                              host = Sys.getenv("pep_ip"),
                              user = Sys.getenv("pep_admin"),
                              rstudioapi::askForPassword(paste("Enter your DB password for user account: ", Sys.getenv("pep_admin"), sep = "")))

seals <- RPostgreSQL::dbGetQuery(con, "SELECT hotspot_id, color_image_name, species_id, thumb_left, thumb_top, thumb_right, thumb_bottom, process_dt_c
                                        FROM surv_chess.tbl_process
                                        WHERE process_dt_c LIKE 'FL12S%'
                                        AND species_id LIKE '%Seal%'")
                                        #AND process_dt_c = process_dt_t
seals_count <- data.frame(table(seals$process_dt_c))
colnames(seals_count) <- c("unfilt_dt", "num_seals_db")
                                        
match <- merge(seals, yolo, by.x = "process_dt_c", by.y = "unfilt_dt", all.x = TRUE, all.y = TRUE)
missed_db <- match[which(is.na(match$color_image_name)), ]
missed_yolo <- match[which(is.na(match$frame_rgb)), ]
counts <-merge(yolo_count, seals_count, by = "unfilt_dt", all = TRUE)
counts[is.na(counts)] <- 0
counts$diff_y2db <- counts$num_seals_yolo - counts$num_seals_db
counts_diff <- counts[which(counts$diff_y2db != 0), ]
