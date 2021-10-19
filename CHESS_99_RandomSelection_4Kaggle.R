# CHESS: Process images for Kaggle
# S. Hardy, 27JUN2017

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
con <- RPostgreSQL::dbConnect(PostgreSQL(), 
                              dbname = Sys.getenv("pep_db"), 
                              host = Sys.getenv("pep_ip"), 
                              user = Sys.getenv("pep_user"), 
                              rstudioapi::askForPassword(paste("Enter your DB password for user account: ", Sys.getenv("pep_user"), sep = "")))
dat <- RPostgreSQL::dbGetQuery(con, "SELECT DISTINCT p.hotspot_id, timestamp, p.thermal_image_name AS filt_thermal16, f.filt_image AS filt_thermal8, p.color_image_name AS filt_color,
                  p.x_pos, p.y_pos, thumb_left, thumb_top, thumb_right, thumb_bottom, hotspot_type, species_id, 
                  filt_image_dir || '/' || process_image_c AS filt_color_file, filt_image_dir || '/' || process_image_t AS filt_thermal16_file, filt_image_dir || '/' || f.filt_image AS filt_thermal8_file
                  FROM surv_chess.tbl_process p
                  INNER JOIN surv_chess.tbl_filt f
                  ON p.process_dt_t = f.filt_dt
                  WHERE filt_image LIKE '%THERM-8-BIT%' AND
                  (hotspot_type = 'Anomaly' OR 
                  (hotspot_type = 'Animal' AND (species_id LIKE '%Seal%' OR species_id = 'Polar Bear')))")
RPostgreSQL::dbDisconnect(con)
image <- data.frame(filt_thermal16 = unique(dat$filt_thermal16))
image$id <- rownames(image)
dat <- merge(dat, image, by = "filt_thermal16")

animal <- unique(dat[which(dat$hotspot_type == 'Animal'), c("id")])
rand_animal <- sample(animal, 2500)
dat$rand_ani <- ifelse(dat$id %in% rand_animal, "Select", "")
anomaly <- unique(dat[which(dat$hotspot_type == 'Anomaly' & dat$rand_ani == ""), c("id")])
rand_anomaly <- sample(anomaly, 2500)
dat$rand_ano <- ifelse(dat$id %in% rand_anomaly, "Select", "")
rm(animal, anomaly, image, rand_animal, rand_anomaly, con)

selected <- dat[which(dat$rand_ani == 'Select' | dat$rand_ano == 'Select'), ]
filt_t16 <- unique(selected$filt_thermal16_file)
filt_t8 <- unique(selected$filt_thermal8_file)
filt_c <- unique(selected$filt_color_file)

file.copy(filt_t16, "//nmfs/akc-nmml/NMML_CHESS_Imagery/ImagesForKaggle")
file.copy(filt_t8, "//nmfs/akc-nmml/NMML_CHESS_Imagery/ImagesForKaggle")
file.copy(filt_c, "//nmfs/akc-nmml/NMML_CHESS_Imagery/ImagesForKaggle")

selected <- selected[, c(2:3, 1, 4:13)]
write.csv(selected, "//nmfs/akc-nmml/NMML_CHESS_Imagery/ImagesForKaggle/_CHESS_ImagesSelected4Kaggle.csv", row.names = FALSE)
