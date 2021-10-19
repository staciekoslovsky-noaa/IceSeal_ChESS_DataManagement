# CHESS: Export polar bear and associated effort data for Paul Conn's power analysis
# S. Hardy, 08NOV2017

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
install_pkg("sf")

# Run code -------------------------------------------------------
# Extract data from DB ------------------------------------------------------------------
con <- RPostgreSQL::dbConnect(PostgreSQL(), 
                              dbname = Sys.getenv("pep_db"), 
                              host = Sys.getenv("pep_ip"), 
                              user = Sys.getenv("pep_user"), 
                              rstudioapi::askForPassword(paste("Enter your DB password for user account: ", Sys.getenv("pep_user"), sep = "")))
effort <- sf::st_read_db(con, 
                         query = "SELECT DISTINCT flight_num, camera_loc, image_name, correct_dt, effort_type, gga_alt, geom 
                                  FROM surv_chess.tbl_effort_raw WHERE effort_type = \'ON\'", 
                         geom_column = "geom")
effort$gga_alt <- effort$gga_alt * 3.28084
bears <- sf::st_read_db(con, 
                        query = "SELECT flight_num, camera_loc, image_name, correct_dt, species_id, e.geom 
                                  FROM surv_chess.tbl_effort_raw e 
                                  INNER JOIN surv_chess.tbl_process p 
                                  ON e.effort_dt = p.process_dt_c 
                                  WHERE effort_type = \'ON\' 
                                  AND species_id = \'Polar Bear\' 
                                  AND hotspot_type <> \'Duplicate\'", 
                        geom_column = "geom")
tracks <- sf::st_read_db(con,
                         query = "SELECT image_name, correct_dt, CASE WHEN detect_bear_track IS NULL THEN \'N\' ELSE detect_bear_track END, geom 
                                  FROM surv_chess.tbl_unfilt_detect_rate_beartrk 
                                  RIGHT JOIN surv_chess.tbl_unfilt_detect_rate_images 
                                  USING (unfilt_image) 
                                  INNER JOIN surv_chess.tbl_effort_raw
                                  ON unfilt_dt = effort_dt",
                         geom_column = "geom")
# Cynthia and Erin do no recommend using 20160407 or 20160414 because of questionable sea ice assignments in the raw data from these dates
fastice_dates <- c("20160407", "20160414", "20160421", "20160428", "20160512", "20160519", "20160526")
for (i in 1:length(fastice_dates)){
  assign(paste("fastice_", fastice_dates[i], sep = ""),
         sf::st_read_db(con,
                        query = paste("SELECT * FROM surv_chess.geo_fastice_", fastice_dates[i], sep = ""),
                        geom_column = "geom"))
}
RPostgreSQL::dbDisconnect(con)
rm(con, install_pkg, fastice_dates)