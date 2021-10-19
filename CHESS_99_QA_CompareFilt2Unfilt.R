# CHESS: Compare filtered and unfiltered date/times
# S. Hardy, 07MAR2017

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
install_pkg("raster")
rm(install_pkg)

# Run code -------------------------------------------------------
# Extract data from DB ------------------------------------------------------------------
con <- RPostgreSQL::dbConnect(PostgreSQL(), 
                              dbname = Sys.getenv("pep_db"), 
                              host = Sys.getenv("pep_ip"), 
                              user = Sys.getenv("pep_user"), 
                              rstudioapi::askForPassword(paste("Enter your DB password for user account: ", Sys.getenv("pep_user"), sep = "")))
image_dir <- RPostgreSQL::dbGetQuery(con, "SELECT DISTINCT split_part(split_part(filt_image_dir, 'N94S_', 2), '/UNFILTERED', 1) AS image_dir FROM surv_chess.tbl_filt")

# Compare filtered images to unfiltered images ------------------------------------------
process_match <- function(match){
  match$match_max <- ""
  match$match_min <- ""
  for (j in 1:nrow(match)){
    x <- raster::raster(match$filt_image_file[j])
    y <- raster::raster(match$unfilt_image_file[j])
    z <- x-y
    match$match_max[j] <- maxValue(z)
    match$match_min[j] <- minValue(z)
    removeTmpFiles(h=0)
  }
  return(match)
}
  
for (i in 1:nrow(image_dir)){
  match <- RPostgreSQL::dbGetQuery(con, paste("SELECT DISTINCT * FROM surv_chess.qa_filt_matching_unfilt WHERE filt_image_dir LIKE \'%", image_dir$image_dir[i], "%\'", sep = ""))
  match$filt_image_file <- paste(match$filt_image_dir, match$filt_image, sep = "/")
  match$unfilt_image_file <- paste(match$unfilt_image_dir, match$unfilt_image, sep = "/")
  if(nrow(match) > 100) {
    match <- match[seq(0, nrow(match), 50), ]
    match <- process_match(match)
  } else if(nrow(match) > 10) {
    pct <- floor(nrow(match) * 0.1)
    match <- match[seq(0, nrow(match), pct), ]
    rm(pct)
    match <- process_match(match)
  } else if(nrow(match) > 0) {
    match <- match[seq(0, nrow(match), 1), ]
    match <- process_match(match)
  } else {
    # Do nothing
  }
  write.csv(match, paste("C:/Stacie.Hardy/Projects/AS_CHESS/Data/QA_Filt2Unfilt/qa_mismatchFilt2Unfilt_", image_dir$image_dir[i], ".csv", sep = ""), row.names = FALSE)
  rm(match)
}

# Disconnect for database and delete unnecessary variables ------------------------------
RPostgreSQL::dbDisconnect(con)
rm(con, i)