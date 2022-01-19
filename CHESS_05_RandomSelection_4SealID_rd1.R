# CHESS: Randomly select seals for species identification
# S. Hardy, 12APR2017

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
# Extract data from DB ------------------------------------------------------------------
con <- RPostgreSQL::dbConnect(PostgreSQL(), 
                              dbname = Sys.getenv("pep_db"), 
                              host = Sys.getenv("pep_ip"), 
                              user = Sys.getenv("pep_user"), 
                              rstudioapi::askForPassword(paste("Enter your DB password for user account: ", Sys.getenv("pep_user"), sep = "")))

process <- RPostgreSQL::dbGetQuery(con, "SELECT * FROM surv_chess.tbl_process WHERE species_id LIKE \'%Seal%\' AND hotspot_type <> \'Duplicate\'")
process$id <- row.names(process)
process$ImageSet <- basename(dirname(dirname(dirname(process$process_file))))

set <- data.frame(read.csv("C://Stacie.Hardy/Projects/AS_CHESS/Data/CHESS_SpeciesID_Datasets_20170412_SKH.csv"), stringsAsFactors = FALSE)

process <- merge(process, set, by = "ImageSet")
process <- process[which(process$SpeciesIDSet == "FirstHalf"), c(2:48)]
#process <- process[!grepl("FL23_P", process$color_image_name), ]
rownames(process) <- seq(length = nrow(process))

# Create random selections of seals------------------------------------------------------
rand250 <- sample(1:nrow(process), 250)
process$rand250 <- ifelse(row.names(process) %in% rand250, "Select", "")
rand50 <- sample(process[which(process$rand250 == "" & process$species_id == "Bearded Seal"), c("id")], 50)
process$rand50 <- ifelse(row.names(process) %in% rand50, "Select", "")
rm(rand250, rand50)

process <- process[which(process$rand250 == "Select" | process$rand50 == "Select"),]
colnames(process)[c(12:16, 18, 22:38)] <- toupper(names(process[, c(12:16, 18, 22:38)]))
# write.csv(process, "//nmfs/akc-nmml/NMML_CHESS_Imagery/Subsample_speciesID/Mar2017/ChESS_speciesIDcheck_mar2017__RandomSelection.csv", row.names = FALSE, quote = FALSE)

process$SPECIES_ID <- ""
process$HOTSPOT_TYPE <- ""
process$SPECIES_CONFIDENCE <- ""
process$NUMBER_OF_SEALS <- ""
process$AGE_CLASS <- ""
process$AGE_CLASS_CONFIDENCE <- ""
process$FOG <- ""
process$MATCH_UNCERTAIN <- ""
process$OUT_OF_FRAME <- ""
process$DISTURBANCE <- ""
process$ALT_SPECIES_ID <- ""
process$ALT_AGE_CLASS <- ""

random_clc <- process[which(process$reviewer != "CLC"), c(2:38)]
random_elr <- process[which(process$reviewer != "ELR"), c(2:38)]
random_gmb <- process[which(process$reviewer != "GMB"), c(2:38)]

# Export random selections of seals -----------------------------------------------------
# write.csv(random_clc, "//nmfs/akc-nmml/NMML_CHESS_Imagery/Subsample_speciesID/Mar2017/ChESS_speciesIDcheck_mar2017_CLC.csv", row.names = FALSE, quote = FALSE)
# write.csv(random_elr, "//nmfs/akc-nmml/NMML_CHESS_Imagery/Subsample_speciesID/Mar2017/ChESS_speciesIDcheck_mar2017_ELR.csv", row.names = FALSE, quote = FALSE)
# write.csv(random_gmb, "//nmfs/akc-nmml/NMML_CHESS_Imagery/Subsample_speciesID/Mar2017/ChESS_speciesIDcheck_mar2017_GMB.csv", row.names = FALSE, quote = FALSE)

# Copy images to server for processing --------------------------------------------------
color <- paste(dirname(process$process_file), "/FILTERED/", process$process_image_c, sep = "")
thermal16 <- paste(dirname(process$process_file), "/FILTERED/", process$process_image_t, sep = "")
thermal08 <- thermal16
thermal08 <- gsub("THERM-16BIT.PNG", "THERM-8-BIT.JPG", thermal08)
thumb <- paste(dirname(process$process_file), "/THUMBNAILS/", process$thumb_name, sep = "")

# file.copy(color, "//nmfs/akc-nmml/NMML_CHESS_Imagery/Subsample_speciesID/Mar2017/FILTERED")
# file.copy(thermal16, "//nmfs/akc-nmml/NMML_CHESS_Imagery/Subsample_speciesID/Mar2017/FILTERED")
# file.copy(thermal08, "//nmfs/akc-nmml/NMML_CHESS_Imagery/Subsample_speciesID/Mar2017/FILTERED")
# file.copy(thumb, "//nmfs/akc-nmml/NMML_CHESS_Imagery/Subsample_speciesID/Mar2017/THUMBNAILS")

# Disconnect for database and delete unnecessary variables ------------------------------
dbDisconnect(con)
rm(con)