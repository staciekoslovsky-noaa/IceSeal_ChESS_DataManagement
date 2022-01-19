# CHESS: Randomly select seals for species identification
# S. Hardy, 12JUN2017

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
process$ImageSet <- basename(dirname(dirname(dirname(process$process_file))))

set <- data.frame(read.csv("//akc0SS-N086/NMML_Users/Stacie.Hardy/Work/Projects/AS_CHESS/Data/CHESS_SpeciesID_Datasets_20170412_SKH.csv"), stringsAsFactors = FALSE)

process <- merge(process, set, by = "ImageSet")
process <- process[which(process$SpeciesIDSet == "SecondHalf"), c(2:48)]
#process <- process[!grepl("FL23_P", process$color_image_name), ]
process$id <- seq(length = nrow(process))

# Create random selections of seals------------------------------------------------------
rand84_clc <- sample(process[which(process$reviewer == "CLC"), c("id")], 84)
rand83_elr <- sample(process[which(process$reviewer == "ELR"), c("id")], 83)
rand83_gmb <- sample(process[which(process$reviewer == "GMB"), c("id")], 83)
process$rand250 <- ifelse(process$id %in% rand84_clc, "Select", "")
process$rand250 <- ifelse(process$id %in% rand83_elr, "Select", process$rand250)
process$rand250 <- ifelse(process$id %in% rand83_gmb, "Select", process$rand250)
rand16_clc <- sample(process[which(process$reviewer == "CLC" & process$rand250 == "" & process$species_id == "Bearded Seal"), c("id")], 16)
rand16_elr <- sample(process[which(process$reviewer == "ELR" & process$rand250 == "" & process$species_id == "Bearded Seal"), c("id")], 17)
rand17_gmb <- sample(process[which(process$reviewer == "GMB" & process$rand250 == "" & process$species_id == "Bearded Seal"), c("id")], 17)
process$rand50 <- ifelse(process$id %in% rand16_clc, "Select", "")
process$rand50 <- ifelse(process$id %in% rand16_elr, "Select", process$rand50)
process$rand50 <- ifelse(process$id %in% rand17_gmb, "Select", process$rand50)
rm(rand250, rand50)

process <- process[which(process$rand250 == "Select" | process$rand50 == "Select"),]
colnames(process)[c(12:16, 18, 22:38)] <- toupper(names(process[, c(12:16, 18, 22:38)]))
write.csv(process, "//nmfs/akc-nmml/NMML_CHESS_Imagery/Subsample_speciesID/Jun2017/ChESS_speciesIDcheck_jun2017__RandomSelection.csv", row.names = FALSE, quote = FALSE)

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
random_spd <- process[which(process$reviewer != "SPD"), c(2:38)]

# Export random selections of seals -----------------------------------------------------
write.csv(random_clc, "//nmfs/akc-nmml/NMML_CHESS_Imagery/Subsample_speciesID/Jun2017/ChESS_speciesIDcheck_jun2017_CLC.csv", row.names = FALSE, quote = FALSE)
write.csv(random_elr, "//nmfs/akc-nmml/NMML_CHESS_Imagery/Subsample_speciesID/Jun2017/ChESS_speciesIDcheck_jun2017_ELR.csv", row.names = FALSE, quote = FALSE)
write.csv(random_gmb, "//nmfs/akc-nmml/NMML_CHESS_Imagery/Subsample_speciesID/Jun2017/ChESS_speciesIDcheck_jun2017_GMB.csv", row.names = FALSE, quote = FALSE)
write.csv(random_spd, "//nmfs/akc-nmml/NMML_CHESS_Imagery/Subsample_speciesID/Jun2017/ChESS_speciesIDcheck_jun2017_SPD.csv", row.names = FALSE, quote = FALSE)

# Copy images to server for processing --------------------------------------------------
color <- paste(dirname(process$process_file), "/FILTERED/", process$process_image_c, sep = "")
thermal16 <- paste(dirname(process$process_file), "/FILTERED/", process$process_image_t, sep = "")
thermal08 <- thermal16
thermal08 <- gsub("THERM-16BIT.PNG", "THERM-8-BIT.JPG", thermal08)
thumb <- paste(dirname(process$process_file), "/THUMBNAILS/", process$thumb_name, sep = "")

file.copy(color, "//nmfs/akc-nmml/NMML_CHESS_Imagery/Subsample_speciesID/Jun2017/FILTERED")
file.copy(thermal16, "//nmfs/akc-nmml/NMML_CHESS_Imagery/Subsample_speciesID/Jun2017/FILTERED")
file.copy(thermal08, "//nmfs/akc-nmml/NMML_CHESS_Imagery/Subsample_speciesID/Jun2017/FILTERED")
file.copy(thumb, "//nmfs/akc-nmml/NMML_CHESS_Imagery/Subsample_speciesID/Jun2017/THUMBNAILS")

# Disconnect for database and delete unnecessary variables ------------------------------
dbDisconnect(con)
rm(con)