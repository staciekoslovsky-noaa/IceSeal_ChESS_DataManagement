# CHESS: Process data from species ID CSVs to single dataset for analysis
# S. Hardy, 18AUG2017

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
### No libraries required! :)

# Run code -------------------------------------------------------
# Read data from CSV --------------------------------------------------------------------
start <- data.frame(read.csv("//nmfs/akc-nmml/NMML_CHESS_Imagery/Subsample_speciesID/Jun2017/ChESS_speciesIDcheck_jun2017__RandomSelection.csv", stringsAsFactors = FALSE))
gmb <- data.frame(read.csv("//nmfs/akc-nmml/NMML_CHESS_Imagery/Subsample_speciesID/Jun2017/Processed_gmb_ChESS_speciesIDcheck_jun2017_GMB.CSV", stringsAsFactors = FALSE))
clc <- data.frame(read.csv("//nmfs/akc-nmml/NMML_CHESS_Imagery/Subsample_speciesID/Jun2017/Processed_clc_ChESS_speciesIDcheck_jun2017_CLC.CSV", stringsAsFactors = FALSE))
spd <- data.frame(read.csv("//nmfs/akc-nmml/NMML_CHESS_Imagery/Subsample_speciesID/Jun2017/Processed_spd_ChESS_speciesIDcheck_jun2017_SPD.CSV", stringsAsFactors = FALSE))
elr <- data.frame(read.csv("//nmfs/akc-nmml/NMML_CHESS_Imagery/Subsample_speciesID/Jun2017/Processed_elr_ChESS_speciesIDcheck_jun2017_ELR.CSV", stringsAsFactors = FALSE))

# Create identifiers for fields ---------------------------------------------------------
hotspot <- unique(data.frame(hotspot_id = start$hotspot_id, stringsAsFactors = FALSE))
hotspot$hotspot_number <- 1:nrow(hotspot)

image <- unique(data.frame(thermal_image_name = start$thermal_image_name, stringsAsFactors = FALSE))
image$image_number <- 1:nrow(image)

# Process individual datasets -----------------------------------------------------------
start <- start[, c("hotspot_id", "thermal_image_name", "gga_alt", "HOTSPOT_TYPE", "SPECIES_ID", "SPECIES_CONFIDENCE", "AGE_CLASS", "AGE_CLASS_CONFIDENCE", "NUMBER_OF_SEALS", "ALT_SPECIES_ID", "ALT_AGE_CLASS", "reviewer")]
gmb <- gmb[, c("hotspot_id", "thermal_image_name", "gga_alt", "HOTSPOT_TYPE", "SPECIES_ID", "SPECIES_CONFIDENCE", "AGE_CLASS", "AGE_CLASS_CONFIDENCE", "NUMBER_OF_SEALS", "ALT_SPECIES_ID", "ALT_AGE_CLASS")]
gmb$reviewer <- "GMB"
clc <- clc[, c("hotspot_id", "thermal_image_name", "gga_alt", "HOTSPOT_TYPE", "SPECIES_ID", "SPECIES_CONFIDENCE", "AGE_CLASS", "AGE_CLASS_CONFIDENCE", "NUMBER_OF_SEALS", "ALT_SPECIES_ID", "ALT_AGE_CLASS")]
clc$reviewer <- "CLC"
spd <- spd[, c("hotspot_id", "thermal_image_name", "gga_alt", "HOTSPOT_TYPE", "SPECIES_ID", "SPECIES_CONFIDENCE", "AGE_CLASS", "AGE_CLASS_CONFIDENCE", "NUMBER_OF_SEALS", "ALT_SPECIES_ID", "ALT_AGE_CLASS")]
spd$reviewer <- "SPD"
elr <- elr[, c("hotspot_id", "thermal_image_name", "gga_alt", "HOTSPOT_TYPE", "SPECIES_ID", "SPECIES_CONFIDENCE", "AGE_CLASS", "AGE_CLASS_CONFIDENCE", "NUMBER_OF_SEALS", "ALT_SPECIES_ID", "ALT_AGE_CLASS")]
elr$reviewer <- "ELR"

# Merge individual datasets -------------------------------------------------------------
data <- rbind(start, gmb, clc, spd, elr)
data <- data[order(data$hotspot_id),]

data$group_id <- 1

data$obs_id <- ifelse(data$reviewer == "CLC", 1, 
                      ifelse(data$reviewer == "GMB", 2, 
                             ifelse(data$reviewer == "ELR", 3,
                                    ifelse(data$reviewer == "SPD", 4, 0))))
data$skill_level <- 1

data <- merge(data, hotspot, by = "hotspot_id")
data <- merge(data, image, by = "thermal_image_name")

data$alt_ft <- data$gga_alt * 3.28084

data$sp_id_conf <- "x"
data$sp_id_conf <- ifelse(data$SPECIES_ID == "Ringed Seal", 
                          ifelse(data$SPECIES_CONFIDENCE == "100%", "rd1",
                                 ifelse(data$SPECIES_CONFIDENCE == "Likely", "rd2", 
                                        ifelse(data$SPECIES_CONFIDENCE == "Guess", "rd3", "rd?"))), data$sp_id_conf)
data$sp_id_conf <- ifelse(data$SPECIES_ID == "Fox", "rd3", data$sp_id_conf)
data$sp_id_conf <- ifelse(data$SPECIES_ID == "Bearded Seal", 
                          ifelse(data$SPECIES_CONFIDENCE == "100%", "bd1",
                                 ifelse(data$SPECIES_CONFIDENCE == "Likely", "bd2", 
                                        ifelse(data$SPECIES_CONFIDENCE == "Guess", "bd3", "bd?"))), data$sp_id_conf)
data$sp_id_conf <- ifelse(data$SPECIES_ID == "Ribbon Seal", 
                          ifelse(data$SPECIES_CONFIDENCE == "100%", "rn1",
                                 ifelse(data$SPECIES_CONFIDENCE == "Likely", "rn2", 
                                        ifelse(data$SPECIES_CONFIDENCE == "Guess", "rn3", "rn?"))), data$sp_id_conf)
data$sp_id_conf <- ifelse(data$SPECIES_ID == "Spotted Seal", 
                          ifelse(data$SPECIES_CONFIDENCE == "100%", "sd1",
                                 ifelse(data$SPECIES_CONFIDENCE == "Likely", "sd2", 
                                        ifelse(data$SPECIES_CONFIDENCE == "Guess", "sd3", "sd?"))), data$sp_id_conf)
data$sp_id_conf <- ifelse(data$SPECIES_ID == "UNK Seal", "unk", data$sp_id_conf)
data$sp_id_conf <- ifelse(data$SPECIES_ID == "Unknown animal", "unk", data$sp_id_conf)

data$age_class_conf <- "x"
data$age_class_conf <- ifelse(data$AGE_CLASS == "Non-Pup", 
                              ifelse(data$AGE_CLASS_CONFIDENCE == "100%", "a1",
                                     ifelse(data$AGE_CLASS_CONFIDENCE == "Likely", "a2", 
                                            ifelse(data$AGE_CLASS_CONFIDENCE == "Guess", "a3", "a?"))), data$age_class_conf)
data$age_class_conf <- ifelse(data$AGE_CLASS == "Pup", 
                              ifelse(data$AGE_CLASS_CONFIDENCE == "100%", "p1",
                                     ifelse(data$AGE_CLASS_CONFIDENCE == "Likely", "p2", 
                                            ifelse(data$AGE_CLASS_CONFIDENCE == "Guess", "p3", "p?"))), data$age_class_conf)
data$age_class_conf <- ifelse(data$AGE_CLASS == "Mom-Pup Pair", 
                              ifelse(data$AGE_CLASS_CONFIDENCE == "100%", "m1",
                                     ifelse(data$AGE_CLASS_CONFIDENCE == "Likely", "m2", 
                                            ifelse(data$AGE_CLASS_CONFIDENCE == "Guess", "m3", "m?"))), data$age_class_conf)

data$side <- ifelse(grepl("_C_", data$thermal_image_name), 1,
                    ifelse(grepl("_S_", data$thermal_image_name), 2, 3))

# Prepare code key for exported data ----------------------------------------------------
hotspot$type <- "hotspot_number"
colnames(hotspot) <- c("description", "id", "type")

image$type <- "image_number"
colnames(image) <- c("description", "id", "type")
codes <- rbind(hotspot, image)
codes <- codes[, c(3, 2, 1)]

codes <- rbind(codes, c("obs_id", "1", "CLC"))
codes <- rbind(codes, c("obs_id", "2", "GMB"))
codes <- rbind(codes, c("obs_id", "3", "ELR"))
codes <- rbind(codes, c("obs_id", "4", "SPD"))

codes <- rbind(codes, c("side", "1", "Middle"))
codes <- rbind(codes, c("side", "2", "Left"))
codes <- rbind(codes, c("side", "3", "Right"))

codes <- rbind(codes, c("sp_id_conf", "rd1", "Ringed, Positive"))
codes <- rbind(codes, c("sp_id_conf", "rd2", "Ringed, Likely"))
codes <- rbind(codes, c("sp_id_conf", "rd3", "Ringed, Guess"))

codes <- rbind(codes, c("sp_id_conf", "bd1", "Bearded, Positive"))
codes <- rbind(codes, c("sp_id_conf", "bd2", "Bearded, Likely"))
codes <- rbind(codes, c("sp_id_conf", "bd3", "Bearded, Guess"))

codes <- rbind(codes, c("sp_id_conf", "rn1", "Ribbon, Positive"))
codes <- rbind(codes, c("sp_id_conf", "rn2", "Ribbon, Likely"))
codes <- rbind(codes, c("sp_id_conf", "rn3", "Ribbon, Guess"))

codes <- rbind(codes, c("sp_id_conf", "sd1", "Spotted, Positive"))
codes <- rbind(codes, c("sp_id_conf", "sd2", "Spotted, Likely"))
codes <- rbind(codes, c("sp_id_conf", "sd3", "Spotted, Guess"))

codes <- rbind(codes, c("sp_id_conf", "unk", "UNK Seal/Unknown Animal"))
codes <- rbind(codes, c("sp_id_conf", "x", "Anomaly/Not Discernible/Evidence of Seal"))

codes <- rbind(codes, c("age_class_conf", "a1", "Non-Pup, Positive"))
codes <- rbind(codes, c("age_class_conf", "a2", "Non-Pup, Likely"))
codes <- rbind(codes, c("age_class_conf", "a3", "Non-Pup, Guess"))

codes <- rbind(codes, c("age_class_conf", "p1", "Pup, Positive"))
codes <- rbind(codes, c("age_class_conf", "p2", "Pup, Likely"))
codes <- rbind(codes, c("age_class_conf", "p3", "Pup, Guess"))

codes <- rbind(codes, c("age_class_conf", "m1", "Mom-Pup Pair, Positive"))
codes <- rbind(codes, c("age_class_conf", "m2", "Mom-Pup Pair, Likely"))
codes <- rbind(codes, c("age_class_conf", "m3", "Mom-Pup Pair, Guess"))

codes <- rbind(codes, c("age_class_conf", "x", "Anomaly/Not Discernible/Evidence of Seal"))

# Prepare data for export ---------------------------------------------------------------
export <- data[, c(16:17, 13:15, 21, 18:20)]

#write.csv(data, "C:/Stacie.Hardy/Projects/AS_CHESS/Data/CHESS_SpeciesID_QAQC_20170418_SKH.csv")
write.csv(export, "//nmfs/akc-nmml/NMML_CHESS_Imagery/Subsample_speciesID/Jun2017/CHESS_CompiledSpeciesID.csv", row.names = FALSE)
write.csv(codes, "//nmfs/akc-nmml/NMML_CHESS_Imagery/Subsample_speciesID/Jun2017/CHESS_CompiledSpeciesID_CodeKey.csv", row.names = FALSE)
