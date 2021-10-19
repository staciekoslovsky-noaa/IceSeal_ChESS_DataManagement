data <- read.csv("//akc0SS-N086/NMML_Users/Stacie.Hardy/Work/Projects/AS_CHESS/RFI/201706_Moreland_ProcessedData/process_20170627.csv", stringsAsFactors = FALSE)
data <- data[which(data$hotspot_type == "Animal"), ]
data$id <- 1:nrow(data)

# Create thermal file
data_thermal <- data[, c("id", "thermal_image_name", "x_pos", "y_pos", "hotspot_type", "species_id", "species_confidence")]
data_thermal$x_min <- data_thermal$x_pos - 5
data_thermal$x_max <- data_thermal$x_pos + 5
data_thermal$y_min <- data_thermal$y_pos - 5
data_thermal$y_max <- data_thermal$y_pos + 5
data_thermal0 <- data_thermal[which(data_thermal$x_pos == 0 | data_thermal$y_pos == 0), ]
data_thermal <- data_thermal[which(data_thermal$x_pos != 0 & data_thermal$y_pos != 0), ]
data_thermal$animal_confidence <- as.numeric(1.0)
data_thermal$species_confidence <- as.numeric(ifelse(data_thermal$species_confidence == "100%", "1.0", 
                                                   ifelse(data_thermal$species_confidence == "Likely", "0.75", "0.5")))
data_thermal$species_id <- ifelse(data_thermal$species_id == "Ringed Seal", "RingedSeal", data_thermal$species_id)
data_thermal$species_id <- ifelse(data_thermal$species_id == "Bearded Seal", "BeardedSeal", data_thermal$species_id)
data_thermal$species_id <- ifelse(data_thermal$species_id == "Polar Bear", "PolarBear", data_thermal$species_id)
data_thermal$species_id <- ifelse(data_thermal$species_id == "UNK Animal", "UNKAnimal", data_thermal$species_id)
data_thermal$species_id <- ifelse(data_thermal$species_id == "UNK Seal", "UNKSeal", data_thermal$species_id)
data_thermal$species_id <- ifelse(data_thermal$species_id == "Spotted Seal", "SpottedSeal", data_thermal$species_id)
data_thermal0 <- data_thermal0[, c("id", "thermal_image_name", "x_min", "y_min", "x_max", "y_max", "species_confidence", "species_id")]
data_thermal <- data_thermal[, c("id", "thermal_image_name", "x_min", "y_min", "x_max", "y_max", "animal_confidence", "species_id", "species_confidence")]

write.csv(data_thermal, "D:/images_thermal.csv", row.names = FALSE, col.names  = FALSE)
write.csv(data_thermal0, "D:/Ice_Seal/Chess4Training0_thermal/images_thermal0.csv")

# Create color file
data_color <- data[, c("id", "color_image_name", "thumb_left", "thumb_top", "thumb_right", "thumb_bottom", "species_confidence", "species_id")]
data_color$thumb_left <- data_color$thumb_left + 200
data_color$thumb_top <- data_color$thumb_top + 200
data_color$thumb_right <- data_color$thumb_right - 200
data_color$thumb_bottom <- data_color$thumb_bottom - 200
data_color$animal_confidence <- as.numeric(1.0)
data_color$species_confidence <- as.numeric(ifelse(data_color$species_confidence == "100%", "1.0", 
                                                   ifelse(data_color$species_confidence == "Likely", "0.75", "0.5")))
data_color$species_id <- ifelse(data_color$species_id == "Ringed Seal", "RingedSeal", data_color$species_id)
data_color$species_id <- ifelse(data_color$species_id == "Bearded Seal", "BeardedSeal", data_color$species_id)
data_color$species_id <- ifelse(data_color$species_id == "Polar Bear", "PolarBear", data_color$species_id)
data_color$species_id <- ifelse(data_color$species_id == "UNK Animal", "UNKAnimal", data_color$species_id)
data_color$species_id <- ifelse(data_color$species_id == "UNK Seal", "UNKSeal", data_color$species_id)
data_color$species_id <- ifelse(data_color$species_id == "Spotted Seal", "SpottedSeal", data_color$species_id)
data_color <- data_color[, c("id", "color_image_name", "thumb_left", "thumb_top", "thumb_right", "thumb_bottom", "animal_confidence", "species_id", "species_confidence")]
write.csv(data_color, "D:/images_color.csv", row.names = FALSE, col.names  = FALSE)

# Copy images
images <- data.frame(full_file = list.files("D:/Chess Kaggle DS", pattern = "PNG$", full.names = TRUE), stringsAsFactors = FALSE)
images$image_name <- basename(images$full_file)

data_images <- merge(data_thermal, images, by.x = "thermal_image_name", by.y = "image_name")
data0_images <- merge(data_thermal0, images, by.x = "thermal_image_name", by.y = "image_name")

file.copy(data_images$full_file, "D:/Chess4Training")
file.copy(data0_images$full_file, "D:/Chess4Training0")