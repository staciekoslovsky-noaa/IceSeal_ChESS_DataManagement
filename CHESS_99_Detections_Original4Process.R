# VIAME Detections: Process IR and RGB CSVs for single review
# S. Hardy, 1APR2019

# Variables ------------------------------------------------------
ir_file <- "//AKC0SS-N086/NMML_Users/Stacie.Hardy/Desktop/chess12S_yolo_ir_20190328_original.csv"
rgb_file <- "//AKC0SS-N086/NMML_Users/Stacie.Hardy/Desktop/chess12S_yolo_rgb_20190328_original.csv"
export_file <- "//AKC0SS-N086/NMML_Users/Stacie.Hardy/Desktop/chess12S_yolo_ir20_rgb80_20190401_original.csv"

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
# Process data --------------------------------------------------
# Read IR file and process
ir <- read.csv(ir_file, header = FALSE, stringsAsFactors = FALSE)
colnames(ir) <- c("hotspot_ir", "frame_ir", "xmin", "ymin", "xmax", "ymax", "not_sure", "confidence_ir", "unsure", "type_ir", "confidence2")
ir <- process_dt_unfilt(ir, "frame_ir", "unfilt_dt")
ir <- ir[, c("hotspot_ir", "frame_ir", "confidence_ir", "type_ir", "unfilt_dt")]
ir_images <- ir[which(ir$confidence_ir > 0.2), ]
ir_images <- unique(ir_images$unfilt_dt)

# Read RGB file and process
fields <- max(count.fields(rgb_file, sep = ','))
rgb <- read.csv(rgb_file, header = FALSE, stringsAsFactors = FALSE, col.names = paste("V", seq_len(fields)))
if(fields == 11) {
  colnames(rgb) <- c("hotspot_rgb", "frame_rgb", "xmin", "ymin", "xmax", "ymax", "not_sure", "confidence_rgb", "unsure", "type_rgb", "confidence2")
} else if (fields == 13) {
  colnames(rgb) <- c("hotspot_rgb", "frame_rgb", "xmin", "ymin", "xmax", "ymax", "not_sure", "confidence_rgb", "unsure", "type_rgb", "confidence2", "type_rgb_x1", "confidence2_x1")
  rgb$confidence2_x1 <- ifelse(is.na(rgb$confidence2_x1), 0, rgb$confidence2_x1)
  rgb$type_rgb <- ifelse(rgb$confidence_rgb == rgb$confidence2_x1, rgb$type_rgb_x1, rgb$type_rgb)
  rgb$confidence2 <- ifelse(rgb$confidence_rgb == rgb$confidence2_x1, rgb$confidence2_x1, rgb$confidence2)
  rgb <- rgb[, c("hotspot_rgb", "frame_rgb", "xmin", "ymin", "xmax", "ymax", "not_sure", "confidence_rgb", "unsure", "type_rgb", "confidence2")]
}
rm(fields)
rgb <- process_dt_unfilt(rgb, "frame_rgb", "unfilt_dt")
rgb <- rgb[which(rgb$confidence_rgb > 0.8), ]

# Create RGB list from IR frames with detections
rgb_ir <- rgb[rgb$unfilt_dt %in% ir_images, ]
rgb_ir <- rgb_ir[, c("hotspot_rgb", "frame_rgb", "xmin", "ymin", "xmax", "ymax", "not_sure", "confidence_rgb", "unsure", "type_rgb", "confidence2")]
write.csv(rgb_ir, export_file, col.names = FALSE, row.names = FALSE)
