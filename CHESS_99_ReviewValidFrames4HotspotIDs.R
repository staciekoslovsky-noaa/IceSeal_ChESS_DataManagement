# CHESS: Process CSV files into single tables
# S. Hardy, 01DEC2017

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
install_pkg("lubridate")
install_pkg("dplyr")
install_pkg("tesseract")
install_pkg("imager")
install_pkg("magick")

options(digits.secs=6)

# Run code -------------------------------------------------------
# Set initial working directory
wd <- "//nmfs/akc-nmml/NMML_CHESS_Imagery"
setwd(wd)

# Create list of camera folders within which data need to be processed ----------------
dir <- list.dirs(wd, full.names = FALSE, recursive = FALSE)
dir <- data.frame(path = dir[grep("FL", dir)], stringsAsFactors = FALSE)
dir <- merge(dir, c("left", "middle", "right"), ALL = true)
colnames(dir) <- c("path", "cameraLoc")
dir$camera_dir <- paste(wd, dir$path, dir$cameraLoc, sep = "/")

# Create table of effort file names ---------------------------------------------------
effortDir <- data.frame(effort_dir = "", stringsAsFactors = FALSE)
for (i in 1:nrow(dir)){
  wd <- dir$camera_dir[[i]]
  df <- data.frame(effort_dir = list.dirs(wd, full.names = TRUE, recursive = FALSE), stringsAsFactors = FALSE)
  effortDir <- rbind(effortDir, df)
}
effortDir <- effortDir[!(effortDir$effort_dir == "" |
                           grepl("target", effortDir$effort_dir) |
                           grepl("test", effortDir$effort_dir)),]


# Create list of detection folders within which data need to be processed -------------
detectDir <- data.frame(detect_dir = "", stringsAsFactors = FALSE)
for (i in 1:length(effortDir)){
  wd <- paste(effortDir[[i]], "/UNFILTERED", sep = "")
  if (identical(dir(wd, pattern = "^detection", recursive = FALSE), character(0)) == FALSE) {
    df <- data.frame(detect_dir = dir(wd, pattern = "^detection", full.names = TRUE, recursive = FALSE), stringsAsFactors = FALSE)
    detectDir <- rbind(detectDir, df)
  }
}
detectDir <- data.frame(detect_dir = detectDir[!(detectDir$detect_dir == ""),], stringsAsFactors = FALSE)


# Create list of images within valid frames folders ---------------------------------------
detectDir$valid_dir <- paste(detectDir$detect_dir, "/FILTERED/valid frames", sep = "")
valid <- data.frame(valid_image = "", stringsAsFactors = FALSE)
for (i in 1:nrow(detectDir)){
  wd <- detectDir$valid_dir[[i]]
  df <- data.frame(valid_image = list.files(wd, pattern = "jpg|png", ignore.case = TRUE, full.names = TRUE, recursive = FALSE), stringsAsFactors = FALSE)
  valid <- rbind(valid, df)
}
rm(df, i, wd)
valid <- data.frame(valid_image = valid[which(valid$valid_image != ""),], stringsAsFactors = FALSE)

# Copy files for manual review -------------------------------------------------------
for (i in 1:nrow(valid)){
  file.copy(valid$valid_image[i], "C:/skh/valid_frames", overwrite = TRUE )
}


# Read OCR -------------------------------------------------------
valid_ocr <- tesseract::ocr(valid$valid_image[1])
for (i in 1:nrow(filt$filt_image)){
  
}

i <- image_read(valid$valid_image[1])
i <- image_quantize(i, colorspace = 'gray')
i <- image_negate(i)
tesseract::ocr(i)
plot(g)
image(x[,,2])






