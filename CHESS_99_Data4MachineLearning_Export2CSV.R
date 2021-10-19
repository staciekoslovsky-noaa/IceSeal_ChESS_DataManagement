# CHESS: Create image lists for copying to external HD for Microsoft
# S. Hardy, 17OCT2018

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

testset_c <- RPostgreSQL::dbGetQuery(con, "SELECT DISTINCT color_image AS color FROM surv_chess.alg_images_testset")
testset_t <- RPostgreSQL::dbGetQuery(con, "SELECT DISTINCT thermal_image AS thermal FROM surv_chess.alg_images_testset")

training_animals_c <- RPostgreSQL::dbGetQuery(con, "SELECT DISTINCT REPLACE(color_path || '/' || color_image, '/', '\') AS color FROM surv_chess.alg_images_traininganimals")
training_animals_t <- RPostgreSQL::dbGetQuery(con, "SELECT DISTINCT REPLACE(thermal_path || '/' || thermal_image, '/', '\') AS thermal FROM surv_chess.alg_images_traininganimals")

potential_animals_c <- RPostgreSQL::dbGetQuery(con, "SELECT DISTINCT REPLACE(color_path || '/' || color_image, '/', '\') AS color FROM surv_chess.alg_images_potentialanimals")
potential_animals_t <- RPostgreSQL::dbGetQuery(con, "SELECT DISTINCT REPLACE(thermal_path || '/' || thermal_image, '/', '\') AS thermal FROM surv_chess.alg_images_potentialanimals")

training_background_c <- RPostgreSQL::dbGetQuery(con, "SELECT DISTINCT REPLACE(color_path || '/' || color_image, '/', '\') AS color FROM surv_chess.alg_images_trainingbackground")
training_background_t <- RPostgreSQL::dbGetQuery(con, "SELECT DISTINCT REPLACE(thermal_path || '/' || thermal_image, '/', '\') AS thermal FROM surv_chess.alg_images_trainingbackground")

write.table(testset_c, "//akc0SS-N086/NMML_Users/Stacie.Hardy/Work/Projects/AS_CHESS/RFI/201810_Morris_TestSetImages4Microsoft/TestSet_ColorImages.csv", row.names = FALSE, col.names = FALSE)
write.table(testset_t, "//akc0SS-N086/NMML_Users/Stacie.Hardy/Work/Projects/AS_CHESS/RFI/201810_Morris_TestSetImages4Microsoft/TestSet_ThermalImages.csv", row.names = FALSE, col.names = FALSE)

write.table(training_animals_c, "//akc0SS-N086/NMML_Users/Stacie.Hardy/Work/Projects/AS_CHESS/RFI/201810_Moreland_ImagesByCategory4Microsoft/TrainingAnimals_ColorImages.csv", row.names = FALSE, col.names = FALSE)
write.table(training_animals_t, "//akc0SS-N086/NMML_Users/Stacie.Hardy/Work/Projects/AS_CHESS/RFI/201810_Moreland_ImagesByCategory4Microsoft/TrainingAnimals_ThermalImages.csv", row.names = FALSE, col.names = FALSE)
write.table(potential_animals_c, "//akc0SS-N086/NMML_Users/Stacie.Hardy/Work/Projects/AS_CHESS/RFI/201810_Moreland_ImagesByCategory4Microsoft/PotentialAnimals_ColorImages.csv", row.names = FALSE, col.names = FALSE)
write.table(potential_animals_t, "//akc0SS-N086/NMML_Users/Stacie.Hardy/Work/Projects/AS_CHESS/RFI/201810_Moreland_ImagesByCategory4Microsoft/PotentialAnimals_ThermalImages.csv", row.names = FALSE, col.names = FALSE)
write.table(training_background_c, "//akc0SS-N086/NMML_Users/Stacie.Hardy/Work/Projects/AS_CHESS/RFI/201810_Moreland_ImagesByCategory4Microsoft/TrainingBackground_ColorImages.csv", row.names = FALSE, col.names = FALSE)
write.table(training_background_t, "//akc0SS-N086/NMML_Users/Stacie.Hardy/Work/Projects/AS_CHESS/RFI/201810_Moreland_ImagesByCategory4Microsoft/TrainingBackground_ThermalImages.csv", row.names = FALSE, col.names = FALSE)