# CHESS: Create image subsets for machine learning
# S. Hardy, 15OCT2018

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
                              user = Sys.getenv("pep_admin"), 
                              rstudioapi::askForPassword(paste("Enter your DB password for user account: ", Sys.getenv("pep_admin"), sep = "")))
animals <- RPostgreSQL::dbGetQuery(con, "SELECT DISTINCT color_image, thermal_image FROM surv_chess.alg_images_traininganimals")
background <- RPostgreSQL::dbGetQuery(con, "SELECT DISTINCT color_image, thermal_image FROM surv_chess.alg_images_trainingbackground")

rand_animal <- sample(unique(animals$color_image), 3000)
animals$selection <- ifelse(animals$color_image %in% rand_animal, "Training1", "")
rand_animal <- sample(unique(animals[which(animals$selection == ""), 1]), 1000)
animals$selection <- ifelse(animals$color_image %in% rand_animal, "Training2", animals$selection)
rand_animal <- sample(unique(animals[which(animals$selection == ""), 1]), 892)
animals$selection <- ifelse(animals$color_image %in% rand_animal, "Training3", animals$selection)
rand_animal <- sample(unique(animals[which(animals$selection == ""), 1]), 100)
animals$selection <- ifelse(animals$color_image %in% rand_animal, "Valid1", animals$selection)
rand_animal <- sample(unique(animals[which(animals$selection == ""), 1]), 100)
animals$selection <- ifelse(animals$color_image %in% rand_animal, "Valid2", animals$selection)
rand_animal <- sample(unique(animals[which(animals$selection == ""), 1]), 100)
animals$selection <- ifelse(animals$color_image %in% rand_animal, "Valid3", animals$selection)
rm(rand_animal)

rand_background <- sample(unique(background$color_image), 200)
background$selection <- ifelse(background$color_image %in% rand_background, "Training1", "")
rand_background <- sample(unique(background[which(background$selection == ""), 1]), 5000)
background$selection <- ifelse(background$color_image %in% rand_background, "Training2", background$selection)
rand_background <- sample(unique(background[which(background$selection == ""), 1]), 10000)
background$selection <- ifelse(background$color_image %in% rand_background, "Training3", background$selection)
rand_background <- sample(unique(background[which(background$selection == ""), 1]), 50)
background$selection <- ifelse(background$color_image %in% rand_background, "Valid1", background$selection)
rand_background <- sample(unique(background[which(background$selection == ""), 1]), 50)
background$selection <- ifelse(background$color_image %in% rand_background, "Valid2", background$selection)
rand_background <- sample(unique(background[which(background$selection == ""), 1]), 100)
background$selection <- ifelse(background$color_image %in% rand_background, "Valid3", background$selection)
rm(rand_background)

training1 <- rbind(animals[which(animals$selection == "Training1"), c(1,2)], background[which(background$selection == "Training1"), c(1,2)])
training2 <- rbind(animals[which(animals$selection == "Training2"), c(1,2)], background[which(background$selection == "Training2"), c(1,2)])
training3 <- rbind(animals[which(animals$selection == "Training3"), c(1,2)], background[which(background$selection == "Training3"), c(1,2)])
valid1 <- rbind(animals[which(animals$selection == "Valid1"), c(1,2)], background[which(background$selection == "Valid1"), c(1,2)])
valid2 <- rbind(animals[which(animals$selection == "Valid2"), c(1,2)], background[which(background$selection == "Valid2"), c(1,2)])
valid3 <- rbind(animals[which(animals$selection == "Valid3"), c(1,2)], background[which(background$selection == "Valid3"), c(1,2)])

RPostgreSQL::dbWriteTable(con, c("surv_chess", "alg_training1"), training1, overwrite = TRUE, row.names = FALSE)
RPostgreSQL::dbWriteTable(con, c("surv_chess", "alg_training2"), training2, overwrite = TRUE, row.names = FALSE)
RPostgreSQL::dbWriteTable(con, c("surv_chess", "alg_training3"), training3, overwrite = TRUE, row.names = FALSE)
RPostgreSQL::dbWriteTable(con, c("surv_chess", "alg_valid1"), valid1, overwrite = TRUE, row.names = FALSE)
RPostgreSQL::dbWriteTable(con, c("surv_chess", "alg_valid2"), valid2, overwrite = TRUE, row.names = FALSE)
RPostgreSQL::dbWriteTable(con, c("surv_chess", "alg_valid3"), valid3, overwrite = TRUE, row.names = FALSE)
RPostgreSQL::dbDisconnect(con)
rm(con)