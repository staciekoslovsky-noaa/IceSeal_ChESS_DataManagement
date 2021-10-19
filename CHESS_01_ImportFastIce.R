# CHESS: Import fast ice data
# S. Hardy, 30MAY2017

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

# Run code -------------------------------------------------------library(RPostgreSQL)
con <- RPostgreSQL::dbConnect(PostgreSQL(), 
                              dbname = Sys.getenv("pep_db"), 
                              host = Sys.getenv("pep_ip"), 
                              user = Sys.getenv("pep_admin"), 
                              rstudioapi::askForPassword(paste("Enter your DB password for user account: ", Sys.getenv("pep_admin"), sep = "")))

dat <- read.csv("//akc0SS-N086/NMML_Users/Stacie.Hardy/Work/Projects/AS_CHESS/Data/CHESS2016_FastIce.csv")
dat$id <- rownames(dat)
dat <- dat[,c(6, 1:5)]

RPostgreSQL::dbWriteTable(con, c("surv_chess", "tbl_fast_ice"), dat, append = TRUE, row.names = FALSE)
rm(dat)
