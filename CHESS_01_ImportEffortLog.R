# CHESS: Import effort log
# S. Hardy, 19MAY2017

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

dat <- read.csv("//akc0SS-N086/NMML_Users/Stacie.Hardy/Work/Projects/AS_CHESS/Data/CHESS2016_EffortLog_Master.csv")
dat <- dat[, c(3:8)]
dat$id <- rownames(dat)
dat <- dat[,c(7, 1:6)]

RPostgreSQL::dbWriteTable(con, c("surv_chess", "tbl_effort_log"), dat, append = TRUE, row.names = FALSE)