# R17: AccessLendingClubWebsite.R
# ---
# Functions used to access the Lending Club website
# download loan stats csv, login, download notes csv for account, download listed notes csv 


# GLOBAL VARIABLES: PREDEFINED
# should ask before defining each of these
if(is.null(getOption("LC_DL_FOLDER"))) {
	dl_folder <- paste0(getwd(), "/LendingClubDownloads/")
	if(!file.exists(dl_folder)) dir.create(dl_folder)
	options(LC_DL_FOLDER = dl_folder)
	}
if(is.null(getOption("LC_LOANSTATS_FILENAME"))) options(LC_LOANSTATS_FILENAME = "LoanStats.csv")
if(is.null(getOption("LC_OWNEDNOTES_FILENAME"))) options(LC_OWNEDNOTES_FILENAME = paste("Notes_", Sys.Date(), ".csv", sep=""))
if(is.null(getOption("LC_BROWSENOTES_FILENAME"))) options(LC_BROWSENOTES_FILENAME = "BrowseNotes.csv")

# Lending Club website pages
if(is.null(getOption("LC_LOANSTATS_ACTION"))) options(LC_LOANSTATS_ACTION = "https://www.lendingclub.com/fileDownload.action?file=LoanStats.csv&type=gen")
if(is.null(getOption("LC_LOGIN_PAGE"))) options(LC_LOGIN_PAGE = "https://www.lendingclub.com/account/gotoLogin.action")
if(is.null(getOption("LC_LOGIN_ACTION"))) options(LC_LOGIN_ACTION = "https://www.lendingclub.com/account/login.action")
if(is.null(getOption("LC_NOTES_ACTION"))) options(LC_NOTES_ACTION = "https://www.lendingclub.com/account/notesRawData.action")
if(is.null(getOption("LC_BROWSE_ACTION"))) options(LC_BROWSE_ACTION = "https://www.lendingclub.com/browse/browseNotesRawDataV3.action")

# Function: DownloadLCLoanStats
# --------
# Uses curl to download the loan statistics file from Lending Club webpage
# Does not require a login
# the '...' passes commands to system2
DownloadLCLoanStats <- function(split=TRUE, compress=TRUE, ...) {
     
     # remove the old file?
     
     # Use curl to retrieve file
     system2(command = "curl", args=c(paste("--url", shQuote(getOption("LC_LOANSTATS_ACTION"))), 
                                      paste("--output", paste(getOption("LC_DL_FOLDER"), getOption("LC_LOANSTATS_FILENAME"), sep=""))),
                            	...) 
     
     if(split) SplitLCLoanStats()
     if(compress) CompressLCLoanStats()
}


# Function: LogIntoLC
# --------
# Uses curl to login to the Lending Club Account
# The username / password should be saved in an encrypted file in the working directory
# Alternatively, you can pass the username / password directly (this is less secure!)
# The function will call helper R files to create the encrypted user / pass as necessary
# WARNING: user name and password will be in R's memory in plaintext for a limited period
# After calling this function, a cookie (cjar) will be saved to the working directory, to allow additional calls to curl using the login information
LogIntoLC <- function(username, password) { 
     if(missing(username) | missing(password)) {
          #if(!("LendingClubLoginInfo.Rdata.gpg" %in% list.files())) source("EncryptLendingClubUserAndPassword.R")    
          #source("DecryptLendingClubUserAndPassword.R")          
       correct <- FALSE; n <- 0
       while(!correct) {
         username <- readline("Enter Lending Club username:")
         password <- readline("Enter Lending Club password:")
         
         correct <- readline(paste("You entered: \nUsername: ", username, "\nPassword: ", password, "\nIs this correct?", sep=""))
         if(correct %in% c("Yes", "YES", "Y", "y", "", "TRUE", "T", "true")) correct <- TRUE else correct <- FALSE
         if(n > 2) stop("Too many incorrect username/password entries.")
         n <- n+1
       }
       
     }   
     stopifnot(!is.na(username), !is.na(password))
     
     system(paste('curl -L --cookie-jar cjar --output /dev/null "', getOption("LC_LOGIN_PAGE"), '"', sep=""))
     system(paste('curl --cookie cjar --cookie-jar cjar --data "login_email=', username, 
                  '" --data "login_password=', password, 
                  '" --location --output /dev/null "', 
                  getOption("LC_LOGIN_ACTION"), '"', sep=""))
     rm(list=c("username", "password")) # shouldn't be strictly necessary, as these should be local to the function we are exiting
     
     return(1)
}


# FUNCTION: DownloadLCAccountNotes
# --------
# Must first call LogIntoLC()
# Download owned notes CSV file for the account
DownloadLCAccountNotes <- function() {     
     do.logout <- FALSE
     dl_folder <- getOption("LC_DL_FOLDER")
     notes_filename <- getOption("LC_OWNEDNOTES_FILENAME")
     
     # Uses existing cookie if available; otherwise creates new login cookie
     # warning: may not work if too much time has passed since last login; if so, call LogOutLC()
     if(!("cjar" %in% list.files())) {
     	do.logout <- TRUE
     	LogIntoLC()
     }
     
     system(paste('curl -b cjar "', getOption("LC_NOTES_ACTION"), '" > ', dl_folder, notes_filename, sep=""))
    if(do.logout) LogOutLC()
      
     # the CSV has a flaw: it includes a comma at the end of each line, while R expects no comma
     # Use sed to delete all ending commas
     # -i '' will make the changes to the file in-place (w/o backup)
     system(paste("sed -i '' -e 's/,$//' ", dl_folder, notes_filename, sep=""))

}


# FUNCTION: DownloadLCOfferedNotes
# --------
# Must first call LogIntoLC()
# Download offered notes CSV file for the account
DownloadLCOfferedNotes <- function() {
	 do.logout <- FALSE
	  dl_folder <- getOption("LC_DL_FOLDER")
     notes_filename <- getOption("LC_BROWSENOTES_FILENAME")
       
     # Uses existing cookie if available; otherwise creates new login cookie
     # warning: may not work if too much time has passed since last login; if so, call LogOutLC()
     if(!("cjar" %in% list.files())) {
     	do.logout <- TRUE
     	LogIntoLC()
     	}
     
     system(paste('curl -b cjar "', getOption("LC_BROWSE_ACTION"), '" > ', dl_folder, notes_filename, sep=""))
      if(do.logout) LogOutLC()
      
     # the CSV has a flaw: it includes a comma at the end of each line, while R expects no comma
     # Use sed to delete all ending commas
     system(paste("sed -i '' -e 's/,$//' ", dl_folder, notes_filename, sep=""))
	
}
     

# FUNCTION: LogOutLC
# --------
# To wipe the login cookie for the site, remove the cjar file
# Does not logout in the traditional sense of selecting "Log Out" from the LC website menu
LogOutLC <- function() {
     system("rm cjar")
}
