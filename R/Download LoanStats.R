# R17: Download LoanStats.R
# ---
# Download Lending Club loanstats csv file

# source("AccessLendingClubWebsite.R")

# Set options
# if(is.null(getOption("DL_FOLDER"))) setOption("DL_FOLDER", paste(getwd(), "/LendingClubDownloads/", sep=""))
# should already be set by AccessLendingClubWebsite.R
SplitLCLoanStats <- function() {
     if(is.null(getOption("LC_DL_FOLDER"))) SetLCOptions()
     
     # Shift working directory to the download folder for the CSV file
	WD.ORIG <- getwd()
	setwd(getOption("LC_DL_FOLDER"))
	
	# Split loan file into two: one file for current credit policy loans, second file for old credit policy loans
	# Old loans come after line: "Loans that do not meet the current credit policy,

split_command <- "
     sed '1d;' LC_FILE |
     awk -v n=1 '
     NR == 1 {mainH = $0}
     {x=\"LoanStats\"n;}
     /^Loans that do not meet the current credit policy/{
          close(x);
     	n++;
     	x=\"LoanStats\"n;
     	{print mainH > x};
     	next;
     	} 
     {print > x;}
     ' 
"

split_command <- sub("LC_FILE", getOption("LC_LOANSTATS_FILENAME"), split_command) # input the actual downloaded file name
system(split_command)

# will save the new files to the working directory
# New style loans: LoanStats1
# Old style loans: LoanStats2

# rename to something more meaningful
file.rename(from = c("LoanStats1", "LoanStats2"),
			to = c("LoanStats_New", "LoanStats_Old"))

options(LC_NEW_LOANS = "LoanStats_New", LC_OLD_LOANS = "LoanStats_Old")

# Reset to original working directory
setwd(WD.ORIG)

}


CompressLCLoanStats <- function() {
     if(is.null(getOption("LC_DL_FOLDER"))) SetLCOptions()
     
     WD.ORIG <- getwd()
	setwd(getOption("LC_DL_FOLDER"))

	# GZIP
# ----
# gzip the downloaded file to save significant space
# gzip files can be opened directly in R
option_names <- c("LC_LOANSTATS_FILENAME", "LC_NEW_LOANS", "LC_OLD_LOANS")
loan_names <- options()[option_names]
new_loan_names <- lapply(loan_names, CompressFile)

# resave new (compressed) file names
options(new_loan_names)
setwd(WD.ORIG)
}

CompressFile <- function(filename) {
	if(is.null(filename)) return(filename)
	if(grepl(".gz$", filename)) return(filename)  # matches only the ending '.gz'
	
	system2(command="gzip", args=filename)
	return(paste(filename, ".gz", sep=""))
}
