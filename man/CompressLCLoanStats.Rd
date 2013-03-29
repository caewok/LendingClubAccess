\name{CompressLCLoanStats}
\alias{CompressLCLoanStats}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
CompressLCLoanStats
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
CompressLCLoanStats()
}
%- maybe also 'usage' for other objects documented here.
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function () 
{
    WD.ORIG <- getwd()
    setwd(getOption("LC_DL_FOLDER"))
    option_names <- c("LC_LOANSTATS_FILENAME", "LC_NEW_LOANS", 
        "LC_OLD_LOANS")
    loan_names <- options()[option_names]
    new_loan_names <- lapply(loan_names, CompressFile)
    options(new_loan_names)
    setwd(WD.ORIG)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
