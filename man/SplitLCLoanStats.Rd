\name{SplitLCLoanStats}
\alias{SplitLCLoanStats}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
SplitLCLoanStats
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
SplitLCLoanStats()
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
    split_command <- "\n     sed '1d;' LC_FILE |\n     awk -v n=1 '\n     NR == 1 {mainH = $0}\n     {x=\"LoanStats\"n;}\n     /^Loans that do not meet the current credit policy/{\n          close(x);\n     \tn++;\n     \tx=\"LoanStats\"n;\n     \t{print mainH > x};\n     \tnext;\n     \t} \n     {print > x;}\n     ' \n"
    split_command <- sub("LC_FILE", getOption("LC_LOANSTATS_FILENAME"), 
        split_command)
    system(split_command)
    file.rename(from = c("LoanStats1", "LoanStats2"), to = c("LoanStats_New", 
        "LoanStats_Old"))
    options(LC_NEW_LOANS = "LoanStats_New", LC_OLD_LOANS = "LoanStats_Old")
    setwd(WD.ORIG)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
