\name{DownloadLCLoanStats}
\alias{DownloadLCLoanStats}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
DownloadLCLoanStats
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
DownloadLCLoanStats(split = TRUE, compress = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{split}{
%%     ~~Describe \code{split} here~~
}
  \item{compress}{
%%     ~~Describe \code{compress} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
}
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
function (split = TRUE, compress = TRUE, ...) 
{
    system2(command = "curl", args = c(paste("--url", shQuote(getOption("LC_LOANSTATS_ACTION"))), 
        paste("--output", paste(getOption("LC_DL_FOLDER"), getOption("LC_LOANSTATS_FILENAME"), 
            sep = ""))), ...)
    if (split) 
        SplitLCLoanStats()
    if (compress) 
        CompressLCLoanStats()
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
