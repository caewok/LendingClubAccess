\name{DownloadLCAccountNotes}
\alias{DownloadLCAccountNotes}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
DownloadLCAccountNotes
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
DownloadLCAccountNotes()
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
    do.logout <- FALSE
    dl_folder <- getOption("LC_DL_FOLDER")
    notes_filename <- getOption("LC_OWNEDNOTES_FILENAME")
    if (!("cjar" \%in\% list.files())) {
        do.logout <- TRUE
        LogIntoLC()
    }
    system(paste("curl -b cjar \"", getOption("LC_NOTES_ACTION"), 
        "\" > ", dl_folder, notes_filename, sep = ""))
    if (do.logout) 
        LogOutLC()
    system(paste("sed -i '' -e 's/,$//' ", dl_folder, notes_filename, 
        sep = ""))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
