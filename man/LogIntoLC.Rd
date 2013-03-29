\name{LogIntoLC}
\alias{LogIntoLC}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
LogIntoLC
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
LogIntoLC(username, password)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{username}{
%%     ~~Describe \code{username} here~~
}
  \item{password}{
%%     ~~Describe \code{password} here~~
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
function (username, password) 
{
    if (missing(username) | missing(password)) {
        correct <- FALSE
        n <- 0
        while (!correct) {
            username <- readline("Enter Lending Club username:")
            password <- readline("Enter Lending Club password:")
            correct <- readline(paste("You entered: \nUsername: ", 
                username, "\nPassword: ", password, "\nIs this correct?", 
                sep = ""))
            if (correct \%in\% c("Yes", "YES", "Y", "y", "", "TRUE", 
                "T", "true")) 
                correct <- TRUE
            else correct <- FALSE
            if (n > 2) 
                stop("Too many incorrect username/password entries.")
            n <- n + 1
        }
    }
    stopifnot(!is.na(username), !is.na(password))
    system(paste("curl -L --cookie-jar cjar --output /dev/null \"", 
        getOption("LC_LOGIN_PAGE"), "\"", sep = ""))
    system(paste("curl --cookie cjar --cookie-jar cjar --data \"login_email=", 
        username, "\" --data \"login_password=", password, "\" --location --output /dev/null \"", 
        getOption("LC_LOGIN_ACTION"), "\"", sep = ""))
    rm(list = c("username", "password"))
    return(1)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
