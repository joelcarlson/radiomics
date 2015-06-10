\name{glcm_differenceEntropy}
\alias{glcm_differenceEntropy}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
glcm_differenceEntropy(glcm, base = 2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{glcm}{
%%     ~~Describe \code{glcm} here~~
}
  \item{base}{
%%     ~~Describe \code{base} here~~
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
function (glcm, base = 2) 
{
    sum <- 0
    for (i in 1:(nrow(glcm) - 1)) {
        pxy <- glcm_xminusy(glcm, i - 1)
        sum <- sum + ifelse(pxy > 0, pxy * logb(pxy, base = base), 
            0)
    }
    return(-1 * sum)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
