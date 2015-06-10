\name{glcm_autoCorrelation}
\alias{glcm_autoCorrelation}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
glcm_autoCorrelation(glcm)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{glcm}{
%%     ~~Describe \code{glcm} here~~
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
function (glcm) 
{
    grey_levels <- seq(1, nrow(glcm), 1)
    grid <- expand.grid(grey_levels, grey_levels)
    scale_matrix <- matrix(grid[, 1] * grid[, 2], nrow = nrow(glcm))
    return(sum(glcm * scale_matrix))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
