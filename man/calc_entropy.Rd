\name{calc_entropy}
\alias{calc_entropy}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
calc_entropy(image, base = 2, nbins = length(image))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{image}{
%%     ~~Describe \code{image} here~~
}
  \item{base}{
%%     ~~Describe \code{base} here~~
}
  \item{nbins}{
%%     ~~Describe \code{nbins} here~~
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
function (image, base = 2, nbins = length(image)) 
{
    im_range <- range(image)
    cuts <- table(cut(image, seq(im_range[1], im_range[2], by = diff(im_range)/nbins), 
        include.lowest = TRUE))/length(image)
    entropy_vals <- vapply(cuts, function(x) ifelse(x != 0, x * 
        logb(x, base = base), 0), FUN.VALUE = 1)
    return(-1 * sum(entropy_vals))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
