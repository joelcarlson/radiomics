\name{discretizeImage2}
\alias{discretizeImage2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
discretizeImage2(image, n_grey = 32)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{image}{
%%     ~~Describe \code{image} here~~
}
  \item{n_grey}{
%%     ~~Describe \code{n_grey} here~~
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
function (image, n_grey = 32) 
{
    l_unique <- length(unique(c(image)))
    if (identical(n_grey, l_unique)) {
        return(image)
    }
    else if (n_grey > l_unique) {
        message(sprintf("n_grey (\%d) cannot be larger than the number of gray levels in the image (\%d)", 
            n_grey, l_unique))
        message(sprintf("n_grey set to \%d", l_unique))
        return(image)
    }
    break_vals <- seq(min(image, na.rm = TRUE), max(image, na.rm = TRUE), 
        length.out = (n_grey + 1))
    label_vals <- round(break_vals[-1], 4)
    discretized <- as.numeric(as.character(cut(image, breaks = break_vals, 
        labels = label_vals, include.lowest = TRUE, right = FALSE)))
    return(matrix(discretized, nrow = nrow(image)))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
