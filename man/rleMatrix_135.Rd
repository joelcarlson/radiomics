\name{rleMatrix_135}
\alias{rleMatrix_135}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
rleMatrix_135(image)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{image}{
%%     ~~Describe \code{image} here~~
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
function (image) 
{
    image <- image[, ncol(image):1]
    image <- t(image)
    image <- image[, ncol(image):1]
    if (nrow(image) > ncol(image)) {
        buffer_matrix <- matrix(NA, ncol = (nrow(image) - ncol(image)), 
            nrow = nrow(image))
        image <- cbind(image, buffer_matrix)
    }
    else if (ncol(image) > nrow(image)) {
        buffer_matrix <- matrix(NA, nrow = (ncol(image) - nrow(image)), 
            ncol = ncol(image))
        image <- rbind(image, buffer_matrix)
    }
    rle_matrix <- matrix(rep(NA, length(diag(image)) * (nrow(image) + 
        ncol(image) - 1)), ncol = length(diag(image)))
    for (i in 1:nrow(rle_matrix)) {
        if (i == 1) {
            diag_vals <- image[1, ncol(image)]
        }
        else if (i <= nrow(image)) {
            diag_vals <- c(diag(image[1:i, (ncol(image) - i + 
                1):ncol(image)]))
        }
        else {
            image <- image[-1, -ncol(image)]
            if (length(image) > 1) {
                diag_vals <- c(diag(image))
            }
            else {
                diag_vals <- c(image)
            }
        }
        rle_matrix[i, 1:length(diag_vals)] <- diag_vals
    }
    NA_rows <- apply(rle_matrix, 1, function(x) all(is.na(x)))
    NA_cols <- apply(rle_matrix, 2, function(x) all(is.na(x)))
    return(rle_matrix[!NA_rows, !NA_cols])
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
