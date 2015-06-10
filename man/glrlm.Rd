\name{glrlm}
\alias{glrlm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
glrlm(image, angle = "0", n_grey = 32, max_run_length = ncol(image))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{image}{
%%     ~~Describe \code{image} here~~
}
  \item{angle}{
%%     ~~Describe \code{angle} here~~
}
  \item{n_grey}{
%%     ~~Describe \code{n_grey} here~~
}
  \item{max_run_length}{
%%     ~~Describe \code{max_run_length} here~~
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
function (image, angle = "0", n_grey = 32, max_run_length = ncol(image)) 
{
    image <- discretizeImage(image, n_grey = n_grey)
    if (identical(angle, "90")) {
        image <- t(image)[, ncol(image):1]
    }
    else if (identical(angle, "45")) {
        image <- rleMatrix_45(image)
    }
    else if (identical(angle, "135")) {
        image <- rleMatrix_135(image)
    }
    else if (!identical(angle, "0")) {
        stop("Shift must be one of '0', '45', '90', '135'.")
    }
    rlencode <- Reduce(rbind, apply(image, 1, function(x) {
        encode <- rle(x)
        return(data.frame(encode$lengths, encode$values))
    }))
    unique_vals <- sort(unique(c(image)))
    count_matrix <- matrix(rep(0, length(unique_vals) * ncol(image)), 
        nrow = length(unique_vals))
    rownames(count_matrix) <- unique_vals
    colnames(count_matrix) <- 1:ncol(image)
    for (i in unique_vals) {
        for (j in 1:max_run_length) {
            count_matrix[which(rownames(count_matrix) == i), 
                j] <- nrow(rlencode[which(rlencode$encode.values == 
                i & rlencode$encode.lengths == j), ])
        }
    }
    return(count_matrix[, 1:max_run_length])
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
