\name{glcm}
\alias{glcm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
glcm(image, shift = "0", d = 1, n_grey = length(unique(c(image))), normalize = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{image}{
%%     ~~Describe \code{image} here~~
}
  \item{shift}{
%%     ~~Describe \code{shift} here~~
}
  \item{d}{
%%     ~~Describe \code{d} here~~
}
  \item{n_grey}{
%%     ~~Describe \code{n_grey} here~~
}
  \item{normalize}{
%%     ~~Describe \code{normalize} here~~
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
function (image, shift = "0", d = 1, n_grey = length(unique(c(image))), 
    normalize = TRUE) 
{
    if (identical(shift, "0")) {
        shift <- c(0, 1) * d
    }
    else if (identical(shift, "45")) {
        shift <- c(-1, 1) * d
    }
    else if (identical(shift, "90")) {
        shift <- c(-1, 0) * d
    }
    else if (identical(shift, "135")) {
        shift <- c(-1, -1) * d
    }
    else {
        stop("Shift must be one of '0', '45', '90', '135'.")
    }
    if (length(dim(image)) != 2) 
        stop("Must be a 2D matrix")
    image <- discretizeImage(image, n_grey = n_grey)
    counts <- buildEmptyCountMatrix(image)
    NA_cols <- matrix(rep(NA, d * nrow(image)), ncol = d)
    image <- cbind(NA_cols, image, NA_cols)
    NA_rows <- matrix(rep(NA, d * ncol(image)), nrow = d)
    image <- rbind(NA_rows, image)
    for (i in (d + 1):nrow(image)) {
        for (j in (d + 1):(ncol(image) - d)) {
            ref_val <- as.character(image[i, j])
            neighbor_val <- as.character(image[i + shift[1], 
                j + shift[2]])
            if (is.na(ref_val) | is.na(neighbor_val)) 
                next
            counts[ref_val, neighbor_val] <- counts[ref_val, 
                neighbor_val] + 1
        }
    }
    counts <- counts + t(counts)
    ifelse(normalize, return(counts/sum(counts)), return(counts))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
