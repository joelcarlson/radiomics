% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/GLRLM.R
\name{glrlm}
\alias{glrlm}
\title{Gray level run length matrix.}
\usage{
glrlm(data, angle = 0, n_grey = 32, max_run_length = min(dim(data)),
  truncate = TRUE, ...)
}
\arguments{
\item{data}{A numeric 2D matrix.}

\item{angle}{One of 0, 45, 90 or 135, the direction the run is calculated.}

\item{n_grey}{an integer value, the number of grey levels the image should
be quantized into.}

\item{max_run_length}{An integer value, the default is the maximum possible
run length. Setting it to a smaller value truncates the output. Desirable
in cases where the matrix is extremely sparse, for example when
there are few long runs.}

\item{truncate}{Logical Remove run lengths which have no entries}

\item{...}{Can be given verbose=FALSE to suppress output from the n_grey conversion.}
}
\value{
a matrix of class "glrlm" of dimension n_grey by run length. The column 
  names represent the length of the run, and row names represent 
  grey values in the image.
}
\description{
\code{glrlm} returns a gray level run length matrix for a given matrix.
}
\details{
Can be visualized using \code{image(glrlm(data))}. For visualization info
see \code{?image.radiomics}
}
\examples{
\dontrun{
hallbey
glrlm(hallbey)
glrlm(hallbey, angle="90") 
}
}
\references{
\url{http://www.sciencedirect.com/science/article/pii/S0146664X75800086}
}
