#' Image Discretization.
#'
#' \code{discretizeImage} Scales the grey values of an image into a specified number of values.
#'
#' This function is called in \code{glcm}, \code{glrlm}, \code{glszm}, and \code{mglszm}.
#' 
#' @param image A numeric image matrix.
#' @param n_grey an integer value, the number of grey levels the image should
#'   be quantized into.
#' @param verbose Logical, a warning is given when the user 
#'  supplies more grey values than exist in the image. Setting this value to FALSE
#'  will suppress this warning.       
#' @return A matrix of the same dimensions as the input matrix. The entries of the matrix
#'  will be set to begin at 1, and go up to the specified value. There is no guarantee
#'  that each gray level between 1 and n_grey will have pixels of that value (for example, 
#'  although n_grey = 32 may be specified, certain images may contain fewer than 32 grey levels).
#'     
#'
#' @examples
#' image(psf)
#' image(discretizeImage(psf, n_grey=5, verbose=F))
#' 
#' image(tumor)
#' image(discretizeImage(tumor, n_grey=8, verbose=F))
#' 
 
discretizeImage <- function(image, n_grey=32, verbose=TRUE){
  #Not a perfect solution. Makes n_grey breaks, but doesn't necessarily populate all of them
  # eg. n_gey could be 100, but only 75 of the levels are used by pixels
  #Make sure discretization is valid
  l_unique <- length(unique(c(image)))
  if(n_grey > l_unique){
    if(verbose) message(sprintf("n_grey (%d) cannot be larger than the number of gray levels in the image (%d)", n_grey, l_unique))
    if(verbose) message(sprintf("n_grey set to %d", l_unique))
    n_grey <- l_unique
  } 
  
  discretized <- cut(image, breaks=seq(min(image), max(image), length.out=(n_grey + 1)),
                     labels = seq(1, n_grey, 1),
                     include.lowest=TRUE, right=FALSE) 
  return(matrix(as.numeric(discretized), nrow=nrow(image)))
}

discretizeImage2 <- function(image, n_grey=32){
  #Different from discretizeImage2 in that it preserves the grey levels of the image (roughly)
  l_unique <- length(unique(c(image)))
  if(identical(n_grey, l_unique)){
    return(image)
  } else if(n_grey > l_unique){
    message(sprintf("n_grey (%d) cannot be larger than the number of gray levels in the image (%d)", n_grey, l_unique))
    message(sprintf("n_grey set to %d", l_unique))
    return(image)
  } 
  
  
  break_vals <- seq(min(image, na.rm=TRUE), max(image, na.rm=TRUE), length.out=(n_grey + 1))
  label_vals <- round(break_vals[-1], 4)
  #need to remove the upper bound label, as we are using the floor of the values
  #label_vals <- label_vals[-length(label_vals)] 
  
  discretized <- as.numeric(as.character(cut(image, breaks=break_vals,
                                             labels = label_vals,
                                             include.lowest=TRUE, right=FALSE))) 
  return(matrix(discretized, nrow=nrow(image)))
}