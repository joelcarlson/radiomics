
add_to_rlm <- function(runs, rlm, max_run_length){
  #Intermediate function, not meant to be called directly
  # adds matching rows and columns from rle tables to rlm
  
  mrow <- match(rownames(runs), rownames(rlm))
  mrow <- mrow[which(!is.na(mrow))] 
  mcol <-  match(colnames(runs), colnames(rlm))
  mcol <- mcol[which(!is.na(mcol))] 
  
  rlm[mrow,mcol] <- rlm[mrow,mcol] + runs[,which(as.numeric(colnames(runs)) <= max_run_length)]
  return(rlm)
}


#' Gray level run length matrix.
#'
#' \code{glrlm} returns a gray level run length matrix for a given matrix.
#'
#' This function can be used alone, or can be executed and it's textural
#' features automatically calculated using \code{calc_features}.
#' 
#' @param image A numeric image matrix.
#' @param angle One of "0", "45", "90" or "135", the direction the run is calculated.
#' @param n_grey an integer value, the number of grey levels the image should
#'   be quantized into.
#' @param max_run_length An integer value, the default is the maximum possible
#'   run length. Setting it to a smaller value truncates the output. Desirable
#'   in cases where the matrix is extremely sparse, for example when
#'   there are few long runs.
#' @param ... Can be given verbose=FALSE to suppress output from the n_grey conversion.       
#' @return a matrix of dimension n_grey by run length, the GLRLM. The column 
#'   names represent the length of the run, and row names represent 
#'   grey values in the image.
#'   See Galloway 1974
#'   (\url{http://www.sciencedirect.com/science/article/pii/S0146664X75800086}) for details.   
#'
#' @examples
#' hallbey
#' glrlm(hallbey)
#' glrlm(hallbey, angle="90") 

glrlm <- function(image, angle="0", n_grey=32, max_run_length = min(dim(image)), ...){
  #discretize image only if n_grey is different from unique grey values in img
  if( ! identical( n_grey, as.numeric(length( unique( c(image) ) )) )){ 
    image <- discretizeImage(image, n_grey=n_grey, ...)
  }
  
  #initialize rlm
  unique_vals <- sort(unique(c(image)))
  rlm <- matrix(0, nrow=length(unique_vals), ncol=max_run_length)
  rownames(rlm) <- unique_vals
  colnames(rlm) <- c( 1:max_run_length )
  
  if(identical( angle, "45") | identical(angle, "135") ){
    if(identical(angle, "45")) image <- image[ nrow(image):1, ]
    
    # check rle of each diagonal of the matrix, add to rlm
    # start at bottom left, work to top right
    bottom <- nrow(image) - 1
    top <- -ncol(image) + 1
    for(i in bottom:top){
      
      runs <- t(table(rle(image[row(image)==(col(image) + i)])))
      rlm <- add_to_rlm(runs, rlm, max_run_length)
      
    }
    
  } else if (identical(angle, "0") | identical(angle, "90")){
    if(identical(angle, "90")) image <- t(image)
    
    for(i in 1:nrow(image)){
      
      runs <- t(table(rle(image[i,])))
      rlm <- add_to_rlm(runs, rlm, max_run_length)
      
    }
    
  } else {
    stop("Shift must be one of '0', '45', '90', '135'.")
  }
  
  return(rlm)
  
}
