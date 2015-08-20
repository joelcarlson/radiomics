#GLSZM

#Quantifies size zone matrices in an image
# See here: http://thibault.biz/Research/ThibaultMatrices/GLSZM/GLSZM.html

#Needs access to reshape2 and spatstat 

#' Gray level size zone matrix.
#'
#' \code{glszm} returns a gray level size zone matrix for a given matrix.
#'
#' This function can be used alone, or can be executed and it's textural
#' features automatically calculated using \code{calc_features}.
#' 
#' @param image A numeric image matrix.
#' @param n_grey an integer value, the number of grey levels the image should
#'   be quantized into.
#' @param ... Can be given verbose=FALSE to suppress output from the n_grey conversion.       
#' @return a matrix of dimension n_grey by region size, the GLSZM. The column 
#'   names represent the region size, row names represent grey level, and 
#'   the entries represent the count of how many times a given size of given grey level
#'   occur.

#'   See (\url{http://thibault.biz/Research/ThibaultMatrices/GLSZM/GLSZM.html}) for details.   
#'
#' @examples
#' image(psf)
#' glszm(psf)
#' 
#' image(discretizeImage(psf, n_grey=5, verbose=F))
#' glszm(psf, n_grey=5, verbose=F) 

glszm <- function(image, n_grey=32, ...){
  #discretize image only if n_grey is different from unique grey values in img
  if( ! identical( n_grey, as.numeric(length( unique( c(image) ) )) )){ 
    image <- discretizeImage(image, n_grey=n_grey, ...)
  }
  grey_lvls <- unique(c(image))
  grey_lvls <- grey_lvls[-is.na(grey_lvls)]
  #convert to image for use with spatstats functions
  image <- spatstat::as.im(image)
  
  #Initialize dataframe to hold count data
  count_data <- data.frame()
  
  
  for(i in grey_lvls){
    # Threshold the image
    imBinary <- spatstat::levelset(image, i, compare="==")
    connections <- spatstat::connected(imBinary)
    
    # Extract counts of each uniqe value 
    counts <- table(table(as.matrix(connections)))
    count_data <- rbind(count_data, data.frame(i, counts))
  }
  
  #Clean up names 
  colnames(count_data) <- c("greylvl", "size", "counts")
  #cast to matrix
  count_data <- reshape2::acast(count_data, greylvl~size, value.var="counts")
  #sort columns, if there is only a single size a vector is returned, hence the if
  if(length(colnames(count_data)) > 1){
    count_data <- count_data[,order(as.numeric(as.character(colnames(count_data))))]
  }
  count_data[is.na(count_data)] <- 0
  

  return(count_data)
}
