# GLCM 

buildEmptyCountMatrix <- function(image){
  ###
  # build a matrix such that the column and rownames 
  # represent pairs of pixel values present in sample
  ###
  unique_vals <- sort(unique(c(image)))
  count_matrix <- matrix(rep(0, length(unique_vals)^2), nrow=length(unique_vals))
  rownames(count_matrix) <- unique_vals 
  colnames(count_matrix) <- unique_vals
  return(count_matrix)
}



glcm <- function(image, angle="0", d=1, n_grey=length(unique(c(image))), normalize=TRUE){
  #
  #Given an image matrix and angle, calculate glcm
  #allows angles of 0 (0,1), 45 (-1,1), 90 (-1,0), 135 (-1,-1)
  # d is distance
  if(identical(angle, "0")){
    angle <- c(0,1)*d
  } else if (identical(angle, "45")){
    angle <- c(-1,1)*d
  } else if (identical(angle, "90")){
    angle <- c(-1,0)*d
  } else if (identical(angle, "135")){
    angle <- c(-1,-1)*d
  } else {
    stop("angle must be one of '0', '45', '90', '135'.")
  }
  
  
  #Minor error checking
  if(length(dim(image)) != 2) stop("Must be a 2D matrix")
  
  
  #discretize image and initialize GLCM based on discretized image
  if( ! identical( n_grey, length( unique( c(image) ) ) )){ 
  
    image <- discretizeImage(image, n_grey=n_grey)
    
  }
  counts <- buildEmptyCountMatrix(image)
  
  
  #Add columns of NAs to left, top, and right side to mitigate edge
  NA_cols <- matrix(rep(NA, d*nrow(image)), ncol=d)
  image <- cbind(NA_cols, image, NA_cols)
  
  NA_rows <- matrix(rep(NA, d*ncol(image)), nrow=d)
  image <- rbind(NA_rows, image)
  
  #loops start from d+1 because d cols & rows are NAs
  for( i in (d+1):nrow(image)){
    #last col is also NA, so don't loop over it
    for( j in (d+1):(ncol(image) - d)){
      ref_val <- as.character(image[i,j])
      neighbor_val <- as.character(image[i + angle[1], j + angle[2]])
      
      if(is.na(ref_val) | is.na(neighbor_val)) next
      
      counts[ref_val, neighbor_val] <- counts[ref_val, neighbor_val] + 1
    }
  }
  
  #GLCMs should be symmetrical, so the transpose is added
  counts <- counts + t(counts)
  
  #Normalize
  ifelse(normalize, return(counts/sum(counts)), return(counts) )
  
  
}