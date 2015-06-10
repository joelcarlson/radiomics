#GLRLM

# Helper functions!

rleMatrix_45 <- function(image){
  #operations to flip matrix by 45
  image <- t(image)
  image <- image[,ncol(image):1]
  
  #This only works on matrices that are square, so make it square with some NAs
  if(nrow(image) > ncol(image)){
    buffer_matrix <- matrix(NA, ncol=(nrow(image) - ncol(image)), nrow=nrow(image) )
    image <- cbind(image, buffer_matrix)
  } else if (ncol(image) > nrow(image)){
    buffer_matrix <- matrix(NA, nrow=(ncol(image) - nrow(image)), ncol=ncol(image) )
    image <- rbind(image, buffer_matrix)
  }
  
  #This is the correct matrix dimension
  rle_matrix <- matrix(rep(NA, length(diag(image))*(nrow(image)+ncol(image)-1)), ncol=length(diag(image)))
  
  for(i in 1:nrow(rle_matrix)){
    #handle 1 separately, due to bug creating 0x0 matrix if image[1,1]=0
    if(i == 1){
      
      diag_vals <- image[1,ncol(image)]
      
    } else if( i <= nrow(image)) {
      
      diag_vals <-  c(diag(image[1:i, (ncol(image) - i + 1):ncol(image)]))
      
    } else {
      #Shave off top and right side to move up diag
      image <- image[-1, -ncol(image)]
      if(length(image) > 1){
        diag_vals <- c(diag(image))
      } else {
        diag_vals <- c(image)
      }
      
    }
    
    rle_matrix[i,1:length(diag_vals)] <- diag_vals   
  }
  
  #strip columns and rows containing only NAs
  NA_rows <- apply(rle_matrix, 1, function(x) all(is.na(x)))
  NA_cols <- apply(rle_matrix, 2, function(x) all(is.na(x)))
  return(rle_matrix[!NA_rows,!NA_cols])
}



rleMatrix_135 <- function(image){
  #operations to flip matrix by 135
  image <- image[,ncol(image):1]
  image <- t(image)
  image <- image[,ncol(image):1]
  
  #This only works on matrices that are square, so make it square with some NAs
  if(nrow(image) > ncol(image)){
    buffer_matrix <- matrix(NA, ncol=(nrow(image) - ncol(image)), nrow=nrow(image) )
    image <- cbind(image, buffer_matrix)
  } else if (ncol(image) > nrow(image)){
    buffer_matrix <- matrix(NA, nrow=(ncol(image) - nrow(image)), ncol=ncol(image) )
    image <- rbind(image, buffer_matrix)
  }
  
  #This is the correct matrix dimension
  rle_matrix <- matrix(rep(NA, length(diag(image))*(nrow(image)+ncol(image)-1)), ncol=length(diag(image)))
  
  for(i in 1:nrow(rle_matrix)){
    #handle 1 separately, due to bug creating 0x0 matrix if image[1,1]=0
    if(i == 1){
      
      diag_vals <- image[1,ncol(image)]
      
    } else if( i <= nrow(image)) {
      
      diag_vals <-  c(diag(image[1:i, (ncol(image) - i + 1):ncol(image)]))
      
    } else {
      #Sequentially shave off top and right side to move up diagonal
      image <- image[-1, -ncol(image)]
      if(length(image) > 1){
        diag_vals <- c(diag(image))
      } else {
        diag_vals <- c(image)
      }
      
    }
    
    rle_matrix[i,1:length(diag_vals)] <- diag_vals   
  }
  
  #strip columns and rows containing only NAs
  NA_rows <- apply(rle_matrix, 1, function(x) all(is.na(x)))
  NA_cols <- apply(rle_matrix, 2, function(x) all(is.na(x)))
  return(rle_matrix[!NA_rows,!NA_cols])
}


#Now supports all angles!
glrlm <- function(image, angle="0", n_grey=32, max_run_length=ncol(image)){
  image <- discretizeImage(image, n_grey=n_grey)
  
  #Modify matrix element locations for 4 angles:
  if(identical(angle, "90")){
    #rotate the image matrix by 90 degrees cw
    image <- t(image)[,ncol(image):1]
    
  } else if (identical(angle, "45")) {
    image <- rleMatrix_45(image)
    
  } else if (identical(angle, "135")){
    image <- rleMatrix_135(image)
    
  } else if (!identical(angle, "0")){
    stop("Shift must be one of '0', '45', '90', '135'.")
  }
  
  #Computes rle of each row separately to exclude runs over the edges of the matrix
  #then binds them into a dataframe
  rlencode <- Reduce(rbind,apply(image, 1, function(x) {encode <- rle(x) 
                                                        return(data.frame(encode$lengths, encode$values))}))
  
  
  #Create empty matrix for counts
  unique_vals <- sort(unique(c(image)))
  count_matrix <- matrix(rep(0, length(unique_vals)*ncol(image)), nrow=length(unique_vals))
  rownames(count_matrix) <- unique_vals 
  colnames(count_matrix) <- 1:ncol(image)
  
  
  for(i in unique_vals){
    for(j in 1:max_run_length){
      count_matrix[which(rownames(count_matrix) == i),j] <- nrow(rlencode[which(rlencode$encode.values==i & rlencode$encode.lengths==j),])
    }
  } 
  
  return(count_matrix[,1:max_run_length])
}

