#GLRLM

# Helper functions!

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


# Further optimized using table for the run length encoding output,
# rather than looping over them

glrlm <- function(image, angle="0", n_grey=32, max_run_length = min(dim(image)), ...){
  image <- discretizeImage(image, n_grey=n_grey, ...)
  
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