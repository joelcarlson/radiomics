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