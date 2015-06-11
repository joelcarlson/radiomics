mglszm <- function(image, truncate=TRUE){
  #TODO: Make weights a function argument
  #TODO: Make number of bits a function argument
  
  #create weights
  #  - -3.5 to 3.5 makes the sum of the weights ~1
  #  - 8 is the number of bits we will use (2^k)
  #  - 2, 4, 8, 16, 32, 64, 128, 256
  #  - Therefore 16 and 32 are the highest weighted
  weights <- dnorm(seq(-3.5,3.5,length.out=8), mean=0, sd=1)
  
  #initialize matrix at max grey level
  #such that it begins as the maximum number of possible rows
  MGLSZM <- matrix(0, nrow=256, ncol=prod(dim(image)), dimnames=list(1:256, 1:prod(dim(image))))
  
  #Loop over bit values, gray levels = 2^k
  for(k in 1:8){
    n_grey = 2^k
    KGLSZM <- glszm(image, n_grey=n_grey, all_cols=TRUE)
    #Scale by weighting factor:
    KGLSZM <- weights[k] * KGLSZM 
    
    #KGLSZM must be expanded to be added to the MGLSZM
    rows <- sort(rep_len(1:nrow(KGLSZM), length.out=256))
    KGLSZM <- KGLSZM[rows, ]
    rownames(KGLSZM) <- 1:256
    
    MGLSZM <- MGLSZM + KGLSZM
  }
  
  # Remove rows containing no information
  if(truncate){
    return(MGLSZM[which(rowSums(MGLSZM) > 0),which(colSums(MGLSZM) > 0)])
  } else {
    return(MGLSZM)
  }
  
}