## GLCM Texture Features

glcm_xplusy <- function(glcm, k){
  sum <- 0
  for(i in 0:(nrow(glcm)-1)){
    for(j in 0:(nrow(glcm)-1)){
      sum <- sum + ifelse((i + j) == k, glcm[i+1, j+1], 0)
    }
  }
  return(sum)
}


glcm_xminusy <- function(glcm, k){
  sum <- 0
  for(i in 0:(nrow(glcm)-1)){
    for(j in 0:(nrow(glcm)-1)){
      sum <- sum + ifelse(abs(i - j) == k, glcm[i+1, j+1], 0)
    }
  }
  return(sum)
}

glcm_mean <- function(glcm){
  #see http://www.fp.ucalgary.ca/mhallbey/ans_ex_12.htm
  return(sum(as.numeric(colnames(glcm)) * colSums(glcm)))
}

glcm_variance <- function(glcm){
  sum <- 0
  mean <- glcm_mean(glcm)
  for(i in 1:nrow(glcm)){
    for(j in 1:ncol(glcm)){
      sum <- sum + ((((i-1) - mean)^2) * glcm[i,j])
    }
  }
  return(sum)
}

glcm_autoCorrelation <- function(glcm){
  #there has to be a more elegant way to do this without a for loop
  #Method: Create matrix where the elements are the product of the row and column index
  # Multiply this by the corresponding elements of the GLCM and take the sum
  grey_levels <- seq(1, nrow(glcm), 1)
  grid <- expand.grid(grey_levels, grey_levels)
  scale_matrix <- matrix(grid[,1] * grid[,2], nrow=nrow(glcm)) 
  
  return(sum(glcm*scale_matrix))
}

glcm_cProminence <- function(glcm){
  #Vectorizing is out the window! 
  #Hopefully any glcm thrown at the algo will be small enough that this is okay!
  sum <- 0
  mean <- glcm_mean(glcm)
  for(i in 1:nrow(glcm)){
    for(j in 1:ncol(glcm)){
      sum <- sum + (((i-1) + (j-1) - 2*mean)^4)*glcm[i,j]
    }
  }
  return(sum)
}

glcm_cShade <- function(glcm){
  sum <- 0
  mean <- glcm_mean(glcm)
  for(i in 1:nrow(glcm)){
    for(j in 1:ncol(glcm)){
      sum <- sum + (((i-1) + (j-1) - 2*mean)^3)*glcm[i,j]
    }
  }
  return(sum)
}


glcm_cTendency <- function(glcm){
  sum <- 0
  mean <- glcm_mean(glcm)
  for(i in 1:nrow(glcm)){
    for(j in 1:ncol(glcm)){
      sum <- sum + (((i-1) + (j-1) - 2*mean)^2)*glcm[i,j]
    }
  }
  return(sum)
}

glcm_contrast <- function(glcm){
  sum <- 0
  for(i in 1:nrow(glcm)){
    for(j in 1:ncol(glcm)){
      sum <- sum + (abs((i-1) - (j-1))^2)*glcm[i,j]
    }
  }
  return(sum)
}


glcm_correlation <- function(glcm){
  sum <- 0
  mean <- glcm_mean(glcm)
  variance <- glcm_variance(glcm)
  for(i in 1:nrow(glcm)){
    for(j in 1:ncol(glcm)){
      sum <- sum + glcm[i,j]*(((i-1) - mean)*((j-1) - mean))/variance
    }
  }
  return(sum)
}


glcm_differenceEntropy <- function(glcm, base=2){
  sum <- 0
  for(i in 1:(nrow(glcm)-1)){
    pxy <- glcm_xminusy(glcm, i-1)
    sum <- sum + ifelse(pxy > 0, pxy*logb(pxy,base=base),0)
  }
  return(-1*sum)
}

glcm_dissimilarity <- function(glcm){
  sum <- 0
  for(i in 1:nrow(glcm)){
    for(j in 1:ncol(glcm)){
      sum <- sum + (abs(i - j))*glcm[i,j]
    }
  }
  return(sum)
}

glcm_energy <- function(glcm){
  return(sum(glcm * glcm))
}


glcm_entropy <- function(glcm, base=2){
  
  return(-1*sum(glcm*ifelse(glcm > 0, logb(glcm, base=base), 0)))
}

glcm_homogeneity1 <- function(glcm){
  sum <- 0
  for(i in 1:nrow(glcm)){
    for(j in 1:ncol(glcm)){
      sum <- sum + glcm[i,j]/(1 + (abs(i - j)))
    }
  }
  return(sum)
}

glcm_homogeneity2 <- function(glcm){
  sum <- 0
  for(i in 1:nrow(glcm)){
    for(j in 1:ncol(glcm)){
      sum <- sum + glcm[i,j]/(1 + (abs(i - j))^2)
    }
  }
  return(sum)
}

glcm_IDMN <- function(glcm){
  sum <- 0
  for(i in 1:nrow(glcm)){
    for(j in 1:ncol(glcm)){
      sum <- sum + glcm[i,j]/(1 + ((abs(i - j)^2)/(nrow(glcm)^2)))
    }
  }
  return(sum)
}

glcm_IDN <- function(glcm){
  sum <- 0
  for(i in 1:nrow(glcm)){
    for(j in 1:ncol(glcm)){
      sum <- sum + glcm[i,j]/(1 + (abs(i - j)/nrow(glcm)))
    }
  }
  return(sum)
}

glcm_inverseVariance <- function(glcm){
  sum <- 0
  for(i in 1:nrow(glcm)){
    for(j in 1:ncol(glcm)){
      sum <- sum + ifelse(i != j, glcm[i,j]/abs(i-j)^2, 0)
    }
  }
  return(sum)
}

glcm_maxProb <- function(glcm){
  return(max(glcm))
}


glcm_sumAverage <- function(glcm){
  sum <- 0
  for(i in 1:(2*nrow(glcm))){
    #bit of a sticky situation with the extra 1s....
    sum <- sum + (i+1)*glcm_xplusy(glcm, i-1)
  }
  return(sum)
}

glcm_sumEntropy <- function(glcm, base=2){
  sum <- 0
  for(i in 1:(2*nrow(glcm))){
    pxy <- glcm_xplusy(glcm, i-1)
    sum <- sum + ifelse(pxy > 0, pxy*logb(pxy,base=base), 0)
  }
  return(-sum)
}

glcm_sumVariance <- function(glcm){
  sum <- 0
  sumEntropy <- glcm_sumEntropy(glcm, base=exp(1))
  
  for(i in 1:(2*nrow(glcm))){
    pxy <- glcm_xplusy(glcm, i-1)
    sum <- sum + ((i + 1  - sumEntropy)^2)*pxy 
  }
  return(sum)
}
