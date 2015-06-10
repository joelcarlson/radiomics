### GLRLM Features

##Gray level non-uniformity
glrlm_GLN <- function(glrlm){
  sum(apply(glrlm, 1, sum)^2)/sum(glrlm)
}

## High Gray level run emphasis
glrlm_HGLRE <- function(glrlm){
  sum(as.numeric(rownames(glrlm))^2 * glrlm)/sum(glrlm)
}


##Long Run Emphasis
glrlm_LRE <- function(glrlm){
  sum(as.numeric(colnames(glrlm))^2 * t(glrlm))/sum(glrlm)
}

##Long run high gray level emphasis
glrlm_LRHGLE <- function(glrlm){
  sum(t(as.numeric(colnames(glrlm))^2 * t(glrlm)) * as.numeric(rownames(glrlm))^2) /sum(glrlm)
}

##Long Run Low Gray Level Emphasis
glrlm_LRLGLE <- function(glrlm){
  sum(t(as.numeric(colnames(glrlm))^2 * t(glrlm)) / as.numeric(rownames(glrlm))^2) /sum(glrlm)
}

## Low gray level run emphasis
glrlm_LGLRE <- function(glrlm){
  sum(glrlm / as.numeric(rownames(glrlm))^2) / sum(glrlm)
}

##Run length non-uniformity
glrlm_RLN <- function(glrlm){
  sum(apply(glrlm, 2, sum)^2)/sum(glrlm)
}

## Run Percentage
glrlm_RP <- function(glrlm){
  n_voxels <- sum(sapply(1:ncol(glrlm), function(i) sum(glrlm[,i])* as.numeric(colnames(glrlm)[i])))
  sum(apply(glrlm/n_voxels, 2, sum))
}

## Short run emphasis
glrlm_SRE <- function(glrlm){
  sum(t(glrlm) / as.numeric(colnames(glrlm))^2) / sum(glrlm)
}


# Short run high gray level emphasis
glrlm_SRHGLE <- function(glrlm){
  sum(t(glrlm * as.numeric(rownames(glrlm))^2) / as.numeric(colnames(glrlm))^2) /sum(glrlm)
}

# Short run low grey emphasis
glrlm_SRLGLE <- function(glrlm){
  sum( t( t(glrlm) / as.numeric(colnames(glrlm))^2 ) / (as.numeric(rownames(glrlm))^2)) / sum(glrlm)
}





