#Small Area Emphasis
glszm_SAE <- function(glszm){
  sum(t(glszm) / as.numeric(colnames(glszm))^2) / sum(glszm)
}

#Large Area Emphasis
glszm_LAE <- function(glszm){
  sum(t(glszm) * as.numeric(colnames(glszm))^2) / sum(glszm)
}

#Intensity Variability
glszm_IV <- function(glszm){
  sum(apply(glszm, 1, sum)^2)/sum(glszm)
}

# Size Zone Variability
glszm_SZV <- function(glszm){
  sum(apply(glszm, 2, sum)^2)/sum(glszm)
}

# Zone percentage
glszm_ZP <- function(glszm){
  n_voxels <- sum(sapply(1:ncol(glszm), function(i) sum(glszm[,i])* as.numeric(colnames(glszm)[i])))
  sum(apply(glszm/n_voxels, 2, sum))
}

#Low intensity emphasis
glszm_LIE <- function(glszm){
  sum(glszm / as.numeric(rownames(glszm))^2) / sum(glszm)
}

# High intensity emphasis
glszm_HIE <- function(glszm){
  sum(glszm * as.numeric(rownames(glszm))^2) / sum(glszm)
}

# Low intensity small area emphasis
glszm_LISAE <- function(glszm){
  sum( t( t(glszm) / as.numeric(colnames(glszm))^2 ) / (as.numeric(rownames(glszm))^2)) / sum(glszm)
}

# High intensity small area emphasis
glszm_HISAE <- function(glszm){
  sum(t(glszm * as.numeric(rownames(glszm))^2) / as.numeric(colnames(glszm))^2) /sum(glszm)
}

# Low intensity large area emphasis
glszm_LILAE <- function(glszm){
  sum(t(as.numeric(colnames(glszm))^2 * t(glszm)) / as.numeric(rownames(glszm))^2) /sum(glszm)
}

# High intensity Large area emphasis
glszm_HILAE <- function(glszm){
  sum(t(as.numeric(colnames(glszm))^2 * t(glszm)) * as.numeric(rownames(glszm))^2) /sum(glszm)
}