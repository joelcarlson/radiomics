##First order statistics

calc_energy <- function(image, dims=NULL){
  #TODO: Add dim check for 2D vs 3D
  return(sum(image*image))
}

calc_entropy <- function(image, base=2, nbins=length(image)){
  # Break image into a hist
  im_range <- range(image)
  cuts <- table(cut(image, seq(im_range[1], im_range[2], by=diff(im_range)/nbins), include.lowest=TRUE))/length(image)
  
  #Logs cannot take 0 values, so let = 0 if no value
  entropy_vals <- vapply(cuts, function(x) ifelse(x != 0, x*logb(x, base=base), 0), FUN.VALUE = 1)
  return(-1*sum(entropy_vals))
}

calc_kurtosis <- function(image){
  scale <- 1/prod(dim(image))
  mu <- mean(image)
  sigma <- sd(image)
  
  function_val <- vapply(image, function(x) ((x - mu)/sigma)^4, FUN.VALUE=1)
  return(scale * sum(function_val) - 3)
}

calc_kurtosisOptimized <- function(image){
  n <- length(image)
  image <- image - mean(image)
  r <- n * sum(image^4) / (sum(image^2)^2)
  return(r * (1 - 1/n)^2 - 3)
}

calc_meanDeviation <- function(image){
  scale <- 1/prod(dim(image))
  mu <- mean(image)
  return(scale * sum(abs(image - mu)))
}

calc_skewness <- function (image, na.rm = FALSE){
  
  if (na.rm){
    image <- image[!is.na(image)]
  }
  
  return(sum((image - mean(image))^3)/(length(image) * sd(image)^3))
}

calc_uniformity <- function(image, nbins=length(image)){
  # Break image into a hist
  im_range <- range(image)
  cuts <- table(cut(image, seq(im_range[1], im_range[2], by=diff(im_range)/nbins), include.lowest=TRUE))/length(image)
  function_vals <- vapply(cuts, function(x) x^2, FUN.VALUE = 1)
  return(sum(function_vals))
}
