#GLSZM

#Quantifies size zone matrices in an image
# See here: http://thibault.biz/Research/ThibaultMatrices/GLSZM/GLSZM.html

#Needs access to reshape2 and spatstat and imageQauntize

glszm <- function(image, n_grey=32, ...){
  image <- discretizeImage(image, n_grey=n_grey, ...)
  grey_lvls <- unique(c(image))
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
  
  #Clean up and cast into a matrix 
  colnames(count_data) <- c("greylvl", "size", "counts")
  count_data <- reshape2::acast(count_data, greylvl~size, value.var="counts")
  count_data <- count_data[,order(as.numeric(as.character(colnames(count_data))))]
  count_data[is.na(count_data)] <- 0
  

  return(count_data)
}