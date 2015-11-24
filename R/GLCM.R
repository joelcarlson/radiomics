#' Gray level co-occurrence matrix.
#'
#' \code{glcm} returns a gray level co-occurrence matrix for a given matrix.
#' 
#' 
#' Can be visualized using \code{image(glcm(data))}. For visualization info
#' see \code{?image.radiomics}
#'
#' 
#' @param data A numeric 2D matrix.
#' @param angle One of "0", "45", "90" or "135", the pixel to which the 
#'   current pixel is compared.
#' @param d an integer value, the distance between the current pixel, and the
#'   pixel to which it is compared.
#' @param n_grey an integer value, the number of grey levels the image should
#'   be quantized into. If greater than the number of unique values in the image,
#'   no action will be taken.
#' @param normalize Logical value, if TRUE (default) the matrix will be normalized such that 
#'   the sum of it's components is 1.
#' @param ... Can be given verbose=FALSE to suppress output from the n_grey conversion.       
#' @return a matrix of dimension n_grey by n_grey, the GLCM. The column and row names represent 
#'   grey values in the image.
#'   
#' @references \url{http://www.fp.ucalgary.ca/mhallbey/tutorial.htm}
#' @examples
#' \dontrun{
#' hallbey
#' glcm(hallbey)
#' glcm(hallbey, angle="90") #vertical GLCM
#' }
#' @export
glcm <- setClass("glcm",
                 contains="matrix"
)

setMethod("initialize", 
          signature = "glcm", 
          definition = function(.Object, data, angle, d, n_grey, normalize, ...){

            #Send to discretizeImage for error checking
            #Discretize grey values if required
            #discretize image and initialize GLCM based on discretized image
            data <- discretizeImage(data, n_grey=n_grey, ...)
            
            unique_vals <- sort(unique(c(data)))
            
            #Given an image matrix and angle, calculate glcm
            #allows angles of 0 (0,1), 45 (-1,1), 90 (-1,0), 135 (-1,-1)
            # d is distance
            if(identical(angle, 0)){
              angle <- c(0,1)*d
            } else if (identical(angle, 45)){
              angle <- c(-1,1)*d
            } else if (identical(angle, 90)){
              angle <- c(-1,0)*d
            } else if (identical(angle, 135)){
              angle <- c(-1,-1)*d
            } else {
              stop("angle must be one of '0', '45', '90', '135'.")
            }
            
            #define count matrix
            counts <- matrix(0, ncol=length(unique_vals), nrow=length(unique_vals) )
            rownames(counts) <- colnames(counts) <- unique_vals
            
            #ref = reference pixel value
            #nei = neighbor pixel value
            #This loop finds indices of neighbor pixels given a ref value and angle, counts occurrence of each
            #pixel value, and adds them into the counts matrix
            for(ref in unique_vals){
              ref_indices <- which(data==ref, arr.ind=TRUE)
              nei_indices <- matrix(c(ref_indices[,'row'] + angle[1], ref_indices[,'col'] + angle[2]), ncol=2)
              #make sure indices exist
              nei_indices <- matrix(nei_indices[which(nei_indices[,1] <= nrow(data) & nei_indices[,2] <= ncol(data)),], ncol=2)
              nei <- data[nei_indices] 
              nei_counts <- as.data.frame(table(nei))
              counts[as.character(ref), as.character(nei_counts$nei) ] <- counts[as.character(ref), as.character(nei_counts$nei)] + nei_counts$Freq
            }
            
            
            #GLCMs should be symmetrical, so the transpose is added
            counts <- counts + t(counts)
            #Normalize
            if(normalize) counts <- counts/sum(counts)
            
            
            .Object@.Data <- counts
            
            .Object
            
          }   )

#' @export          
glcm <- function(data, angle = 0, d=1, n_grey = 32, normalize=TRUE, ...){
 return(new("glcm", data, angle, d, n_grey, normalize, ...))
}
