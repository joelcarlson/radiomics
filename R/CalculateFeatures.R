
# Function to combine all of the texture analysis and first order features to give a nice full feature set
# for image processing

#' Calculate texture and first order statistics.
#'
#' \code{calc_features} calculates the first order and textural features for a given matrix
#'
#' @param image A numeric image matrix.
#' @param features A vector containing any of "first order", "glcm", "glrlm", 
#'   "glszm", and "mglszm". Inclusion of these strings in the vector will lead
#'   to the calculation of the related image features.
#' @param n_grey an integer value, the number of grey levels the image should
#'   be quantized into.
#' @param d an integer value, the distance between the current pixel, and the
#'   pixel to which it is compared. To be passed to \code{glcm}.   
#' @param verbose Logical, a warning is given when the user 
#'  supplies more grey values than exist in the image. Setting this value to FALSE
#'  will suppress this warning.     
#' @param max_run_length An integer value, the default is the maximum possible
#'   run length. Setting it to a smaller value truncates the output. Desirable
#'   in cases where the matrix is extremely sparse, for example when
#'   there are few long runs. To be passed to \code{glrlm}.  
#' @return A data frame with a single observation. The columns of the dataframe 
#'   correspond to the calculated features.
#'
#' @examples
#' calc_features(hallbey)
#' calc_features(psf, n_grey=10)
#' @seealso \code{\link{glcm}}
#'   \code{\link{glrlm}}
#'   \code{\link{glszm}}
#'   \code{\link{mglszm}}

calc_features <- function(image, features = c("first order", "glcm", "glrlm", "glszm", "mglszm"),
                          n_grey=32, d=1, verbose=FALSE, max_run_length=min(dim(image))){
  # Lists of features for calculation:
  
  # First order ----------------------------
  
  first_order_features <- list(calc_energy = calc_energy,
                               calc_entropy = calc_entropy,
                               calc_kurtosis = calc_kurtosisOptimized,
                               calc_meanDeviation = calc_meanDeviation,
                               calc_skewness = calc_skewness,
                               calc_uniformity = calc_uniformity,
                               calc_mean = mean,
                               calc_median = median,
                               calc_max = max,
                               calc_min = min,
                               #calc_diff = diff,
                               #calc_variance = var,
                               calc_RMS = function(x) sqrt(mean(x^2)),
                               calc_sd = sd
                               )
  
  # GLCM -----------------------------------
  
  glcm_features <- list(glcm_mean = glcm_mean,
                        glcm_variance = glcm_variance,
                        glcm_autoCorrelation = glcm_autoCorrelation,
                        glcm_cProminence = glcm_cProminence,
                        glcm_cShade = glcm_cShade,
                        glcm_cTendency = glcm_cTendency,
                        glcm_contrast = glcm_contrast,
                        glcm_correlation = glcm_correlation,
                        glcm_differenceEntropy = glcm_differenceEntropy,  
                        glcm_dissimilarity = glcm_dissimilarity,
                        glcm_energy = glcm_energy,
                        glcm_entropy = glcm_entropy, 
                        glcm_homogeneity1 = glcm_homogeneity1,
                        glcm_homogeneity2 = glcm_homogeneity2, 
                        glcm_IDMN = glcm_IDMN,
                        glcm_IDN = glcm_IDN,
                        glcm_inverseVariance = glcm_inverseVariance,
                        glcm_maxProb = glcm_maxProb,
                        glcm_sumAverage = glcm_sumAverage, 
                        glcm_sumEntropy = glcm_sumEntropy,
                        glcm_sumVariance = glcm_sumVariance
                        )
  
  # GLRLM ----------------------------------
  
  glrlm_features <- list(glrlm_GLN = glrlm_GLN,
                         glrlm_HGLRE = glrlm_HGLRE,
                         glrlm_LRE = glrlm_LRE, 
                         glrlm_LRHGLE = glrlm_LRHGLE,
                         glrlm_LRLGLE = glrlm_LRLGLE,
                         glrlm_LGLRE = glrlm_LGLRE,
                         glrlm_RLN = glrlm_RLN, 
                         glrlm_RP = glrlm_RP,
                         glrlm_SRE = glrlm_SRE,
                         glrlm_SRHGLE = glrlm_SRHGLE,
                         glrlm_SRLGLE = glrlm_SRLGLE
                         )
  
  # GLSZM ----------------------------------
  
  glszm_features <- list(glszm_SAE = glszm_SAE,
                         glszm_LAE = glszm_LAE,
                         glszm_IV = glszm_IV, 
                         glszm_SZV = glszm_SZV,
                         glszm_ZP = glszm_ZP,
                         glszm_LIE = glszm_LIE,
                         glszm_HIE = glszm_HIE,
                         glszm_LISAE = glszm_LISAE, 
                         glszm_HISAE = glszm_HISAE, 
                         glszm_LILAE = glszm_LILAE, 
                         glszm_HILAE = glszm_HILAE
                         )
  # ----------------------------------------
  
  # Calculate requested features
  
  df <- data.frame(image_name = deparse(substitute(image)), n_grey=n_grey, glcm_d=d)
  
  
  if("mglszm" %in% features){
    mglszm_im <- mglszm(image, verbose=verbose)
    
    mglszm_df <- data.frame(lapply(glszm_features, function(f) f(mglszm_im)))
    df <- cbind(df, mglszm_df)
  }
  
  # Discretize image now to save all other functions from doing so
  image <- discretizeImage(image, n_grey=n_grey, verbose=verbose)
  
  if("first order" %in% features){
    fo_df <- data.frame(lapply(first_order_features, function(f) f(image)))
    df <- cbind(df, fo_df)
  }
  
  if("glcm" %in% features){
    # Average all 4 angles for rotation invariance
    glcm_0 <- glcm(image, angle="0", n_grey=n_grey, d=d, verbose=F)
    glcm_45 <- glcm(image, angle="45", n_grey=n_grey, d=d, verbose=F)
    glcm_90 <- glcm(image, angle="90", n_grey=n_grey, d=d, verbose=F)
    glcm_135 <- glcm(image, angle="135", n_grey=n_grey, d=d, verbose=F)
    glcm_im <- (glcm_0 + glcm_45 + glcm_90 + glcm_135) / 4
    
    glcm_df <- data.frame(lapply(glcm_features, function(f) f(glcm_im)))
    df <- cbind(df, glcm_df)
  }
  
  if("glrlm" %in% features){
    #average all angles for rotation invariance
    glrlm_0 <- glrlm(image, angle="0", max_run_length=max_run_length, verbose=F)
    glrlm_45 <- glrlm(image, angle="45", max_run_length=max_run_length, verbose=F)
    glrlm_90 <- glrlm(image, angle="90", max_run_length=max_run_length, verbose=F)
    glrlm_135 <- glrlm(image, angle="135", max_run_length=max_run_length, verbose=F)
    glrlm_im <- (glrlm_0 + glrlm_45 + glrlm_90 + glrlm_135) / 4
    
    glrlm_df <- data.frame(lapply(glrlm_features, function(f) f(glrlm_im)))
    df <- cbind(df, glrlm_df)  
  }
  
  if("glszm" %in% features){
    glszm_im <- glszm(image, n_grey=n_grey, verbose=verbose)
    
    glszm_df <- data.frame(lapply(glszm_features, function(f) f(glszm_im)))
    df <- cbind(df, glszm_df)
  }
  

  
  df
} 









