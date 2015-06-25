

calc_features <- function(image, features = c("first order", "glcm", "glrlm", "glszm"),
                          n_grey=32, angle="0", verbose=FALSE, max_run_length=min(dim(image)), ...){
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
  feature_list <- list()
  #df <- data.frame()
  if("first order" %in% features){
    fo_df <- data.frame(lapply(first_order_features, function(f) f(image)))
    #feature_list$"First order features" <- lapply(first_order_features, function(f) f(image))
  }
  
  if("glcm" %in% features){
    glcm_df <- data.frame(lapply(glcm_features, function(f) f(glcm(image, n_grey=n_grey, angle=angle, verbose=verbose))))
  }
  
  if("glrlm" %in% features){
    glrlm_df <- data.frame(lapply(glrlm_features, function(f) f(glrlm(image, n_grey=n_grey, angle=angle, verbose=verbose, max_run_length=max_run_length))))
  }
  
  if("glszm" %in% features){
    glszm_df <- data.frame(lapply(glszm_features, function(f) f(glszm(image, n_grey=n_grey, verbose=verbose))))
  }
  
  df <- cbind(fo_df, glcm_df, glrlm_df, glszm_df)
  df
} 









