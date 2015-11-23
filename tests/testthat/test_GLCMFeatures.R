library(radiomics)
context("GLCM Features")

hbGLCM <- glcm(hallbey, n_grey=4, verbose=FALSE)
test_that("0 degree GLCM Features are properly calculated", {
  #Values taken from http://www.fp.ucalgary.ca/mhallbey/tutorial.htm
  expect_equal(glcm_mean(hbGLCM), 1.291667, tolerance = .02)
  expect_equal(glcm_variance(hbGLCM), 1.039931, tolerance = .02)
  expect_equal(glcm_dissimilarity(hbGLCM), 0.4167, tolerance = .02)
  expect_equal(glcm_contrast(hbGLCM), 0.583, tolerance = .02)
  expect_equal(glcm_homogeneity2(hbGLCM), 0.807, tolerance = 002)
  expect_equal(glcm_energy(hbGLCM), 0.145, tolerance = .02)  
  expect_equal(glcm_entropy(hbGLCM, base=exp(1)), 2.0951, tolerance = .02)
  expect_equal(glcm_correlation(hbGLCM), 0.718, tolerance = .02)  
})

