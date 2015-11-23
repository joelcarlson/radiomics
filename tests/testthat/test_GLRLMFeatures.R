library(radiomics)
context("GLRLM Features")

hbGLRLM0 <- glrlm(hallbey, n_grey=4)
test_that("0 degree GLRLM Features are properly calculated", {
  #Values taken from http://www.fp.ucalgary.ca/mhallbey/tutorial.htm
  expect_equal(glrlm_GLN(hbGLRLM0), 2.25, tolerance = .02)
  expect_equal(glrlm_HGLRE(hbGLRLM0), 2.375, tolerance = .02)
  expect_equal(glrlm_LRE(hbGLRLM0), 4.25, tolerance = .02)
  expect_equal(glrlm_LRHGLE(hbGLRLM0), 12, tolerance = .02)
  expect_equal(glrlm_LRLGLE(hbGLRLM0), 1.46, tolerance = 002)
  expect_equal(glrlm_LGLRE(hbGLRLM0), 0.32, tolerance = .02)  
  expect_equal(glrlm_RLN(hbGLRLM0), 4.75, tolerance = .02)
  expect_equal(glrlm_RP(hbGLRLM0), 0.5, tolerance = .02)  
  expect_equal(glrlm_SRE(hbGLRLM0), 0.3263889, tolerance = .02)
  expect_equal(glrlm_SRHGLE(hbGLRLM0), 0.524, tolerance = .02) 
  expect_equal(glrlm_SRLGLE(hbGLRLM0), 0.077, tolerance = .02)
})


hbGLRLM45 <- glrlm(hallbey, angle=45, n_grey=4)
test_that("45 degree GLRLM Features are properly calculated", {
  #Values taken from http://www.fp.ucalgary.ca/mhallbey/tutorial.htm
  expect_equal(glrlm_GLN(hbGLRLM45), 2.818182, tolerance = .02)
  expect_equal(glrlm_HGLRE(hbGLRLM45), 3, tolerance = .02)
  expect_equal(glrlm_LRE(hbGLRLM45), 2.363636, tolerance = .02)
  expect_equal(glrlm_LRHGLE(hbGLRLM45), 5.45, tolerance = .02)
  expect_equal(glrlm_LRLGLE(hbGLRLM45), 0.77, tolerance = 002)
  expect_equal(glrlm_LGLRE(hbGLRLM45), 0.36, tolerance = .02)  
  expect_equal(glrlm_RLN(hbGLRLM45), 5.545455, tolerance = .02)
  expect_equal(glrlm_RP(hbGLRLM45), 0.6875, tolerance = .02)  
  expect_equal(glrlm_SRE(hbGLRLM45), 0.6590909, tolerance = .02)
  expect_equal(glrlm_SRHGLE(hbGLRLM45),  2.38, tolerance = .02) 
  expect_equal(glrlm_SRLGLE(hbGLRLM45), 0.259, tolerance = .02)
})

