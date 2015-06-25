library(radiomics)
context("GLRLM Features")

hbGLRLM0 <- glrlm(hallbey, n_grey=4)
test_that("0 degree GLRLM Features are properly calculated", {
  #Values taken from http://www.fp.ucalgary.ca/mhallbey/tutorial.htm
  expect_equal(glrlm_GLN(hbGLRLM0), 2.25, tolerance = .02)
  expect_equal(glrlm_HGLRE(hbGLRLM0), 5.625, tolerance = .02)
  expect_equal(glrlm_LRE(hbGLRLM0), 4.25, tolerance = .02)
  expect_equal(glrlm_LRHGLE(hbGLRLM0), 27.75, tolerance = .02)
  expect_equal(glrlm_LRLGLE(hbGLRLM0), 1.586806, tolerance = 002)
  expect_equal(glrlm_LGLRE(hbGLRLM0), 0.4730903, tolerance = .02)  
  expect_equal(glrlm_RLN(hbGLRLM0), 4.75, tolerance = .02)
  expect_equal(glrlm_RP(hbGLRLM0), 0.5, tolerance = .02)  
  expect_equal(glrlm_SRE(hbGLRLM0), 0.3263889, tolerance = .02)
  expect_equal(glrlm_SRHGLE(hbGLRLM0), 1.34375, tolerance = .02) 
  expect_equal(glrlm_SRLGLE(hbGLRLM0), 0.2100936, tolerance = .02)
})


hbGLRLM45 <- glrlm(hallbey, angle="45", n_grey=4)
test_that("45 degree GLRLM Features are properly calculated", {
  #Values taken from http://www.fp.ucalgary.ca/mhallbey/tutorial.htm
  expect_equal(glrlm_GLN(hbGLRLM45), 2.818182, tolerance = .02)
  expect_equal(glrlm_HGLRE(hbGLRLM45), 6.727273, tolerance = .02)
  expect_equal(glrlm_LRE(hbGLRLM45), 2.363636, tolerance = .02)
  expect_equal(glrlm_LRHGLE(hbGLRLM45), 13.27273, tolerance = .02)
  expect_equal(glrlm_LRLGLE(hbGLRLM45), 1.056818, tolerance = 002)
  expect_equal(glrlm_LGLRE(hbGLRLM45), 0.3825758, tolerance = .02)  
  expect_equal(glrlm_RLN(hbGLRLM45), 5.545455, tolerance = .02)
  expect_equal(glrlm_RP(hbGLRLM45), 0.6875, tolerance = .02)  
  expect_equal(glrlm_SRE(hbGLRLM45), 0.6590909, tolerance = .02)
  expect_equal(glrlm_SRHGLE(hbGLRLM45),  5.090909, tolerance = .02) 
  expect_equal(glrlm_SRLGLE(hbGLRLM45), 0.2140152, tolerance = .02)
})

