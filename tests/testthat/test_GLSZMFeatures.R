library(radiomics)
context("GLSZM Features")

hbGLSZM <- glszm(hallbey, n_grey=4)
test_that("GLSZM Features are properly calculated", {
  #Values taken from http://www.fp.ucalgary.ca/mhallbey/tutorial.htm
  expect_equal(glszm_SAE(hbGLSZM), 0.098125, tolerance = .02)
  expect_equal(glszm_LAE(hbGLSZM), 17.5, tolerance = .02)
  expect_equal(glszm_IV(hbGLSZM), 1, tolerance = .02)
  expect_equal(glszm_SZV(hbGLSZM), 1.5, tolerance = .02)
  expect_equal(glszm_ZP(hbGLSZM), 0.25, tolerance = 002)
  expect_equal(glszm_LIE(hbGLSZM), 0.3559028, tolerance = .02)  
  expect_equal(glszm_HIE(hbGLSZM), 7.5, tolerance = .02)
  expect_equal(glszm_LISAE(hbGLSZM), 0.01892361, tolerance = .02)  
  expect_equal(glszm_HISAE(hbGLSZM), 1.1625, tolerance = .02)
  expect_equal(glszm_LILAE(hbGLSZM), 8.006944, tolerance = .02) 
  expect_equal(glszm_HILAE(hbGLSZM), 94.5, tolerance = .02)
})
