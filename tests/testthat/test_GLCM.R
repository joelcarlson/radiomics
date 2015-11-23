library(radiomics)
context("GLCM")

test_that("GLCM is properly generated", {
  expect_error(glcm(data.frame()))
  expect_error(glcm())
  expect_error(glcm(c()))
  expect_error(glcm(list()))
  expect_error(glcm(matrix()))
  expect_is(glcm(hallbey, n_grey=4), "glcm")
  expect_is(glcm(hallbey, n_grey=4), "matrix")
})