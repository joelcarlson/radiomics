library(radiomics)
context("GLRLM")

test_that("GLRLM is properly generated", {
  expect_error(glrlm(data.frame()))
  expect_error(glrlm())
  expect_error(glrlm(c()))
  expect_error(glrlm(list()))
  expect_error(glrlm(matrix()))
  expect_is(glrlm(hallbey, n_grey=4), "glrlm")
  expect_is(glrlm(hallbey, n_grey=4), "matrix")
})