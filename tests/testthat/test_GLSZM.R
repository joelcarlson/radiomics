library(radiomics)
context("GLSZM")

test_that("GLSZM is properly generated", {
  expect_error(glszm(data.frame()))
  expect_error(glszm())
  expect_error(glszm(c()))
  expect_error(glszm(list()))
  expect_error(glszm(matrix()))
  expect_is(glszm(hallbey, n_grey=4), "glszm")
  expect_is(glszm(hallbey, n_grey=4), "matrix")
})