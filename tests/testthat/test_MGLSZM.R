library(radiomics)
context("MGLSZM")

test_that("MGLSZM is properly generated", {
  expect_error(mglszm(data.frame()))
  expect_error(mglszm())
  expect_error(mglszm(c()))
  expect_error(mglszm(list()))
  expect_error(mglszm(matrix()))
  expect_is(mglszm(hallbey), "mglszm")
  expect_is(mglszm(hallbey), "matrix")
})