context("printing")
test_that("print S3 method works properly", {
  expect_output(print(rex("x")), "x")

  expect_output(print(rex("x\\")), "x\\\\")

  expect_output(print(rex("x[")), "x\\\\\\[")
})
