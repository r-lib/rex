test_that("rex() ignores empty args", {
  expect_equal(rex("x", ), rex("x"))
})
