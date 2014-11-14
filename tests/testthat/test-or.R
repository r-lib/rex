context("or")
test_that("%or% creates an alternation", {
  re <- rex("w", "x" %or% "y", "z")
  expect_equal(re, regex("w(?:x|y)z"))

  expect_true(grepl(re, "wxz"))
  expect_true(grepl(re, "wyz"))
  expect_false(grepl(re, "waz"))
})

test_that("or with multiple inputs works", {
  re <- rex(or("x", "yx", "z"))
  expect_equal(re, regex("(?:x|yx|z)"))
  lapply(c("x", "yx", "z"), function(x) {
    expect_true(grepl(re, x, perl=TRUE), info=x)
  })

  expect_false(grepl(re, c("y")))
  expect_false(grepl(re, c("a")))
})
test_that("or with variable inputs works", {
  variable <- c("test", "variable")
  re <- rex(or(variable))

  expect_equal(re, regex("(?:test|variable)"))

  lapply(variable, function(x){
    expect_true(grepl(re, x), info=x)
  })
})
