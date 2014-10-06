context("or")
test_that("%or% creates an alternation", {
  re = rex("w", "x" %or% "y", "z")
  expect_equal(re, regex("w(?:x|y)z"))

  expect_true(grepl(re, "wxz"))
  expect_true(grepl(re, "wyz"))
  expect_false(grepl(re, "waz"))
})

