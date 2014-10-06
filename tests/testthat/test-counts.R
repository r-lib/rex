context("n_times")
test_that("description", {
  re1 <- rex("x" %>% n_times(2))
  re2 <- rex("x" %>% n(2))

  expect_identical(re1, re2)

  expect_equal(re1,
    regex("(?:x){2}"))

  expect_true(grepl(re1, "xx"))
  expect_false(grepl(re1, "x"))
})

context("between")
test_that("creates a bounded repetition", {
  re <- rex("x" %>% between(2, 4))
  expect_equal(re, regex("(?:x){2,4}"))

  expect_true(grepl(re, "xxx"))
  expect_false(grepl(re, "x"))
})

context("at_least")
test_that("creates a bounded repetition", {
  re <- rex("x" %>% at_least(3))
  expect_equal(re, regex("(?:x){3,}"))

  expect_true(grepl(re, "xxx"))
  expect_false(grepl(re, "xx"))
})

context("at_most")
test_that("creates a repetition of n times at most", {
  re <- rex(start, "x" %>% at_most(3), end)
  expect_equal(re, regex("^(?:x){,3}$"))

  expect_true(grepl(re, "xxx"))
  expect_false(grepl(re, "xxxxx"))
})
