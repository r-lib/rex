context("zero_or_more")
test_that("recognizes basic characters", {
  expect_equal(rex(zero_or_more("a"), "b"),
    regex("(?:a)*b"))
})

test_that("recognizes special identifiers", {
  expect_equal(rex(zero_or_more(number), "b"),
    regex("(?:[[:digit:]])*b"))
})

test_that("recognizes shortcuts", {
  re <- rex(zero_or_more(number, type = "lazy"), "E")
  expect_equal(re,
    regex("(?:[[:digit:]])*?E"))

  expect_equal(regmatches(m=regexpr(re, "123EEE", perl = TRUE), "123EEE"), "123E")
})

context("one_or_more")
test_that("recognizes basic characters", {
  expect_equal(rex(one_or_more("a"), "b"),
    regex("(?:a)+b"))
})

test_that("recognizes special identifiers", {
  expect_equal(rex(one_or_more(letter), "b"),
    regex("(?:[[:alpha:]])+b"))
})


context("maybe")
test_that("recognizes basic characters", {
    re <- rex("x", maybe("y"), "z")
    expect_equal(re, regex("x(?:y)?z"))
    expect_true(grepl(re, "xyz"))
    expect_true(grepl(re, "xz"))
})

context("not")
test_that("creates a negative lookahead", {
  re <- rex("x", not("y"), "z")
  expect_equal(re, regex("x(?:(?!y).)*z"))
  expect_true(grepl(re, "xazbc", perl=TRUE))
  expect_true(grepl(re, "xxzabc", perl=TRUE))
  expect_false(grepl(re, "xyzabc", perl=TRUE))

  re <- rex("x432", not("yars"), "tsrz")
  expect_true(grepl(re, "x432tsrz", perl=TRUE))
  expect_true(grepl(re, "x432yartsrz", perl=TRUE))
  expect_false(grepl(re, "x432yarstsrz", perl=TRUE))

  re <- rex(start, not("ars"), "x432")
  expect_true(grepl(re, "x432", perl=TRUE))
  expect_true(grepl(re, "arx432", perl=TRUE))
  expect_false(grepl(re, "arsx432", perl=TRUE))
})
