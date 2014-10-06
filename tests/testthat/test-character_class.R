context("one_of")
test_that("simple text is correct", {
  re <- rex(one_of(1:9))

  expect_equal(re, regex("[123456789]"))
  lapply(1:9, function(x) {
    expect_true(grepl(re, x, perl = TRUE), info=x)
  })
  expect_false(grepl(re, "a", perl = TRUE))


  vals <- c("a", "b", "c", 0, 3, 5)

  re <- rex(one_of(vals))

  expect_equal(re, regex("[abc035]"))

  lapply(vals, function(x) {
    expect_true(grepl(re, x, perl = TRUE), info = x)
  })

  expect_false(grepl(re, "d", perl = TRUE))
})

test_that("escapes correctly", {
  vals <- c("[", "]")

  re <- rex(one_of(vals))

  expect_equal(re, regex("[\\[\\]]"))

  lapply(vals, function(x) {
    expect_true(grepl(re, x, perl = TRUE), info = x)
  })

  expect_false(grepl(re, "{", perl = TRUE))
})

context("none_of")
test_that("simple text is correct", {
  re <- rex(none_of(1:9))

  expect_equal(re, regex("[^123456789]"))
  lapply(1:9, function(x) {
    expect_false(grepl(re, x, perl = TRUE), info=x)
  })
  expect_true(grepl(re, "a", perl = TRUE))


  vals <- c("a", "b", "c", 0, 3, 5)

  re <- rex(none_of(vals))

  expect_equal(re, regex("[^abc035]"))

  lapply(vals, function(x) {
    expect_false(grepl(re, x, perl = TRUE), info = x)
  })

  expect_true(grepl(re, "d", perl = TRUE))
})

test_that("escapes correctly", {
  vals <- c("[", "]")

  re <- rex(none_of(vals))

  expect_equal(re, regex("[^\\[\\]]"))

  lapply(vals, function(x) {
    expect_false(grepl(re, x, perl = TRUE), info = x)
  })

  expect_true(grepl(re, "{", perl = TRUE))
})

