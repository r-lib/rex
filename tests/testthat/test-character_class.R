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

context("character_class")
test_that("examples are correct", {
  # grey = gray
  re <- rex("gr", one_of("a", "e"), "y")
  expect_equal(grepl(re, c("grey", "gray")), c(TRUE, TRUE)) # TRUE TRUE

  # Match non-vowels
  re <- rex(none_of("a", "e", "i", "o", "u"))
  # They can also be in the same string
  re2 <- rex(none_of("aeiou"))
  expect_identical(re, re2)
  expect_equal(grepl(re, c("k", "l", "e")), c(TRUE, TRUE, FALSE)) # TRUE TRUE FALSE

  # Match range
  re <- rex(range("a", "e"))
  expect_equal(grepl(re, c("b", "d", "f")), c(TRUE, TRUE, FALSE)) # TRUE TRUE FALSE

  # Explicit creation (note you have to escape manually here)
  re <- rex(character_class("abcd\\["))
  expect_equal(grepl(re, c("a", "d", "[", "]")), c(TRUE, TRUE, TRUE, FALSE)) # TRUE TRUE TRUE FALSE
})
test_that("escapes special characters", {
  re <- rex(exclude_range("[", "}"))

  expect_equal(re, regex("[^\\[-}]"))

  lapply(c("[", "}"), function(x) {
    expect_false(grepl(re, x, perl = TRUE), info=x)
  })

  expect_true(grepl(re, "A", perl = TRUE))
})
