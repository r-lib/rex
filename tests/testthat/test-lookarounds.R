context("lookarounds")
test_that("lookarounds work", {

  r <- rex(
    start, "foo" %if_next_isnt% "bar"
  )

  expect_true(grepl(r, "fooba", perl = TRUE))
  expect_false(grepl(r, "foobar", perl = TRUE))
})
context("if_next_is")
test_that("matches basic characters", {
  re <- rex("a" %if_next_is% "b")
  expect_equal(re, regex("(?:a(?=b))"))
  expect_true(grepl(re, "ab", perl=TRUE))
  expect_false(grepl(re, "ac", perl=TRUE))
})
test_that("escapes special characters", {
  re <- rex("[" %if_next_is% "?=")
  expect_equal(re, regex("(?:\\[(?=\\?=))"))
  expect_true(grepl(re, "[?=", perl=TRUE))
  expect_false(grepl(re, "?=[", perl=TRUE))
})

context("if_next_isnt")
test_that("matches basic characters", {
  re <- rex("a" %if_next_isnt% "b")
  expect_equal(re, regex("(?:a(?!b))"))
  expect_true(grepl(re, "ac", perl=TRUE))
  expect_false(grepl(re, "ab", perl=TRUE))
})
test_that("escapes special characters", {
  re <- rex("[" %if_next_isnt% "?=")
  expect_equal(re, regex("(?:\\[(?!\\?=))"))
  expect_true(grepl(re, "?=[", perl=TRUE))
  expect_false(grepl(re, "[?=", perl=TRUE))
})

context("if_prev_is")
test_that("matches basic characters", {
  re <- rex("a" %if_prev_is% "b")
  expect_equal(re, regex("(?:(?<=b)a)"))
  expect_true(grepl(re, "ba", perl=TRUE))
  expect_false(grepl(re, "ab", perl=TRUE))
})
test_that("escapes special characters", {
  re <- rex("[" %if_prev_is% "<=?")
  expect_equal(re, regex("(?:(?<=<=\\?)\\[)"))
  expect_true(grepl(re, "<=?[", perl=TRUE))
  expect_false(grepl(re, "[b", perl=TRUE))
})

context("if_prev_isnt")
test_that("matches basic characters", {
  re <- rex("a" %if_prev_isnt% "b")
  expect_equal(re, regex("(?:(?<!b)a)"))
  expect_true(grepl(re, "ab", perl=TRUE))
  expect_false(grepl(re, "ba", perl=TRUE))
})
test_that("escapes special characters", {
  re <- rex("[" %if_prev_isnt% "!<?")
  expect_equal(re, regex("(?:(?<!!<\\?)\\[)"))
  expect_true(grepl(re, "[b", perl=TRUE))
  expect_false(grepl(re, "!<?[", perl=TRUE))
})
