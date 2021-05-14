context("capture")
test_that("matches basic characters", {
  x <- "text"
  re <- rex(capture(x))

  expect_equal(re, regex("(text)"))

  expect_true(grepl(re, x))

  expect_equal(gsub(re, "\\1", x), x)

  expect_equal(gsub(re, "replacement", x), "replacement")
})

test_that("escapes special characters", {
  x <- "^[x$\\"
  re <- rex(capture(x))

  expect_equal(re, regex("(\\^\\[x\\$\\\\)"))

  expect_true(grepl(re, x))

  expect_equal(gsub(re, "\\1", x), x)

  expect_equal(gsub(re, "replacement", x), "replacement")
})

test_that("examples work", {
 re <- rex(
   # first quotation mark
   capture(quote),

   # match all non-matching quotation marks
   zero_or_more(except(capture_group(1))),

   # end quotation mark (matches first)
   capture_group(1)
 )

 expect_equal(re, regex("(['\"])(?:[^\\g{1}])*\\g{1}"))

 lapply(c("\"\"", "\"'\"", "\"arst\"", "''", "'arst'", "'\"'"),
   function(x) {
     expect_true(grepl(re, x, perl = TRUE), info=x)
   }
 )

 lapply(c("'a", "'asr\""), function(x) {
    expect_false(grepl(re, x, perl = TRUE), info = x)
 })

})

context("named capture")
test_that("examples work", {
  re <- rex(
    capture(name = "fruit", or("apple", "orange")),
    "=",
    capture_group("fruit")
    )

  expect_true(grepl(re, "apple=apple", perl=TRUE))
  expect_true(grepl(re, "orange=orange", perl=TRUE))
  expect_false(grepl(re, "apple=orange", perl=TRUE))
})

test_that("naming via argument name works", {
  re <- rex(
    capture(fruit = or("apple", "orange")),
    "=",
    capture_group("fruit")
    )
  expect_true(grepl("<fruit>", re))

  expect_true(grepl(re, "apple=apple", perl=TRUE))
  expect_true(grepl(re, "orange=orange", perl=TRUE))
  expect_false(grepl(re, "apple=orange", perl=TRUE))
})

test_that("naming via argument name works with multiple arguments", {
  re1 <- rex(
    capture(greeting = "hello", " ", "world", "!"),
    anything,
    capture_group("greeting")
  )
  ## Name can be given on any argument in ..., and multiple times as
  ## long as they're all the same.
  re2 <- rex(
    capture("hello", greeting = " ", greeting = "world", "!"),
    anything,
    capture_group("greeting")
  )
  expect_error(
    re3 <- rex(
      capture(greeting = "hello", " ", greet = "world", "!"),
      anything,
      capture_group("greeting")
    )
  )

  expect_true(grepl("<greeting>", re1))
  expect_true(grepl("<greeting>", re2))
  expect_true(grepl(re1, "hello world! Again: hello world!", perl=TRUE))
  expect_true(grepl(re2, "hello world! Again: hello world!", perl=TRUE))
})
