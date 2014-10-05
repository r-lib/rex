context("re_matches")
df2 = function(...) { data.frame(..., stringsAsFactors = FALSE, check.names = FALSE) }
string = c("this is Text", "chr-12", "12343 66544456")

test_that("re_matches if given other than character vector", {

  expect_equal(re_matches(NA, rex(digit)), NA)
  expect_equal(re_matches(1, rex(digit)), TRUE)
  expect_equal(re_matches(1, rex(capture(digit))), df2("1" = "1"))
  expect_equal(re_matches("a", rex(capture(digit))), df2("1" = NA_character_))
})

test_that("re_matches no capture returns a logical", {

  expect_equal(re_matches(string, rex(digit)), c(FALSE, TRUE, TRUE))
  expect_equal(re_matches(string, rex(digits)), c(FALSE, TRUE, TRUE))
  expect_equal(re_matches(string, rex(alpha)), c(TRUE, TRUE, FALSE))
  expect_equal(re_matches(string, rex("-")), c(FALSE, TRUE, FALSE))
})

test_that("re_matches capture returns a data frame", {

  expect_equal(re_matches(string, rex(capture(digits))),
    df2(`1`=c(NA_character_, "12", "12343")))
  expect_equal(re_matches(string, rex(capture(alphas))),
    df2(`1`=c("this", "chr", NA_character_)))

  expect_equal(re_matches(string, rex(capture(alphas), " ", capture(alphas))),
    df2(`1`=c("this", NA_character_, NA_character_),
        `2`=c("is", NA_character_, NA_character_)))
})

test_that("re_matches named capture returns named data frame", {

  expect_equal(re_matches(string, rex(capture(digits, name = "numbers"))), df2(numbers=c(NA_character_, "12", "12343")))
  expect_equal(re_matches(string, rex(capture(alphas, name = "letters"))), df2(letters=c("this", "chr", NA_character_)))
})

test_that("examples are correct", {
string = c("this is a", "test string")
expect_equal(re_matches(string, rex(capture(alphas, name = "first_word"), space,
                capture(alphas, name = "second_word"))),
            df2(first_word = c("this", "test"),
                second_word = c("is", "string")))

expect_equal(re_matches(string, rex(capture("test"))), df2(`1`=c(NA_character_, "test")))
})
