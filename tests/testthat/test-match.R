context("re_matches")
df2 <- function(...) {
  data.frame(..., stringsAsFactors = FALSE, check.names = FALSE)
}
string <- c("this is Text", "chr-12", "12343 66544456")

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

test_that("re_matches with global returns a list of matches", {

  # Global without captures doesn"t make a ton of sense but lets test it anyway
  expect_equal(re_matches(string, rex("is"), global = TRUE),
    list(c(TRUE, TRUE), FALSE, FALSE))
})

test_that("re_matches with global returns a list of data.frames", {

  expect_equal(re_matches(string, rex(capture(any_letters, "is")), global = TRUE),
    list(df2(`1` = c("this", "is")), df2(`1` = NA_character_), df2(`1` = NA_character_)) )
  expect_equal(re_matches(string, rex(capture(digits, name = "number")), global = TRUE),
    list(df2("number" = NA_character_), df2("number" = "12"), df2("number" = c("12343", "66544456"))))
})

test_that("re_matches named capture returns named data frame", {

  expect_equal(re_matches(string, rex(capture(digits, name = "numbers"))),
    df2(numbers=c(NA_character_, "12", "12343")))
  expect_equal(re_matches(string, rex(capture(alphas, name = "letters"))),
    df2(letters=c("this", "chr", NA_character_)))
})

test_that("examples are correct", {
string <- c("this is a", "test string")
expect_equal(re_matches(string, rex(capture(alphas, name = "first_word"), space,
                capture(alphas, name = "second_word"))),
            df2(first_word = c("this", "test"),
                second_word = c("is", "string")))

expect_equal(re_matches(string, rex(capture("test"))),
  df2(`1`=c(NA_character_, "test")))
})

context("re_matches - locations")
test_that("re_matches if given other than character vector", {

  expect_equal(re_matches(NA, rex(digit), locations = TRUE),
    df2(start = NA_integer_, end = NA_integer_))

  expect_equal(re_matches(1, rex(digit), locations = TRUE),
    df2(start = 1L, end = 1L))

  expect_equal(re_matches(1, rex(capture(digit)), locations = TRUE),
    df2("1" = "1", "1.start" = 1L, "1.end" = 1L))

  expect_equal(re_matches("a", rex(capture(digit)), locations = TRUE),
    df2("1" = NA_character_, "1.start" = NA_integer_, "1.end" = NA_integer_))
})

test_that("re_matches no capture returns a logical", {

  expect_equal(re_matches(string, rex(digit), locations = TRUE),
    df2(start = c(NA_integer_, 5L, 1L), end = c(NA_integer_, 5L, 1L)))

  expect_equal(re_matches(string, rex(digits), locations = TRUE),
    df2(start = c(NA_integer_, 5L, 1L), end = c(NA_integer_, 6L, 5L)))

  expect_equal(re_matches(string, rex(alpha), locations = TRUE),
    df2(start = c(1L, 1L, NA_integer_), end = c(1L, 1L, NA_integer_)))

  expect_equal(re_matches(string, rex("-"), locations = TRUE),
    df2(start = c(NA_integer_, 4L, NA_integer_), end = c(NA_integer_, 4L, NA_integer_)))
})

test_that("re_matches capture returns a data frame", {

  expect_equal(re_matches(string, rex(capture(digits)), locations = TRUE),
    df2("1" = c(NA_character_, "12", "12343"), "1.start" = c(NA_integer_, 5L, 1L), "1.end" = c(NA_integer_, 6L, 5L)))

  expect_equal(re_matches(string, rex(capture(alphas)), locations = TRUE),
    df2("1" = c("this", "chr", NA_character_), "1.start" = c(1L, 1L, NA_integer_), "1.end" = c(4L, 3L, NA_integer_)))

  expect_equal(re_matches(string, rex(capture(alphas), " ", capture(alphas)), locations = TRUE),
    df2(
      "1" = c("this", NA_character_, NA_character_),
      "1.start" = c(1L, NA_integer_, NA_integer_),
      "1.end" = c(4L, NA_integer_, NA_integer_),

      "2" = c("is", NA_character_, NA_character_),
      "2.start" = c(6L, NA_integer_, NA_integer_),
      "2.end" = c(7L, NA_integer_, NA_integer_)
    ))
})

test_that("re_matches named capture returns named data frame", {

  expect_equal(re_matches(string, rex(capture(digits, name = "numbers")), locations = TRUE),
    df2("numbers" = c(NA_character_, "12", "12343"),
        "numbers.start" = c(NA_integer_, 5L, 1L),
        "numbers.end" = c(NA_integer_, 6L, 5L)))

  expect_equal(re_matches(string, rex(capture(alphas, name = "letters")), locations = TRUE),
    df2("letters" = c("this", "chr", NA_character_),
        "letters.start" = c(1L, 1L, NA_integer_),
        "letters.end" = c(4L, 3L, NA_integer_)))

})

test_that("re_matches with global returns a list of matches", {

  expect_equal(re_matches(string, rex("is"), global = TRUE, locations = TRUE),
    list(
      df2(start = c(3L, 6L), end = c(4L, 7L)),
      df2(start = NA_integer_, end = NA_integer_),
      df2(start = NA_integer_, end = NA_integer_)
      )
    )
})

test_that("re_matches with global returns a list of data.frames", {

  expect_equal(re_matches(string, rex(capture(any_letters, "is")), global = TRUE, locations = TRUE),
    list(
      df2("1" = c("this", "is"), "1.start" = c(1L, 6L), "1.end" = c(4L, 7L)),
      df2("1" = NA_character_, "1.start" = NA_integer_, "1.end" = NA_integer_),
      df2("1" = NA_character_, "1.start" = NA_integer_, "1.end" = NA_integer_)
      )
    )

  expect_equal(re_matches(string, rex(capture(digits, name = "number")), global = TRUE, locations = TRUE),
    list(
      df2(number = NA_character_, number.start = NA_integer_, number.end = NA_integer_),
      df2(number = "12", number.start = 5L, number.end = 6L),
      df2(number = c("12343", "66544456"), number.start = c(1L, 7L), number.end = c(5L, 14L))
      )
    )
})

test_that("examples are correct", {
string <- c("this is a", "test string")
expect_equal(re_matches(string, rex(capture(alphas, name = "first_word"), space,
                capture(alphas, name = "second_word"))),
            df2(first_word = c("this", "test"),
                second_word = c("is", "string")))

expect_equal(re_matches(string, rex(capture("test"))),
  df2(`1`=c(NA_character_, "test")))
})

context("re_substitutes")
test_that("s substitutes properly, with and without options", {
  expect_equal(re_substitutes(string, rex("Text"), "test"),
    c("this is test", "chr-12", "12343 66544456"))

  expect_equal(re_substitutes(string, rex("text"), "test", options="insensitive"),
    c("this is test", "chr-12", "12343 66544456"))

  expect_equal(re_substitutes(string, "i", "x", global = TRUE),
    c("thxs xs Text", "chr-12", "12343 66544456"))
})

context("match_args")
test_that("match args stops if the arg does not match", {
  expect_error(match_args("test", names(option_map)))

  expect_equal(match_args("ungreedy", names(option_map)), "ungreedy")

  expect_equal(match_args("ungre", names(option_map)), "ungreedy")
})
