context("helper_functions")

test_that("inverse negates the shortcut", {
  test_shortcuts <- shortcuts[c("single_quote", "letter")]

  expect_equal(
    list(
      non_single_quote = structure("^'", class = c("character_class", "regex")),
      non_letter = structure("^[:alpha:]", class = c("character_class", "regex"))
    ), inverse(test_shortcuts))
})
test_that("plural makes shortcut match one or more", {
  test_shortcuts <- shortcuts[c("single_quote", "letter")]

  expect_equal(
    list(
      single_quotes = structure("[']+", class = c("regex")),
      letters = structure("[[:alpha:]]+", class = c("regex"))
    ), plural(test_shortcuts))
})
test_that("multiple makes shortcut match zero or more", {
  test_shortcuts <- shortcuts[c("single_quote", "letter")]

  expect_equal(
    list(
      any_single_quotes = structure("[']*", class = c("regex")),
      any_letters = structure("[[:alpha:]]*", class = c("regex"))
    ), multiple(test_shortcuts))
})
