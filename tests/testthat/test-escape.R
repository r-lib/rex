context("escape")
test_that("default escape works properly", {
  expect_equal(escape(1), structure("1", class="regex"))
})
