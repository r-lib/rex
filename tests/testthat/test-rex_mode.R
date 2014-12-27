context("rex_mode")
test_that("rex_mode attaches .rex$env to the search_path", {
  expect_false(".rex$env" %in% search())

  rex_mode()

  expect_true(".rex$env" %in% search())

  rex_mode()

  expect_false(".rex$env" %in% search())
})
