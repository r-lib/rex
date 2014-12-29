context("zzz")
test_that("onAttach prints welcome message", {
  with_mock(`base::packageStartupMessage` = message,
    expect_message(.onAttach(), "Welcome to rex")
  )
})
