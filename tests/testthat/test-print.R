context("printing")
test_that("print S3 method works properly", {
  expect_output(print(rex("x")), "x")

  expect_output(print(rex("x\\")), "x\\\\")

  expect_output(print(rex("x[")), "x\\\\\\[")
})

test_that("register_shortcuts calls utils::globalVariables", {
  with_mock(`utils::globalVariables` = function(x, ...) { x },
    expect_equal(ls(.rex$env), register_shortcuts("rex"))
  )
})
