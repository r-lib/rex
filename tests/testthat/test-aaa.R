context("aaa")
test_that("register adds a function to the .rex$env enviornment", {
  a <- identity

  register(a)

  expect_true("a" %in% ls(envir=.rex$env))

  rm("a", envir = .rex$env)
})

test_that("register_object adds all the objects to the .rex$env environment", {
  b <- list(x=1, y = 2)

  register_object(b)

  expect_true("x" %in% ls(envir=.rex$env))

  expect_true("y" %in% ls(envir=.rex$env))

  expect_false("b" %in% ls(envir=.rex$env))

  rm("x", envir = .rex$env)

  rm("y", envir = .rex$env)
})
