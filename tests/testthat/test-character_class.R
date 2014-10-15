context("one_of")
test_that("simple text is correct", {
  re <- rex(one_of(1:9))

  expect_equal(re, regex("[123456789]"))
  lapply(1:9, function(x) {
    expect_true(grepl(re, x, perl = TRUE), info=x)
  })
  expect_false(grepl(re, "a", perl = TRUE))


  vals <- c("a", "b", "c", 0, 3, 5)

  re <- rex(one_of(vals))

  expect_equal(re, regex("[abc035]"))

  lapply(vals, function(x) {
    expect_true(grepl(re, x, perl = TRUE), info = x)
  })

  expect_false(grepl(re, "d", perl = TRUE))
})

test_that("escapes correctly", {
  vals <- c("[", "]")

  re <- rex(one_of(vals))

  expect_equal(re, regex("[\\[\\]]"))

  lapply(vals, function(x) {
    expect_true(grepl(re, x, perl = TRUE), info = x)
  })

  expect_false(grepl(re, "{", perl = TRUE))
})

tests <- c(quote("a"),
      quote(any),
      quote(quote),
      quote(quotes),
      quote(lower),
      quote(upper),
      quote(list(upper, lower)),
      quote(list("[", "]")))

test_that("any_of equals zero_or_more(one_of())", {
  lapply(tests,
    function(x) {
      re1 <- rex(zero_or_more(one_of(eval(x))))
      re2 <- rex(any_of(eval(x)))

      expect_equal(re1, re2, info = paste(sep=" : ", re1, re2))
    })
})

test_that("some_of equals one_or_more(one_of())", {
  lapply(tests,
    function(x) {
      re1 <- rex(one_or_more(one_of(eval(x))))
      re2 <- rex(some_of(eval(x)))

      expect_equal(re1, re2, info = paste(sep=" : ", re1, re2))
    })
})

test_that("except_any equals zero_or_more(none_of())", {
  lapply(tests,
    function(x) {
      re1 <- rex(zero_or_more(none_of(eval(x))))
      re2 <- rex(except_any_of(eval(x)))

      expect_equal(re1, re2, info = paste(sep=" : ", re1, re2))
    })
})

test_that("except_some equals one_or_more(none_of())", {
  lapply(tests,
    function(x) {
      re1 <- rex(one_or_more(none_of(eval(x))))
      re2 <- rex(except_some_of(eval(x)))

      expect_equal(re1, re2, info = paste(sep=" : ", re1, re2))
    })
})

context("none_of")
test_that("simple text is correct", {
  re <- rex(none_of(1:9))

  expect_equal(re, regex("[^123456789]"))
  lapply(1:9, function(x) {
    expect_false(grepl(re, x, perl = TRUE), info=x)
  })
  expect_true(grepl(re, "a", perl = TRUE))


  vals <- c("a", "b", "c", 0, 3, 5)

  re <- rex(none_of(vals))

  expect_equal(re, regex("[^abc035]"))

  lapply(vals, function(x) {
    expect_false(grepl(re, x, perl = TRUE), info = x)
  })

  expect_true(grepl(re, "d", perl = TRUE))
})

test_that("escapes correctly", {
  vals <- c("[", "]")

  re <- rex(none_of(vals))

  expect_equal(re, regex("[^\\[\\]]"))

  lapply(vals, function(x) {
    expect_false(grepl(re, x, perl = TRUE), info = x)
  })

  expect_true(grepl(re, "{", perl = TRUE))
})

context("range")
test_that("matches basic characters", {
  re <- rex(range(1, 3))

  expect_equal(re, regex("[1-3]"))

  lapply(1:3, function(x) {
    expect_true(grepl(re, x), info=x)
  })

  lapply(4:9, function(x) {
    expect_false(grepl(re, x), info=x)
  })

})
test_that("escapes special characters", {
  re <- rex(range("[", "}"))

  expect_equal(re, regex("[\\[-}]"))

  lapply(c("[", "}"), function(x) {
    expect_true(grepl(re, x), info=x)
  })

})

context("exclude_range")
test_that("matches basic characters", {
  re <- rex(exclude_range(1, 3))

  expect_equal(re, regex("[^1-3]"))

  lapply(1:3, function(x) {
    expect_false(grepl(re, x, perl = TRUE), info=x)
  })

  lapply(4:9, function(x) {
    expect_true(grepl(re, x, perl = TRUE), info=x)
  })

})

context("one_of")
test_that("matches basic characters", {
  expect_equal(rex(one_of("a", "b", "rst")), regex("[abrst]"))
})

test_that("escapes special characters", {
  expect_equal(rex(one_of("^", "b", "\\")), regex("[\\^b\\\\]"))
})

context("except")
test_that("matches basic characters", {
  expect_equal(rex(except("a", "b", "rst")), regex("[^abrst]"))
})

test_that("escapes special characters", {
  expect_equal(rex(except("^", "b")), regex("[^\\^b]"))
})

test_that("none_of is the same as except", {
  expect_equal(rex(none_of("^", "b", 1:10)), rex(except("^", "b", 1:10)))
})

context("character_class")
test_that("examples are correct", {
  # grey = gray
  re <- rex("gr", one_of("a", "e"), "y")
  expect_equal(grepl(re, c("grey", "gray")), c(TRUE, TRUE)) # TRUE TRUE

  # Match non-vowels
  re <- rex(none_of("a", "e", "i", "o", "u"))
  # They can also be in the same string
  re2 <- rex(none_of("aeiou"))
  expect_identical(re, re2)
  expect_equal(grepl(re, c("k", "l", "e")), c(TRUE, TRUE, FALSE)) # TRUE TRUE FALSE

  # Match range
  re <- rex(range("a", "e"))
  expect_equal(grepl(re, c("b", "d", "f")), c(TRUE, TRUE, FALSE)) # TRUE TRUE FALSE

  # Explicit creation (note you have to escape manually here)
  re <- rex(character_class("abcd\\["))
  expect_equal(grepl(re, c("a", "d", "[", "]")), c(TRUE, TRUE, TRUE, FALSE)) # TRUE TRUE TRUE FALSE
})
test_that("escapes special characters", {
  re <- rex(exclude_range("[", "}"))

  expect_equal(re, regex("[^\\[-}]"))

  lapply(c("[", "}"), function(x) {
    expect_false(grepl(re, x, perl = TRUE), info=x)
  })

  expect_true(grepl(re, "A", perl = TRUE))
})
