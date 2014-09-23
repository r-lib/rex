context("rex")

test_that("starts_with works", {
  
  r <- rex(
    starts_with(letter)
  )
  
  expect_true(grepl(r, "abcdef"))
  expect_false(grepl(r, "123456", perl = TRUE))
  
})

test_that("ends_with works", {
  
  r <- rex(
    ends_with("Z")
  )
  
  expect_true(grepl(r, "abcZ"))
  expect_false(grepl(r, "abc"))
  
})

test_that("version parsing works", {
  
  r <- rex(
    start,
    capture(numbers),
    any,
    capture(numbers),
    any,
    capture(numbers),
    any,
    capture(numbers),
    end
  )
  
  expect_identical(
    gsub(r, "\\1 \\2 \\3 \\4", "3.1.1-1", perl = TRUE),
    "3 1 1 1"
  )
  
})

test_that("verbs in rex work", {
  
  r <- rex(
    starts_with("foo"), zero_or_more(any), ends_with("bar")
  )
  
  expect_true(grepl(r, "fooABCbar", perl = TRUE))
  expect_true(grepl(r, "foo123\tbar", perl = TRUE))
  
})

test_that("lookarounds work", {
  
  r <- rex(
    starts_with("foo") %if_next_isnt% "bar"
  )
  
  expect_true(grepl(r, "fooba", perl = TRUE))
  expect_false(grepl(r, "foobar", perl = TRUE))
})

test_that("URL parsing works", {
  
  ## Decompose a URL into its components.
  ## Example by LT (http://www.cs.uiowa.edu/~luke/R/regexp.html).
  x <- "http://stat.umn.edu:80/xyz"
  m <- regexec("^(([^:]+)://)?([^:/]+)(:([0-9]+))?(/.*)", x)
  m
  regmatches(x, m)
  ## Element 3 is the protocol, 4 is the host, 6 is the port, and 7
  ## is the path.  We can use this to make a function for extracting the
  ## parts of a URL:
  URL_parts <- function(x) {
    m <- regexec("^(([^:]+)://)?([^:/]+)(:([0-9]+))?(/.*)", x)
    parts <- do.call(rbind,
                     lapply(regmatches(x, m), `[`, c(3L, 4L, 6L, 7L)))
    colnames(parts) <- c("protocol","host","port","path")
    parts
  }
  URL_parts(x)
  
  r <- rex(
    
    start,
    
    ## match the protocol -- may exist or may not
    maybe(capture(
      capture(one_or_more(not(":"))),
      "://"
    )),
    
    ## match the path
    capture(one_or_more(not(":/"))),
    
    ## get the port
    maybe(capture(":", capture(one_or_more(numbers)))),
    
    ## and the rest
    maybe(capture("/", zero_or_more(any))),
    
    end
    
  )
  
  rbind(r = r, m = "^(([^:]+)://)?([^:/]+)(:([0-9]+))?(/.*)")
  n <- gregexpr(r, x, perl = TRUE)[[1]]
  split_matches <- function(string, matches) {
    starts <- attr(matches, "capture.start")
    lengths <- attr(matches, "capture.length")
    ends <- starts + lengths - 1
    c(string, substring(string, starts, ends))
  }
  
  split_matches(x, n)
  regmatches(x, m)[[1]]
  expect_equal(regmatches(x, m)[[1]], split_matches(x, n))
    
})