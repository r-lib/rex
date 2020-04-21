context("rex")

test_that("start works", {

  r <- rex(
    start, letter
    )

  expect_true(grepl(r, "abcdef"))
  expect_false(grepl(r, "123456", perl = TRUE))

})

test_that("end works", {

  r <- rex(
    "Z", end
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
    start, "foo", zero_or_more(any), "bar", end
    )

  expect_true(grepl(r, "fooABCbar", perl = TRUE))
  expect_true(grepl(r, "foo123\tbar", perl = TRUE))

})

test_that("Simple URL parsing works", {

  # TODO: get these working better again
  ## Decompose a URL into its components.
  ## Example by LT (http://www.cs.uiowa.edu/~luke/R/regexp.html).
  x <- "http://stat.umn.edu:80/xyz"
  re <- "^(?:(((?:(?:(?!:).)*)+)://))?((?:(?:(?!:/).)*)+)(?:(:((?:[[:digit:]]+)+)))?(?:(/(?:.)*))?$"
  #m <- regexec(re, x)
  #m
  #regmatches(x, m)
  ## Element 3 is the protocol, 4 is the host, 6 is the port, and 7
  ## is the path.  We can use this to make a function for extracting the
  ## parts of a URL:
  URL_parts <- function(x) {
    m <- regexec(re, x)
    parts <- do.call(rbind,
      lapply(regmatches(x, m), `[`, c(3L, 4L, 6L, 7L)))
    colnames(parts) <- c("protocol","host","port","path")
    parts
  }
  #URL_parts(x)

  r <- rex(

    start,

    ## match the protocol -- may exist or may not
    maybe(capture(
        capture(except_some_of(":")),
        "://"
        )),

    ## match the path
    capture(one_or_more(not(":/"))),

    ## get the port
    maybe(capture(":", capture(numbers))),

    ## and the rest
    maybe(capture("/", anything)),

    end

    )

  rbind(r = r, m = re)
  n <- gregexpr(r, x, perl = TRUE)[[1]]
  split_matches <- function(string, matches) {
    starts <- attr(matches, "capture.start")
    lengths <- attr(matches, "capture.length")
    ends <- starts + lengths - 1
    c(string, substring(string, starts, ends))
  }

  #split_matches(x, n)
  #regmatches(x, m)[[1]]
  #expect_equal(regmatches(x, m)[[1]], split_matches(x, n))

})

context("URL Validation")
test_that("URL Validation works", {
  valid_chars <- rex(except_some_of(".", "/", " ", "-"))

  `%>%` <- magrittr::`%>%`

  re <- rex(
    start,

    # protocol identifier (optional) + //
    group(list("http", maybe("s")) %or% "ftp", "://"),

    # user:pass authentication (optional)
    maybe(non_spaces,
      maybe(":", zero_or_more(non_space)),
      "@"),

    #host name
    group(zero_or_more(valid_chars, zero_or_more("-")), one_or_more(valid_chars)),

    #domain name
    zero_or_more(".", zero_or_more(valid_chars, zero_or_more("-")), one_or_more(valid_chars)),

    #TLD identifier
    group(".", valid_chars %>% at_least(2)),

    # server port number (optional)
    maybe(":", digit %>% between(2, 5)),

    # resource path (optional)
    maybe("/", non_space %>% zero_or_more()),

    end
    )

  good <- c("http://foo.com/blah_blah",
    "http://foo.com/blah_blah/",
    "http://foo.com/blah_blah_(wikipedia)",
    "http://foo.com/blah_blah_(wikipedia)_(again)",
    "http://www.example.com/wpstyle/?p=364",
    "https://www.example.com/foo/?bar=baz&inga=42&quux",
    "http://✪df.ws/123",
    "http://userid:password@example.com:8080",
    "http://userid:password@example.com:8080/",
    "http://userid@example.com",
    "http://userid@example.com/",
    "http://userid@example.com:8080",
    "http://userid@example.com:8080/",
    "http://userid:password@example.com",
    "http://userid:password@example.com/",
    "http://➡.ws/䨹",
    "http://⌘.ws",
    "http://⌘.ws/",
    "http://foo.com/blah_(wikipedia)#cite-1",
    "http://foo.com/blah_(wikipedia)_blah#cite-1",
    "http://foo.com/unicode_(✪)_in_parens",
    "http://foo.com/(something)?after=parens",
    "http://☺.damowmow.com/",
    "http://code.google.com/events/#&product=browser",
    "http://j.mp",
    "ftp://foo.bar/baz",
    "http://foo.bar/?q=Test%20URL-encoded%20stuff",
    "http://مثال.إختبار",
    "http://例子.测试",
    "http://-.~_!$&'()*+,;=:%40:80%2f::::::@example.com",
    "http://1337.net",
    "http://a.b-c.de",
    "http://223.255.255.254")

  bad <- c(
    "http://",
    "http://.",
    "http://..",
    "http://../",
    "http://?",
    "http://??",
    "http://??/",
    "http://#",
    "http://##",
    "http://##/",
    "http://foo.bar?q=Spaces should be encoded",
    "//",
    "//a",
    "///a",
    "///",
    "http:///a",
    "foo.com",
    "rdar://1234",
    "h://test", # nolint
    "http:// shouldfail.com",
    ":// should fail",
    "http://foo.bar/foo(bar)baz quux",
    "ftps://foo.bar/",
    "http://-error-.invalid/",
    "http://-a.b.co",
    "http://a.b-.co",
    "http://0.0.0.0",
    "http://3628126748",
    "http://.www.foo.bar/",
    "http://www.foo.bar./",
    "http://.www.foo.bar./")

  lapply(good, function(x) {
    expect_true(grepl(re, x, perl = TRUE), info=x)
    })
  lapply(bad, function(x) {
    expect_false(grepl(re, x, perl = TRUE), info=x)
    })
})

context("start")
test_that("matches basic characters", {
  expect_equal(rex(start, "f"), regex("^f"))
})

test_that("escapes special characters", {
  expect_equal(rex(start, "."), regex("^\\."))
})

test_that("matches basic characters", {
  expect_equal(rex(start, "x" %>% n_times(3)), regex("^(?:x){3}"))
})

test_that("matches special identifiers", {
  expect_equal(rex(start, number %>% n_times(2)), regex("^(?:[[:digit:]]){2}"))
})

context("appending expressions")
test_that("adds basic characters", {
  expect_equal(rex("x", "y", "z"),
    regex("xyz"))
  expect_equal(rex("x", maybe("y"), "z"),
    regex("x(?:y)?z"))
})

test_that("escapes special characters", {
  expect_equal(rex(numbers %>% between(0, 2), ".", "$", end),
    regex("(?:[[:digit:]]+){0,2}\\.\\$$"))
})

context("shortcuts - end")
test_that("matches basic characters", {
  expect_equal(rex("x", "y", end),
    regex("xy$"))
})


test_that("escapes special characters", {
  expect_equal(rex("x", "$", end),
    regex("x\\$$"))
})

context("general regex")
test_that("returns a well-formed regex", {
  expect_equal(rex(start, "w", "x" %or% "y", "z", end),
    regex("^w(?:x|y)z$"))
})

context("rex examples")
re <-
  rex(start,
    number %>% n_times(3),
    "-",
    letter %>% n_times(2),
    maybe("#"),
    "a" %or% "b",
    "c" %>% between(2, 4),
    "$",
    end
    )

expect_true(grepl(re, "123-xy#accc$", perl=TRUE))
expect_true(grepl(re, "999-dfbcc$"))
expect_false(grepl(re, "000-df#baccccccccc$"))
expect_false(grepl(re, "444-dd3ac$"))

context("issues")
test_that("#11 Modifiers and named character classes", {
  p <- rex(none_of(alpha))
  expect_true(grepl(p, "6", perl = TRUE))
  })
