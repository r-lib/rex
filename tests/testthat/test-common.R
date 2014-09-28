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

test_that("lookarounds work", {
  
  r <- rex(
    start, "foo" %if_next_isnt% "bar"
  )
  
  expect_true(grepl(r, "fooba", perl = TRUE))
  expect_false(grepl(r, "foobar", perl = TRUE))
})

test_that("Simple URL parsing works", {
  
  # TODO: get these working better again
  ## Decompose a URL into its components.
  ## Example by LT (http://www.cs.uiowa.edu/~luke/R/regexp.html).
  x <- "http://stat.umn.edu:80/xyz"
  re <- "^(?:(((?:(?:(?!:).)*)+)://))?((?:(?:(?!:/).)*)+)(?:(:((?:\\d+)+)))?(?:(/(?:.)*))?$"
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


test_that("URL Validation works", {
  valid_chars <- one_of(regex('a-z0-9\u00a1-\uffff'))

  re = rex(
    start,

    # protocol identifier (optional) + //
    group(list('http', maybe('s')) %or% 'ftp', '://'),

    # user:pass authentication (optional)
    maybe(non_spaces,
      maybe(':', zero_or_more(non_space)),
      '@'),

    #host name
    group(zero_or_more(valid_chars, zero_or_more('-')), one_or_more(valid_chars)),

    #domain name
    zero_or_more('.', zero_or_more(valid_chars, zero_or_more('-')), one_or_more(valid_chars)),

    #TLD identifier
    group('.', valid_chars %>% at_least(2)),

    # server port number (optional)
    maybe(':', digit %>% between(2, 5)),

    # resource path (optional)
    maybe('/', non_space %>% zero_or_more()),

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
  "http://उदाहरण.परीक्षा",
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
  "h://test",
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

  expect_true(all(grepl(re, good) == TRUE))
  expect_true(all(grepl(re, bad) == FALSE))
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
  expect_equal(rex(start, number %>% n_times(2)), regex("^(?:\\d){2}"))
})

context("append")
test_that("adds basic characters", {
  expect_equal(rex("x", "y", "z"),
    regex("xyz"))
  expect_equal(rex("x", maybe("y"), "z"),
    regex("x(?:y)?z"))
})

test_that("escapes special characters", {
  expect_equal(rex(numbers %>% between(0, 2), '.', "$", end),
    regex("(?:\\d+){0,2}\\.\\$$"))
})

# TODO: Error if end is not last
#test_that("raises an error after ending", {
  #expect_error(rex(ends_with("x"), "y"))
#})

context("maybe")
test_that("recognizes basic characters", {
    re = rex("x", maybe("y"), "z")
    expect_equal(re, regex("x(?:y)?z"))
    expect_true(grepl(re, "xyz"))
    expect_true(grepl(re, "xz"))
})

context("not")
test_that("creates a negative lookahead", {
  re = rex("x", not("y"), "z")
  expect_equal(re, regex("x(?:(?!y).)*z"))
  expect_true(grepl(re, "xazbc", perl=TRUE))
  expect_true(grepl(re, "xxzabc", perl=TRUE))
  expect_false(grepl(re, "xyzabc", perl=TRUE))

  re = rex("x432", not("yars"), "tsrz")
  expect_true(grepl(re, "x432tsrz", perl=TRUE))
  expect_true(grepl(re, "x432yartsrz", perl=TRUE))
  expect_false(grepl(re, "x432yarstsrz", perl=TRUE))

  re = rex(start, not("ars"), "x432")
  expect_true(grepl(re, "x432", perl=TRUE))
  expect_true(grepl(re, "arx432", perl=TRUE))
  expect_false(grepl(re, "arsx432", perl=TRUE))
})

context('or')
test_that('%or% creates an alternation', {
  re = rex('w', 'x' %or% 'y', 'z')
  expect_equal(re, regex('w(?:x|y)z'))
  expect_true(grepl(re, 'wxz'))
  expect_true(grepl(re, 'wyz'))
  expect_false(grepl(re, 'waz'))
})

context('between')
test_that('creates a bounded repetition', {
  expect_equal(rex('x' %>% between(2, 4)),
    regex('(?:x){2,4}'))
})

context('at_least')
test_that('creates a bounded repetition', {
  expect_equal(rex('x' %>% at_least(3)),
    regex('(?:x){3,}'))
})

context('at_most')
test_that('creates a repetition of n times at most', {
  expect_equal(rex('x' %>% at_most(3)),
    regex('(?:x){,3}'))
})

context('zero_or_more')
test_that('recognizes basic characters', {
  expect_equal(rex(zero_or_more('a'), 'b'),
    regex('(?:a)*b'))
})

test_that('recognizes special identifiers', {
  expect_equal(rex(zero_or_more(number), 'b'),
    regex('(?:\\d)*b'))
})

context('one_or_more')
test_that('recognizes basic characters', {
  expect_equal(rex(one_or_more('a'), 'b'),
    regex('(?:a)+b'))
})

test_that('recognizes special identifiers', {
  expect_equal(rex(one_or_more(letter), 'b'),
    regex('(?:[a-zA-Z])+b'))
})

context('end_with')
test_that('matches basic characters', {
  expect_equal(rex('x', "y", end),
    regex('xy$'))
})


test_that('escapes special characters', {
  expect_equal(rex('x', "$", end),
    regex('x\\$$'))
})

context('general regex')
test_that('returns a well-formed regex', {
  expect_equal(rex(start, 'w', 'x' %or% 'y', 'z', end),
    regex('^w(?:x|y)z$'))
})

context('examples')
re =
  rex(start,
    number %>% n_times(3),
    '-',
    letter %>% n_times(2),
    maybe('#'),
    'a' %or% 'b',
    'c' %>% between(2, 4),
    '$',
    end
    )

expect_true(grepl(re, "123-xy#accc$", perl=TRUE))
expect_true(grepl(re, "999-dfbcc$"))
expect_false(grepl(re, "000-df#baccccccccc$"))
expect_false(grepl(re, "444-dd3ac$"))

context('one_of')
test_that('matches basic characters', {
  expect_equal(rex(one_of('a', 'b', 'rst')), regex('[abrst]'))
})

test_that('escapes special characters', {
  expect_equal(rex(one_of('^', 'b')), regex('[\\^b]'))
})

context('except')
test_that('matches basic characters', {
  expect_equal(rex(except('a', 'b', 'rst')), regex('[^abrst]'))
})

test_that('escapes special characters', {
  expect_equal(rex(except('^', 'b')), regex('[^\\^b]'))
})

test_that('none_of is the same as except', {
  expect_equal(rex(none_of('^', 'b', 1:10)), rex(except('^', 'b', 1:10)))
})

context('or')
test_that('or with multiple inputs works', {
  re = rex(or('x', 'yx', 'z'))
  expect_equal(re, regex('(?:x|yx|z)'))
  expect_true(all(grepl(re, c('x', 'yx', 'z'), perl=TRUE) == TRUE))

  expect_false(grepl(re, c('y')))
})

context("if_next_is")
test_that("matches basic characters", {
  re <- rex("a" %if_next_is% "b")
  expect_equal(re, regex("(?:a(?=b))"))
  expect_true(grepl(re, "ab", perl=TRUE))
  expect_false(grepl(re, "ac", perl=TRUE))
})
test_that("escapes special characters", {
  re <- rex("[" %if_next_is% "?=")
  expect_equal(re, regex("(?:\\[(?=\\?=))"))
  expect_true(grepl(re, "[?=", perl=TRUE))
  expect_false(grepl(re, "?=[", perl=TRUE))
})

context("if_next_isnt")
test_that("matches basic characters", {
  re <- rex("a" %if_next_isnt% "b")
  expect_equal(re, regex("(?:a(?!b))"))
  expect_true(grepl(re, "ac", perl=TRUE))
  expect_false(grepl(re, "ab", perl=TRUE))
})
test_that("escapes special characters", {
  re <- rex("[" %if_next_isnt% "?=")
  expect_equal(re, regex("(?:\\[(?!\\?=))"))
  expect_true(grepl(re, "?=[", perl=TRUE))
  expect_false(grepl(re, "[?=", perl=TRUE))
})

context("if_prev_is")
test_that("matches basic characters", {
  re <- rex("a" %if_prev_is% "b")
  expect_equal(re, regex("(?:(?<=b)a)"))
  expect_true(grepl(re, "ba", perl=TRUE))
  expect_false(grepl(re, "ab", perl=TRUE))
})
test_that("escapes special characters", {
  re <- rex("[" %if_prev_is% "<=?")
  expect_equal(re, regex("(?:(?<=<=\\?)\\[)"))
  expect_true(grepl(re, "<=?[", perl=TRUE))
  expect_false(grepl(re, "[b", perl=TRUE))
})

context("if_prev_isnt")
test_that("matches basic characters", {
  re <- rex("a" %if_prev_isnt% "b")
  expect_equal(re, regex("(?:(?<!b)a)"))
  expect_true(grepl(re, "ab", perl=TRUE))
  expect_false(grepl(re, "ba", perl=TRUE))
})
test_that("escapes special characters", {
  re <- rex("[" %if_prev_isnt% "!<?")
  expect_equal(re, regex("(?:(?<!!<\\?)\\[)"))
  expect_true(grepl(re, "[b", perl=TRUE))
  expect_false(grepl(re, "!<?[", perl=TRUE))
})

context("issues")
test_that("#11 Modifiers and named character classes", {
  p <- rex(none_of(alpha))
  expect_true(grepl(p, '6', perl=TRUE))
})
