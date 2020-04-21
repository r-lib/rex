# Rex

<!-- badges: start -->
[![Build Status](https://travis-ci.org/kevinushey/rex.png?branch=master)](https://travis-ci.org/kevinushey/rex)
[![codecov.io](https://codecov.io/github/kevinushey/rex/coverage.svg?branch=master)](https://codecov.io/github/kevinushey/rex?branch=master)
[![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://www.tidyverse.org/lifecycle/#stable)
<!-- badges: end -->

### Friendly Regular Expressions

Regular expressions are very powerful feature, however they are often difficult
to interpret. Rex allows you to build complex regular expressions from human
readable expressions.  So instead of writing (and later trying to decipher)
```r
r <- "^(?:(((?:[^:])+)://))?((?:(?:(?!:/).)*)+)(?:(:([[:digit:]]+)))?(?:(/.*))?$"
```

You can write

```r
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
```

While these expressions are a big longer than their corresponding regular
expression, they are much more readable and maintainable.

## Installation

```r
install.packages("rex")
```

## Usage

The vignettes have longer form usage examples.

- [URL Validation](http://rpubs.com/jimhester/rex-url_parsing)
- [Webserver Log Parsing](http://rpubs.com/jimhester/rex-log_parsing)

Each `rex()` function call can include a number of functions and shortcuts.
For a full list of the functions available please see `?rex` and `?shortcuts`.

### Rex Mode

Rex functions are not exported because they are only useful within `rex()`
calls, but they can be temporarily attached using `rex_mode()` which allows
them to be auto-completed.

### Using Rex in other packages

Using `rex` in other packages will generate spurious NOTEs from `R CMD check`
unless you include a call to `rex::register_shortcuts()` with your package name
somewhere in your package source.  This function registers all of the rex
shortcuts as valid variables fixing the NOTEs.

## See Also
- [Regularity](https://github.com/andrewberls/regularity) - Ruby library that
  partially inspired `rex`.
- [PCRE](http://www.pcre.org/) - Perl Compatible Regular Expressions, the
  engine that `rex` regular expressions use.
- [Perl 5 Regular Expressions](http://perldoc.perl.org/perlre.html) - Perl
  regular expression documentation, which are nearly 100% compatible with PCRE.
