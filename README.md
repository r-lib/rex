# Rex
[![Build Status](https://travis-ci.org/kevinushey/rex.png?branch=master)](https://travis-ci.org/kevinushey/rex)
### Friendly regular expressions for R

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
library(devtools)
install_github("kevinushey/rex")
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

## See Also
- [Regularity](https://github.com/andrewberls/regularity) - Ruby library that
  partially inspired rex.
- [PCRE](http://www.pcre.org/) - Perl Compatible Regular Expressions, the
  engine that rex regular expressions use.
- [Perl 5 Regular Expressions](http://perldoc.perl.org/perlre.html) - Good
  resource for Perl 5 regular expressions, which are nearly 100% compatible
  with PCRE.
