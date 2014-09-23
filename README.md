# Rex
[![Build Status](https://travis-ci.org/kevinushey/rex.png?branch=master)](https://travis-ci.org/kevinushey/rex)
### Friendly regular expressions for R

Regular expressions are very powerful feature, however they are often difficult
to interpret. Rex allows you to build complex regular expressions from human
readable expressions.  So instead of writing (and later trying to decipher)
```r
r <- "^(?:(((?:[^:])+)://))?((?:[^:/])+)(?:(:((?:\\d+)+)))?(?:(/(?:.)*))?$"
```

You can write

```r
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
```

## Methods
### General
- `capture()`
- `named_capture()`
- `%or%`

### Modifiers
- `zero_or_more()`
- `one_or_more()`
- `maybe()`
- `possessive()`
- `lazy()`
- `not()`
- `n_times()`

### Lookaround
- `if_next_is()`
- `if_next_isnt()`
- `if_prev_is()`
- `if_prev_isnt()`
- `begin()`
- `end()`

## Inspirations
- [Regularity](https://github.com/andrewberls/regularity)
