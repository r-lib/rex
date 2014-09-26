#' @include escape.R
#' @include utils.R
NULL

#' Lookarounds
#' 
#' These functions provide an interface to perl lookarounds.
#' 
#' Special binary functions are used to infer an ordering, since often you
#' might wish to match a word / set of characters conditional on the start
#' and end of that word.
#' 
#' @param x A regex pattern.
#' @param y A regex pattern.
#' @name lookarounds
#' @title Lookarounds
NULL

#' @export
"%if_next_is%" <- function(x, y) {
  p(escape(x), "(?=", escape(y), ")")
}

not <- if_next_isnt <- function(...) {
  p("(?:(?!", escape_dots(...), ").)*")
}
#' @export
"%if_next_isnt%" <- function(x, y) {
  p(escape(x), "(?!", escape(y), ")")
}

#' @export
"%if_prev_is%" <- function(x, y) {
  p("(?<=", escape(y), ")", escape(x))
}

#' @export
"%if_prev_isnt%" <- function(x, y) {
  p("(?<!", escape(y), ")", escape(x))
}
