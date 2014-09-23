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
#' @export
if_next_is <- function(x) {
  p("(?=", x, ")")
}

#' @export
"%if_next_is%" <- function(x, y) {
  p(x, "(?=", y, ")")
}

#' @export
if_next_isnt <- function(x) {
  p("(?!", x, ")")
}

#' @export
"%if_next_isnt%" <- function(x, y) {
  p(x, "(?!", y, ")")
}

#' @export
if_prev_is <- function(x) {
  p("(?<=", x, ")")
}

#' @export
"%if_prev_is%" <- function(x, y) {
  p("(?<=", y, ")", x)
}

#' @export
if_prev_isnt <- function(x) {
  p("(?<!", x, ")")
}

#' @export
"%if_prev_isnt%" <- function(x, y) {
  p(x, "(?<!", y, ")")
}

#' @export
begin <- function(...) {
  p("^", ...)
}

#' @export
end <- function(...) {
  p(..., "$")
}
