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
if_next_is <- function(...) {
  p("(?=", p(escape_dots(...)), ")")
}

#' @export
"%if_next_is%" <- function(x, y) {
  p(escape(x), "(?=", escape(y), ")")
}

#' @export
if_next_isnt <- function(...) {
  p("(?!", p(escape_dots(...)), ")")
}

#' @export
"%if_next_isnt%" <- function(x, y) {
  p(x, "(?!", escape(y), ")")
}

#' @export
if_prev_is <- function(...) {
  p("(?<=", p(escape_dots(...)), ")")
}

#' @export
"%if_prev_is%" <- function(x, y) {
  p("(?<=", escape(y), ")", escape(x))
}

#' @export
if_prev_isnt <- function(...) {
  p("(?<!", p(escape_dots(...)), ")")
}

#' @export
"%if_prev_isnt%" <- function(x, y) {
  p(escape(x), "(?<!", escape(y), ")")
}

#' @export
begin <- starts_with <- function(...) {
  p("^", p(escape_dots(...)))
}

#' @export
end <- ends_with <- function(...) {
  p(p(escape_dots(...)), "$")
}
