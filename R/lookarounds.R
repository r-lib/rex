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
#' \itemize{
#'   \item \code{\%if_next_is\%}: \code{TRUE} if x follows y
#'   \item \code{\%if_next_isnt\%}: \code{TRUE} if x does not follow y
#'   \item \code{\%if_prev_is\%}: \code{TRUE} if y comes before x
#'   \item \code{\%if_prev_isnt\%}: \code{TRUE} if y does not come before x
#' }
#' @param x A regex pattern.
#' @param y A regex pattern.
#' @name lookarounds
#' @title Lookarounds
#' @family rex
#' @seealso Perl 5 Documentation \url{http://perldoc.perl.org/perlre.html#Extended-Patterns}
NULL

#' @rdname lookarounds
`%if_next_is%` <- function(x, y) {
  p("(?:", escape(x), "(?=", escape(y), ")", ")")
}
register(`%if_next_is%`)

#' @rdname lookarounds
`%if_next_isnt%` <- function(x, y) {
  p("(?:", escape(x), "(?!", escape(y), ")", ")")
}
register(`%if_next_isnt%`)

#' @rdname lookarounds
`%if_prev_is%` <- function(x, y) {
  p("(?:", "(?<=", escape(y), ")", escape(x), ")")
}
register(`%if_prev_is%`)

#' @rdname lookarounds
`%if_prev_isnt%` <- function(x, y) {
  p("(?:", "(?<!", escape(y), ")", escape(x), ")")
}
register(`%if_prev_isnt%`)

#' Do not match
#'
#' @inheritParams capture
#' @inheritParams zero_or_more
#' @family rex
# This is slightly different than if_next_isn't because we want to match
# anything that is not the search term as well
not <- function(..., type = c("greedy", "lazy", "possessive")) {
  add_type(p("(?:(?!", escape_dots(...), ").)*"), type = type)
}
register(not)
