#' @include escape.R
#' @include utils.R
NULL

#' @export
zero_or_more <- function(...) {
  p("(?:", p(escape_dots(...)), ")*")
}

#' @export
one_or_more <- function(...) {
  p("(?:", p(escape_dots(...)), ")+")
}

#' @export
maybe <- zero_or_one <- function(...) {
  p("(?:", p(escape_dots(...)), ")?")
}

#' @export
possessive <- function(...) {
  p("(?:", p(escape_dots(...)), ")*+")
}

#' @export
lazy <- first_match <- match_once <- function(...) {
  p("(?:", p(escape_dots(...)), ")*?")
}
