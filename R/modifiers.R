#' @include utils.R
NULL

#' @export
zero_or_more <- function(...) {
  p("(?:", escape_dots(...), ")*")
}

#' @export
one_or_more <- function(...) {
  p("(?:", escape_dots(...), ")+")
}

#' @export
maybe <- zero_or_one <- function(...) {
  p("(?:", escape_dots(...), ")?")
}

#' @export
possessive <- function(...) {
  p("(?:", escape_dots(...), ")*+")
}

#' @export
lazy <- first_match <- match_once <- function(...) {
  p("(?:", escape_dots(...), ")*?")
}

#' @export
not <- function(...) {
  p("[^", escape_dots(...), "]")
}
