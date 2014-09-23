#' @export
zero_or_more <- function(...) {
  p("(?:", ..., ")*")
}

#' @export
one_or_more <- function(...) {
  p("(?:", ..., ")+")
}

#' @export
maybe <- zero_or_one <- function(...) {
  p("(?:", ..., ")?")
}

#' @export
possessive <- function(...) {
  p("(?:", ..., ")*+")
}

#' @export
lazy <- first_match <- match_once <- function(...) {
  p("(?:", ..., ")*?")
}

#' @export
not <- function(...) {
  p("[^", ..., "]")
}
