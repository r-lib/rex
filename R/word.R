#' @include escape.R
#' @include utils.R
NULL

#' @export
n_times <- function(x, n) {
  p("(?:", p(escape(x)), "){", n, "}")
}

#' @export
between <- function(x, low, high) {
  p("(?:", p(escape(x)), "){", low, ",", high, "}")
}

#' @export
at_least <- function(x, n) {
  between(x, n, '')
}

#' @export
at_most <- function(x, n) {
  between(x, '', n)
}
