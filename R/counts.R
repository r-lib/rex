#' @include escape.R
#' @include utils.R
NULL

# TODO: Improve documentation, put +, * here as well
#' Counts
#'
#' Functions to restrict a regex to a specific number
#' @param x A regex pattern.
#' @param n An integer number
#' @param low An integer number for the lower limit.
#' @param high An integer number for the upper limit.
#' @aliases n
#' @family rex
#' @export
n_times <- function(x, n) {
  p("(?:", p(escape(x)), "){", n, "}")
}

#' @export
n <- n_times

#' @export
#' @describeIn n_times \code{x} must occur between \code{low} and \code{high} times.
between <- function(x, low, high) {
  p("(?:", p(escape(x)), "){", low, ",", high, "}")
}

#' @export
#' @describeIn n_times \code{x} must occur at least \code{n} times.
at_least <- function(x, n) {
  between(x, n, '')
}

#' @export
#' @describeIn n_times \code{x} must occur at most \code{n} times.
at_most <- function(x, n) {
  between(x, '', n)
}
