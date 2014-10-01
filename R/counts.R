#' @include escape.R
#' @include utils.R
NULL

#' Counts
#'
#' Functions to restrict a regex to a specific number
#' @param x A regex pattern.
#' @param n An integer number
#' @param low An integer number for the lower limit.
#' @param high An integer number for the upper limit.
#' @inheritParams zero_or_more
#' @aliases n
#' @family rex
#' @export
n_times <- function(x, n, type = c("greedy", "lazy", "possessive")) {
  add_type(p("(?:", p(escape(x)), "){", n, "}"), type)
}

#' @export
n <- n_times

#' @export
#' @describeIn n_times \code{x} must occur between \code{low} and \code{high} times.
between <- function(x, low, high, type = c("greedy", "lazy", "possessive")) {
  add_type(p("(?:", p(escape(x)), "){", low, ",", high, "}"), type)
}

#' @export
#' @describeIn n_times \code{x} must occur at least \code{n} times.
at_least <- function(x, n, type = c("greedy", "lazy", "possessive")) {
  add_type(between(x, n, ''), type)
}

#' @export
#' @describeIn n_times \code{x} must occur at most \code{n} times.
at_most <- function(x, n, type = c("greedy", "lazy", "possessive")) {
  add_type(between(x, '', n), type)
}
