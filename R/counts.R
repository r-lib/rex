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
#' @family rex
#' @name counts
NULL

#' @aliases n
#' @describeIn counts \code{x} must occur exactly \code{n} times.
n_times <- n <- function(x, n, type = c("greedy", "lazy", "possessive")) {
  add_type(p("(?:", p(escape(x)), "){", n, "}"), type)
}
register(n_times, n)

#' @describeIn counts \code{x} must occur between \code{low} and \code{high} times.
between <- function(x, low, high, type = c("greedy", "lazy", "possessive")) {
  add_type(p("(?:", p(escape(x)), "){", low, ",", high, "}"), type)
}
register(between)

#' @describeIn counts \code{x} must occur at least \code{n} times.
at_least <- function(x, n, type = c("greedy", "lazy", "possessive")) {
  add_type(between(x, n, ""), type)
}
register(at_least)

#' @describeIn counts \code{x} must occur at most \code{n} times.
at_most <- function(x, n, type = c("greedy", "lazy", "possessive")) {
  add_type(between(x, 0, n), type)
}
register(at_most)
