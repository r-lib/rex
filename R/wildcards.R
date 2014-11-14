#' @include escape.R
#' @include utils.R
NULL

#' Wildcards
#'
#' @inheritParams capture
#' @param type the type of match to perform.
#'
#' There are three match types
#' \enumerate{
#'   \item \code{greedy}: match the longest string.  This is the default matching type.
#'   \item \code{lazy}: match the shortest string.  This matches the shortest string from the same anchor point, not necessarily the shortest global string.
#'   \item \code{possessive}: match and don't allow backtracking
#' }
#' @family rex
#' @name wildcards
NULL

#' @describeIn wildcards match \code{...} zero or more times.
zero_or_more <- function(..., type = c("greedy", "lazy", "possessive")) {
  add_type(p("(?:", p(escape_dots(...)), ")*"), type)
}
register(zero_or_more)

#' @describeIn wildcards match \code{...} one or more times.
one_or_more <- function(..., type = c("greedy", "lazy", "possessive")) {
  add_type(p("(?:", p(escape_dots(...)), ")+"), type)
}
register(one_or_more)

#' @describeIn wildcards match \code{...} zero or one times.
#' @aliases zero_or_one
maybe <- zero_or_one <- function(..., type = c("greedy", "lazy", "possessive")) {
  p("(?:", p(escape_dots(...)), ")?")
}
register(maybe, zero_or_one)

add_type <- function(x, type = c("greedy", "lazy", "possessive")) {
  type <- match.arg(type)

  switch(type,
    greedy = x,
    lazy = p(x, "?"),
    possessive = p(x, "+")
  )
}

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
