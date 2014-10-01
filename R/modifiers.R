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
#' @export
zero_or_more <- function(..., type = c("greedy", "lazy", "possessive")) {
  add_type(p("(?:", p(escape_dots(...)), ")*"), type)
}

#' @describeIn zero_or_more match one or more
#' @export
one_or_more <- function(..., type = c("greedy", "lazy", "possessive")) {
  add_type(p("(?:", p(escape_dots(...)), ")+"), type)
}

#' @describeIn zero_or_more match zero or one
#' @aliases zero_or_one
#' @export
maybe <- zero_or_one <- function(..., type = c("greedy", "lazy", "possessive")) {
  p("(?:", p(escape_dots(...)), ")?")
}

add_type <- function(x, type = c("greedy", "lazy", "possessive")) {
  type = match.arg(type)

  switch(type,
    greedy = x,
    lazy = p(x, "?"),
    possessive = p(x, "+")
  )
}
