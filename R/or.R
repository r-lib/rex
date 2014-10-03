#' @include utils.R
#' @include escape.R
NULL

#' Or
#'
#' The special binary function \code{\%or\%} can be used to specify a set
#' of optional matches.
#'
#' @rdname or
#' @usage x \%or\% y
#' @param x A string.
#' @param y A string.
#' @inheritParams capture
#' @export
"%or%" <- function(x, y) {
  group(p(escape(x)), regex("|"), p(escape(y)))
}

#' describeIn or regular function can also be used, useful for more than 2 arguments.
#' @export
or <- function(...) {
  group(regex(paste0(collapse = "|", unlist(escape_dots(...)))))
}
