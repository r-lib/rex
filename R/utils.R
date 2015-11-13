p <- function(...) {
  regex(paste(sep = "", collapse = "", ...))
}

#' Pipe operator
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL

`%==%` <- function(x, y) {
  identical(x, y)
}
