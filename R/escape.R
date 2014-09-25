#' @include utils.R

#' @export
escape <- function(x) UseMethod("escape")

#' @export
escape.regex <- function(x) x

#' @export
escape.character <- function(x) {
  chars <-
    c('*',
      '.',
      '?',
      '^',
      '+',
      '$',
      '|',
      '(',
      ')',
      '[',
      ']',
      '{',
      '}')
  gsub(paste0('([\\', paste0(collapse="\\", chars), "])"), "\\\\\\1", x, perl=TRUE)
}

#' @export
escape.default <- escape.character

#' @export
escape.list <- function(x) {
  lapply(x, escape)
}

escape_dots <- function(...) {
  p(escape(list(...)))
}
