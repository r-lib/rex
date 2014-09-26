#' @include utils.R

#' @export
escape <- function(x) UseMethod("escape")

#' @export
escape.regex <- function(x) x

#' @export
escape.POSIX <- function(x) {
  p('[', x, ']')
}

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
escape.default <- function(x) {
  escape.character(as.character(x))
}

#' @export
escape.list <- function(x) {
  lapply(x, escape)
}

escape_dots <- function(...) {
  escape(list(...))
}

## Use different escaping within character classes
#' @export
bracket_escape <- function(x) UseMethod("bracket_escape")

#' @export
bracket_escape.regex <- bracket_escape.POSIX <- function(x) x

#' @export
bracket_escape.character <- function(x) {
  chars <- c("-", "^", "[", "]")
  gsub(paste0('([\\', paste0(collapse="\\", chars), "])"), "\\\\\\1", x, perl=TRUE)
}

#' @export
bracket_escape.list <- function(x) {
  lapply(x, bracket_escape)
}

#' @export
bracket_escape.default <- function(x) {
  bracket_escape.character(as.character(x))
}
