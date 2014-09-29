#' @include utils.R

#' @export
escape <- function(x) UseMethod("escape")

#' @export
escape.regex <- function(x) x

#' @export
escape.character_class <- function(x) {
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
  unlist(escape(eval(list(...))))
}

## Use different escaping within character classes
#' @export
character_class_escape <- function(x) UseMethod("character_class_escape")

#' @export
character_class_escape.regex <- function(x) x

#' @export
character_class_escape.character_class <- character_class_escape.regex

#' @export
character_class_escape.character <- function(x) {
  chars <- c("-", "^", "[", "]")
  regex(gsub(paste0('([\\', paste0(collapse="\\", chars), "])"), "\\\\\\1", x, perl=TRUE))
}

#' @export
character_class_escape.list <- function(x) {
  lapply(x, character_class_escape)
}

#' @export
character_class_escape.default <- function(x) {
  character_class_escape.character(as.character(x))
}
