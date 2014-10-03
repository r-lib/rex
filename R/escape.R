#' @include utils.R
NULL

#' Escape characters for a regex
#'
#' @param x Object to escape.
#' @export
escape <- function(x) UseMethod("escape")

#' @describeIn escape Objects are simply passed through unchanged.
#' @export
escape.regex <- function(x) x

#' @describeIn escape Objects are surrounded by braces.
#' @export
escape.character_class <- function(x) {
  p("[", x, "]")
}

#' @describeIn escape Objects are properly escaped for regular expressions.
#' @export
escape.character <- function(x) {
  chars <-
    c("*",
      ".",
      "?",
      "^",
      "+",
      "$",
      "|",
      "(",
      ")",
      "[",
      "]",
      "{",
      "}")
  regex(gsub(paste0("([\\", paste0(collapse = "\\", chars), "])"), "\\\\\\1", x, perl = TRUE))
}

#' @describeIn escape default escape coerces to character and escapes.
#' @export
escape.default <- function(x) {
  escape.character(as.character(x))
}

#' @describeIn escape simply call escape on all elements of the list.
#' @export
escape.list <- function(x) {
  lapply(x, escape)
}

escape_dots <- function(...) {
  unlist(escape(eval(list(...))))
}

#' Character class escapes
#' @inheritParams escape
#' @export
character_class_escape <- function(x) UseMethod("character_class_escape")

#' @describeIn character_class_escape objects are passed through unchanged.
#' @export
character_class_escape.regex <- function(x) x

#' @describeIn character_class_escape objects are passed through unchanged.
#' @export
character_class_escape.character_class <- character_class_escape.regex

#' @describeIn character_class_escape objects properly escaped for character classes.
#' @export
character_class_escape.character <- function(x) {
  chars <- c("-", "^", "[", "]")
  regex(gsub(paste0("([\\", paste0(collapse = "\\", chars), "])"), "\\\\\\1", x, perl = TRUE))
}

#' @describeIn character_class_escape call \code{character_class_escape} on all elements of the list.
#' @export
character_class_escape.list <- function(x) {
  lapply(x, character_class_escape)
}

#' @describeIn character_class_escape coerce to \code{character} and \code{character_class_escape}.
#' @export
character_class_escape.default <- function(x) {
  character_class_escape.character(as.character(x))
}

character_class_escape_dots <- function(...) {
  unlist(character_class_escape(eval(list(...))))
}
