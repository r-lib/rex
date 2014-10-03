#' @include escape.R
#' @include character_class.R
#' @include utils.R
NULL

#' Generate a regular expression.
#' @export
#' @family rex
#' @param ... \code{\link{shortcuts}}, R variables, text, or other \pkg{rex}
#' functions.
#' @param env environment to evaluate the rex expression in.
#' @aliases rex_
rex <- function(..., env = parent.frame()) {
  args <- lazyeval::lazy_dots(...)
  rex_(args, env)
}

#' @export
rex_ <- function(args, env = parent.frame()) {

  args <- lazyeval::as.lazy_dots(args, env)

  output <- regex(p(escape(lazyeval::lazy_eval(args, shortcuts))))

  return(output)
}

#' Convert regex to a character
#' @param x \code{regex} object.
#' @param ... further args ignored by method.
#' @export
as.character.regex <- function(x, ...) escape(x)

#' Print regex object
#' @param x Object to be printed.
#' @param ... further args are ignored.
#' @export
print.regex <- function(x, ...){
  cat(paste(strwrap(x), collapse = "\n"), "\n", sep = "")
}

#' Regular Expression
#'
#' Specify an explicit regular expression.  This expression must already be
#' escaped.
#' @param x Object to be coerced to a regex.
#' @export
regex <- function(x) structure(x, class = "regex")

