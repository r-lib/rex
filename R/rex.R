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

  # this needs the as.list because eval only looks at the enclos if envir is
  # not an environment
  regex(p(escape(lazyeval::lazy_eval(args, as.list(.rex$env)))))
}

#' @describeIn regex coerce regex object to a character
#' @export
as.character.regex <- function(x, ...) x

#' Coerce objects to a \code{\link{regex}}.
#' @name as.regex
#' @param x Object to coerce to \code{\link{regex}}.
#' @param ... further arguments passed to methods.
#' @export
as.regex <- function(x, ...) UseMethod("as.regex")

#' @export
#' @describeIn as.regex Simply escape the Object.
as.regex.default <- function(x, ...) escape(x)

#' @export
#' @describeIn regex Print regex object
print.regex <- function(x, ...){
  cat(paste(strwrap(x), collapse = "\n"), "\n", sep = "")
}

#' Regular Expression
#'
#' Specify an explicit regular expression.  This expression must already be
#' escaped.
#' @param x Object
#' @param ... further arguments
#' @seealso \code{\link{as.regex}} to coerce to a regex object.
#' @export
regex <- function(x, ...) structure(x, class = "regex")

#' Register the Rex shortcuts
#'
#' If you are using rex in another package you need to call this function to
#' register all of the rex shortcuts so that spurious NOTEs about global
#' variables being generated during R CMD check.
#' @param pkg_name the package to register the shortcuts in
#' @export
register_shortcuts <- function(pkg_name) {
  invisible(utils::globalVariables(ls(.rex$env), pkg_name))
}
