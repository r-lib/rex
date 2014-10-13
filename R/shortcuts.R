#' Single shortcuts
#'
#' Each of these shortcuts has both a plural (-s) and inverse (non_) form.
single_shortcuts <- list(

  ## Character class shortcuts
  alnum = character_class("[:alnum:]"),
  alpha = character_class("[:alpha:]"),
  letter = character_class("[:alpha:]"),
  blank = character_class("[:blank:]"),
  cntrl = character_class("[:cntrl:]"),
  digit = character_class("[:digit:]"),
  number = character_class("[:digit:]"),
  graph = character_class("[:graph:]"),
  lower = character_class("[:lower:]"),
  print = character_class("[:print:]"),
  punct = character_class("[:punct:]"),
  space = character_class("[:space:]"),
  upper = character_class("[:upper:]"),
  xdigit = character_class("[:xdigit:]"),

  single_quote = character_class("'"),
  double_quote = character_class("\""),
  quote = character_class("'\"")
)

basic_shortcuts <- list(

  dot = escape("."),
  any = any_char <- regex("."),
  something = regex(".+"),
  anything = regex(".*"),

  start = regex("^"),
  end = regex("$")

)

inverse <- function(x) {
  x[] <- lapply(x, function(xx) { val = paste0("^", xx); class(val) <- class(xx); val })
  names(x) <- paste0("non_", names(x))
  x
}

plural <- function(x) {
  x[] <- lapply(x, function(xx) { val = paste0(escape(xx), "+"); class(val) <- "regex"; val })
  names(x) <- paste0(names(x), "s")
  x
}

multiple <- function(x) {
  x[] <- lapply(x, function(xx) { val = paste0(escape(xx), "*"); class(val) <- "regex"; val })
  names(x) <- paste0("any_", names(x), "s")
  x
}

#' Shortcuts
#'
#' Commonly used character classes and regular expressions.  These shortcuts
#' are substituted inside \code{rex} calls.
#'
#' \code{names(shortcuts)} will give you the full list of available shortcuts.
#' @export
#' @family rex

shortcuts <- c(
  basic_shortcuts,
  single_shortcuts,
  plural(single_shortcuts),
  multiple(single_shortcuts),
  inverse(single_shortcuts),
  plural(inverse(single_shortcuts)),
  multiple(inverse(single_shortcuts))
)

register_object(shortcuts)
