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
  newline = regex("\\R"),

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
  end = regex("$"),

  boundary = regex("\\b"),
  non_boundary = regex("\\B")
)

inverse <- function(x) {
  modify_and_name(x, "^%s", "non_%s", escape = FALSE)
}

plural <- function(x) {
  modify_and_name(x, "%s+", "%ss", "regex")
}

multiple <- function(x) {
  modify_and_name(x, "%s*", "any_%ss", "regex")
}

modify_and_name <- function(x, modifier, name_template, class = NULL, escape = TRUE) {
  class_missing <- missing(class)
  stats::setNames(lapply(x, function(xx) {
    structure(sprintf(modifier, if (isTRUE(escape)) escape(xx) else xx),
              class = if (class_missing) class(xx) else class)
  }), sprintf(name_template, names(x)))
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
