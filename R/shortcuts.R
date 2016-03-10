shortcut <- function(...) {
  data <- list(...)
  if (length(data) == 1L) {
    data <- data[[1L]]
  }
  structure(data, class = "shortcut")
}

#' Single shortcuts
#'
#' Each of these shortcuts has both a plural (-s) and inverse (non_) form.
single_shortcuts <- shortcut(

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

basic_shortcuts <- shortcut(

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
  x[] <- lapply(x, function(xx) {
    val <- paste0("^", xx); class(val) <- class(xx)
    val
  })
  names(x) <- paste0("non_", names(x))
  x
}

plural <- function(x) {
  x[] <- lapply(x, function(xx) {
    val <- paste0(escape(xx), "+"); class(val) <- "regex"
    val
  })
  names(x) <- paste0(names(x), "s")
  x
}

multiple <- function(x) {
  x[] <- lapply(x, function(xx) {
    val <- paste0(escape(xx), "*"); class(val) <- "regex"
    val
  })
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

shortcuts <- shortcut(c(
  basic_shortcuts,
  single_shortcuts,
  plural(single_shortcuts),
  multiple(single_shortcuts),
  inverse(single_shortcuts),
  plural(inverse(single_shortcuts)),
  multiple(inverse(single_shortcuts))
))

default_data_format.shortcut <- function(x) {
  build_rd <- get("build_rd", envir = asNamespace("roxygen2"))
  rd <- get("rd", envir = asNamespace("roxygen2"))

  build_rd(rd("\\preformatted{"), paste0(names(x), " - ", x, collapse = "\n"), rd("}"))
}

register_object(shortcuts)
