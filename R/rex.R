#' @include escape.R
#' @include character_class.R
#' @include utils.R
NULL

#' @export
#' @family rex
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

#' @export
print.regex <- function(x, ...){
  cat(paste(strwrap(x), collapse="\n"), "\n", sep="")
}


#' @export
regex <- function(x) structure(x, class='regex')

shortcuts <- list(

  ## Character class shortcuts
  alnum = character_class("[:alnum:]"),
  alpha = letter <- character_class("[:alpha:]"),
  blank = character_class("[:blank:]"),
  cntrl = character_class("[:cntrl:]"),
  digit = character_class("[:digit:]"),
  graph = character_class("[:graph:]"),
  lower = character_class("[:lower:]"),
  print = character_class("[:print:]"),
  punct = character_class("[:punct:]"),
  space = character_class("[:space:]"),
  upper = character_class("[:upper:]"),
  xdigit = character_class("[:xdigit:]"),

  letter = character_class("a-zA-Z"),
  letters = regex("[a-zA-Z]+"),
  non_letter = character_class("^a-zA-Z"),

  space = regex("\\s"),
  spaces = regex("\\s+"),
  non_space = regex("\\S"),
  non_spaces = regex("\\S+"),

  number = regex("\\d"),
  numbers = regex("\\d+"),
  non_number = non_digit <- regex("\\D"),

  start = regex("^"),
  end = regex("$"),

  dot = escape("."),

  any = any_char <- regex("."),
  any_chars = regex(".+"),
  anything = regex(".*"),

  quotes = character_class("'\"")
)
