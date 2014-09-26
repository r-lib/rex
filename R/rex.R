#' @include escape.R
#' @include utils.R
NULL

#' @export
#' @family rex
capture <- . <- function(...) {
  p( "(", p(escape_dots(...)), ")" )
}

#' @export
#' @family rex
named_capture <- .. <- function(name, ... ) {
  p( "(?<", name, ">", p(escape_dots(...)), ")" )
}

#' @export
#' @family rex
group <- function(...) {
  p( "(?:", p(escape_dots(...)), ")" )
}

#' @export
#' @family rex
rex <- function(..., env = parent.frame()) {
  args <- lazyeval::lazy_dots(...)
  rex_(args, env)
}

#' @export
#' @family rex
one_of <- function(...) {
  p( "[", p(bracket_escape(list(...))), "]" )
}

#' @export
#' @family rex
except <- none_of <- function(...) {
  p( "[^", p(bracket_escape(list(...))), "]" )
}

#' @export
#' @family rex
range <- function(x, y) {
  p(escape(x), '-', escape(y))
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

#' @export
POSIX <- function(x) structure(x, class='POSIX')

shortcuts <- list(

  ## Paste / repeater
  "*" = function(x, y) {
    paste( rep(x, times=y), collapse="" )
  },

  ## Character class shortcuts
  alnum = POSIX("[:alnum:]"),
  alpha = letter <- POSIX("[:alpha:]"),
  blank = POSIX("[:blank:]"),
  cntrl = POSIX("[:cntrl:]"),
  digit = POSIX("[:digit:]"),
  graph = POSIX("[:graph:]"),
  lower = POSIX("[:lower:]"),
  print = POSIX("[:print:]"),
  punct = POSIX("[:punct:]"),
  space = POSIX("[:space:]"),
  upper = POSIX("[:upper:]"),
  xdigit = POSIX("[:xdigit:]"),

  letter = POSIX("a-zA-Z"),
  letters = regex("[a-zA-Z]+"),
  non_letter = POSIX("^a-zA-Z"),

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
  anything = regex(".*")
)
