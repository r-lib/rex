## generic pasting function
#' @export
#' @family rex
p <- function(...) {
  regex(paste( sep="", collapse="", ...))
}

#' @export
#' @family rex
capture <- . <- function(...) {
  p( "(", escape_dots(...), ")" )
}

#' @export
#' @family rex
named_capture <- .. <- function(name, ... ) {
  p( "(?<", name, ">", escape_dots(...), ")" )
}

#' @export
#' @family rex
group <- function(...) {
  p( "(?:", escape_dots(...), ")" )
}

#' @export
#' @family rex
rex <- function(...) {

  ## Paste / repeater
  "*" <- function(x, y) {
    paste( rep(x, times=y), collapse="" )
  }

  ## Character class shortcuts
  alnum <- regex("[[:alnum:]]")
  alpha <- letter <- regex("[[:alpha:]]")
  blank <- regex("[[:blank:]]")
  cntrl <- regex("[[:cntrl:]]")
  digit <- regex("[[:digit:]]")
  graph <- regex("[[:graph:]]")
  lower <- regex("[[:lower:]]")
  print <- regex("[[:print:]]")
  punct <- regex("[[:punct:]]")
  space <- regex("[[:space:]]")
  upper <- regex("[[:upper:]]")
  xdigit <- regex("[[:xdigit:]]")

  space <- regex("\\s")
  spaces <- regex("\\s+")
  non_space <- regex("\\S")
  non_spaces <- regex("\\S+")

  number <- digit <- regex("\\d")
  numbers <- digits <- regex("\\d+")
  non_number <- non_digit <- regex("\\D")

  letter <- regex("[a-zA-Z]")
  letters <- regex("[a-zA-Z]+")
  non_letter <- regex("[^a-zA-Z]")

  start <- regex("^")
  end <- regex("$")

  dot <- "\\."
  any <- any_char <- regex(".")
  any_chars <- regex(".+")
  anything <- regex(".*")

  output <- eval(substitute(p(escape(list(...)))), enclos = parent.frame())
  n <- nchar(output)
  return(output)
}

#' @export
regex <- function(x) structure(x, class='regex')

#' @export
print.regex <- function(x, ...){
  cat(paste(strwrap(x), collapse="\n"), "\n", sep="")
}
#' @export
escape <- function(x) UseMethod("escape")

#' @export
escape.regex <- function(x) x

#' @export
escape.character <- function(x) {
  chars =
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
escape.default <- escape.character

#' @export
escape.list <- function(x) {
  lapply(x, escape)
}

escape_dots <- function(...) {
  p(escape(list(...)))
}

#' Pipe operator
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL
