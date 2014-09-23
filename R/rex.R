## generic pasting function
#' @export
#' @family rex
p <- function(...) {
  paste( sep="", collapse="", ... )
}

#' @export
#' @family rex
capture <- . <- function(...) {
  p( "(", ..., ")" )
}

#' @export
#' @family rex
named_capture <- .. <- function( name, ... ) {
  p( "(?<", name, ">", ..., ")" )
}

#' @export
#' @family rex
rex <- function(...) {
  
  ## Paste / repeater
  "*" <- function(x, y) {
    paste( rep(x, times=y), collapse="" )
  }
  
  ## Character class shortcuts
  alnum <- "[[:alnum:]]"
  alpha <- letter <- "[[:alpha:]]"
  blank <- "[[:blank:]]"
  cntrl <- "[[:cntrl:]]"
  digit <- "[[:digit:]]"
  graph <- "[[:graph:]]"
  lower <- "[[:lower:]]"
  print <- "[[:print:]]"
  punct <- "[[:punct:]]"
  space <- "[[:space:]]"
  upper <- "[[:upper:]]"
  xdigit <- "[[:xdigit:]]"
  
  space <- "\\s"
  spaces <- "\\s+"
  non_space <- "\\S"
  
  number <- digit <- "\\d"
  numbers <- digits <- "\\d+"
  non_number <- non_digit <- "\\D"
  
  letter <- "[a-zA-Z]"
  letters <- "[a-zA-Z]+"
  non_letter <- "[^a-zA-Z]"
  
  start <- "^"
  end <- "$"
  
  dot <- "\\."
  any <- any_char <- "."
  
  output <- eval(substitute(p(...)), enclos = parent.frame())
  n <- nchar(output)
  if (substring(output, n, n) != "$")
    output <- paste0(output, ".*$")
  return(output)
  
}