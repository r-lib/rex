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
  alpha <- "[[:alpha:]]"
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
  
  space <- whitespace <- "\\s"
  non_whitespace <- "\\S"
  spaces <- "\\s+"
  
  number <- digit <- "\\d"
  numbers <- "\\d+"
  non_number <- non_digit <- "\\D"
  
  numbers <- "\\d+"
  letters <- "\\w+"
  start <- "^"
  end <- "$"
  word <- char <- "\\w"
  non_word <- non_char <- "\\W"
  any_char <- "."
  
  output <- eval( substitute( p(...) ), enclos=parent.frame() )
  n <- nchar(output)
  if (substring(output, n-1, n) != ".*")
    output <- paste0(output, ".*")
  return(output)
  
}