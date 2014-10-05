

#' Rex Mode
#' 
#' \code{rex_mode()} switch \pkg{rex} mode on and off. While within
#' rex mode, functions used within the \code{\link{rex}} function are
#' attached, and so one can get e.g. auto-completion within editors.
#' 
#' @export
rex_mode <- function() {
  
  ## Enter rex mode
  if (!.rex$mode) {
    .rex$mode <- TRUE
    attach(.rex$env)
  }
  
  ## Exit rex mode
  else {
    .rex$mode <- FALSE
    detach(.rex$env)
  }
  
}
