#' Toggles \pkg{rex} mode.
#'
#' While within rex mode, functions used within the \code{\link{rex}} function
#' are attached, so one can get e.g. auto-completion within editors.
#'
#' @export
rex_mode <- function() {

  ## Enter rex mode
  if (!.rex$mode) {
    .rex$mode <- TRUE
    message("Rex functions and shortcuts attached!")
    ## We know what we're doing, so hide the R CMD check note
    suppressMessages(
      eval(call("attach", call("$", as.name(".rex"), as.name("env"))))
    )
  }

  ## Exit rex mode
  else {
    message("Rex functions and shortcuts detached!")
    .rex$mode <- FALSE
    detach(".rex$env")
  }

}
