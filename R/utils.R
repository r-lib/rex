p <- function(...) {
  regex(paste0(collapse = "", ...))
}

`%==%` <- function(x, y) {
  identical(x, y)
}
