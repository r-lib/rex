p <- function(...) {
  regex(paste(sep = "", collapse = "", ...))
}

`%==%` <- function(x, y) {
  identical(x, y)
}
