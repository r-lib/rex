##' @export
n_times <- function(x, n) {
  p("(?:", x, "){", n, "}")
}
