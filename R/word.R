##' @export
n_times <- function(x, n) {
  p("(?:", escape(x), "){", n, "}")
}

##' @export
between <- function(x, low, high) {
  p("(?:", escape(x), "){", low, ",", high, "}")
}

##' @export
at_least <- function(x, n) {
  bewtween(x, n, '')
}

##' @export
at_most <- function(x, n) {
  between(x, '', n)
}
