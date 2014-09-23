zero_or_more <- function(...) {
  p("(?:", ..., ")*")
}

one_or_more <- function(...) {
  p("(?:", ..., ")+")
}

maybe <- zero_or_one <- function(...) {
  p("(?:", ..., ")?")
}

possessive <- function(...) {
  p("(?:", ..., ")*+")
}

lazy <- first_match <- match_once <- function(...) {
  p("(?:", ..., ")*?")
}

not <- function(...) {
  p("[^", ..., "]")
}