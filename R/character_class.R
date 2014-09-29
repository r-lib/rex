#' @include utils.R
#' @include escape.R
NULL

#' Create character classes
#'
#' There are four different ways you can create a character class.

#' @describeIn character_class explicitly define a character class
#' @inheritParams capture
#' @export
#' @family rex
character_class <- function(x) structure(x, class=c("character_class", "regex"))

#' @export
#' @describeIn character_class specify which characters to include
one_of <- function(...) {
  p( "[", p(character_class_escape_dots(...)), "]" )
}

#' @export
#' @describeIn character_class specify which characters to exclude
except <- none_of <- function(...) {
  p( "[^", p(character_class_escape_dots(...)), "]" )
}

#' @export
#' @describeIn character_class specify a range of which characters to include
range <- function(x, y) {
  character_class(p(character_class_escape(x), '-', character_class_escape(y)))
}
