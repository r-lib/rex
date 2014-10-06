#' @include utils.R
#' @include escape.R
NULL

#' Create character classes
#'
#' There are multiple ways you can create a character class.

#' @describeIn character_class explicitly define a character class
#' @inheritParams capture
#' @param start beginning of character class
#' @param end end of character class
#' @param x text to include in the character class (must be escaped manually)
#' @export
#' @family rex
#' @examples
#' # grey = gray
#' re <- rex("gr", one_of("a", "e"), "y")
#' grepl(re, c("grey", "gray")) # TRUE TRUE
#'
#' # Match non-vowels
#' re <- rex(none_of("a", "e", "i", "o", "u"))
#' # They can also be in the same string
#' re <- rex(none_of("aeiou"))
#' grepl(re, c("k", "l", "e")) # TRUE TRUE FALSE
#'
#' # Match range
#' re <- rex(range("a", "e"))
#' grepl(re, c("b", "d", "f")) # TRUE TRUE FALSE
#'
#' # Explicit creation
#' re <- rex(character_class("abcd\\["))
#' grepl(re, c("a", "d", "[", "]")) # TRUE TRUE TRUE FALSE
character_class <- function(x) structure(x, class = c("character_class", "regex"))

#' @describeIn character_class specify which characters to include
one_of <- function(...) {
  p( "[", p(character_class_escape_dots(...)), "]" )
}
register(one_of)

#' @describeIn character_class specify which characters to exclude
#' @aliases except
none_of <- except <- function(...) {
  p( "[^", p(character_class_escape_dots(...)), "]" )
}
register(none_of, except)

#' @describeIn character_class specify a range of which characters to include
range <- function(start, end) {
  character_class(p(character_class_escape(start), "-", character_class_escape(end)))
}
register(range)

#' @describeIn character_class specify a range of which characters to exclude
exclude_range <- function(start, end) {
  character_class(p("^", character_class_escape(start), "-", character_class_escape(end)))
}
register(exclude_range)
