#' @include utils.R
#' @include escape.R
NULL

#' Create character classes
#'
#' There are multiple ways you can define a character class.

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
#' @describeIn character_class explicitly define a character class
character_class <- function(x) structure(x, class = c("character_class", "regex"))

#' @describeIn character_class matches one of the specified characters.
one_of <- function(...) {
  p( "[", p(character_class_escape_dots(...)), "]" )
}
register(one_of)

#' @describeIn character_class matches zero or more of the specified characters.
any_of <- function(...) {
  zero_or_more(one_of(...))
}
register(any_of)

#' @describeIn character_class matches one or more of the specified characters.
some_of <- function(...) {
  one_or_more(one_of(...))
}
register(some_of)

#' @describeIn character_class matches anything but one of the specified characters.
#' @aliases except
none_of <- except <- function(...) {
  p( "[^", p(character_class_escape_dots(...)), "]" )
}
register(none_of, except)

#' @describeIn character_class matches zero or more of anything but the specified characters.
except_any <- function(...) {
  zero_or_more(none_of(...))
}
register(except_any)

#' @describeIn character_class matches one or more of anything but the specified characters.
except_some <- function(...) {
  one_or_more(none_of(...))
}

#' @describeIn character_class matches one of any of the characters in the range.
range <- function(start, end) {
  character_class(p(character_class_escape(start), "-", character_class_escape(end)))
}
register(range)

#' @describeIn character_class matches one of any of the characters except those in the range.
exclude_range <- function(start, end) {
  character_class(p("^", character_class_escape(start), "-", character_class_escape(end)))
}
register(exclude_range)
