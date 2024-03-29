#' @include escape.R
#' @include utils.R
NULL

#' Create a capture group
#'
#' Used to save the matched value within the group for use later in the regular
#' expression or to extract the values captured.  Both named and unnamed groups
#' can later be referenced using \code{\link{capture_group}}.
#'
#' @param name of the group.  Unnamed capture groups are numbers starting at 1
#' in the order they appear in the regular expression.  If two groups have the
#' same name, the leftmost group is the used in any reference.
#' @param ... \code{\link{shortcuts}}, R variables, text, or other \pkg{rex}
#' functions.
#' @family rex
#' @aliases .
#' @seealso \code{\link{group}} for grouping without capturing.  Perl 5 Capture
#' Groups \url{https://perldoc.perl.org/perlre#Capture-groups}
#' @examples
#'
#' # Match paired quotation marks
#' re <- rex(
#'   # first quotation mark
#'   capture(quotes),
#'
#'   # match all non-matching quotation marks
#'   zero_or_more(except(capture_group(1))),
#'
#'   # end quotation mark (matches first)
#'   capture_group(1)
#' )
#'
#' #named capture - don't match apples to oranges
#' re <- rex(
#'   capture(name = "fruit", or("apple", "orange")),
#'   "=",
#'   capture_group("fruit")
#' )
capture <- . <- function(..., name = NULL) {
  if(!is.null(name)) {
    name <- paste0("?<", name, ">")
  }
  p( "(", name, p(escape_dots(...)), ")" )
}
register(capture, .)

#' @rdname capture
capture_group <- function(name) {
  p( "\\g{", name, "}" )
}
register(capture_group)

#' Create a grouped expression
#'
#' This is similar to \code{\link{capture}} except that it does not store the
#' value of the group.  Best used when you want to combine several parts
#' together and do not reference or extract the grouped value later.
#' @inheritParams capture
#' @seealso \code{\link{capture}} for grouping with capturing.  Perl 5 Extended
#' Patterns \url{https://perldoc.perl.org/perlre#Extended-Patterns}
#' @family rex
group <- function(...) {
  p( "(?:", p(escape_dots(...)), ")" )
}
register(group)
