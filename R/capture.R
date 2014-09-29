#' @include escape.R
#' @include utils.R
NULL

#' Create a capture group
#'
#' Used to save the matched value within the group for use later in the regular
#' expression or to extract the values captured.  There are two types of
#' capture groups.  Both types can later be referenced using \code{\link{capture_group}}.
#' \describe{
#'   \item{Unnamed Capture}{- \code{\link{capture}} or
#'   \code{\link{.}}.  The groups are numbered incrementally starting at one.}
#'   \item{Named Capture}{- \code{\link{named_capture}} or
#'   \code{\link{..}}.  The groups are named explicitly by the user.}
#' }
#'
#' @param name of the group.  Unnamed capture groups are numbers starting at 1
#' in the order they appear in the regular expression.  If two groups have the
#' same name, the leftmost group is the used in any reference.
#' @param ... variables to capture.  Can be \code{\link{shortcuts}}, R
#' variables, text, or other \pkg{rex} functions.
#' @export
#' @family rex
#' @rdname capture
#' @aliases .
#' @seealso \code{\link{group}} for grouping without capturing.  Perl 5 Capture
#' Groups \url{http://perldoc.perl.org/perlre.html#Capture-groups}
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
#'   named_capture("fruit", or("apple", "orange")),
#'   "=",
#'   capture_group("fruit")
#' )
capture <- . <- function(...) {
  p( "(", p(escape_dots(...)), ")" )
}

#' @export
#' @rdname capture
#' @aliases ..
named_capture <- .. <- function(name, ... ) {
  p( "(?<", name, ">", p(escape_dots(...)), ")" )
}

#' @export
#' @rdname capture
capture_group <- function(name) {
  p( "\\g{", name, "}" )
}

#' Create a grouped expression
#'
#' This is similar to \code{\link{capture}} except that it does not store the
#' value of the group.  Best used when you want to combine several parts
#' together and do not reference or extract the grouped value later.
#' @inheritParams capture
#' @export
#' @seealso \code{\link{capture}} for grouping with capturing.  Perl 5 Extended
#' Patterns \url{http://perldoc.perl.org/perlre.html#Extended-Patterns}
#' @family rex
group <- function(...) {
  p( "(?:", p(escape_dots(...)), ")" )
}
