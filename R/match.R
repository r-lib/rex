#' Match function
#'
#' @param data character vector to match against
#' @param pattern regular expression to use for matching
#' @return if no captures, returns a logical vector the same length as the
#' input character vector specifying if the relevant value matched or not.  If
#' there are captures in the regular expression, returns a data.frame with a
#' column for each capture group.
#' @examples
#' string = c("this is a", "test string")
#' re_matches(string, rex("test")) # FALSE FALSE
#'
#' # named capture
#' re_matches(string, rex(capture(alphas, name = "first_word"), space,
#'   capture(alphas, name = "second_word")))
#' #   first_word second_word
#' # 1       this          is
#' # 2       test      string
#'
#' # capture returns NA when it fails to match
#' re_matches(string, rex(capture("test")))
#' #      1
#' # 1 test
#' # 2 <NA>
#' @export
re_matches <- function(data, pattern) {
  process_matches <- function(match, string) {

    # if no capture just return if the regex matched
    if(no_capture(match)) {
      return(match != -1)
    }

    # if a capture return a data frame with the capture results for each string
    starts <- attr(match, "capture.start")
    lengths <- attr(match, "capture.length")
    ends <- starts + lengths - 1

    strings = substring(string, starts, ends)

    not_matched <- starts == -1L
    strings[not_matched] <- NA

    res = matrix(nrow = length(data), strings)

    colnames(res) = auto_name(attr(match, "capture.names"))

    as.data.frame(res, stringsAsFactors = FALSE)
  }

  process_matches(regexpr(pattern = pattern, data, perl = TRUE), data)
}

no_capture <- function(match) {
  is.null(attr(match, "capture.start", exact = TRUE))
}

auto_name <- function(names) {
  missing <- names == ""
  if (all(!missing)) {
    return(names)
  }
  names[ names == "" ] = seq_along(names)[ names == "" ]
  names
}
