#' Match function
#'
#' @param data character vector to match against
#' @param pattern regular expression to use for matching
#' @param options regular expression options to use
#' @return if no captures, returns a logical vector the same length as the
#' input character vector specifying if the relevant value matched or not.  If
#' there are captures in the regular expression, returns a list of named
#' character vectors with the captured text.  If the g option is used with
#' capturing, the output is a list of lists.
#' @seealso \code{\link{regex}} Section `Perl-like Regular Expressions` for a
#' discussion of the supported options
#' @examples
#' string = c("this is a Test", "string")
#' re_matches(string, "test") # FALSE FALSE
#' re_matches(string, "Test", options='i') # TRUE FALSE
#' re_matches(string, "(Test)") # "Test" "FALSE"
#' @export
re_matches <- function(data, pattern, options = "") {
  process_matches <- function(res, data) {
    starts <- attr(res, "capture.start")
    if (is.null(starts)) {
      return(res != -1)
    }
    lengths <- attr(res, "capture.length")
    names <- attr(res, "capture.names")
    ret = list()
    for (itr in seq_len(ncol(starts))) {
      ret[[itr]] = unname(ifelse(starts[,itr] == -1, "FALSE",
                          substring(data, starts[,itr], starts[,itr] + lengths[,itr] - 1)))
    }
    names(ret) = ifelse(names == "", 1:ncol(starts), names)
    ret
  }

  if (grepl("g", options)) {
    options <- gsub("g", "", options)
    pattern <- add_options(pattern, options)
    mapply(process_matches, gregexpr(pattern = pattern, data, perl = TRUE), data)
  }
  else {
    pattern <- add_options(pattern, options)
    process_matches(regexpr(pattern = pattern, data, perl = TRUE), data)
  }
}

#' Substitution function
#'
#' @param data character vector to substitute
#' @param pattern regular expression to match
#' @param replacement replacement text to use
#' @param options option flags
#' @seealso \code{\link{regex}} Section "Perl-like Regular Expressions" for a
#' discussion of the supported options
#' @examples
#' string = c("this is a Test", "string")
#' re_substitutes(string, "test", "not a test", "i")
#' re_substitutes(string, "i", "x", "g")
#' @export
re_substitutes <- function(data, pattern, replacement, options = "") {
  res <- if (grepl(options, "g") == TRUE) {
    options <- gsub("g", "", options)
    pattern <- add_options(pattern, options)
    gsub(x = data, pattern = pattern, replacement = replacement, perl = TRUE)
  }
  else {
    pattern <- add_options(pattern, options)
    sub(x = data, pattern = pattern, replacement = replacement, perl = TRUE)
  }
  res
}

add_options <- function(pattern, options) {
  p("(?", p(options), ")", pattern)
}
