#' Match function
#'
#' @param data character vector to match against
#' @param pattern regular expression to use for matching
#' @param global use global matching
#' @param options regular expression options
#' @param locations rather than returning the values of the matched (or
#' captured) string, return a \code{data.frame} of the match locations in the
#' string.
#' @param ... options passed to regexpr or gregexpr
#' @return if no captures, returns a logical vector the same length as the
#' input character vector specifying if the relevant value matched or not.  If
#' there are captures in the regular expression, returns a \code{data.frame} with a
#' column for each capture group.  If \code{global} is \code{TRUE}, returns a
#' list of \code{data.frame}s.
#' @seealso \code{\link{regexp}} Section "Perl-like Regular Expressions" for a
#' discussion of the supported options
#' @examples
#' string <- c("this is a", "test string")
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
re_matches <- function(data, pattern, global = FALSE, options = NULL, locations = FALSE, ...) {

  pattern <- add_options(pattern, options)

  process_matches <- function(match, string) {

    if(no_capture(match)) {

      # if no capture and no location just return if the regex matched
      if(!locations) {
        return(match != -1L)
      }

      # else return a data frame of the start and end locations
      match[ match == -1L ] <- NA_integer_
      starts <- match
      attributes(starts) <- NULL

      lengths <- attr(match, "match.length")
      ends <- starts + lengths - 1L

      return(data.frame(start = starts, end = ends))
    }

    # if a capture return a data frame with the capture results for each string
    starts <- attr(match, "capture.start")
    lengths <- attr(match, "capture.length")
    ends <- starts + lengths - 1L

    not_matched <- starts == -1L

    if(!locations) {
      strings <- substring(string, starts, ends)

      strings[not_matched] <- NA_integer_

      res <- matrix(ncol = ncol(starts), strings)

      colnames(res) <- auto_name(attr(match, "capture.names"))

      as.data.frame(res, stringsAsFactors = FALSE)
    }
    else {
      starts[not_matched] <- NA_integer_

      ends[not_matched] <- NA_integer_

      nms <- auto_name(attr(match, "capture.names"))

      res <- data.frame(matrix(ncol = ncol(starts), starts), matrix(ncol = ncol(ends), ends))

      colnames(res) <- paste(sep=".", rep(nms, each = 2L), c("start", "end"))
      res
    }
  }

  if(global %==% TRUE) {
    mapply(process_matches, gregexpr(pattern = pattern, data, perl = TRUE, ...), data, SIMPLIFY = FALSE)
  }
  else {
    process_matches(regexpr(pattern = pattern, data, perl = TRUE, ...), data)
  }
}

#' Substitute regular expressions in a string with another string.
#'
#' @param data character vector to substitute
#' @param pattern regular expression to match
#' @param replacement replacement text to use
#' @param global substitute all occurrences
#' @param options option flags
#' @param ... options passed to sub or gsub
#' @seealso \code{\link{regexp}} Section "Perl-like Regular Expressions" for a
#' discussion of the supported options
#' @examples
#' string <- c("this is a Test", "string")
#' re_substitutes(string, "test", "not a test", options = "insensitive")
#' re_substitutes(string, "i", "x", global = TRUE)
#' re_substitutes(string, "(test)", "not a \\1", options = "insensitive")
#' @export
re_substitutes <- function(data, pattern, replacement, global = FALSE, options = NULL, ...) {
  pattern <- add_options(pattern, options)
  method <- if (isTRUE(global)) gsub else sub
  method(x = data, pattern = pattern, replacement = replacement, perl = TRUE, ...)
}

add_options <-  function(pattern, options) {
  if (!is.null(options)) {
    options <- match_args(options, names(option_map))
    pattern <- p("(?", p(option_map[options]), ")", pattern)
  }
  else {
    pattern
  }
}

match_args <- function(arg, choices) {
  matches <- pmatch(arg, choices)
  if(any(is.na(matches))){
    stop(gettextf("'arg' should be one of %s", paste(dQuote(choices),
          collapse = ", ")), domain = NA)
  }
  choices[matches]
}

option_map <- c(
  "insensitive" = "i",
  "multi-line" = "m",
  "single-line" = "s",
  "extended" = "x",
  "ungreedy" = "U"
  )

no_capture <- function(match) {
  is.null(attr(match, "capture.start", exact = TRUE))
}

auto_name <- function(names) {
  missing <- names == ""
  if (all(!missing)) {
    return(names)
  }
  names[missing] <- seq_along(names)[missing]
  names
}
