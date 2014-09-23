\name{\%of_length\%}
\alias{\%of_length\%}
\title{Of Length}
\usage{
x \%of_length\% y
}
\arguments{
  \item{x}{A regex pattern to match multiple times.}

  \item{y}{A vector of (at most) two entries, where
  \code{y[1]} denotes the minimum number of matches, and
  \code{y[2]} denotes the maximum number of matches. If
  \code{y[2] == ''}, then there is no maximum.}
}
\description{
This special binary function is used in combination with a
pattern to indicate that you wish to match the pattern a
certain number of times.
}
\examples{
## match 'ab' recurring 2 to 4 times
my_regex <- rex( capture(
   if_prev_is(" "),
   word("ab") \%of_length\% c(2, 4),
   if_next_is(" ")
   ) )
gsub( my_regex, "\\\\1_was_matched", "ab abab ababab abababab ababababab abababababab", perl=TRUE )
}

