% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/capture.R
\name{group}
\alias{group}
\title{Create a grouped expression}
\usage{
group(...)
}
\arguments{
\item{...}{\code{\link{shortcuts}}, R variables, text, or other \pkg{rex}
functions.}
}
\description{
This is similar to \code{\link{capture}} except that it does not store the
value of the group.  Best used when you want to combine several parts
together and do not reference or extract the grouped value later.
}
\seealso{
\code{\link{capture}} for grouping with capturing.  Perl 5 Extended
Patterns \url{https://perldoc.perl.org/perlre#Extended-Patterns}

Other rex: 
\code{\link{\%or\%}()},
\code{\link{capture}()},
\code{\link{character_class}()},
\code{\link{counts}},
\code{\link{lookarounds}},
\code{\link{not}()},
\code{\link{rex}()},
\code{\link{shortcuts}},
\code{\link{wildcards}}
}
\concept{rex}
