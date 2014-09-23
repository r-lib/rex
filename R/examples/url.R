## Decompose a URL into its components.
x <- "http://stat.umn.edu:80/xyz"
mm <- "^(([^:]+)://)?([^:/]+)(:([0-9]+))?(/.*)"
m <- regexec("^(([^:]+)://)?([^:/]+)(:([0-9]+))?(/.*)", x)
my_regex <- rex(
  
  ## match the protocol -- may exist or may not
  start, zero_or_one( named_capture( "protocol", 
    capture( one_or_more("[^:]") ),
      "://"
  ) ),
  
  ## match the whole thing
  capture(
    one_or_more(
      not(":/")
      )
    ),
  
  ## 
  zero_or_one( capture(
    ":", capture(
      one_or_more( numbers )
      )
    )
  ),
  capture( zero_or_more("/.") )
)

n <- regexec( my_regex, x )

regmatches( x, regexec(my_regex, x) )