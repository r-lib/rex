#' ---
#' title: "Stackoverflow Usage Examples"
#' author: "Jim Hester"
#' date: "`r Sys.Date()`"
#' output: rmarkdown::html_vignette
#' vignette: >
#'   %\VignetteIndexEntry{Stackoverflow Usage Examples}
#'   %\VignetteEngine{knitr::rmarkdown}
#'   %\usepackage[utf8]{inputenc}
#' ---

#+ cache=FALSE, include=FALSE
opts_chunk$set(collapse = TRUE, comment = "#>")
render_markdown(strict = TRUE)

#' ### [http://stackoverflow.com/questions/27106552][]

#' Using [rex](http://cran.r-project.org/web/packages/rex/index.html) may make this type of task a little simpler.

x <- c("a=1", "b=3", "a=9", "c=2", "b=4", "a=2")

#' First extract the names and values from the strings.

library(rex)
matches <- re_matches(x,
  rex(
    capture(name="name", letter),
    "=",
    capture(name="value", digit)
    ))
matches

#' Then tally the groups using `split()`.

groups <- split(as.numeric(matches$value), matches$name)
groups

#' If we try to convert directly to a data.frame from `split()` the groups with
#' fewer members will have their members recycled rather than `NA`, so instead
#' explicitly fill with `NA`.

largest_group <- max(sapply(groups, length))
largest_group

groups <- lapply(groups, function(group) {
  if (length(group) < largest_group) {
    group[largest_group] <- NA
  }
  group
})
groups

#' Finally we can create the data.frame

do.call("data.frame", groups)

#' ### [http://stackoverflow.com/questions/14146362/][] ###

#' Using [rex](http://cran.r-project.org/web/packages/rex/index.html) may make this type of task a little simpler.

mystrings <- c("X2/D2/F4",
               "X10/D9/F4",
               "X3/D22/F4",
               "X9/D22/F9")

library(rex)
matches <- re_matches(mystrings,
  rex(
    "/",
    any,
    capture(name = "numbers", digits)
    )
  )
as.numeric(matches$numbers)

#' ### [http://stackoverflow.com/questions/8613237/][] ##
#' Using [rex](http://cran.r-project.org/web/packages/rex/index.html) may make this type of task a little simpler.

j <- "What kind of cheese isn't your cheese? (wonder) Nacho cheese! (groan) (Laugh)"

library(rex)
matches <- re_matches(j,
  rex(
    "(",
    capture(name = "text", except_any_of(")")),
    ")"),
  global = TRUE)
matches

#' ### [http://stackoverflow.com/questions/22976472][]
#' Using [rex](http://cran.r-project.org/web/packages/rex/index.html) may make this type of task a little simpler.

txt <- as.character("this is just a test! i'm not sure if this is O.K. or if it will work? who knows. regex is sorta new to me..  There are certain cases that I may not figure out??  sad!  ^_^")

re <- rex(
  capture(name = "first_letter", alnum),
  capture(name = "sentence",
    any_non_puncts,
    zero_or_more(
      group(
        punct %if_next_isnt% space,
        any_non_puncts
        )
      ),
    maybe(punct)
    )
  )

re_substitutes(txt, re, "\\U\\1\\E\\2", global = TRUE)

#' ### [http://stackoverflow.com/questions/27172007][]
#' Using [rex](http://cran.r-project.org/web/packages/rex/index.html) may make this type of task a little simpler.

x <- data.frame(
  locationid = c(
    1073744023,
    1073744022,
    1073744025,
    1073744024,
    1073744021,
    1073744026
    ),
  address = c(
    "525 East 68th Street, New York, NY      10065, USA",
    "270 Park Avenue, New York, NY 10017, USA",
    "Rockefeller Center, 50 Rockefeller Plaza, New York, NY 10020, USA",
    "1251 Avenue of the Americas, New York, NY 10020, USA",
    "1301 Avenue of the Americas, New York, NY 10019, USA",
    "44 West 45th Street, New York, NY 10036, USA"
    ))

library(rex)

sep <- rex(",", spaces)

re <-
  rex(
    capture(name = "address",
      except_some_of(",")
    ),
    sep,
    capture(name = "city",
      except_some_of(",")
    ),
    sep,
    capture(name = "state",
      uppers
    ),
    spaces,
    capture(name = "zip",
      some_of(digit, "-")
    ),
    sep,
    capture(name = "country",
      something
    ))

re_matches(x$address, re)

#' ### [http://stackoverflow.com/questions/27155297/][]
#' Using [rex](http://cran.r-project.org/web/packages/rex/index.html) may make this type of task a little simpler.

library(rex)
x <- c(
"https://support.google.com/blogger/topic/12457
https://support.google.com/blogger/topic/12457.
https://support.google.com/blogger/topic/12457] 
<<https://support.google.com/blogger/topic/12457>>
https://support.google.com/blogger/topic/12457,
https://support.google.com/blogger/topic/12457),
xxxxxxhttps://support.google.com/blogger/topic/12457),hhhththta")

re <- rex(
  capture(name = "url",
    "https://support.google.com/blogger/topic/",
    digits
    ))

re_matches(x, re, global = TRUE)[[1]]

#' ### [http://stackoverflow.com/questions/27219421][]
#' Using [rex](http://cran.r-project.org/web/packages/rex/index.html) may make this type of task a little simpler.
tmp <- c("Little Street","A323", "Essex Road (A43)", "M43","Orange street","M4","B2045","New Street")

library(rex)
classify_road <- function(x) {
  res <- re_matches(x,
    rex(
      capture(name = "type",
        upper
      ),
      digit
    )
  )

  res$type[ is.na(res$type) ] <- "Minor"
  paste(res$type, "Road")
}

classify_road(tmp)

#' ### [http://stackoverflow.com/questions/22247410][]
#' Using [rex](http://cran.r-project.org/web/packages/rex/index.html) may make this type of task a little simpler.

x <- "this is a multiline text 
          some more test here 
          before we get to the good stuff 
          \\end{figure}"

re <- rex("\\end{figure}")
re_matches(x, re)

regexpr(re, x, perl = TRUE)

#' ### [http://stackoverflow.com/questions/23447261][]
#' Using [rex](http://cran.r-project.org/web/packages/rex/index.html) may make this type of task a little simpler.

x = structure(list(text = structure(c(4L, 6L, 1L, 2L, 5L, 3L), .Label =     c("ãããæããããéãããæãããInappropriate announce:-(", 
"@AirAsia your direct debit (Maybank) payment gateways is not working. Is it something     you are working to fix?", 
"@AirAsia Apart from the slight delay and shortage of food on our way back from Phuket, both flights were very smooth. Kudos :)", 
"RT @AirAsia: ØØÙØÙÙÙÙ ÙØØØ ØØØÙ ÙØØØØÙ ØØØØÙÙÙí í Now you can enjoy a #great :D breakfast onboard with our new breakfast meals! :D", 
"xdek ke flight @AirAsia Malaysia to LA... hahah..:p bagi la promo murah2 sikit, kompom aku beli...", 
"You know there is a problem when customer service asks you to wait for 103 minutes and your no is 42 in the queue. X-("
), class = "factor"), created = structure(c(5L, 4L, 4L, 3L, 2L, 
1L), .Label = c("1/2/2014 16:14", "1/2/2014 17:00", "3/2/2014 0:54", 
"3/2/2014 0:58", "3/2/2014 1:28"), class = "factor")), .Names = c("text", 
"created"), class = "data.frame", row.names = c(NA, -6L))

emots <- as.character(outer(c(":", ";", ":-", ";-"), c(")", "(", "]", "[", "D", "o", "O", "P", "p"), paste0))

re_matches(x$text,
  rex(
    capture(name = "emoticons",
      or(emots)
    )
  ),
  global = T)

#' ### [http://stackoverflow.com/questions/27234040][]
#' Using [rex](http://cran.r-project.org/web/packages/rex/index.html) may make this type of task a little simpler.

z <- "<TABLE ALIGN=\"RIGHT\" BORDER CELLSPACING=\"0\" CELLPADDING=\"0\">
   <CAPTION><B>MESA HIGH VICTORIES</B></CAPTION>
   <TH>Team</TH>
   <TH>Score</TH>
   <TR ALIGN=\"CENTER\">
   <TD><B>Parkfield High Demons</B></TD>
   <TD><B>28 to 21</B></TD>
   </TR>
   <TR ALIGN=\"CENTER\">
   <TD><B>Burns High Badgers</B></TD>
   <TD><B>14 to 13</B></TD>
   </TR>
   </TABLE>"

re_matches(z,
  rex(
    capture(name="table",
      "<TABLE", zero_or_more(any, type = "lazy"), "<TR"
    )
  ), options="single-line")


#' ### [http://stackoverflow.com/questions/27236435][]
#' Using [rex](http://cran.r-project.org/web/packages/rex/index.html) may make this type of task a little simpler.
x <- "John a11|a12|\n  Ana a21|a22|\n  Jake a31|a23|\n   "

re_matches(x,
  rex(
      any_spaces,
      capture(name = "text",
        except_some_of("|")
      ),
      any_spaces),
  global = TRUE)[[1]]

#' ### [http://stackoverflow.com/questions/25764839][]
#' Using [rex](http://cran.r-project.org/web/packages/rex/index.html) may make this type of task a little simpler.
x <- "MSGSRRKATPASRTRVGNYEMGRTLGEGSFAKVKYAKNTVTGDQAAIKILDREKVFRHKMVEQLKREISTMKLIKHPNVVEIIEVMASKTKIYIVLELVNGGELFDKIAQQGRLKEDEARRYFQQLINAVDYCHSRGVYHRDLKPENLILDANGVLKVSDFGLSAFSRQVREDGLLHTACGTPNYVAPEVLSDKGYDGAAADVWSCGVILFVLMAGYLPFDEPNLMTLYKRICKAEFSCPPWFSQGAKRVIKRILEPNPITRISIAELLEDEWFKKGYKPPSFDQDDEDITIDDVDAAFSNSKECLVTEKKEKPVSMNAFELISSSSEFSLENLFEKQAQLVKKETRFTSQRSASEIMSKMEETAKPLGFNVRKDNYKIKMKGDKSGRKGQLSVATEVFEVAPSLHVVELRKTGGDTLEFHKVCDSFYKNFSSGLKDVVWNTDAAAEEQKQ"
re_matches(x,
  rex(
    capture(name = "amino_acids",
      n(any, 6),
      "K",
      n(any, 6)
      )
    ),
  global = TRUE)[[1]]

locs <- re_matches(x,
  rex(
    "K" %if_prev_is% n(any, 6) %if_next_is% n(any, 6)
    ),
  global = TRUE, locations = TRUE)[[1]]

substring(x, locs$start - 6, locs$end + 6)

#' ### [http://stackoverflow.com/questions/15954171][]
#' Using [rex](http://cran.r-project.org/web/packages/rex/index.html) may make this type of task a little simpler.
x <- c("System configuration: lcpu=96 mem=196608MB ent=16.00")

library(rex)
val <- as.numeric(
  re_matches(x,
    rex("ent=",
      capture(name = "ent", some_of(digit, "."))
      )
    )$ent
  )

#' ### [http://stackoverflow.com/questions/27273996][]
#' Using [rex](http://cran.r-project.org/web/packages/rex/index.html) to construct the regular expression may make it more understandable.
x <- c("_A00_A1234B_", "_A00_A12345B_", "_A1_A12345_")

#' approach #1, assumes always is between the second underscores.
re_matches(x,
  rex(
    "_",
    anything,
    "_",
    capture(anything),
    "_"
  )
)

#' approach #2, assumes an alpha, followed by 4 or 5 digits with a possible trailing alpha.
re_matches(x,
  rex(
    capture(
      alpha,
      between(digit, 4, 5),
      maybe(alpha)
    )
  )
)

#' ### [http://stackoverflow.com/questions/27238323][]
#' Using [rex](http://cran.r-project.org/web/packages/rex/index.html) may make this type of task a little simpler.
string = c("ABC3JFD456", "ARST4DS324")

re_matches(string,
  rex(
    capture(name = "first_number", digit)
    )
  )

#' ### <http://stackoverflow.com/questions/27252250> ###
#' Using [rex](http://cran.r-project.org/web/packages/rex/index.html) may make this type of task a little simpler.
df <- structure(list(Object = c("T00055", "T00055", "E00336", "E00336",
"E00336", "E00336", "T 00054"), Coding = c("T 00055_005_<002_+",
"T 00055_008_<002_+", "E 00336_041_<001_+001_+", "E 00336_041_<001_+001_+001_+",
"E 00336_041_<001_+001_+002_+", "E 00336_041_<001_+001_+002_<",
"T 00054_013_<003_<015_+003_<001_<"), Fn = c(2L, 2L, 3L, 4L,
4L, 4L, 4L), Remaining = c(30L, 30L, 0L, 10L, 56L, 52L, 52L)), .Names = c("Object",
"Coding", "Fn", "Remaining"), row.names = c(NA, -7L), class = "data.frame")

subset(df, grepl(rex(at_least(group("_+", anything), 2)), Coding))

#' ### <http://stackoverflow.com/questions/27195734>
#' Using [rex](http://cran.r-project.org/web/packages/rex/index.html) may make this type of task a little simpler.

ids <- c("367025001", "CT_341796001", "M13X01692-01", "13C025050901", "13C00699551")

re_substitutes(ids,
  rex(non_digits %or% list("01", end)),
  "",
  global = TRUE)

#' ### <http://stackoverflow.com/questions/27237233>
#' Using [rex](http://cran.r-project.org/web/packages/rex/index.html) may make this type of task a little simpler.
library("rvest")
library("stringr")

minimal <- html("<!doctype html><title>blah</title> <p>&nbsp;foo")

bodytext <- minimal %>%
  html_node("body") %>%
  html_text

re_substitutes(bodytext, rex(spaces), "", global = TRUE)

#' ### <http://stackoverflow.com/questions/27227229>
#+ message=FALSE
string <- "this\\(system) {is} [full]."
library(Hmisc)
gsub("\\\\(.)", "\\1", escapeRegex(string))

#' Alternatively [rex](http://cran.r-project.org/web/packages/rex/index.html) may make this type of task a little simpler.
library(rex)
re_substitutes(escape(string), rex("\\", capture(any)), "\\1", global = TRUE)

#' ### <http://stackoverflow.com/questions/27317497>
#' [rex](http://cran.r-project.org/web/packages/rex/) has a [vignette for parsing server logs](http://cran.r-project.org/web/packages/rex/vignettes/log_parsing.html). While the format is not exactly the same as your log you should be able to adapt it to your case fairly easily.
#' As far as reading the log in assuming the file fits in memory your best bet is to read the whole file first with `readLines()`, then the following will put each field into a `data.frame` column.
x <- "Feb  6 12:14:14 localhost haproxy[14389]: 10.0.1.2:33317 [06/Feb/2009:12:14:14.655] http-in static/srv1 10/0/30/69/109 200 2750 - - ---- 1/1/1/1/0 0/0 {1wt.eu} {} \"GET /index.html HTTP/1.1\""
library(rex)
re <- rex(

  capture(name = "process_name", alpha),
  "[",
    capture(name = "pid", digits),
  "]:",
  spaces,
  capture(name = "client_ip", any_of(digit, ".")),
  ":",
  capture(name = "client_port", digits),
  spaces,
  "[",
    capture(name = "accept_date", except_some_of("]")),
  "]",
  spaces,
  capture(name = "frontend_name", non_spaces),
  spaces,
  capture(name = "backend_name", except_some_of("/")),
  "/",
  capture(name = "server_name", non_spaces),
  spaces,
  capture(name = "Tq", some_of("-", digit)),
  "/",
  capture(name = "Tw", some_of("-", digit)),
  "/",
  capture(name = "Tc", some_of("-", digit)),
  "/",
  capture(name = "Tr", some_of("-", digit)),
  "/",
  capture(name = "Tt", some_of("+", digit)),
  spaces,
  capture(name = "status_code", digits),
  spaces,
  capture(name = "bytes_read", some_of("+", digit)),
  spaces,
  capture(name = "captured_request_cookie", non_spaces),
  spaces,
  capture(name = "captured_response_cookie", non_spaces),
  spaces,
  capture(name = "termination_state", non_spaces),
  spaces,
  capture(name = "actconn", digits),
  "/",
  capture(name = "feconn", digits),
  "/",
  capture(name = "beconn", digits),
  "/",
  capture(name = "srv_conn", digits),
  "/",
  capture(name = "retries", some_of("+", digit)),
  spaces,
  capture(name = "srv_queue", digits),
  "/",
  capture(name = "backend_queue", digits),
  spaces,
  "{",
    capture(name = "captured_request_headers", except_any_of("}")),
  "}",
  spaces,
  "{",
    capture(name = "captured_response_headers", except_any_of("}")),
  "}",
  spaces,
  double_quote,
    capture(name = "http_request", non_quotes),
  double_quote)

re_matches(x, re)

#' ### <http://stackoverflow.com/questions/27422350/extract-character-preceding-first-dot-in-a-string>
#' Using [rex](http://cran.r-project.org/web/packages/rex/index.html) may make this type of task a little simpler.

my.data <- read.table(text = '
     my.string  state
     .........    A
     1........    B
     112......    C
     11111....    D
     1111113..    E
     111111111    F
     111111111    G
', header = TRUE, stringsAsFactors = FALSE)

library(rex)

re_matches(my.data$my.string,
  rex(capture(except(".")), "."))$'1'

#' ### <http://stackoverflow.com/questions/27410736>
#' Using [rex](http://cran.r-project.org/web/packages/rex/index.html) may make this type of task a little simpler.
string <- "Shakira - Wolf - 02.Hips don't lie.mp3"

library(rex)
re_matches(string,
  rex(capture(zero_or_more(any, type='lazy')), spaces, "-"))$'1'

#' ### <http://stackoverflow.com/questions/27400286/>
#' Using [rex](http://cran.r-project.org/web/packages/rex/index.html) may make this type of task a little simpler.

string <- "I t is tim e to g o" 
library(rex)
re_substitutes(string, rex(
    space %if_next_is%
      list(
        list(non_space, space, at_least(non_space, 2)) %or%
        list(non_space, end)
      )
    ), "", global = TRUE)

#' ### <http://stackoverflow.com/questions/27553126>
#' Using [rex](http://cran.r-project.org/web/packages/rex/index.html) may make this type of task a little simpler.

string <- "01:04:43.064 [12439] <2> xyz
01:04:43.067 [12439] <2> a lmn
01:04:43.068 [12439] <4> j klm
x_times_wait to <3000>
01:04:43.068 [12439] <4> j klm
enter_object <5000> main k"

library(rex)

timestamp <- rex(n(digit, 2), ":", n(digit, 2), ":", n(digit, 2), ".", n(digit, 3))

re <- rex(timestamp, space,
          "[", digits, "]", space,
          "<", digits, ">", space,
          capture(anything))

re_matches(string, re, global = TRUE)
