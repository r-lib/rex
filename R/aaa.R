.rex <- new.env(parent = emptyenv())
.rex$env <- new.env(parent = emptyenv())
.rex$mode <- FALSE

register <- function(...) {
  names <- gsub("`", "", as.character(eval(substitute(alist(...)))), fixed = TRUE)

  list2env(structure(list(...), .Names = names), envir = .rex$env)
}

register_object <- function(object) {
  list2env(as.list(object), envir = .rex$env)
}
