.rex <- new.env(parent = emptyenv())
.rex$env <- new.env(parent = emptyenv())
.rex$mode <- FALSE

register <- function(...) {
  names <- gsub("`", "", as.character(eval(substitute(alist(...)))))
  vals <- list(...)

  for(itr in seq_along(vals)) {
    .rex$env[[names[itr]]] <- vals[[itr]]
  }
}
