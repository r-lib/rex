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

register_object <- function(object) {
  for(itr in seq_along(object)) {
    name <- names(object)[[itr]]
    .rex$env[[name]] <- object[[itr]]
  }
}
