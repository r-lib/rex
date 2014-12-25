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

#' Re-register the rex functions
#'
#' Use if you need to re-define the functions after package load.
#' @param env the environment to take the new definitions from.
#' @export
reregister_functions <- function(env) {
  names <- ls(.rex$env)

  for (name in names) {
    if (is.function(env[[name]])) {
      .rex$env[[name]] <- env[[name]]
    }
  }
}
