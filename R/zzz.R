.onLoad <- function(lib, pkg) {
  
  ## Put all of the un-exported 'rex' functions into an environment that
  ## can be attached for editor / IDE autocompletion.
  objects <- c(
    shortcuts,
    single_shortcuts
  )
  
  for (i in seq_along(objects)) {
    name <- names(objects)[[i]]
    .rex$env[[names(objects)[i]]] <- objects[[i]]
  }
  
}

.onAttach <- function(lib, pkg) {
  packageStartupMessage("Welcome to rex, the friendly regular expression helper!")
}
