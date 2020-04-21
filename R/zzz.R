.onAttach <- function(lib, pkg) { # nolint
  if (!interactive() || stats::runif(1) > 0.1) return()

  packageStartupMessage("Welcome to rex, the friendly regular expression helper!\n",
                        "Use 'rex_mode()' to toggle code completion for rex shortcuts and functions.")
}
