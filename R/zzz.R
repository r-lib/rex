.onAttach <- function(lib, pkg) { # nolint
  packageStartupMessage("Welcome to rex, the friendly regular expression helper!\n",
                        "Use 'rex_mode()' to toggle code completion for rex shortcuts and functions.")
}
