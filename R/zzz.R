
wbpip_default_options <- list(
  wbpip.verbose = TRUE
)

.onLoad <- function(libname, pkgname) {
  op <- options()
  toset <- !(names(wbpip_default_options) %in% names(op))
  if (any(toset)) options(wbpip_default_options[toset])

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## defined values --------
  invisible()
}
