.onUnload <- function (libpath) {
  library.dynam.unload("wbpip", libpath)
}
