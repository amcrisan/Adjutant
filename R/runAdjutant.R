#' @export
runAdjutant <- function() {
  appDir <- system.file("shinyapp", package = "adjutant")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `mypackage`.", call. = FALSE)
  }
  
  shiny::runApp(appDir, display.mode = "normal")
}