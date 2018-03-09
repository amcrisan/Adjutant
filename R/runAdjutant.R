#' @export
runAdjutant <- function() {
  appDir <- system.file("shinyapp", package = "adjutant")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `mypackage`.", call. = FALSE)
  }
  
  
  #passing the current directory to the shiny app
  .GlobalEnv$workDir <- getwd()
  on.exit(rm(workDir, envir=.GlobalEnv))
  
  shiny::runApp(appDir, display.mode = "normal")
}