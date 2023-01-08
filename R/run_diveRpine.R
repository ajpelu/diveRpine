#' Run the diveRpine Shiny Application
#'
#' `run_diveRpine` executes the diveRpine shiny app

#' @export
#' @importFrom shiny shinyApp
#'
run_diveRpine <- function() {
  appDir <- system.file("app", package = "diveRpine")
  if (appDir == "") {
    stop("Could not find app directory. Try re-installing `diveRpine`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
