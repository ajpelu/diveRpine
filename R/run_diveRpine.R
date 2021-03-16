#' Run the diveRpine Shiny Application
#'
#' \code{run_diveRpine} runs the diveRpine shiny apps

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
