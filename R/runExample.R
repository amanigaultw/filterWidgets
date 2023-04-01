#' Run example app
#'
#' @param example shiny app name
#'
#' @export
runExample <- function(example = c("filterPieExample")) {
  appDir <- system.file("shiny-examples", example[1], package = "filterWidgets")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `filterWidgets`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
