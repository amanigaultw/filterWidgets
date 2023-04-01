#' pie chart filter widget
#'
#' an apache pie chart that can be clicked to apply filters within an R shiny app.
#'
#' @param data data list object to be plotted; typically generated using \code{dataFormat()}.
#' @param width width of the graph as CSS units.
#' @param height height of the graph as CSS units.
#' @param elementId html element id.
#' @param options list of apache echarts options
#' @param reactiveID string id to be passed to \code{Shiny.setInputValue()} called within filterPie.js
#'
#' @import htmlwidgets
#'
#' @export
#'
#' @examples
#'
#' rawVector <- sample(c("French", "German", "British", "American", "Canadian", "Dutch"),
#' 1000, replace=TRUE, prob=c(0.2, 0.3, 0.3, .1, .05, .05))
#'
#' rawVector |>
#'    dataFormatPie() |>
#'    filterPie()
#'
filterPie <- function(data, options = list(), reactiveID = NULL, width = NULL, height = NULL, elementId = NULL) {

  if(is.null(reactiveID)) reactiveID <- "pie_filter_selection"

  # forward options using x
  x = list(
    data = data,
    options = options,
    reactiveID = reactiveID
  )

  # create widget
  htmlwidgets::createWidget(
    name = 'filterPie',
    x,
    width = width,
    height = height,
    package = 'filterWidgets',
    elementId = elementId
  )
}

#' Shiny bindings for filterPie
#'
#' Output and render functions for using filterPie within Shiny
#' applications and interactive Rmd documents.
#'
#' @param outputId output variable to read from
#' @param width,height Must be a valid CSS unit (like \code{'100\%'},
#'   \code{'400px'}, \code{'auto'}) or a number, which will be coerced to a
#'   string and have \code{'px'} appended.
#' @param expr An expression that generates a filterPie
#' @param env The environment in which to evaluate \code{expr}.
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This
#'   is useful if you want to save an expression in a variable.
#'
#' @name filterPie-shiny
#'
#' @export
filterPieOutput <- function(outputId, width = '100%', height = '400px'){
  htmlwidgets::shinyWidgetOutput(outputId, 'filterPie', width, height, package = 'filterWidgets')
}

#' @rdname filterPie-shiny
#' @export
renderFilterPie <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  htmlwidgets::shinyRenderWidget(expr, filterPieOutput, env, quoted = TRUE)
}
