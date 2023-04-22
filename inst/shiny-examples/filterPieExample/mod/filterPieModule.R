filterPieModuleUI <- function(id) {

  ns <- NS(id)

  tagList(
    filterPieOutput(ns('filterPiePlot'))
  )

}

filterPieServer <- function(id, data, filterVar) {
  moduleServer(
    id,

    function(input, output, session) {

      filterVector <- data[, filterVar]

      output$filterPiePlot <- renderFilterPie(
        filterVector |>
          dataFormatPie() |>
          filterPie(reactiveID = session$ns("filterPie"))
      )

      filteredData <- reactive({
        if(length(input$filterPie) == 0){
          data
        }else{
          data[filterVector == input$filterPie, ]
        }
      })

      return(filteredData)
    }
  )
}
