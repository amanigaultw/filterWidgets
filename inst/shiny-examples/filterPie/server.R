server <- function(input, output, session) {
  filteredData <- filterPieServer("test", rawdata, filterVar)

  output$table = DT::renderDataTable({
    DT::datatable(filteredData(), options = list(lengthMenu = c(5, 30, 50), pageLength = 5))
  })
}
