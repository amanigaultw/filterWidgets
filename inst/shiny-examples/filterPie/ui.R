ui <- fluidPage(
  filterPieModuleUI("test"),
  h2("Click on the pie chart to filter the table data."),
  hr(),
  DT::dataTableOutput('table')
)
