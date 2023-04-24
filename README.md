# filterWidgets
 
Some custom R shiny filter widgets.

## Installation

You can install the development version of filterWidgets from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("amanigaultw/filterWidgets")
```

## Example App

Illustrating how filterWidgets could be used within Shiny apps.

``` r
#pie chart filter
filterWidgets::runExample()

#accordion filter
filterWidgets::runExample("accordionFilter")

#wide accordion filter
filterWidgets::runExample("wideAccordionFilter")

```

## Example Usage

Here is how to include the accordion filter widget into your own R Shiny apps.

```r
#load libraries
library(shiny)
library(shiny.semantic)
library(DT)
library(filterWidgets)

#example data
data <- data.frame(nationality = sample(c("French", "German", "British"), 1000, replace=TRUE, prob=c(0.4, 0.3, 0.3)),
                   sex = sample(c("Male", "Female"), 1000, replace=TRUE, prob=c(0.5, 0.5)),
                   age = sample(c("child", "adult", "older adult"), 1000, replace=TRUE, prob=c(0.1, 0.7, 0.2)),
                   language = sample(c("unilingual", "bilingual"), 1000, replace=TRUE, prob=c(0.7, 0.3)),
                   TV = sample(c("less than 2h TV / day", "more than 2h TV / day"), 1000, replace=TRUE, prob=c(0.7, 0.3)),
                   politics = sample(c("left", "center", "right"), 1000, replace=TRUE, prob=c(0.3, 0.4, 0.3)),
                   letters = sample(LETTERS, 1000, replace = T))

#the app
shinyApp(
  ui = semanticPage(
    fluidRow(
      accordionFilterModuleUI("test", style = "width:300px;margin:auto"),
      br(),
      br(),
      DTOutput("table")
    )
  ),

  server = function(input, output, session) {

    filtered_data <- accordionFilterModuleServer("test", reactive(data), names(data))
    output$table <- renderDT({datatable(filtered_data())})

  }
)
```

Here is how to include the pie filter widget into your own R Shiny apps.

```r
#load libraries
library(shiny)
library(shiny.semantic)
library(DT)
library(filterWidgets)
library(htmlwidgets)

#load modules
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

#example input data
rawdata <- data.frame(nationality = sample(c("French", "German", "British", "American", "Canadian", "Dutch"), 1000, replace=TRUE, prob=c(0.2, 0.3, 0.3, .1, .05, .05)),
                      sex = sample(c("Male", "Female"), 1000, replace=TRUE, prob=c(0.5, 0.5)),
                      age = sample(c("child", "adult", "older adult"), 1000, replace=TRUE, prob=c(0.1, 0.7, 0.2)),
                      politics = sample(c("left", "center", "right"), 1000, replace=TRUE, prob=c(0.3, 0.4, 0.3)))
filterVar <- "nationality"

#the app
shinyApp(
  ui = semanticPage(
    filterPieModuleUI("test"),
    h2("Click on the pie chart to filter the table data."),
    hr(),
    DT::dataTableOutput('table')
  ),
  
  server = function(input, output, session) {
    
    filteredData <- filterPieServer("test", rawdata, filterVar)
    
    output$table = DT::renderDataTable({
      DT::datatable(filteredData(), options = list(lengthMenu = c(5, 30, 50), pageLength = 5))
    })
    
  }
)
```
