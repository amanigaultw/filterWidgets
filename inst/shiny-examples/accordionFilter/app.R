data <- data.frame(nationality = sample(c("French", "German", "British"), 1000, replace=TRUE, prob=c(0.4, 0.3, 0.3)),
                   sex = sample(c("Male", "Female"), 1000, replace=TRUE, prob=c(0.5, 0.5)),
                   age = sample(c("child", "adult", "older adult"), 1000, replace=TRUE, prob=c(0.1, 0.7, 0.2)),
                   language = sample(c("unilingual", "bilingual"), 1000, replace=TRUE, prob=c(0.7, 0.3)),
                   TV = sample(c("less than 2h TV / day", "more than 2h TV / day"), 1000, replace=TRUE, prob=c(0.7, 0.3)),
                   politics = sample(c("left", "center", "right"), 1000, replace=TRUE, prob=c(0.3, 0.4, 0.3)),
                   letters = sample(LETTERS, 1000, replace = T))

library(shiny)
library(shiny.semantic)
library(DT)

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
