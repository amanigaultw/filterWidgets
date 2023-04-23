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

    grid(
      grid_template = shiny.semantic::grid_template(
        default = list(
          areas = rbind(
            c("filter1", "filter2", "filter3"),
            c("filter4", "filter5", "filter6"),
            c("table", "table", "table")
          ),
          cols_width = c("1fr", "1fr", "1fr"),
          rows_height = c("auto")
        )
      ),
      area_styles = list("filter1" = 'padding-top:1rem;padding-right:1rem;padding-left:1rem;',
                         "filter2" = 'padding-top:1rem;padding-right:1rem;padding-left:1rem;',
                         "filter3" = 'padding-top:1rem;padding-right:1rem;padding-left:1rem;',
                         "filter4" = 'padding-top:1rem;padding-right:1rem;padding-left:1rem;',
                         "filter5" = 'padding-top:1rem;padding-right:1rem;padding-left:1rem;',
                         "filter6" = 'padding-top:1rem;padding-right:1rem;padding-left:1rem;',
                         "table" = 'padding-top:3rem'),
      filter1 = accordionFilterModuleUI("test1", style = "margin:auto"),
      filter2 = accordionFilterModuleUI("test2", style = "margin:auto"),
      filter3 = accordionFilterModuleUI("test3", style = "margin:auto"),
      filter4 = accordionFilterModuleUI("test4", style = "margin:auto"),
      filter5 = accordionFilterModuleUI("test5", style = "margin:auto"),
      filter6 = accordionFilterModuleUI("test6", style = "margin:auto"),
      table = DTOutput("table")
    )

  ),

  server = function(input, output, session) {

    filtered_data1 <- accordionFilterModuleServer("test1", reactive(data), names(data)[1])
    filtered_data2 <- accordionFilterModuleServer("test2", filtered_data1, names(data)[2])
    filtered_data3 <- accordionFilterModuleServer("test3", filtered_data2, names(data)[3])
    filtered_data4 <- accordionFilterModuleServer("test4", filtered_data3, names(data)[4])
    filtered_data5 <- accordionFilterModuleServer("test5", filtered_data4, names(data)[5])
    filtered_data6 <- accordionFilterModuleServer("test6", filtered_data5, names(data)[6])

    output$table <- renderDT({datatable(filtered_data6(), options = list(pageLength = 25))})

  }
)
