#' accordion filter UI module element
#'
#' @param id module id string
#' @param style css style properties to be applied to a div containing the widget
#'
#' @return ui elements of the widget
#' @export
accordion_checkbox_filter_module_ui <- function(id, style = NULL) {

  ns <- shiny::NS(id)

  if(is.null(style)) style <- "width:300px;margin:auto"

  shiny::tagList(
    shiny::div(style = style,
               grid(grid_template(default = list(
                 areas = rbind(
                   c("button1", "button2"),
                   c("checkbox", "checkbox")
                 ),
                 cols_width = c("1fr", "1fr"),
                 rows_height = c("50px", "100%")
               )),
               area_styles = list(button1 = "padding-top:1rem;",
                                  button2 = "padding-top:1rem;display:flex;justify-content:flex-end;",
                                  checkbox = "padding-top:1rem;"),
               button1 = uiOutput(ns("apply_button")),
               button2 = uiOutput(ns("reset_button")),
               checkbox = uiOutput(ns("accordion"))
               )
    )
  )

}


#' accordion filter UI module element
#'
#' @param id module id string
#' @param unfiltered_data dataframe not yet filtered
#' @param filter_var a single column name from unfiltered_data, to be used as a filter variable
#' @param filtered_data dataframe as filtered by other filterers or widgets (useful for chaining filters together)
#'
#' @return a filtered version of the dataset as a reactive
#' @export
accordion_checkbox_filter_module_server <- function(id, unfiltered_data, filter_var, filtered_data = NULL) {
  shiny::moduleServer(
    id,

    function(input, output, session) {

      ns <- session$ns

      rv <- reactiveValues()

      if(is.null(filtered_data)) isolate({filtered_data <- unfiltered_data})

      observeEvent(unfiltered_data(),{
        rv$unfiltered_data <- unfiltered_data()
        rv$filtered_data <- filtered_data()
        rv$values <- sort(unique(unfiltered_data()[[filter_var]]))
        rv$choices <- c("SELECT ALL", "DESELECT ALL", rv$values)
        rv$selected <- rv$values
        rv$values_retained <- rv$values
        rv$apply_ready <- F
        rv$reset_ready <- F
      })

      output$accordion <- renderUI({
        title <- paste0(filter_var, " (", length(unique(rv$filtered_data[[filter_var]])), ")")
        content <- shiny.semantic::multiple_checkbox(input_id = ns("checkbox"),
                                                     label = "",
                                                     choices = rv$choices,
                                                     selected = rv$selected)
        custom_style <- ""

        shiny::tagList(
          shiny::div(class = "ui styled fluid accordion", style = custom_style,
                     shiny::tagList(
                       shiny::div(class = "title", shiny.semantic::icon("dropdown"), title),
                       shiny::div(class = "content",
                                  shiny::div(class = "transition hidden",
                                             shiny::div(content)
                                  )
                       )
                     )
          ),
          shiny::tags$script("$('.ui.accordion').accordion();")
        )
      })

      output$apply_button <- shiny::renderUI({
        if(rv$apply_ready){
          shiny.semantic::action_button(ns("apply"), "Apply", icon = shiny.semantic::icon("filter"), style = "margin: 0 0 0 0;")
        }
      })

      output$reset_button <- shiny::renderUI({
        if(rv$reset_ready){
          shiny.semantic::action_button(ns("reset"), "Reset", icon = shiny.semantic::icon("filter"), style = "margin: 0 0 0 0;")
        }
      })

      observeEvent(input$checkbox, {
        #print(input$checkbox)
        if("SELECT ALL" %in% input$checkbox){
          shiny.semantic::update_multiple_checkbox(input_id = "checkbox",
                                                   selected = rv$values)
          #rv$values_retained <- rv$values
        }else if("DESELECT ALL" %in% input$checkbox) {
          shiny.semantic::update_multiple_checkbox(input_id = "checkbox",
                                                   selected = character())
          #rv$values_retained <- character()
        }else{
          rv$values_retained <- input$checkbox
        }
        #print(rv$values_retained)
      }, ignoreNULL = F)

      triggers <- reactive({list(rv$values_retained, rv$filtered_data)})

      observeEvent(triggers(), {
        current_values <- sort(unique(rv$filtered_data[[filter_var]]))
        #print(paste("current_values:", paste(current_values, collapse = "; ")))
        #print(paste("rv$values_retained:", paste(rv$values_retained, collapse = "; ")))
        #print(paste("rv$values:", paste(rv$values, collapse = "; ")))
        if(length(current_values) == 0 & length(rv$values_retained) == 0 | identical(sort(current_values), sort(rv$values_retained))){
          rv$apply_ready <- F
        }else{
          rv$apply_ready <- T
        }
        if(identical(sort(current_values), sort(rv$values))){
          rv$reset_ready <- F
        }else{
          rv$reset_ready <- T
        }
        #print(rv$apply_ready)
        #print(rv$reset_ready)
      }, ignoreNULL = F)

      observeEvent(input$apply, {
        #print("apply button pressed")
        rv$filtered_data <- rv$unfiltered_data[rv$unfiltered_data[[filter_var]] %in% rv$values_retained, ]
        rv$selected <- rv$values_retained
        rv$apply_ready <- F
      })

      observeEvent(input$reset, {
        #print("reset button pressed")
        rv$values_retained <- rv$values
        rv$filtered_data <- rv$unfiltered_data
        rv$selected <- rv$values
        rv$reset_ready <- F
      })

      output <- shiny::reactive({
        if(is.null(unfiltered_data())) return(NULL)
        return(rv$filtered_data)
      })

      return(output)
    }
  )
}
