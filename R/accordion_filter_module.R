#' accordion filter UI module element
#'
#' @param id module id string
#' @param style css style properties to be applied to a div containing the widget
#'
#' @return ui elements of the widget
#' @export
accordionFilterModuleUI <- function(id, style = NULL) {

  ns <- shiny::NS(id)

  if(is.null(style)) style <- "width:300px;margin:auto"

  shiny::tagList(
    shiny::uiOutput(ns("style")),
    shiny::div(shiny.semantic::grid(
      grid_template = shiny.semantic::grid_template(
        default = list(
          areas = rbind(
            c("apply", "reset"),
            c("filter", "filter")
          ),
          cols_width = c("1fr", "1fr"),
          rows_height = c("auto")
        )
      ),
      area_styles = list("apply" = 'padding-top:1rem;height:50px',
                         "reset" = 'padding-top:1rem;display:flex;justify-content:flex-end;',
                         "filter" = 'padding-top:1rem'),
      apply = shiny::uiOutput(ns("apply_button")),
      reset = shiny::uiOutput(ns("reset_button")),
      filter = shiny::uiOutput(ns("accordion"))
    ),
    style = style),
  )

}


#' accordion filter UI module element
#'
#' @param id module id string
#' @param data dataframe to be filtered
#' @param filterVars columns names of
#'
#' @return a filtered version of the dataset as a reactive
#' @export
accordionFilterModuleServer <- function(id, data, filterVars) {
  shiny::moduleServer(
    id,

    function(input, output, session) {

      ns <- session$ns

      rv <- shiny::reactiveValues(filter_list = NULL,
                                  temp_filter_list = NULL)

      shiny::observeEvent(data(),{
        if(is.null(data())){
          return(NULL)
        }
        if(is.null(rv$temp_filter_list)){
          rv$filter_list <- get_filter_list(data(), filterVars)
          rv$temp_filter_list <- get_filter_list(data(), filterVars)
        }
      })

      output$identical <- shiny::reactive({
        identical <- identical(rv$temp_filter_list, rv$filter_list)
        return(identical)
      })
      shiny::outputOptions(output, 'identical', suspendWhenHidden = FALSE)

      output$unfiltered <- shiny::reactive({
        unfiltered <- identical(rv$filter_list, get_filter_list(shiny::isolate(data()), filterVars))
        return(unfiltered)
      })
      shiny::outputOptions(output, 'unfiltered', suspendWhenHidden = FALSE)

      output$accordion <- shiny::renderUI({
        rv$filter_list |>
          get_accordion_content(ns) |>
          custom_accordion()
      })

      output$style <- shiny::renderUI({
        get_tables_style_tag(ns(names(rv$filter_list)))
      })

      output$apply_button <- shiny::renderUI({
        shiny::conditionalPanel(condition = paste0("output['", id,"-identical'] == false"),
                                shiny.semantic::action_button(ns("apply"), "Apply", icon = shiny.semantic::icon("filter")))
      })

      output$reset_button <- shiny::renderUI({
        shiny::conditionalPanel(condition = paste0("output['", id,"-unfiltered'] == false"),
                                shiny.semantic::action_button(ns("reset"), "Reset", icon = shiny.semantic::icon("filter")))
      })

      shiny::observe({
        rv$input_list <- lapply(seq_along(shiny::isolate(rv$filter_list)), function(i) input[[paste0(names(shiny::isolate(rv$filter_list))[i], "_rows_selected")]])
        rv$temp_filter_list <- apply_input_vectors(shiny::isolate(rv$filter_list), rv$input_list)
      })

      shiny::observeEvent(input$apply,{
        rv$filter_list <- rv$temp_filter_list
      })

      shiny::observeEvent(input$reset,{
        rv$filter_list <- rv$temp_filter_list <- get_filter_list(shiny::isolate(data()), filterVars)
      })

      return(shiny::reactive({apply_filter_list(data(), rv$filter_list)}))
    }
  )
}


#### helper function ####

custom_accordion <- function(accordion_list, fluid = TRUE,
                             styled = TRUE, custom_style = "") {
  id = uuid::UUIDgenerate(use.time = FALSE)
  fluid <- ifelse(fluid, "fluid", "")
  styled <- ifelse(styled, "styled", "")
  accordion_class = glue::glue("ui {styled} {fluid} accordion")
  shiny::tagList(
    shiny::div(id = id, class = accordion_class, style = custom_style,
               accordion_list |> purrr::map(function(x) {
                 if (is.null(x$title) || is.null(x$content))
                   stop("There must be both title and content fields in `accordion_list`")
                 active <- "active"
                 shiny::tagList(
                   shiny::div(class = paste("title", active), shiny.semantic::icon("dropdown"), x$title),
                   shiny::div(class = paste("content", active),
                              shiny::p(class = "transition hidden",
                                       shiny::div(x$content)
                              )
                   )
                 )
               })
    ),
    shiny::tags$script(shiny::HTML("$('.ui.accordion').accordion();")),
    shiny::tags$script(shiny::HTML(paste0("
    var divs = document.getElementById('", id, "').getElementsByClassName('active');
    for (var i = 0; i < divs.length; i += 1) {divs[i].click();}")))
  )
}

get_filter_list <- function(raw_data, filter_vars){
  filter_list <- list()
  for(i in seq_along(filter_vars)){
    filter_list[[i]] <- list(input = data.frame(values = sort(unique(raw_data[[filter_vars[i]]])),
                                                excluded = F))
    names(filter_list)[i] <- filter_vars[i]
  }

  filter_list
}

get_table_style <- function(table_id){
  paste0("
      #", table_id, ">.dataTables_wrapper .dataTables_scroll div.dataTables_scrollBody>table.dataTable tr.selected td,
      #", table_id, ">.dataTables_wrapper .dataTables_scroll div.dataTables_scrollBody>table.dataTable td.selected{
      box-shadow: none !important;
      text-decoration: line-through;
      }
      #", table_id, ">.dataTables_wrapper .dataTables_scroll div.dataTables_scrollBody>table.dataTable.stripe>tbody>tr.even.selected>*,
      #", table_id, ">.dataTables_wrapper .dataTables_scroll div.dataTables_scrollBody>table.dataTable.display>tbody>tr.even.selected>*{
      background-color: rgba(0, 0, 0, 0) !important;
      }
      #", table_id, ">.dataTables_wrapper .dataTables_scroll div.dataTables_scrollBody>table.dataTable.stripe>tbody>tr.odd.selected>*,
      #", table_id, ">.dataTables_wrapper .dataTables_scroll div.dataTables_scrollBody>table.dataTable.display>tbody>tr.odd.selected>*{
      background-color: rgba(0, 0, 0, 0.023) !important;
      }
      #", table_id, ">.dataTables_wrapper .dataTables_scroll div.dataTables_scrollBody>table.dataTable tbody tr.selected>*{
      color:#333;
      }
  ")
}

get_tables_style_tag <- function(table_ids){
  shiny::tags$style(
    shiny::HTML(
      paste0(sapply(table_ids, get_table_style), collapse = "")
    )
  )
}

get_table <- function(id, data){
  DT::datatable(data,
                elementId = id,
                selection = list(mode = 'multiple', selected = which(data$excluded == TRUE)),
                options = list(dom = 't',
                               scrollY = 'auto',
                               paging = FALSE,
                               columnDefs = list(
                                 list(visible=FALSE, targets=c(0,2))
                               ),
                               headerCallback = htmlwidgets::JS(
                                 "function(thead){$(thead).remove();}")
                ))
}

get_accordion_content <- function(filter_list, ns = NULL){

  if(is.null(ns)) ns <- function(x) x

  lapply(seq_along(filter_list), function(i) list(title = paste(names(filter_list)[[i]], "(", length(filter_list[[i]]$input$excluded[filter_list[[i]]$input$excluded == F]), ")"),
                                                  content = get_table(ns(names(filter_list)[[i]]), filter_list[[i]]$input)))
}

apply_input_vectors <- function(filter_list, input_list){
  updated_filter_list <- filter_list
  for(i in seq_along(filter_list)){
    updated_filter_list[[i]]$input$excluded <- is.element(seq_len(nrow(filter_list[[i]]$input)), input_list[[i]])
  }
  updated_filter_list
}

apply_filter_list <- function(data, filter_list){
  for(i in seq_along(filter_list)){
    values_retained <- filter_list[[i]]$input$values[filter_list[[i]]$input$excluded == F]
    data <- data[data[, names(filter_list)[i]] %in% values_retained, ]
  }
  data
}
