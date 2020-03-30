#' @title module_dataimport_server
#'
#' @param input Shiny server input object
#' @param output Shiny server output object
#' @param session Shiny session object
#' @param rv The global 'reactiveValues()' object, defined in server.R
#' @param input_re The Shiny server input object, wrapped into a reactive
#'   expression: input_re = reactive({input})
#'
#' @export
#'
# module_dataimport_server
module_dataimport_server <- function(input,
                                     output,
                                     session,
                                     rv,
                                     input_re) {

  observeEvent(
    eventExpr = input_re()[["moduleDataimport-dataimport_example_data"]],
    handlerExpr = {

      print("Importing")

      rv$data <- example_data

      rv$load_flag <- TRUE
    })

  observe({
    req(rv$data)

    print(colnames(SummarizedExperiment::colData(rv$data)))

    if (is.null(rv$updated_choices)) {
      choices <- sapply(
        colnames(SummarizedExperiment::colData(rv$data)),
        FUN = function(x) {
          return(x)
        },
        simplify = F,
        USE.NAMES = T
      )
      # populate group dropdown
      updateSelectInput(
        session = session,
        inputId = "dataimport_grouping_variable",
        choices = choices
      )
      rv$updated_choices <- TRUE
    }
  })

  observeEvent(
    eventExpr = input_re()[["moduleDataimport-dataimport_start_analysis"]],
    handlerExpr = {
      # TODO add progressbar for import etc.
      rv$data_logtrans <- log_trans(rv$data)

      print("Finished import")
      rv$import_finished <- TRUE
    }
  )

  observeEvent(
    eventExpr = input_re()[["moduleDataimport-dataimport_grouping_variable"]],
    handlerExpr = {
      rv$grouping_variable <-
        input_re()[["moduleDataimport-dataimport_grouping_variable"]]
      message(paste0("Grouping variable: ", rv$grouping_variable))
    }
  )


}


#' @title module_dataimport_ui
#'
#' @param id A character. The identifier of the shiny object
#'
#' @export
#'
# module_dataimport_ui
module_dataimport_ui <- function(id) {
  ns <- NS(id)

  tagList(# first row
    fluidRow(
      column(
        9,
        box(
          title = "Upload study infos",
          actionButton(
            inputId = ns("dataimport_example_data"),
            label = "Load example data"
          )
        )
      ),
      column(
        3,
        box(
          title = "Study definitions",
          selectInput(
            inputId = ns("dataimport_grouping_variable"),
            label = "Select grouping variable",
            choices = NULL,
            multiple = FALSE
          ),
          tags$hr(),
          actionButton(
            inputId = ns("dataimport_start_analysis"),
            label = "Start analysis"
          )
        )
      )
    )
  )
}
