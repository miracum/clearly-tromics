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

  # file upload here
  observe({
    req(input_re()[["moduleDataimport-import_metadata"]])

    ending <- rv$ending <- strsplit(
      input_re()[["moduleDataimport-import_metadata"]]$name,
      ".",
      fixed = T)[[1]]

    if (ending[2] %in% c("csv", "CSV")) {
      file <- reactiveFileReader(
        1000, session,
        input_re()[["moduleDataimport-import_metadata"]]$datapath,
        utils::read.csv,
        header = T,
        row.names = 1
      )
      rv$data_metadata <- file()
      shinyjs::disable("dataimport_example_data")
    }
  })


  observe({
    req(input_re()[["moduleDataimport-import_countdata"]])

    ending <- rv$ending <- strsplit(
      input_re()[["moduleDataimport-import_countdata"]]$name,
      ".",
      fixed = T)[[1]]

    if (ending[2] %in% c("csv", "CSV")) {
      file <- reactiveFileReader(
        1000, session,
        input_re()[["moduleDataimport-import_countdata"]]$datapath,
        utils::read.csv,
        header = T,
        row.names = 1
      )
      rv$data_countdata <- file()
      shinyjs::disable("dataimport_example_data")
    }
  })

  observe({
    req(rv$data_countdata)

    if (is.null(rv$rendered_import_tables)) {
      if (!is.null(rv$data_countdata) && !is.null(rv$data_metadata)) {


        output$dataimport_countdata <- DT::renderDataTable({
          DT::datatable(rv$data_countdata,
                        options = list(scrollX = TRUE,
                                       pageLength = 20,
                                       dom = "ltips")
          )
        })

        output$dataimport_metadata <- DT::renderDataTable({
          DT::datatable(rv$data_metadata,
                        options = list(scrollX = TRUE,
                                       pageLength = 20,
                                       dom = "ltips")
          )
        })


        # workaround to tell ui, that analysis is performed
        output$tables <- reactive({
          return(TRUE)
        })
        outputOptions(output, "tables",
                      suspendWhenHidden = FALSE)

        rv$rendered_import_tables <- TRUE
      }
    }
  })

  observeEvent(
    eventExpr = input_re()[["moduleDataimport-dataimport_preprocess"]],
    handlerExpr = {

      if (!is.null(rv$data_countdata) && !is.null(rv$data_metadata)) {

        if (isFALSE(identical(sort(colnames(rv$data_countdata)),
                              sort(rownames(rv$data_metadata))))) {
          showModal(modalDialog(
            "Metadata row names not matching count data row names",
            title = "Error",
            footer = modalButton("OK")
          ))
        } else {


          sort_vec <- sapply(rownames(rv$data_metadata),
                             FUN = function(x) {
                               which(x == colnames(rv$data_countdata))
                             })

          rv$data_countdata <- rv$data_countdata[, unname(sort_vec)]


          shinyjs::disable("import_metadata")
          shinyjs::disable("import_countdata")

          # Create a Progress object
          progress <- shiny::Progress$new()
          # Make sure it closes when we exit this reactive, even if
          # there's an error
          on.exit(progress$close())
          progress$set(
            message = paste0("Create DESeqDataSet"),
            value = 0
          )

          progress$inc(
            1 / 1,
            detail = paste("... creating dataset ...")
          )

          rv$data <- tryCatch(
            expr = {
              ret <- data_input(
                counttable = rv$data_countdata,
                metadata = rv$data_metadata,
                design = rv$design
              )
              ret
            }, warning = function(w) {
              showModal(modalDialog(
                w,
                title = "Error",
                footer = modalButton("OK")
              ))
              ret <- NULL
              ret
            }, error = function(e) {
              showModal(modalDialog(
                e,
                title = "Error",
                footer = modalButton("OK")
              ))
              ret <- NULL
              ret
            }, finally = function(f) {
              return(ret)
            }
          )

          progress$close()

          if (is.null(rv$data)) {
            shinyjs::click("reset")
          } else {
            # workaround to tell ui, that analysis is performed
            output$preprocess_present <- reactive({
              return(TRUE)
            })
            outputOptions(output, "preprocess_present",
                          suspendWhenHidden = FALSE)

            rv$load_flag <- TRUE
            shinyjs::disable("dataimport_preprocess")
          }
        }
      }
    })

  observeEvent(
    eventExpr = input_re()[["moduleDataimport-dataimport_example_data"]],
    handlerExpr = {

      # Create a Progress object
      progress <- shiny::Progress$new()
      # Make sure it closes when we exit this reactive, even if
      # there's an error
      on.exit(progress$close())
      progress$set(
        message = paste0("Loading example dataset."),
        value = 0
      )

      progress$inc(
        1 / 1,
        detail = paste("... loading ...")
      )

      # load example data
      rv$data_countdata <- tRomics::countdata
      rv$data_metadata <- tRomics::metadata

      progress$close()
    })

  observe({
    req(rv$data_metadata)

    if (is.null(rv$updated_choices)) {
      choices <- sapply(
        colnames(rv$data_metadata),
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
      shinyjs::disable("dataimport_example_data")
    }
  })

  observeEvent(
    eventExpr = input_re()[["moduleDataimport-dataimport_confirm_design"]],
    handlerExpr = {
      # check for valid formula
      if (grepl(
        pattern = "~",
        x = input_re()[["moduleDataimport-dataimport_design"]],
      )) {

        rv$design <- paste(
          input_re()[["moduleDataimport-dataimport_design"]]
        )
        message(rv$design)

        shinyjs::disable("dataimport_confirm_design")

        output$dataimport_design_formula <- renderText({
          rv$design
        })

        # workaround to tell ui, that analysis is performed
        output$design_present <- reactive({
          return(TRUE)
        })
        outputOptions(output, "design_present",
                      suspendWhenHidden = FALSE)

      } else {
        showModal(modalDialog(
          paste(
            "Please provide a valid design formula"
          ),
          title = "Error",
          footer = modalButton("OK")
        ))
      }
    })

  observeEvent(
    eventExpr = input_re()[["moduleDataimport-dataimport_start_analysis"]],
    handlerExpr = {

      if (!is.null(rv$data)) {

        shinyjs::disable("dataimport_start_analysis")
        shinyjs::disable("dataimport_grouping_variable")

        # Create a Progress object
        progress <- shiny::Progress$new()
        # Make sure it closes when we exit this reactive, even if
        # there's an error
        on.exit(progress$close())
        progress$set(
          message = paste0("Transforming data."),
          value = 0
        )

        progress$inc(
          1 / 1,
          detail = paste("... transforming ...")
        )

        # transform data
        rv$data_logtrans <- log_trans(data = rv$data)
        rv$import_finished <- TRUE

        shinyjs::disable("dataimport_grouping_variable")

        progress$close()
      } else {
        showModal(modalDialog(
          paste(
            "Please preprocess your data first"
          ),
          title = "Error",
          footer = modalButton("OK")
        ))
      }
    })

  observeEvent(
    eventExpr = input_re()[["moduleDataimport-dataimport_grouping_variable"]],
    handlerExpr = {
      rv$grouping_variable <-
        input_re()[["moduleDataimport-dataimport_grouping_variable"]]
      message(paste0("Grouping variable: ", rv$grouping_variable))
    }
  )

  observe({
    req(rv$grouping_variable)
    updateTextInput(
      session = session,
      inputId = "dataimport_design",
      value = paste(
        "~ 0 +",
        rv$grouping_variable
      )
    )
  })


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
          column(
            6,
            fileInput(
              inputId = ns("import_metadata"),
              label = paste0("Please provide one CSV file containing the ",
                             "study metadata."),
              accept = c(".csv", "text/csv")
            )
          ),
          column(
            6,
            fileInput(
              inputId = ns("import_countdata"),
              label = paste0("Please provide one CSV file containing the ",
                             "study count data."),
              accept = c(".csv", "text/csv")
            )
          ),
          tags$hr(),
          actionButton(
            inputId = ns("dataimport_example_data"),
            label = "Load example data"
          ),
          width = 12
        )
      ),
      column(
        3,
        box(
          title = "Study definitions",
          conditionalPanel(
            condition = "output['moduleDataimport-tables']",
            selectInput(
              inputId = ns("dataimport_grouping_variable"),
              label = "Select grouping variable (for design formula)",
              choices = NULL,
              multiple = FALSE
            ),
            tags$hr(),
            textInput(
              inputId = ns("dataimport_design"),
              label = "Please fill in the design formula prefix"
            ),
            actionButton(
              inputId = ns("dataimport_confirm_design"),
              label = "Confirm design formula"
            ),
            conditionalPanel(
              condition = "output['moduleDataimport-design_present']",
              tags$hr(),
              verbatimTextOutput(ns("dataimport_design_formula")),
              tags$hr(),
              actionButton(
                inputId = ns("dataimport_preprocess"),
                label = "Preprocess data"
              ),
              conditionalPanel(
                condition = "output['moduleDataimport-preprocess_present']",
                tags$hr(),
                actionButton(
                  inputId = ns("dataimport_start_analysis"),
                  label = "Start analysis"
                )
              )
            )
          ),
          width = 12
        )
      )
    ),
    fluidRow(
      conditionalPanel(
        condition = "output['moduleDataimport-tables']",
        box(
          tabsetPanel(
            tabPanel(
              title = "Metadata",
              DT::dataTableOutput(ns("dataimport_metadata"))
            ),
            tabPanel(
              title = "Count data",
              DT::dataTableOutput(ns("dataimport_countdata"))
            )
          ),
          width = 12
        )
      )
    )
  )
}
