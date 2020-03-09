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


  observe({
    req(input_re()[["moduleDataimport-geo_studyinfos"]])

    ending <- rv$ending <- strsplit(
      input_re()[["moduleDataimport-geo_studyinfos"]]$name,
      ".",
      fixed = T)[[1]]

    if (ending[2] %in% c("csv", "CSV")) {
      file <- reactiveFileReader(
        1000, session,
        input_re()[["moduleDataimport-geo_studyinfos"]]$datapath,
        data.table::fread,
        header = T
      )

      rv$studiesinfo_import <- file()
    }
  })

  observeEvent(
    eventExpr = input_re()[["moduleDataimport-geo_load"]],
    handlerExpr = {
      shinyjs::disable("geo_studyinfos")
      shinyjs::disable("import_load_example")
      rv$studiesinfo_import <- data.table::as.data.table(rv$x)
      rv$import_load <- TRUE

    }
  )

  observe({
    req(rv$import_load)
    # create studies list
    rv$geo_studiesinfo <- tryCatch(
      expr = {
        ret <- sigident.preproc::diag_studiesinfo(
          tab = rv$studiesinfo_import,
          type = "discovery"
        )
      },
      error = function(e) {
        message(e)
        ret <- NULL
      },
      warning = function(w) {
        message(w)
        ret <- NULL
      },
      finally = function(f) {
        return(ret)
      }
    )

    if (!is.null(rv$geo_studiesinfo)) {

      rv$load_flag <- TRUE
    }
  })

  observe({
    req(rv$studiesinfo_import)

    # data frame needed for editable datatable
    rv$x <- as.data.frame(rv$studiesinfo_import)

    output$geo_studies_table <- DT::renderDataTable({
      DT::datatable(rv$x,
                    options = list(scrollX = TRUE,
                                   pageLength = 20,
                                   dom = "ltip"),
                    editable = TRUE)
    })

    # Editable Datatable
    # https://github.com/rstudio/DT/pull/480
    proxy <- DT::dataTableProxy("moduleDataimport-geo_studies_table")

    observeEvent(
      eventExpr =
        input_re()[["moduleDataimport-geo_studies_table_cell_edit"]],
      handlerExpr = {

        info <- input_re()[["moduleDataimport-geo_studies_table_cell_edit"]]
        utils::str(info)
        i <- info$row
        j <- info$col
        v <- info$value

        rv$x[i, j] <-
          DT::coerceValue(v, rv$x[i, j])

        DT::replaceData(
          proxy,
          rv$x,
          resetPaging = FALSE
        )
      })

    output$file_uploaded <- reactive({
      return(TRUE)
    })
    outputOptions(output, "file_uploaded",
                  suspendWhenHidden = FALSE)
  })

  observe({
    req(rv$load_flag)

    withProgress(
      expr = {
        # Debugging: comment the following for development purposes.
        # sigident.preproc::load_geo_data(
        #   studiesinfo = rv$geo_studiesinfo,
        #   datadir = rv$datadir,
        #   plotdir = rv$plotdir,
        #   idtype = "affy",
        #   viz_batch_boxp = input_re()[["moduleDataimport-geo_viz_hist"]],
        #   viz_batch_gpca = input_re()[["moduleDataimport-geo_viz_gpca"]]
        # )

        # catch diagnosis, sample_metadata, mergeset and mergedset
        # from global env
        rv$diagnosis <- eval(parse(text = "diagnosis"),
                             envir = 1L)
        rv$sample_metadata <- eval(parse(text = "sample_metadata"),
                                   envir = 1L)
        rv$mergeset <- eval(parse(text = "mergeset"),
                            envir = 1L)
        rv$mergedset <- eval(parse(text = "mergedset"),
                             envir = 1L)

        # we are finished with the import
        rv$import_finished <- TRUE
      },
      value = 1 / 1,
      message = "Loading datasets from GEO db..."
    )
  })


  # load example button
  observeEvent(
    eventExpr = input_re()[["moduleDataimport-import_load_example"]],
    handlerExpr = {
      shinyjs::disable("geo_studyinfos")
      shinyjs::disable("import_load_example")
      rv$studiesinfo_import <- data.table::fread(
        system.file(
          "demo_files/example_import.csv",
          package = "sigident.preproc"
        )
      )
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
          fileInput(inputId = "moduleDataimport-geo_studyinfos",
                    label = paste0("Please one CSV file containing the ",
                                   "study infos."),
                    accept = c(".csv", "text/csv")
          ),
          tags$hr(),
          actionButton(
            inputId = "moduleDataimport-import_load_example",
            label = "Load example"
          ),
          width = 12
        )
      ),
      column(
        3,
        box(
          title = "Plot batch effects",
          helpText("Caution: this takes some additional time"),
          checkboxInput(
            inputId = "moduleDataimport-geo_viz_hist",
            label = "Histogram",
            value = FALSE
          ),
          checkboxInput(
            inputId = "moduleDataimport-geo_viz_gpca",
            label = "gPCA",
            value = FALSE
          ),
          conditionalPanel(
            condition = "output['moduleDataimport-file_uploaded']",
            actionButton(
              inputId = "moduleDataimport-geo_load",
              label = "Load studies",
              icon = icon("download")
            )
          ),
          width = 12
        )
      ),
      conditionalPanel(
        condition = "output['moduleDataimport-file_uploaded']",
        box(
          title = "Study infos",
          DT::dataTableOutput(
            "moduleDataimport-geo_studies_table"
          ),
          width = 12
        )
      )
    )
  )
}
