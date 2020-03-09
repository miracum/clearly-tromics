#' @title module_deg_server
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
# module_deg_server
module_deg_server <- function(input,
                              output,
                              session,
                              rv,
                              input_re) {

  observeEvent(
    eventExpr = input_re()[["moduleDEG-deg_start"]],
    handlerExpr = {
      rv$deg_q <- input_re()[["moduleDEG-deg_fdr"]]

      progress1 <- shiny::Progress$new()
      # Make sure it closes when we exit this reactive, even if there's an error
      on.exit(progress1$close())
      progress1$set(message = "Performing DEG analysis",
                    value = 0)


      progress1$inc(
        1 / 4,
        detail = paste("... identifying DEGs ...")
      )
      rv$genes <- sigident.func::identify_degs(
        mergeset = rv$mergeset,
        diagnosis = rv$diagnosis,
        q_value = rv$deg_q
      )

      # heatmap creation
      progress1$inc(
        1 / 4,
        detail = paste("... creating the heatmap ...")
      )
      # create colors for map
      ht_colors <- sigident.func::color_heatmap(
        sample_metadata = rv$sample_metadata
      ) #% cancer = red

      sigident.func::plot_deg_heatmap(
        mergeset = rv$mergeset,
        genes = rv$genes,
        patientcolors = ht_colors,
        filename = paste0(rv$plotdir, "DEG_heatmap.png")
      )
      # workaround to tell ui, that analysis is performed
      output$heatmap <- reactive({
        return(TRUE)
      })
      outputOptions(output, "heatmap",
                    suspendWhenHidden = FALSE)

      progress1$inc(
        1 / 4,
        detail = paste("... creating DEG infos ...")
      )
      rv$deg_info <- sigident.func::export_deg_annotations(
        mergedset = rv$mergedset,
        genes = rv$genes,
        idtype = rv$idtype
      )

      # export table with differential expression parameters and annotations
      progress1$inc(
        1 / 4,
        detail = paste("... creating DEG results ...")
      )
      rv$deg_results <- sigident.func::limma_top_table(
        mergeset = rv$mergeset,
        diagnosis = rv$diagnosis,
        q_value = rv$deg_q
      )
      progress1$close()
    }
  )

  observe({
    req(rv$deg_info)
    output$deg_table_info <- DT::renderDataTable({
      DT::datatable(rv$deg_info,
                    options = list(scrollX = TRUE,
                                   pageLength = 20,
                                   dom = "ltip"))
    })

    output$dl_deg_info <- downloadHandler(
      filename = function() {
        "deg_info.csv"
      },
      content = function(file) {
        data.table::fwrite(rv$deg_info, file)
      },
      contentType = "text/csv"
    )
  })

  observe({
    req(rv$deg_results)
    output$deg_table_results <- DT::renderDataTable({
      DT::datatable(rv$deg_results,
                    options = list(scrollX = TRUE,
                                   pageLength = 20,
                                   dom = "ltip"))
    })

    output$dl_deg_results <- downloadHandler(
      filename = function() {
        "deg_results.csv"
      },
      content = function(file) {
        data.table::fwrite(rv$deg_results, file)
      },
      contentType = "text/csv"
    )
  })

  observe({
    output$deg_heatmap <- renderImage(
      expr = {
        filename <- paste0(
          rv$plotdir,
          "DEG_heatmap.png"
        )
        # Return a list containing the filename
        list(src = filename)
      },
      deleteFile = FALSE)

    output$dl_deg_heatmap <- downloadHandler(
      filename = function() {
        "DEG_heatmap.png"
      },
      content = function(file) {
        file.copy(paste0(
          rv$plotdir,
          "DEG_heatmap.png"
        ),
        file)
      },
      contentType = "image/png"
    )
  })
}


#' @title module_deg_ui
#'
#' @param id A character. The identifier of the shiny object
#'
#' @export
#'
# module_deg_ui
module_deg_ui <- function(id) {
  ns <- NS(id)

  tagList(# first row
    fluidRow(
      box(
        title = "DEG analysis",
        conditionalPanel(
          condition = "output['moduleDEG-heatmap']",
          div(class = "row",
              style = "text-align: center",
              downloadButton(
                outputId = ns("dl_deg_heatmap"),
                label = "Download Heatmap",
                style = paste0(
                  "white-space: normal; ",
                  "text-align:center; ",
                  "padding: 9.5px 9.5px 9.5px 9.5px; ",
                  "margin: 6px 10px 6px 10px;")
              )
          ),
          imageOutput(
            outputId = ns("deg_heatmap")
          ),
          tags$head(
            tags$style(
              type = "text/css",
              paste0("#moduleDEG-deg_heatmap img ",
                     "{max-height: 100%; max-width: 100%; width: auto}"))
          )),
        width = 9
      ),
      box(
        title = "Options",
        numericInput(
          inputId = ns("deg_fdr"),
          label = "False discovery rate (default: 0.05)",
          value = 0.05,
          min = 0,
          max = 1,
          step = 0.001
        ),
        actionButton(
          inputId = ns("deg_start"),
          label = "Start DEG analysis"
        ),
        width = 3
      )
    ),
    fluidRow(
      box(
        tabsetPanel(
          tabPanel(
            title = "DEG info",
            DT::dataTableOutput(
              ns("deg_table_info")
            ),
            div(class = "row",
                style = "text-align: center",
                downloadButton(
                  outputId = ns("dl_deg_info"),
                  label = "Download DEG info",
                  style = paste0(
                    "white-space: normal; ",
                    "text-align:center; ",
                    "padding: 9.5px 9.5px 9.5px 9.5px; ",
                    "margin: 6px 10px 6px 10px;")
                )
            )
          ),
          tabPanel(
            title = "DEG results",
            DT::dataTableOutput(
              ns("deg_table_results")
            ),
            div(class = "row",
                style = "text-align: center",
                downloadButton(
                  outputId = ns("dl_deg_results"),
                  label = "Download DEG results",
                  style = paste0(
                    "white-space: normal; ",
                    "text-align:center; ",
                    "padding: 9.5px 9.5px 9.5px 9.5px; ",
                    "margin: 6px 10px 6px 10px;")
                )
            )
          )
        ),
        width = 12
      )
    )
  )
}
