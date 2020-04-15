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

  observe({
    req(rv$finished_plotting)

    if (is.null(rv$data_deseq)) {

      # Create a Progress object
      progress <- shiny::Progress$new()
      # Make sure it closes when we exit this reactive, even if
      # there's an error
      on.exit(progress$close())
      progress$set(
        message = paste0("Data transformations."),
        value = 0
      )

      progress$inc(
        1 / 3,
        detail = paste("... DESeq2::DESeq ...")
      )
      rv$data_deseq <- DESeq2::DESeq(rv$data)

      progress$inc(
        1 / 3,
        detail = paste("... extracting groups ...")
      )
      rv$deg$groups <- as.character(
        unique(
          SummarizedExperiment::colData(rv$data)[[rv$grouping_variable]]
        )
      )

      progress$inc(
        1 / 3,
        detail = paste("... creating contrast list ...")
      )
      rv$deg$contrast_list <- gtools::permutations(
        n = length(rv$deg$groups),
        r = 2,
        v = rv$deg$groups
      )
      colnames(rv$deg$contrast_list) <- c("compare", "control")
      rv$finished_contrast <- TRUE
      progress$close()
    }
  })

  observe({
    req(rv$finished_contrast)

    if (is.null(rv$finished_deg_checkboxes)) {
      cb_labels <- sapply(
        X = seq_len(nrow(rv$deg$contrast_list)),
        FUN = function(x) {
          paste(
            rv$deg$contrast_list[x, "compare"],
            "vs.",
            rv$deg$contrast_list[x, "control"]
          )
        }
      )

      output$deg_contrast_list_cb <- renderUI({
        checkboxGroupInput(
          inputId = "moduleDEG-deg_contrast_checkbox_group",
          label = "Please select the compare/control combinations to analyse",
          choices = sapply(
            X = cb_labels,
            FUN = function(x) {
              return(which(cb_labels == x))
            }, simplify = F, USE.NAMES = T
          ),
          inline = FALSE
        )
      })
      rv$finished_deg_checkboxes <- TRUE
    }
  })

  observeEvent(
    eventExpr = input_re()[["moduleDEG-deg_start"]],
    handlerExpr = {

      selected_rows <- as.integer(
        input_re()[["moduleDEG-deg_contrast_checkbox_group"]]
      )

      if (length(selected_rows) > 0) {

        shinyjs::disable("deg_start")
        shinyjs::disable("deg_fdr")
        shinyjs::disable("deg_orgdb")

        rv$deg$results <- list()

        rv$deg_q <- input_re()[["moduleDEG-deg_fdr"]]

        # Create a Progress object
        progress <- shiny::Progress$new()
        # Make sure it closes when we exit this reactive, even if
        # there's an error
        on.exit(progress$close())
        progress$set(
          message = paste0("Performing DEG analysis."),
          value = 0
        )

        # extract necessary comparisons
        comparisons <- rv$deg$contrast_list[selected_rows, ]

        for (contr in seq_len(nrow(comparisons))) {

          list_extract <- comparisons[contr, ]
          compare_group <- list_extract[["compare"]]
          control_group <- list_extract[["control"]]

          progress$inc(
            1 / nrow(comparisons),
            detail = paste(
              "...  Compare:", compare_group,
              "Control:", control_group, "..."
            )
          )

          output_name <- tolower(
            paste(rv$grouping_variable,
                  compare_group,
                  control_group,
                  sep = "_")
          )

          # Create a Progress object
          progress1 <- shiny::Progress$new()
          # Make sure it closes when we exit this reactive, even if
          # there's an error
          on.exit(progress1$close())
          progress1$set(
            message = paste0("DEG analysis"),
            value = 0
          )

          progress1$inc(
            1 / 4,
            detail = paste("... deg_analysis ...")
          )
          rv$deg$results[[output_name]] <- deg_analysis(
            data = rv$data_deseq,
            group_variable = rv$grouping_variable,
            compare_group = compare_group,
            control_group = control_group
          )

          progress1$inc(
            1 / 4,
            detail = paste("... annotation ...")
          )
          orgdb <- input_re()[["moduleDEG-deg_orgdb"]]

          if (!(orgdb %in% utils::installed.packages()[, "Package"])) {
            withProgress({
              BiocManager::install(
                pkgs = orgdb,
                update = F
              )
            },
            value = 0.5,
            message = paste0("Downloading ", orgdb)
            )
          }

          rv$deg$results[[output_name]]$symbol <-
            deg_annotation(
              keys = row.names(rv$deg$results[[output_name]]),
              orgdb = orgdb,
              column = "SYMBOL"
            )
          rv$deg$results[[output_name]]$entrez <-
            deg_annotation(
              keys = row.names(rv$deg$results[[output_name]]),
              orgdb = orgdb,
              column = "ENTREZID"
            )

          progress1$inc(
            1 / 4,
            detail = paste("... CSV export ...")
          )
          utils::write.csv(
            x = rv$deg$results[[output_name]],
            file = paste0(rv$csvdir, output_name, ".csv")
          )

          progress1$inc(
            1 / 4,
            detail = paste("... volcano plot ...")
          )
          plot_volcano(
            results_object = rv$deg$results[[output_name]],
            filename = paste0(rv$plotdir,
                              "Volcano_", output_name, ".png"
            ),
            title = output_name
          )
          progress1$close()
        }
        progress$close()
        rv$finished_deg <- TRUE
      } else {
        shiny::showModal(
          modalDialog(
            title = "No comparison selected",
            "Please choose at least one comparison to perform DEG analysis."
          )
        )
      }
    }
  )

  # render gui here
  observe({
    req(rv$finished_deg)

    for (output_name in names(rv$deg$results)) {
      print(output_name)

      # append tabs
      appendTab(
        inputId = "deg_result_tabs",
        tab = tabPanel(
          title = output_name,
          div(class = "row",
              style = "text-align: center",
              downloadButton(
                outputId = paste0("moduleDEG-tab_dl_", output_name),
                label = paste0("Download ", output_name),
                style = paste0(
                  "white-space: normal; ",
                  "text-align:center; ",
                  "padding: 9.5px 9.5px 9.5px 9.5px; ",
                  "margin: 6px 10px 6px 10px;")
              )
          ),
          DT::dataTableOutput(
            outputId = paste0("moduleDEG-tab_", output_name)
          )
        ),
        select = TRUE
      )

      rv$finished_deg_tabs <- TRUE
    }
  })

  observe({
    req(rv$finished_deg_tabs)

    if (is.null(rv$finished_deg_outputs)) {

      lapply(
        X = names(rv$deg$results),
        FUN = function(output_name) {

          # fill table
          output[[paste0("tab_", output_name)]] <-
            DT::renderDataTable({
              DT::datatable(
                data.frame(rv$deg$results[[output_name]]),
                options = list(scrollX = TRUE,
                               pageLength = 20,
                               dom = "ltip"))
            })

          # create dl button
          output[[paste0("tab_dl_", output_name)]] <-
            downloadHandler(
              filename = function() {
                paste0(output_name, ".csv")
              },
              content = function(file) {
                file.copy(
                  from = paste0(rv$csvdir, output_name, ".csv"),
                  to = file
                )
              },
              contentType = "text/csv"
            )
        }
      )
      rv$finished_deg_outputs <- TRUE
    }
  })

  observe({
    output$deg_volcano <- renderImage(
      expr = {
        filename <- paste0(
          rv$plotdir,
          paste0("Volcano_", input$deg_result_tabs, ".png")
        )
        # Return a list containing the filename
        list(src = filename)
      },
      deleteFile = FALSE)

    output$dl_deg_volcano <- downloadHandler(
      filename = function() {
        paste0("Volcano_", input$deg_result_tabs, ".png")
      },
      content = function(file) {
        file.copy(paste0(
          rv$plotdir,
          paste0("Volcano_", input$deg_result_tabs, ".png")
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
      column(
        9,
        box(
          title = "DEG analysis",
          div(class = "row",
              style = "text-align: center",
              downloadButton(
                outputId = ns("dl_deg_volcano"),
                label = "Download Volcano plot",
                style = paste0(
                  "white-space: normal; ",
                  "text-align:center; ",
                  "padding: 9.5px 9.5px 9.5px 9.5px; ",
                  "margin: 6px 10px 6px 10px;")
              )
          ),
          imageOutput(
            outputId = ns("deg_volcano")
          ),
          tags$head(
            tags$style(
              type = "text/css",
              paste0(
                "#moduleDEG-deg_volcano img ",
                "{max-height: 100%; max-width: 100%; width: auto; ",
                "display: block; margin-left: auto; margin-right: auto;}"))
          ),
          width = 12
        )
      ),
      column(
        3,
        box(
          title = "Group-Comparisons to test",
          uiOutput(ns("deg_contrast_list_cb")),
          width = 12
        ),
        box(
          title = "Options",
          # nolint start
          # numericInput(
          #   inputId = ns("deg_fdr"),
          #   label = "False discovery rate (default: 0.05)",
          #   value = 0.05,
          #   min = 0,
          #   max = 1,
          #   step = 0.001
          # ),
          # nolint end
          textInput(
            inputId = ns("deg_orgdb"),
            label = "OrgDB",
            value = "org.Rn.eg.db"
          ),
          tags$hr(),
          actionButton(
            inputId = ns("deg_start"),
            label = "Start DEG analysis"
          ),
          width = 12
        )
      )
    ),
    fluidRow(
      box(
        tabsetPanel(id = ns("deg_result_tabs")),
        width = 12
      )
    )
  )
}
