#' @title module_visualization_server
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
# module_visualization_server
module_visualization_server <- function(input,
                                     output,
                                     session,
                                     rv,
                                     input_re) {

  observe({
    req(rv$data_logtrans)

    # Create a Progress object
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if
    # there's an error
    on.exit(progress$close())
    progress$set(
      message = paste0("Plotting"),
      value = 0
    )

    progress$inc(
      1 / 3,
      detail = paste("... plotting PCA ...")
    )

    plot_pca(
      data = rv$data_logtrans,
      filename = paste0(rv$plotdir, "PCA_plot.png"),
      color_var = rv$grouping_variable
    )

    progress$inc(
      1 / 3,
      detail = paste("... plotting MDS ...")
    )

    plot_mds(
      data = rv$data_logtrans,
      filename = paste0(rv$plotdir, "MDS_plot.png"),
      color_var = rv$grouping_variable
    )

    progress$inc(
      1 / 3,
      detail = paste("... plotting heatmap ...")
    )

    plot_heatmap(
      data = rv$data_logtrans,
      filename = paste0(rv$plotdir, "DEG_heatmap.png"),
      ngenes = 1000
    )

    rv$finished_plotting <- TRUE

    progress$close()
  })


  observe({
    output$viz_pca <- renderImage(
      expr = {
        filename <- paste0(
          rv$plotdir,
          "PCA_plot.png"
        )
        # Return a list containing the filename
        list(src = filename)
      },
      deleteFile = FALSE)

    output$dl_viz_pca <- downloadHandler(
      filename = function() {
        paste0("PCA_plot.png")
      },
      content = function(file) {
        file.copy(paste0(
          rv$plotdir,
          "PCA_plot.png"
        ),
        file)
      },
      contentType = "image/png"
    )


    output$viz_mds <- renderImage(
      expr = {
        filename <- paste0(
          rv$plotdir,
          "MDS_plot.png"
        )
        # Return a list containing the filename
        list(src = filename)
      },
      deleteFile = FALSE)

    output$dl_viz_mds <- downloadHandler(
      filename = function() {
        paste0("MDS_plot.png")
      },
      content = function(file) {
        file.copy(paste0(
          rv$plotdir,
          "MDS_plot.png"
        ),
        file)
      },
      contentType = "image/png"
    )
  })


}


#' @title module_visualization_ui
#'
#' @param id A character. The identifier of the shiny object
#'
#' @export
#'
# module_visualization_ui
module_visualization_ui <- function(id) {
  ns <- NS(id)

  tagList(# first row
    box(
      title = "PCA plot",
      div(class = "row",
          style = "text-align: center",
          downloadButton(
            outputId = ns("dl_viz_pca"),
            label = "Download PCA plot",
            style = paste0(
              "white-space: normal; ",
              "text-align:center; ",
              "padding: 9.5px 9.5px 9.5px 9.5px; ",
              "margin: 6px 10px 6px 10px;")
          )
      ),
      imageOutput(
        outputId = ns("viz_pca")
      ),
      tags$head(
        tags$style(
          type = "text/css",
          paste0("#moduleVisualization-viz_pca img ",
                 "{max-height: 100%; max-width: 100%; width: auto}"))
      ),
      width = 6
    ),
    box(
      title = "MDS plot",
      div(class = "row",
          style = "text-align: center",
          downloadButton(
            outputId = ns("dl_viz_mds"),
            label = "Download MDS plot",
            style = paste0(
              "white-space: normal; ",
              "text-align:center; ",
              "padding: 9.5px 9.5px 9.5px 9.5px; ",
              "margin: 6px 10px 6px 10px;")
          )
      ),
      imageOutput(
        outputId = ns("viz_mds")
      ),
      tags$head(
        tags$style(
          type = "text/css",
          paste0("#moduleVisualization-viz_mds img ",
                 "{max-height: 100%; max-width: 100%; width: auto}"))
      ),
      width = 6
    )

  )
}
