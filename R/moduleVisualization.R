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

    plot_pca(
      rld = rv$data_logtrans,
      filename = paste0(rv$plotdir, "PCA_plot.png"),
      color_var = rv$grouping_variable
    )

    plot_mds(
      rld = rv$data_logtrans,
      filename = paste0(rv$plotdir, "MDS_plot.png"),
      color_var = rv$grouping_variable
    )
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
