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
  )
}
