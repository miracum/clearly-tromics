#' @title Launch the tRomics gui
#' @param port The port, tRomics is running on (default: 3838)
#' @param dir A character string. The directory, where to create folders to
#'   e.g. store plots etc. (default: tempdir()). CAUTION: relative paths are
#'   not supported.
#'
#' @return tRomics application
#'
#' @import shiny shinydashboard
#' @importFrom data.table .N ":="
#'
#' @export
#'
launch_app <- function(port=3838,
                       dir = tempdir()) {

  global_env_hack <- function(key,
                              val,
                              pos) {
    assign(
      key,
      val,
      envir = as.environment(pos)
    )
  }

  global_env_hack("dir",
                  dir,
                  1L)

  # create temp-dirs
  dir.create(paste0(dir, "/csv/"))
  dir.create(paste0(dir, "/plots/"))
  dir.create(paste0(dir, "/datadir/"))

  options(shiny.port = port)
  shiny::shinyAppDir(
    appDir = system.file("application", package = "tRomics")
  )
}
