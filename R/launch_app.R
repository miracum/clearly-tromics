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

  options(shiny.port = port)
  shiny::shinyAppDir(
    appDir = system.file("application", package = "tRomics")
  )
}

# document datasets
#' @title countdata
#'
#' @description A dataset containing the example count data
#'
#' @name countdata
#' @docType data
#' @keywords data
NULL

#' @title metadata
#'
#' @description A dataset containing the example meta data
#'
#' @name metadata
#' @docType data
#' @keywords data
NULL

#' @title mirna_countdata
#'
#' @description A dataset containing the miRNA example count data
#'
#' @name mirna_countdata
#' @docType data
#' @keywords data
NULL

#' @title mirna_metadata
#'
#' @description A dataset containing the miRNA example meta data
#'
#' @name mirna_metadata
#' @docType data
#' @keywords data
NULL
