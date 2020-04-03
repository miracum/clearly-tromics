#' @title annotation
#'
#' @description Function to add common annotations from several common databases
#'
#' @param keys existing annotation
#' @param orgdn A annotation package
#' @param column Data to be retrieved from the database
#' @param keytype Type of existing annotation
#'
#' @export
annotation <- function(
  keys,
  orgdb = "org.Rn.eg.db",
  column,
  keytype = "ENSEMBL"
) {


  if (!(orgdb %in% installed.packages()[,"Package"])) {
    BiocManager::install(
      pkgs = orgdb,
      update = F
    )
  }

  ret <- AnnotationDbi::mapIds(
      x = eval(parse(text = orgdb)),
      keys = keys,
      column = column,
      keytype = keytype,
      multiVals = "first"
  )
  return(ret)
}
