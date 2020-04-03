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
  column = "SYMBOL",
  keytype = "ENSEMBL"
) {
  return(
    AnnotationDbi::mapIds(
      x = orgdb,
      keys = row.names(keys),
      column = column,
      keytype = keytype,
      multiVals = "first")
  )
}
