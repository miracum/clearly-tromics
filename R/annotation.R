#' @title annotation
#'
#' @description Helper function to create roc plots
#'
#' @param filename A character string. The filename.
#' @param rld
#' @param ngenes An integer. Number of genes to display (default: 1000).
#'
#' @export
annotation <- function(
  keys,
  orgdb = "org.Rn.eg.db",
  column = "SYMBOL",
  keytype = "ENSEMBL",
  multi_vals = "first"
) {
  return(
    AnnotationDbi::mapIds(
      x = orgDB,
      keys = row.names(keys),
      column = column,
      keytype = keytype,
      multiVals = multi_vals)
  )
}
