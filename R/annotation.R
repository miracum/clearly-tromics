#' @title deg_annotation
#'
#' @description Function to add common annotations from
#'   several common databases
#'
#' @param keys existing annotation
#' @param orgdb A annotation package
#' @param column Data to be retrieved from the database
#' @param keytype Type of existing annotation
#'
#' @export
deg_annotation <- function(
  keys,
  orgdb,
  column,
  keytype = "ENSEMBL"
) {

  require(orgdb, character.only = TRUE)

  ret <- AnnotationDbi::mapIds(
      x = eval(parse(text = orgdb)),
      keys = keys,
      column = column,
      keytype = keytype,
      multiVals = "first"
  )
  return(ret)
}
