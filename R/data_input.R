#' @title data_input
#'
#' @description Function for data input and pre-filtering
#'
#' @param counttable filename or path to .csv-file containing raw read counts
#' @param metadata filename or path to .csv-file containing sampe descriptions
#' @param design design formula for linear model
#' @export

data_input <- function(
    counttable,
    metadata,
    design
) {
    return(
dds <- DESeq2::DESeqDataSetFromMatrix(countData = as.matrix(utils::read.csv(counttable, row.names = 1)),
                                      colData = utils::read.csv(metadata, row.names = 1),
                                      design = design)
+
dds <- dds[rowSums(DESeq2::counts(dds)))]                             
    )
}