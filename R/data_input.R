#' @title data_input
#'
#' @description Function for data input and pre-filtering
#'
#' @param counttable Filename or path to .csv-file containing raw read counts
#' @param metadata Filename or path to .csv-file containing sample descriptions
#' @param design Design formula for linear model (as a character string).
#' @export

data_input <- function(
    counttable,
    metadata,
    design) {

    dds <- DESeq2::DESeqDataSetFromMatrix(
        countData = as.matrix(utils::read.csv(counttable, row.names = 1)),
        colData = utils::read.csv(metadata, row.names = 1),
        design = stats::as.formula(design)
    )

    dds <- dds[rowSums(DESeq2::counts(dds)) >= 10, ]

    return(dds)
}


#' @title log_trans
#'
#' @description Wrapper function for log-transformation of counts
#'
#' @param data TODO doku
#'
#' @export
log_trans <- function(data) {
    rld <- DESeq2::rlog(dds, blind = F)
    return(rld)
}
