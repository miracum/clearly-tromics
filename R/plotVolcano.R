#' @title plotVolcano
#'
#' @description Function for plotting the results of differential
#'   expression analysis as volcano plot
#'
#' @param results The resultsfile.
#' @param title A characte string. The plot title
#'
#' @export
#'
# plotVolcano  <- function(
#   results,
#   title,
#   FDR
# ) {
#   return(
#
#     EnhancedVolcano::EnhancedVolcano(as.data.frame(results)),
#      title = title,
#      lab = as.character(row.names(results)),
#      selectLab = '',
#      subtitle = '',
#      x = 'log2FoldChange',
#      y = 'padj',
#      legend=c('not significant','Log (base 2) fold-change','FDR',
#              'FDR & Log (base 2) fold-change'),
#      pCutoff = 0.05,
#      FCcutoff = 1,
#      transcriptPointSize = 3.0,
#      colAlpha = 0.5,
#      ylab = bquote(~-Log[10]~FDR) # TODO export package?
#
#   )
# }
