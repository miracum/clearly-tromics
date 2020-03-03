#' @title plot_rocplot
#'
#' @description Helper function to create roc plots
#'
#' @param filename A character string. The filename.
#' @param rld
#' @param ngenes An integer. Number of genes to display (default: 1000).
#'
#' @export
plot_heatmap <- function(rld,
                         filename,
                         ngenes = 1000) {

  ##heatmap
  top_variance_genes <- utils::head(
    order(
      matrixStats::rowVars(
        SummarizedExperiment::assay(rld)),
      decreasing=T),
    ngenes
    )
  var_mat <- SummarizedExperiment::assay(rld)[top_variance_genes, ]
  var_mat <- var_mat - rowMeans(var_mat)

  annotation_data <- as.data.frame(SummarizedExperiment::colData(rld))


  grDevices::png(
    filename = filename,
    res = 150,
    height = 1000,
    width = 1500
  )
  print({
    pheatmap::pheatmap(
      var_mat,
      scale = 'row',
      show_rownames = F,
      color = gplots::bluered(100),
      fontsize = 8,
      annotation_col = annotation_data,
      cluster_cols = F
    )
  })
  grDevices::dev.off()
}

