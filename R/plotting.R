#' @title plot_pca
#'
#' @description Function for principle component analysis and plotting
#'
#' @param filename A character string. The filename.
#' @param data Object containing log-transformed counts
#' @param color_var Variable deciding group coloring
#'
#' @export
plot_pca <- function(data,
                     filename,
                     color_var) {

  pca_data <- DESeq2::plotPCA(
    data,
    intgroup = color_var,
    returnData = T
  )

  percent_var <- round(100 * attr(pca_data, "percentVar"))

  grDevices::png(
    filename = filename,
    res = 150,
    height = 1000,
    width = 1500
  )
  print({
    ggplot2::ggplot(
      data = pca_data,
      ggplot2::aes_string(x = "PC1", y = "PC2", color = color_var)
    ) +
      ggplot2::geom_point(size = 3) +
      ggplot2::xlab(paste0("PC1: ", percent_var[1], "% variance")) +
      ggplot2::ylab(paste0("PC2: ", percent_var[2], "% variance")) +
      ggplot2::coord_fixed()
  })
  grDevices::dev.off()
}



#' @title plot_mds
#'
#' @description Function to create multi-dimensional scaling plot
#'
#' @inheritParams plot_pca
#'
#' @export
plot_mds <- function(data,
                     filename,
                     color_var) {

  sample_dists <- stats::dist(t(SummarizedExperiment::assay(data)))
  sample_dist_matrix <- as.matrix(sample_dists)
  mds <- as.data.frame(SummarizedExperiment::colData(data))
  mds <- cbind(mds, stats::cmdscale(sample_dist_matrix))

  grDevices::png(
    filename = filename,
    res = 150,
    height = 1000,
    width = 1500
  )
  print({
    ggplot2::ggplot(
      data = mds,
      ggplot2::aes_string(x = "`1`", y = "`2`", color = color_var)) +
      ggplot2::geom_point(size = 3) +
      ggplot2::coord_fixed()
  })
  grDevices::dev.off()
}


#' @title plot_heatmap
#'
#' @description Function to create heatmap of top-ranked genes (by variance)
#'
#' @param ngenes Number of top variance genes to be included in the heatmap
#'
#' @inheritParams plot_pca
#'
#' @export
plot_heatmap <- function(data,
                         filename,
                         ngenes = 1000) {

  ##heatmap
  top_variance_genes <- utils::head(
    order(
      matrixStats::rowVars(
        SummarizedExperiment::assay(data)),
      decreasing = T),
    ngenes
  )
  var_mat <- SummarizedExperiment::assay(data)[top_variance_genes, ]
  var_mat <- var_mat - rowMeans(var_mat)

  annotation_data <- as.data.frame(SummarizedExperiment::colData(data))


  grDevices::png(
    filename = filename,
    res = 150,
    height = 1000,
    width = 1500
  )
  print({
    pheatmap::pheatmap(
      var_mat,
      scale = "row",
      show_rownames = F,
      color = gplots::bluered(100),
      fontsize = 8,
      annotation_col = annotation_data,
      cluster_cols = F
    )
  })
  grDevices::dev.off()
}



#' @title plot_volcano
#'
#' @description Function for plotting the results of differential
#'   expression analysis as volcano plot
#'
#' @param results_object The resultsfile generated with deg_analysis function
#' @param title A characte string. The plot title
#'
#' @inheritParams plot_pca
#'
#' @export
plot_volcano  <- function(results_object,
                          filename,
                          title) {

  grDevices::png(
    filename = filename,
    res = 150,
    height = 1000,
    width = 1500
  )
  print({
    EnhancedVolcano::EnhancedVolcano(
      as.data.frame(results_object),
      title = title,
      lab = as.character(row.names(results_object)),
      selectLab = "",
      subtitle = "",
      x = "log2FoldChange",
      y = "padj",
      legend = c("not significant", "Log (base 2) fold-change", "FDR",
                 "FDR & Log (base 2) fold-change"),
      pCutoff = 0.05,
      FCcutoff = 1,
      pointSize = 3.0,
      colAlpha = 0.5,
      ylab = bquote(~-Log[10]~FDR))
  })
  grDevices::dev.off()
}
