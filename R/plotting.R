#' @title plot_pca
#'
#' @description Function for principle component analysis and plotting
#'
#' @param filename A character string. The filename.
#' @param rld Object containing log-transformed counts
#' @param color_var Variable deciding group coloring
#'
#' @export
plot_pca <- function(rld,
                     filename,
                     color_var) {

  pcaData <- DESeq2::plotPCA(
    rld,
    intgroup = color_var,
    returnData = T
  )

  percentVar <- round(100 * attr(pcaData, "percentVar"))

  grDevices::png(
    filename = filename,
    res = 150,
    height = 1000,
    width = 1500
  )
  print({
    ggplot2::ggplot(
      data = pcaData,
      ggplot2::aes_string(x = "PC1", y = "PC2", color = color_var)
    ) +
      ggplot2::geom_point(size = 3) +
      ggplot2::xlab(paste0("PC1: ", percentVar[1], "% variance")) +
      ggplot2::ylab(paste0("PC2: ", percentVar[2], "% variance")) +
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
plot_mds <- function(rld,
                     filename,
                     color_var) {

  sampleDists <- stats::dist(t(SummarizedExperiment::assay(rld)))
  sampleDistMatrix <- as.matrix( sampleDists )
  mds <- as.data.frame(SummarizedExperiment::colData(rld))
  mds <- cbind(mds, stats::cmdscale(sampleDistMatrix))

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
#' @description Function to create multi-dimensional scaling plot
#'
#' @inheritParams plot_pca
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



#' @title plot_volcano
#'
#' @description Function for plotting the results of differential expression analysis as volcano plot
#'
#' @param results The resultsfile.
#' @param title A characte string. The plot title
#'
#' @export
#'
plot_volcano  <- function(
  results,
  title,
  FDR
) {


  EnhancedVolcano::EnhancedVolcano(as.data.frame(results)),
  title = title,
  lab = as.character(row.names(results)),
  selectLab = '',
  subtitle = '',
  x = 'log2FoldChange',
  y = 'padj',
  legend=c('not significant','Log (base 2) fold-change','FDR',
           'FDR & Log (base 2) fold-change'),
  pCutoff = 0.05,
  FCcutoff = 1,
  transcriptPointSize = 3.0,
  colAlpha = 0.5,
  ylab = bquote(~-Log[10]~FDR) # TODO export package?
}
