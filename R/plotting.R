#' @title plot_pca
#'
#' @description Helper function to create roc plots
#'
#' @param filename A character string. The filename.
#' @param rld
#' @param #TODO
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
      ggplot2::geom_point(size=3) +
      ggplot2::xlab(paste0("PC1: ", percentVar[1], "% variance")) +
      ggplot2::ylab(paste0("PC2: ", percentVar[2], "% variance")) +
      ggplot2::coord_fixed()
  })
  grDevices::dev.off()
}



#' @title plot_mds
#'
#' @description Helper function to create roc plots
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
