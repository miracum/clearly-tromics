##Differential gene expression analysis

library(DESeq2)
library(ggplot2)
library(dplyr)
library(org.Rn.eg.db)
library(gplots)
library(pheatmap)
library(EnhancedVolcano)


# #Input of count matrix
# counttable <- "P:/Hannover/Kreutzer/raw_data/count_data.csv"
# metadata <- "P:/Hannover/Kreutzer/raw_data/metadata.csv"
#
# countdata <- as.matrix(utils::read.csv(counttable, row.names = 1))
# coldata <- utils::read.csv(metadata, row.names = 1)
#
# ##Checking annotation
# all(rownames(coldata) %in% colnames(countdata))
# all(rownames(coldata) == colnames(countdata))
#
# ##Creating DESeq2dataset object
#
# dds <- DESeq2::DESeqDataSetFromMatrix(countData = countdata,
#                               colData = coldata,
#                               design = ~ 0 + treatment)
#
# #Pre-filtering
# nrow(dds)
# keep <- rowSums(DESeq2::counts(dds)) >= 10
# dds <- dds[keep,]
# nrow(dds)
## TODO INFO replaced 30.03.2020
dds <- data_input(counttable = "inst/example_data/count_data.csv",
                  metadata = "inst/example_data/metadata.csv",
                  design = "~ 0 + treatment")


#rlog transformation
# rld <- DESeq2::rlog(dds, blind = F)
# utils::head(assay(rld), 3)
rld <- log_trans(dds)

grouping_variable <- "treatment"

#PCA
# pcaData <- DESeq2::plotPCA(rld,intgroup = 'treatment',returnData = T)
# pcaData
#
# percentVar <- round(100*attr(pcaData, 'percentVar'))
# ggplot2::ggplot(pcaData, ggplot2::aes(x=PC1, y=PC2, color=treatment)) +
#   ggplot2::geom_point(size=3) +
#   ggplot2::xlab(paste0('PC1: ', percentVar[1], '% variance')) +
#   ggplot2::ylab(paste0('PC2: ', percentVar[2], '% variance')) +
#   ggplot2::coord_fixed()
plot_pca(
  data = rld,
  filename = "plots/PCA_plot.png",
  color_var = grouping_variable
)


#MDS
# sampleDists <- stats::dist(t(assay(rld)))
# sampleDistMatrix <- as.matrix( sampleDists )
# mds <- as.data.frame(colData(rld))  %>%
#   cbind(stats::cmdscale(sampleDistMatrix))
#
# ggplot2::ggplot(mds, aes(x = `1`, y = `2`, color = treatment)) +
#   ggplot2::geom_point(size = 3) +
#   ggplot2::coord_fixed()
plot_mds(
  data = rld,
  filename = "plots/MDS_plot.png",
  color_var = grouping_variable
)



groups <- as.character(unique(SummarizedExperiment::colData(dds)[[grouping_variable]]))
contrast_list <- gtools::permutations(n = length(groups), r = 2, v = groups)
colnames(contrast_list) <- c("compare", "control")

rv <- list()

dds <- DESeq2::DESeq(dds)


orgdb <- "org.Rn.eg.db"
require(orgdb, character.only = TRUE)

for (contr in seq_len(nrow(contrast_list))) {

  message(contr)

  list_extract <- contrast_list[contr, ]
  compare_group <- list_extract[["compare"]]
  control_group <- list_extract[["control"]]

  output_name <- tolower(
    paste(grouping_variable, compare_group, control_group, sep = "_")
  )

  rv[[output_name]] <- deg_analysis(
    data = dds,
    group_variable = grouping_variable,
    compare_group = compare_group,
    control_group = control_group
  )

  rv[[output_name]]$symbol <- deg_annotation(
    keys = row.names(rv[[output_name]]),
    orgdb = orgdb,
    column = "SYMBOL"
  )
  rv[[output_name]]$entrez <- deg_annotation(
    keys = row.names(rv[[output_name]]),
    orgdb = orgdb,
    column = "ENTREZID"
  )

  utils::write.csv(
    x = rv[[output_name]],
    file = paste0("./csv/", output_name, ".csv")
  )
}


# #Differential expression analysis
# dds <- DESeq2::DESeq(dds)
# A_Bufalin_vs_DMSO <- DESeq2::results(dds, contrast = c('treatment','Bufalin','DMSO'),
#                              pAdjustMethod = 'BH')
# B_Lycorine_vs_DMSO <- DESeq2::results(dds, contrast = c('treatment','Lycorine','DMSO'),
#                               pAdjustMethod = 'BH')
# C_Lycorine_vs_Bufalin <- DESeq2::results(dds, contrast = c('treatment','Lycorine','Bufalin'),
#                                  pAdjustMethod = 'BH')



##Annotation
# A_Bufalin_vs_DMSO$symbol <- mapIds(org.Rn.eg.db,
#                                    keys=row.names(A_Bufalin_vs_DMSO),
#                                    column="SYMBOL",
#                                    keytype="ENSEMBL",
#                                    multiVals="first")
# A_Bufalin_vs_DMSO$entrez <- mapIds(org.Rn.eg.db,
#                                    keys=row.names(A_Bufalin_vs_DMSO),
#                                    column="ENTREZID",
#                                    keytype="ENSEMBL",
#                                    multiVals="first")
# B_Lycorine_vs_DMSO$symbol <- mapIds(org.Rn.eg.db,
#                                     keys=row.names(B_Lycorine_vs_DMSO),
#                                     column="SYMBOL",
#                                     keytype="ENSEMBL",
#                                     multiVals="first")
# B_Lycorine_vs_DMSO$entrez <- mapIds(org.Rn.eg.db,
#                                     keys=row.names(B_Lycorine_vs_DMSO),
#                                     column="ENTREZID",
#                                     keytype="ENSEMBL",
#                                     multiVals="first")
# C_Lycorine_vs_Bufalin$symbol <- mapIds(org.Rn.eg.db,
#                                        keys=row.names(C_Lycorine_vs_Bufalin),
#                                        column="SYMBOL",
#                                        keytype="ENSEMBL",
#                                        multiVals="first")
# C_Lycorine_vs_Bufalin$entrez <- mapIds(org.Rn.eg.db,
#                                        keys=row.names(C_Lycorine_vs_Bufalin),
#                                        column="ENTREZID",
#                                        keytype="ENSEMBL",
#                                        multiVals="first")
#
#
## TODO INFO replaced 3.3.2020
# A_Bufalin_vs_DMSO$symbol <- tRomics::annotation(keys = A_Bufalin_vs_DMSO)
# A_Bufalin_vs_DMSO$entrez <- tRomics::annotation(keys = A_Bufalin_vs_DMSO)
# B_Lycorine_vs_DMSO$symbol <- tRomics::annotation(keys = B_Lycorine_vs_DMSO)
# B_Lycorine_vs_DMSO$entrez <- tRomics::annotation(keys = B_Lycorine_vs_DMSO)
# C_Lycorine_vs_Bufalin$symbol <- tRomics::annotation(keys = C_Lycorine_vs_Bufalin)
# C_Lycorine_vs_Bufalin$entrez <- tRomics::annotation(keys = C_Lycorine_vs_Bufalin)


#write.csv(A_Bufalin_vs_DMSO, 'A_Bufalin_vs_DMSO.csv')
#write.csv(B_Lycorine_vs_DMSO, 'B_Lycorine_vs_DMSO.csv')
#write.csv(C_Lycorine_vs_Bufalin, 'C_Lycorine_vs_Bufalin.csv')

##heatmap
## TODO INFO replaced 3.3.2020
# topVarianceGenes <- head(order(rowVars(assay(rld)), decreasing=T),1000)
# matrix <- assay(rld)[topVarianceGenes,]
# matrix <- matrix - rowMeans(matrix)
#
# annotation_data <- as.data.frame(colData(rld))
# pheatmap(matrix, scale = 'row',show_rownames = F, color = bluered(100), fontsize = 8, annotation_col = coldata, cluster_cols = F)
plot_heatmap(
  data = rld,
  filename = "plots/heatmap.png",
  ngenes = 1000
)

#volcano plot
## TODO INFO replaced 30.03.2020
#tiff("Volcano.tiff", width = 800, height = 800, units = 'px')

# B_Lycorine_vs_DMSO_df <- as.data.frame(B_Lycorine_vs_DMSO)
# B_Lycorine_vs_DMSO_df <- B_Lycorine_vs_DMSO_df %>% filter(!is.na(padj))
# EnhancedVolcano(B_Lycorine_vs_DMSO_df,
#                 title = 'Lycorine vs. DMSO',
#                 lab = as.character(row.names(B_Lycorine_vs_DMSO_df)),
#                 selectLab = '',
#                 subtitle = '',
#                 x = 'log2FoldChange',
#                 y = 'padj',
#                 legend=c('not significant','Log (base 2) fold-change','FDR',
#                          'FDR & Log (base 2) fold-change'),
#                 pCutoff = 0.05,
#                 FCcutoff = 1,
#                 transcriptPointSize = 3.0,
#                 colAlpha = 0.5,
#                 xlim = c(-6,6),
#                 ylim = c(0,100),
#                 ylab = bquote(~-Log[10]~FDR))
# #dev.off()
#
# C_Lycorine_vs_Bufalin_df <- as.data.frame(C_Lycorine_vs_Bufalin)
# C_Lycorine_vs_Bufalin_df <- C_Lycorine_vs_Bufalin_df %>% filter(!is.na(padj))
# EnhancedVolcano::EnhancedVolcano(C_Lycorine_vs_Bufalin_df,
#                 title = 'Lycorine vs. Bufalin',
#                 lab = as.character(row.names(C_Lycorine_vs_Bufalin_df)),
#                 selectLab = '',
#                 subtitle = '',
#                 x = 'log2FoldChange',
#                 y = 'padj',
#                 legend=c('not significant','Log (base 2) fold-change','FDR',
#                          'FDR & Log (base 2) fold-change'),
#                 pCutoff = 0.05,
#                 FCcutoff = 1,
#                 transcriptPointSize = 3.0,
#                 colAlpha = 0.5,
#                 xlim = c(-10,10),
#                 ylim = c(0,100),
#                 ylab = bquote(~-Log[10]~FDR))

plot_volcano  <- (results_object = results_object,
  title = "Volcano Plot")

tRomics::plotVolcano(results = rv[["treatment_lycorine_dmso"]], title = 'Lycorine vs DMSO')
tRomics::plotVolcano(results = C_Lycorine_vs_Bufalin, title = 'LYcorine vs Bufalin')

A_Bufalin_vs_DMSO_sig <- as.data.frame(A_Bufalin_vs_DMSO) %>% filter(padj < 0.05 & abs(log2FoldChange) >1)
B_Lycorine_vs_DMSO_sig <- as.data.frame(B_Lycorine_vs_DMSO) %>% filter(padj < 0.05 & abs(log2FoldChange) >1)
C_Lycorine_vs_Bufalin_sig <- as.data.frame(C_Lycorine_vs_Bufalin) %>% filter(padj < 0.05 & abs(log2FoldChange) >1)

