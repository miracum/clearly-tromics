library(DESeq2)
library(gdata)
library(ggplot2)
library(dplyr)
library(AnnotationDbi)
library(org.Hs.eg.db)
library(openxlsx)


#Input of count matrix
counttable <- "D:/Erlangen/Mougiakakos_Group/Stoll_CD137_Monocyten/new_analysis/raw_data/raw_counts.csv"
metadata <- "D:/Erlangen/Mougiakakos_Group/Stoll_CD137_Monocyten/new_analysis/raw_data/metadata.csv"

countdata <- as.matrix(read.csv(counttable, row.names = 1))

coldata <- read.csv(metadata, row.names = 1)
coldata <- coldata[,c('condition', 'donor')]
coldata$donor <- as.factor(coldata$donor)

##Checking annotation
all(rownames(coldata) %in% colnames(countdata))
all(rownames(coldata) == colnames(countdata))

##extracting donor K218
countdata <- countdata[,-c(3,4)]
coldata <- coldata[-c(3,4),]

##Creating DESeq2dataset object

dds <- DESeqDataSetFromMatrix(countData = countdata,
                              colData = coldata,
                              design = ~ 0 + condition + donor)

#Pre-filtering
nrow(dds)
keep <- rowSums(counts(dds)) >= 10
dds <- dds[keep,]
nrow(dds)

#rlog transformation
rld <- rlog(dds, blind = F)
head(assay(rld), 3)

#PCA
pcaData <- plotPCA(rld, intgroup = c('condition','donor'), returnData = T)
pcaData

png(file="PCA.png")
percentVar <- round(100*attr(pcaData, 'percentVar'))
ggplot(pcaData, aes(x=PC1, y=PC2, color=condition, shape=donor)) + 
  geom_point(size=3) + 
  xlab(paste0('PC1: ', percentVar[1], '% variance')) +
  ylab(paste0('PC2: ', percentVar[2], '% variance')) +
  coord_fixed()
dev.off()

#MDS
sampleDists <- dist(t(assay(rld)))
sampleDistMatrix <- as.matrix( sampleDists )
mds <- as.data.frame(colData(rld))  %>%
  cbind(cmdscale(sampleDistMatrix))
png(file="MDS.png")
ggplot(mds, aes(x = `1`, y = `2`, color = condition, shape = donor)) +
  geom_point(size = 3) + 
  coord_fixed()
dev.off()

#Differential expression analysis
dds <- DESeq(dds)
res_CD137_highvslow <- results(dds, contrast = c('condition','CD137_high','CD137_low'),
                          pAdjustMethod = 'BH')



##Annotation
res_CD137_highvslow$symbol <- mapIds(org.Hs.eg.db,
                                keys=row.names(res_CD137_highvslow),
                                column="SYMBOL",
                                keytype="ENSEMBL",
                                multiVals="first")
res_CD137_highvslow$entrez <- mapIds(org.Hs.eg.db,
                                keys=row.names(res_CD137_highvslow),
                                column="ENTREZID",
                                keytype="ENSEMBL",
                                multiVals="first")

write.csv(res_CD137_highvslow, file = "CD137highvslow.csv")
res_CD137_highvslow <- as.data.frame(res_CD137_highvslow)
write.xlsx(res_CD137_highvslow, file = "CD137highvslow_no218.xlsx", row.names = T)

