library(Rsubread)

##Read alignment
#Index building
ref <- 'C:/Users/biokurs/Documents/Rsubread/reference/Homo_sapiens.GRCh38.dna.primary_assembly.fa.gz'
buildindex(basename = 'reference_index', reference = ref, gappedIndex = T)

#Read mapping
folder <- 'D:/fastq/'
readfiles <- c("NG-17663_K217_CD137_High_lib292072_6141_8_1.fastq.gz",
               "NG-17663_K217_CD137_Low_lib292068_6141_8_1.fastq.gz" ,
               "NG-17663_K218_CD137_High_lib292073_6141_8_1.fastq.gz",
               "NG-17663_K218_CD137_Low_lib292069_6141_8_1.fastq.gz" ,
               "NG-17663_K222_CD137_High_lib292074_6141_8_1.fastq.gz",
               "NG-17663_K222_CD137_Low_lib292070_6141_8_1.fastq.gz" ,
               "NG-17663_K226_CD137_High_lib292075_6141_8_1.fastq.gz",
               "NG-17663_K226_CD137_Low_lib292071_6141_8_1.fastq.gz" )

outputfiles <- c("NG-17663_K217_CD137_High_lib292072_6141_8_1.bam",
                 "NG-17663_K217_CD137_Low_lib292068_6141_8_1.bam" ,
                 "NG-17663_K218_CD137_High_lib292073_6141_8_1.bam",
                 "NG-17663_K218_CD137_Low_lib292069_6141_8_1.bam" ,
                 "NG-17663_K222_CD137_High_lib292074_6141_8_1.bam",
                 "NG-17663_K222_CD137_Low_lib292070_6141_8_1.bam" ,
                 "NG-17663_K226_CD137_High_lib292075_6141_8_1.bam",
                 "NG-17663_K226_CD137_Low_lib292071_6141_8_1.bam")

align(index = 'reference_index', readfile1 = readfiles[1], type = 'rna',
      output_file = outputfiles[1], nthreads = 4)

align(index = 'reference_index', readfile1 = readfiles[2], type = 'rna',
      output_file = outputfiles[2], nthreads = 4)

align(index = 'reference_index', readfile1 = readfiles[3], type = 'rna',
      output_file = outputfiles[3], nthreads = 4)

align(index = 'reference_index', readfile1 = readfiles[4], type = 'rna',
      output_file = outputfiles[4], nthreads = 4)

align(index = 'reference_index', readfile1 = readfiles[5], type = 'rna',
      output_file = outputfiles[5], nthreads = 4)

align(index = 'reference_index', readfile1 = readfiles[6], type = 'rna',
      output_file = outputfiles[6], nthreads = 4)

align(index = 'reference_index', readfile1 = readfiles[7], type = 'rna',
      output_file = outputfiles[7], nthreads = 4)

align(index = 'reference_index', readfile1 = readfiles[8], type = 'rna',
      output_file = outputfiles[8], nthreads = 4)

##Counting mapped reads for genomic features
fc_SE <- featureCounts(outputfiles, annot.ext = 'Homo_sapiens.GRCh38.98.chr.gtf.gz',
                       isGTFAnnotationFile = T, GTF.featureType = 'gene',
                       GTF.attrType = 'gene_id', nthreads = 4)

head(fc_SE$counts)
write.csv(fc_SE$counts, file = 'raw_counts.csv')

##Differential gene expression analysis

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

