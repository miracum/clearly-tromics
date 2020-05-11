launch_app(
  dir = "/home/user/development/Rpackages/sigident/tromics/"
)



counttable <- "inst/example_data/E-MTAB-7897_count_data.csv"
metadata <- "inst/example_data/E-MTAB-7897_metadata.csv"
design <- "~ 0 + disease"

countdata <- utils::read.csv(counttable, header = T, row.names = 1)
metadata <- utils::read.csv(metadata, header = T, row.names = 1)

countdata <- countdata[, -1]

data <- tRomics::data_input(
  counttable = countdata,
  metadata = metadata,
  design = design
)



# debugging mi_rna data
design <- "~ 0 + treatment"
dds <- tRomics::data_input(
  counttable = tRomics::mirna_countdata,
  metadata = tRomics::mirna_metadata,
  design = design
)

rld <- tRomics::log_trans(dds)

grouping_variable <- "treatment"

groups <- as.character(unique(SummarizedExperiment::colData(dds)[[grouping_variable]]))
contrast_list <- gtools::permutations(n = length(groups), r = 2, v = groups)
colnames(contrast_list) <- c("compare", "control")

rv <- list()

dds <- DESeq2::DESeq(dds)

orgdb <- "org.Rn.eg.db"
require(orgdb, character.only = TRUE)

contr <- 1

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

AnnotationDbi::mapIds(
  x = eval(parse(text = orgdb)),
  keys = keys,
  column = column,
  keytype = keytype,
  multiVals = "first"
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
