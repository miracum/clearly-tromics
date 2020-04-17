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
