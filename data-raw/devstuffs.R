packagename <- "tRomics"

# remove existing description object
unlink("DESCRIPTION")
# Create a new description object
my_desc <- desc::description$new("!new")
# Set your package name
my_desc$set("Package", packagename)
# Set author names
my_desc$set_authors(c(
  person("Maximilian", "Fuchs", email = "maximilian.fuchs@fau.de", role = c('cre', 'aut')),
  person("Lorenz A.", "Kapsner", role = c('aut'),
         email = "lorenz.kapsner@uk-erlangen.de",
         comment = c(ORCID = "0000-0003-1866-860X")),
  person("Meik", "Kunz", role = "ctb")
))
# Remove some author fields
my_desc$del("Maintainer")
# Set the version
my_desc$set_version("0.0.2.9003")
# The title of your package
my_desc$set(Title = "GUI for transcriptome profiling analysis")
# The description of your package
my_desc$set(Description = "GUI for integrative in silico analysis for deciphering global transcriptome profiling data.")
# The description of your package
my_desc$set("Date" = as.character(Sys.Date()))
# The urls
my_desc$set("URL", "https://github.com/miracum/clearly-tromics")
my_desc$set("BugReports", "https://github.com/miracum/clearly-tromics/issues")
# License
my_desc$set("License", "GPL-3")

# BioConductor stuff
my_desc$set("biocViews" = "")


# Save everyting
my_desc$write(file = "DESCRIPTION")

# License
usethis::use_gpl3_license(name="Universitätsklinikum Erlangen")


# add Imports and Depends
# Listing a package in either Depends or Imports ensures that it’s installed when needed
# Imports just loads the package, Depends attaches it
# Loading will load code, data and any DLLs; register S3 and S4 methods; and run the .onLoad() function.
##      After loading, the package is available in memory, but because it’s not in the search path,
##      you won’t be able to access its components without using ::.
##      Confusingly, :: will also load a package automatically if it isn’t already loaded.
##      It’s rare to load a package explicitly, but you can do so with requireNamespace() or loadNamespace().
# Attaching puts the package in the search path. You can’t attach a package without first loading it,
##      so both library() or require() load then attach the package.
##      You can see the currently attached packages with search().

# Depends
usethis::use_package("R", min_version = "2.10", type = "Depends")

# Imports (CRAN packages)
usethis::use_package("shiny", type="Imports")
usethis::use_package("shinydashboard", type="Imports")
usethis::use_package("shinyjs", type="Imports")
usethis::use_package("utils", type="Imports")
usethis::use_package("DT", type="Imports")
usethis::use_package("data.table", type="Imports")
usethis::use_package("DESeq2", type="Imports")
#usethis::use_package("gdata", type="Imports")
usethis::use_package("gplots", type="Imports")
usethis::use_package("gtools", type="Imports")
usethis::use_package("ggplot2", type="Imports")
#usethis::use_package("dplyr", type="Imports")
usethis::use_package("AnnotationDbi", type="Imports")
usethis::use_package("pheatmap", type="Imports")
usethis::use_package("EnhancedVolcano", type="Imports")
usethis::use_package("matrixStats", type="Imports")
usethis::use_package("SummarizedExperiment", type="Imports")
usethis::use_package("BiocManager", type="Imports")

# Suggests
usethis::use_package("testthat", type="Suggests")
usethis::use_package("processx", type="Suggests")
usethis::use_package("lintr", type="Suggests")


# buildignore
usethis::use_build_ignore(".gitlab-ci.yml")
usethis::use_build_ignore("data-raw")
usethis::use_build_ignore(".vscode")
usethis::use_build_ignore("/plots/")
usethis::use_build_ignore(".vscode")
usethis::use_build_ignore(".lintr")

# gitignore
usethis::use_git_ignore("/*")
usethis::use_git_ignore("/*/")
usethis::use_git_ignore("*.log")
usethis::use_git_ignore("!/.gitignore")
usethis::use_git_ignore("!/.Rbuildignore")
usethis::use_git_ignore("!/.gitlab-ci.yml")
usethis::use_git_ignore("!/data-raw/")
usethis::use_git_ignore("!/DESCRIPTION")
usethis::use_git_ignore("!/inst/")
usethis::use_git_ignore("!/inst/example_data/count_data.csv")
usethis::use_git_ignore("!/inst/example_data/metadata.csv")
usethis::use_git_ignore("!/LICENSE.md")
usethis::use_git_ignore("!/man/")
usethis::use_git_ignore("!NAMESPACE")
usethis::use_git_ignore("!/R/")
usethis::use_git_ignore("!/ci/")
usethis::use_git_ignore("!/R/")
usethis::use_git_ignore("!/data/countdata.rda")
usethis::use_git_ignore("!/data/metadata.rda")
usethis::use_git_ignore("!/vignettes/")
usethis::use_git_ignore("!/vignettes/*.Rmd")
usethis::use_git_ignore("!/README.md")
usethis::use_git_ignore("!/tests/")
usethis::use_git_ignore("/.Rhistory")
usethis::use_git_ignore("!/*.Rproj")
usethis::use_git_ignore("/.Rproj*")
usethis::use_git_ignore("/.RData")
usethis::use_git_ignore("/.vscode")
usethis::use_git_ignore("/plots/")
usethis::use_git_ignore("!/.lintr")

# code coverage
#covr::package_coverage()

# lint package
#lintr::lint_package()

# use data

counttable <- "inst/example_data/count_data.csv"
metadata <- "inst/example_data/metadata.csv"
design <- "~ 0 + treatment"

countdata <- utils::read.csv(counttable, header = T, row.names = 1,
                             stringsAsFactors = TRUE)
metadata <- utils::read.csv(metadata, header = T, row.names = 1,
                            stringsAsFactors = TRUE)
design <- "~ 0 + treatment"

example_data <- tRomics::data_input(
  counttable = countdata,
  metadata = metadata,
  design = design
)

usethis::use_data(countdata, internal = F, overwrite = T)
usethis::use_data(metadata, internal = F, overwrite = T)


counttable <- "inst/example_data/miRNA_counts.csv"
metadata <- "inst/example_data/miRNA_metadata.csv"
design <- "~ 0 + treatment"

mirna_countdata <- utils::read.csv(counttable, header = T, row.names = 1,
                                   stringsAsFactors = TRUE)
mirna_metadata <- utils::read.csv(metadata, header = T, row.names = 1,
                                  stringsAsFactors = TRUE)
design <- "~ 0 + treatment"

mirna_example_data <- tRomics::data_input(
  counttable = mirna_countdata,
  metadata = mirna_metadata,
  design = design
)

usethis::use_data(mirna_countdata, internal = F, overwrite = T)
usethis::use_data(mirna_metadata, internal = F, overwrite = T)
