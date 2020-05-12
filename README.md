# tRomics (!!! under development !!!)

<!-- badges: start -->
[![pipeline status](https://gitlab.miracum.org/clearly/tromics/badges/master/pipeline.svg)](https://gitlab.miracum.org/clearly/tromics/commits/master)
[![coverage report](https://gitlab.miracum.org/clearly/tromics/badges/master/coverage.svg)](https://gitlab.miracum.org/clearly/tromics/commits/master)
<!-- badges: end -->

`tRomics` is an R package that provides a shiny web application for integrative in silico analysis for deciphering global transcriptome profiling data.

# Installation

You can install *tRomics* with the following commands in R:

``` r
options('repos' = 'https://ftp.fau.de/cran/')
install.packages("devtools")
devtools::install_git("https://gitlab.miracum.org/clearly/tromics.git")
```
# Start shiny app

To start `tRomics`, just run the following command in R. A browser tab should open displaying the web application. Alternatively you can type the URL "localhost:3838/" in your browser.

To run the analyses, please click on the button *Load example data* on the application's dashboard.

```r
library(tRomics)
launch_app()
```

# Example files

The example files were created by Fabian Kreutzer and Jan Fiedler from EMTTS, MH Hannover. These example data are integrated into this shiny application and can there directly being loaded by clicking the button 'Load example data'.
Those data are also avialable via this R package:  

* mRNA:
  + Count data: [count_data.csv](inst/example_data/count_data.csv)
  + Meta data: [metadata.csv](inst/example_data/metadata.csv)
* miRNA:
  + Count data: [miRNA_counts.csv](inst/example_data/miRNA_counts.csv)
  + Meta data: [miRNA_metadata.csv](inst/example_data/miRNA_metadata.csv)

# Expected input file structure
* The app works based on two input files (Compare example file):
  - Countdata: A comma-separated text file that contains raw read counts. Each 
  line represents a gene   and the row names have to be Ensembl-IDs. Each column represents one         sample. 
  - Metadata: A comma-separated text file that contains information about the samples. Each row         contains the info for one sample and the rownames have to match the column names from the countdata   file. The columns can contain several information about the samples (e.g. treatment, donor, group     ...).

# File upload


# Visualization

# DEG analysis



# More Infos

- about CLEARLY: [https://www.transcanfp7.eu/index.php/abstract/clearly.html](https://www.transcanfp7.eu/index.php/abstract/clearly.html)
- about MIRACUM: [https://www.miracum.org/](https://www.miracum.org/)
- about the Medical Informatics Initiative: [https://www.medizininformatik-initiative.de/index.php/de](https://www.medizininformatik-initiative.de/index.php/de)
- about Shiny: https://www.rstudio.com/products/shiny/
- RStudio and Shiny are trademarks of RStudio, Inc.
