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

```r
library(tRomics)
launch_app()
```

# More Infos

- about Shiny: https://www.rstudio.com/products/shiny/
- RStudio and Shiny are trademarks of RStudio, Inc.
