context("lints")

if (dir.exists("../../00_pkg_src")) {
  prefix <- "../../00_pkg_src/tRomics/"
} else if (dir.exists("../../R")) {
  prefix <- "../../"
} else if (dir.exists("./R")) {
  prefix <- "./"
}


test_that(
  desc = "test lints",
  code = {
    lintlist <- list(
      "R" = list(
        "launch_app.R" = NULL,
        "annotation.R" = NULL,
        "moduleDataimport.R" = NULL,
        "moduleDEG.R" = NULL,
        "moduleVisualization.R" = NULL,
        "plotting.R" = NULL
      ),
      "tests/testthat" = list(
        "test-startup.R" = NULL,
        "test-lints.R" = NULL
      ),
      "inst/application" = list(
        "server.R" = NULL,
        "ui.R" = NULL
      )
    )
    for (directory in names(lintlist)) {
      print(directory)
      for (fname in names(lintlist[[directory]])) {
        print(fname)
        #% print(list.files(prefix))

        # skip on covr
        skip_on_covr()

        lintr::expect_lint(
          file = paste0(
            prefix,
            directory,
            "/",
            fname
          ),
          checks = lintlist[[directory]][[fname]]
        )
      }
    }
  }
)
