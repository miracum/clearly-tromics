#' @title deg_analysis
#'
#' @description Function for calculating DEGs and export of results
#'
#' @param group_variable Category of comparison
#' @param compare_group attribute group in comparison
#' @param control group in comparison
#'
#' @inheritParams log_trans
#'
#' @export

deg_analysis <- function(
  data,
  group_variable,
  compare_group,
  control_group
) {

  results_object <- DESeq2::results(
    object = dds,
    contrast = c(group_variable,
                 compare_group,
                 control_group),
    pAdjustMethod = "BH"
  )

  return(results_object)
}
