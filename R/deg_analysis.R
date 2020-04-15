#' @title deg_analysis
#'
#' @description Wrapper function for calculating DEGs and export of results
#'
#' @param data DESeq2 object
#' @param group_variable Category of comparison
#' @param compare_group attribute group in comparison
#' @param control_group group in comparison
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
    object = data,
    contrast = c(group_variable,
                 compare_group,
                 control_group),
    pAdjustMethod = "BH"
  )

  return(results_object)
}
