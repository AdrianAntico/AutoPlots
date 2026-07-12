#' Run AutoPlots installed-package QA
#'
#' @description
#' Runs the stable installed-package QA contract for AutoPlots. This public
#' entry point validates representative rendering and layout contracts while
#' keeping implementation-specific QA helpers internal.
#'
#' @return A data.table with QA check rows and normalized status values.
#'
#' @examples
#' \dontrun{
#' qa_autoplots_package()
#' }
#'
#' @export
qa_autoplots_package <- function() {
  importance <- qa_importance_pareto()
  importance_row <- data.table::data.table(
    check = "importance_pareto",
    status = if (identical(importance$status, "PASS")) "success" else "error",
    message = paste("ImportancePareto series:", paste(importance$series, collapse = ", "))
  )

  data.table::rbindlist(list(
    importance_row,
    qa_resizable_display_plots()
  ), use.names = TRUE, fill = TRUE)
}
