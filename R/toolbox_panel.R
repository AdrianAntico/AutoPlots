#' Accessible chart controls outside the plotting area
#'
#' Adds named keyboard/touch controls for selection zoom, reset and PNG export.
#' More tools reveals the existing native ECharts toolbox; no analytical values
#' or series are changed. The chart resizes within its original widget bounds.
#' Native toolbox icons retain ECharts' own interaction/accessibility limits.
#' @param e An echarts4r widget with an existing toolbox.
#' @return The widget, with a presentation-only render hook.
#' @export
e_toolbox_panel <- function(e) {
  if (!inherits(e, "echarts4r")) stop("e must be an echarts4r widget.", call. = FALSE)
  script <- system.file("toolbox-panel.js", package = "AutoPlots")
  if (!nzchar(script)) stop("AutoPlots chart-control asset is missing.", call. = FALSE)
  htmlwidgets::onRender(e, paste(readLines(script, warn = FALSE), collapse = "\n"))
}
