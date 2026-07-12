# Prepare data for ImportancePareto.
.ap_prepare_importance_pareto_data <- function(data,
                                               XVar = NULL,
                                               YVar = NULL,
                                               TopN = 25,
                                               Sort = TRUE) {
  if (is.null(data)) stop("data is required.")

  dt <- data.table::as.data.table(data.table::copy(data))
  if (nrow(dt) == 0L) stop("data must contain at least one row.")

  if (is.null(XVar)) {
    feature_candidates <- intersect(
      c("Feature", "feature", "Variable", "variable", "VariableName", "variable_name", "Name", "name"),
      names(dt)
    )
    if (length(feature_candidates) > 0L) {
      XVar <- feature_candidates[[1L]]
    } else {
      x_candidates <- names(dt)[vapply(dt, function(x) is.character(x) || is.factor(x), logical(1))]
      if (length(x_candidates) == 0L) stop("XVar is required when no character or factor feature column can be inferred.")
      XVar <- x_candidates[[1L]]
    }
  }

  if (is.null(YVar)) {
    importance_candidates <- intersect(
      c("Importance", "importance", "Importances", "importances", "Gain", "gain", "Overall", "overall", "Value", "value"),
      names(dt)
    )
    numeric_importance_candidates <- importance_candidates[
      vapply(importance_candidates, function(x) is.numeric(dt[[x]]), logical(1))
    ]
    if (length(numeric_importance_candidates) > 0L) {
      YVar <- numeric_importance_candidates[[1L]]
    } else {
      y_candidates <- names(dt)[vapply(dt, is.numeric, logical(1))]
      if (length(y_candidates) == 0L) stop("YVar is required when no numeric importance column can be inferred.")
      YVar <- y_candidates[[1L]]
    }
  }

  if (!XVar %in% names(dt)) stop("XVar must exist in data.")
  if (!YVar %in% names(dt)) stop("YVar must exist in data.")
  if (!is.numeric(dt[[YVar]])) stop("YVar must be numeric.")

  dt <- dt[!is.na(get(XVar)) & is.finite(get(YVar))]
  if (nrow(dt) == 0L) stop("No finite importance values remain after removing missing values.")

  if (is.factor(dt[[XVar]])) dt[, (XVar) := as.character(get(XVar))]
  dt[, ".ap_feature" := as.character(get(XVar))]
  dt[, ".ap_importance" := as.numeric(get(YVar))]
  dt[, ".ap_contribution" := abs(get(".ap_importance"))]

  total_contribution <- sum(dt[[".ap_contribution"]], na.rm = TRUE)
  if (!is.finite(total_contribution) || total_contribution <= 0) {
    stop("Total absolute importance must be greater than zero.")
  }

  if (isTRUE(Sort)) {
    data.table::setorderv(dt, ".ap_contribution", order = -1L)
  }

  TopN <- as.integer(TopN)
  if (is.na(TopN) || TopN <= 0L) TopN <- nrow(dt)
  dt <- dt[seq_len(min(TopN, nrow(dt)))]

  dt[, ".ap_rank" := seq_len(.N)]
  dt[, ".ap_cumulative_percent" := cumsum(get(".ap_contribution")) / total_contribution * 100]

  out <- data.table::data.table(
    Feature = dt[[".ap_feature"]],
    Importance = dt[[".ap_importance"]],
    Contribution = dt[[".ap_contribution"]],
    `Cumulative Contribution` = dt[[".ap_cumulative_percent"]],
    Rank = dt[[".ap_rank"]]
  )

  list(
    data = out,
    x_var = XVar,
    y_var = YVar,
    total_rows = nrow(data),
    displayed_rows = nrow(out),
    total_contribution = total_contribution
  )
}

# Add a cumulative cutoff line to an ImportancePareto chart.
.ap_add_importance_pareto_cutoff <- function(e, Cutoff = 0.8) {
  if (is.null(Cutoff) || isFALSE(Cutoff)) return(e)
  if (!is.numeric(Cutoff) || length(Cutoff) != 1L || !is.finite(Cutoff)) {
    stop("Cutoff must be NULL or a single numeric value.")
  }

  cutoff_percent <- if (Cutoff <= 1) Cutoff * 100 else Cutoff
  if (cutoff_percent <= 0 || cutoff_percent > 100) {
    stop("Cutoff must be between 0 and 1 or between 0 and 100.")
  }

  echarts4r::e_mark_line(
    e = e,
    serie = "Cumulative Contribution",
    data = list(list(
      name = paste0(round(cutoff_percent, 1), "% cutoff"),
      yAxis = cutoff_percent
    )),
    title = paste0(round(cutoff_percent, 1), "% cutoff"),
    title_position = "end"
  )
}

#' Importance Pareto Plot
#'
#' @description
#' Create a composite variable-importance plot that combines ranked importance
#' bars with a cumulative contribution line. This is useful for understanding
#' both the top drivers and how quickly their combined contribution approaches
#' the total importance.
#'
#' @author Adrian Antico
#' @family Model Evaluation
#'
#' @param data Source data frame or data.table containing feature names and
#' importance values.
#' @param XVar Feature/name column. If NULL, common names such as `Feature` or
#' `Variable` are inferred when possible.
#' @param YVar Numeric importance column. If NULL, common names such as
#' `Importance`, `Importances`, or `Gain` are inferred when possible.
#' @param TopN Number of ranked features to display.
#' @param Sort If TRUE, sort by absolute importance descending before taking
#' `TopN`.
#' @param Cumulative If TRUE, add the cumulative contribution line.
#' @param CumulativeAxis Side for the cumulative percentage axis. Use `"right"`
#' or `"left"`.
#' @param Cutoff Optional cumulative reference line. Values between 0 and 1 are
#' treated as proportions; values between 1 and 100 are treated as percentages.
#' Use NULL to omit the cutoff line.
#' @param Theme AutoPlots/echarts theme name.
#' @param Title Plot title. If NULL, a default title is used.
#' @param Subtitle Plot subtitle.
#' @param Height Widget height.
#' @param Width Widget width.
#' @param ShowLabels If TRUE, show labels on the importance bars.
#' @param MouseScroll If TRUE, use inside data zoom. Otherwise, show external
#' data zoom controls.
#' @param Encoding Reserved for future consumer-aware information encoding.
#' Current supported values are `"human"`, `"llm"`, `"thumbnail"`,
#' `"presentation"`, `"executive"`, and `"developer"`.
#' @param Debug If TRUE, print intermediate diagnostics.
#' @param ... Reserved for future extensions.
#'
#' @return An echarts4r widget.
#' @export
#'
#' @examples
#' importance <- data.table::data.table(
#'   Feature = paste0("Feature_", seq_len(12)),
#'   Importance = sort(stats::runif(12, 1, 100), decreasing = TRUE)
#' )
#' ImportancePareto(importance, XVar = "Feature", YVar = "Importance", TopN = 10)
#'
#' catboost_style <- data.table::data.table(
#'   Feature = paste0("feature_", seq_len(20)),
#'   Importances = rev(seq_len(20)) ^ 1.2
#' )
#' ImportancePareto(catboost_style, TopN = 15, Cutoff = 0.9)
ImportancePareto <- function(data,
                             XVar = NULL,
                             YVar = NULL,
                             TopN = 25,
                             Sort = TRUE,
                             Cumulative = TRUE,
                             CumulativeAxis = "right",
                             Cutoff = 0.8,
                             Theme = "dark",
                             Title = NULL,
                             Subtitle = NULL,
                             Height = NULL,
                             Width = NULL,
                             ShowLabels = FALSE,
                             MouseScroll = FALSE,
                             Encoding = "human",
                             Debug = FALSE,
                             ...) {
  Encoding <- match.arg(
    Encoding,
    choices = c("human", "llm", "thumbnail", "presentation", "executive", "developer")
  )
  CumulativeAxis <- match.arg(CumulativeAxis, choices = c("right", "left"))

  prep <- .ap_prepare_importance_pareto_data(
    data = data,
    XVar = XVar,
    YVar = YVar,
    TopN = TopN,
    Sort = Sort
  )

  dt <- prep$data
  if (isTRUE(Debug)) print(dt)

  if (is.null(Title)) Title <- "Importance Pareto"
  if (is.null(Subtitle)) {
    Subtitle <- paste0(
      "Top ", prep$displayed_rows,
      " of ", prep$total_rows,
      " features; cumulative line is percent of total importance"
    )
  }

  x_label_rotate <- switch(
    Encoding,
    thumbnail = 0,
    llm = if (nrow(dt) > 12L) 90 else 45,
    if (nrow(dt) > 12L) 45 else 0
  )
  x_label_size <- switch(
    Encoding,
    thumbnail = 10,
    llm = if (nrow(dt) > 20L) 10 else 12,
    12
  )

  p1 <- echarts4r::e_charts_(
    dt,
    x = "Feature",
    darkMode = TRUE,
    emphasis = list(focus = "series"),
    dispose = TRUE,
    width = Width,
    height = Height
  )

  p1 <- e_bar_full(
    e = p1,
    serie = "Importance",
    label = ShowLabels
  )

  if (isTRUE(Cumulative)) {
    cumulative_y_index <- 1L
    p1 <- echarts4r::e_line_(
      e = p1,
      serie = "Cumulative Contribution",
      name = "Cumulative Contribution",
      y_index = cumulative_y_index,
      smooth = TRUE,
      showSymbol = TRUE,
      lineStyle = list(width = 3, type = "solid"),
      label = list(show = FALSE)
    )
  }

  p1 <- echarts4r::e_theme(e = p1, name = Theme)

  p1 <- e_grid_full(
    e = p1,
    grid.left = "7%",
    grid.right = if (isTRUE(Cumulative)) "10%" else "5%",
    grid.bottom = if (nrow(dt) > 12L || x_label_rotate > 0) "18%" else "10%",
    grid.containLabel = TRUE
  )

  p1 <- e_tooltip_full(
    e = p1,
    tooltip.show = TRUE,
    tooltip.trigger = "axis",
    tooltip.axisPointer.type = "cross"
  )

  p1 <- e_toolbox_full(
    e = p1,
    toolbox.show = TRUE,
    toolbox.feature.magicType.show = FALSE,
    toolbox.feature.dataZoom.show = TRUE,
    toolbox.feature.dataView.show = TRUE,
    toolbox.feature.restore.show = TRUE,
    toolbox.feature.saveAsImage.show = TRUE
  )

  p1 <- e_x_axis_full(
    e = p1,
    xAxis.title = prep$x_var,
    xAxis.axisLabel.rotate = x_label_rotate,
    xAxis.axisLabel.fontSize = x_label_size,
    xAxis.nameTextStyle.padding = 25
  )

  importance_position <- if (isTRUE(Cumulative) && CumulativeAxis == "left") "right" else "left"
  p1 <- e_y_axis_full(
    e = p1,
    index = 0,
    yAxis.title = prep$y_var,
    yAxis.min = 0,
    yAxis.position = importance_position,
    yAxis.axisLabel.fontSize = 12
  )

  if (isTRUE(Cumulative)) {
    p1 <- e_y_axis_full(
      e = p1,
      index = 1,
      yAxis.title = "Cumulative %",
      yAxis.min = 0,
      yAxis.max = 100,
      yAxis.position = CumulativeAxis,
      yAxis.axisLabel.fontSize = 12
    )

    p1 <- .ap_add_importance_pareto_cutoff(p1, Cutoff = Cutoff)
  }

  if (MouseScroll) {
    p1 <- echarts4r::e_datazoom(e = p1, type = "inside", x_index = 0)
  } else {
    p1 <- echarts4r::e_datazoom(e = p1, x_index = 0)
  }

  p1 <- e_title_full(
    e = p1,
    title.text = Title,
    title.subtext = Subtitle,
    title.left = "left"
  )

  p1 <- e_legend_full(
    e = p1,
    legend.show = TRUE,
    legend.type = "plain",
    legend.orient = "horizontal",
    legend.top = 28,
    legend.right = 10,
    legend.height = NULL
  )

  p1
}

# Smoke test for ImportancePareto.
qa_importance_pareto <- function() {
  fake_importance <- data.table::data.table(
    Feature = paste0("Feature_", seq_len(30)),
    Importances = sort(stats::runif(30, min = 1, max = 100), decreasing = TRUE)
  )

  plot <- ImportancePareto(
    data = fake_importance,
    TopN = 20,
    Cutoff = 0.8,
    Theme = "dark",
    Height = "500px",
    Width = "900px"
  )

  series_names <- vapply(plot$x$opts$series, function(x) {
    if (is.null(x$name)) NA_character_ else x$name
  }, character(1))
  list(
    status = if (inherits(plot, "echarts4r") &&
      "Importance" %in% series_names &&
      "Cumulative Contribution" %in% series_names) "PASS" else "FAIL",
    plot_class = class(plot),
    series = series_names,
    has_cutoff = !is.null(plot$x$opts$series[[which(series_names == "Cumulative Contribution")]]$markLine),
    displayed_rows = length(plot$x$data[[1]]$Feature)
  )
}
