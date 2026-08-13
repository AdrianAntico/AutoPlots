#' Plot an unsupervised embedding
#'
#' Reusable visualization for PCA scores, manifold embeddings, and other
#' two-dimensional fitted representations. Model fitting remains the caller's
#' responsibility.
#'
#' @param dt Tabular embedding output.
#' @param XVar,YVar Numeric embedding columns.
#' @param ClusterVar Optional cluster/segment column.
#' @param Title Plot title.
#' @param EchartsTheme AutoPlots theme.
#' @param ... Additional arguments passed to [Plot.Scatter()].
#' @return An `echarts4r` htmlwidget.
#' @export
Plot.ClusterEmbedding <- function(dt, XVar, YVar, ClusterVar = NULL,
    Title = "Cluster embedding", EchartsTheme = "macarons", ...) {
  data <- data.table::as.data.table(data.table::copy(dt))
  autoplot_require_columns(data, c(XVar, YVar, ClusterVar))
  if (!is.numeric(data[[XVar]]) || !is.numeric(data[[YVar]]))
    stop("Embedding axes must be numeric.", call. = FALSE)
  Plot.Scatter(dt = data, XVar = XVar, YVar = YVar, GroupVar = ClusterVar,
    Title = Title, EchartsTheme = EchartsTheme, ...)
}

#' Plot component explained variance
#'
#' @param explained_variance Numeric explained-variance proportions.
#' @param cumulative Optional cumulative proportions.
#' @param Title Plot title.
#' @param EchartsTheme AutoPlots theme.
#' @param ... Additional arguments passed to [Plot.Bar()].
#' @return An `echarts4r` htmlwidget.
#' @export
Plot.ExplainedVariance <- function(explained_variance, cumulative = NULL,
    Title = "Explained variance", EchartsTheme = "macarons", ...) {
  values <- as.numeric(explained_variance)
  if (!length(values) || any(!is.finite(values)))
    stop("Explained variance must contain finite numeric values.", call. = FALSE)
  data <- data.table::data.table(
    component = paste0("Component ", seq_along(values)),
    explained_variance = values
  )
  if (!is.null(cumulative)) data[, cumulative := as.numeric(cumulative)]
  Plot.Bar(dt = data, PreAgg = TRUE, XVar = "component",
    YVar = "explained_variance", Title = Title, EchartsTheme = EchartsTheme, ...)
}

#' Plot cluster sizes
#'
#' @param cluster_sizes Named or unnamed cluster counts.
#' @param Title Plot title.
#' @param EchartsTheme AutoPlots theme.
#' @param ... Additional arguments passed to [Plot.Bar()].
#' @return An `echarts4r` htmlwidget.
#' @export
Plot.ClusterSizes <- function(cluster_sizes, Title = "Cluster sizes",
    EchartsTheme = "macarons", ...) {
  sizes <- as.numeric(cluster_sizes)
  if (!length(sizes) || any(!is.finite(sizes)) || any(sizes < 0))
    stop("Cluster sizes must contain finite non-negative values.", call. = FALSE)
  labels <- names(cluster_sizes)
  if (is.null(labels) || any(!nzchar(labels))) labels <- as.character(seq_along(sizes))
  data <- data.table::data.table(cluster = labels, observations = sizes)
  Plot.Bar(dt = data, PreAgg = TRUE, XVar = "cluster", YVar = "observations",
    Title = Title, EchartsTheme = EchartsTheme, ...)
}

#' Plot anomaly-score distribution
#'
#' @param scores Numeric anomaly scores.
#' @param flags Optional logical flagged-observation indicator.
#' @param Title Plot title.
#' @param EchartsTheme AutoPlots theme.
#' @param ... Additional arguments passed to [Plot.Histogram()].
#' @return An `echarts4r` htmlwidget.
#' @export
Plot.AnomalyScores <- function(scores, flags = NULL,
    Title = "Anomaly-score distribution", EchartsTheme = "macarons", ...) {
  values <- as.numeric(scores)
  if (!length(values) || any(!is.finite(values)))
    stop("Anomaly scores must contain finite numeric values.", call. = FALSE)
  data <- data.table::data.table(anomaly_score = values)
  group <- NULL
  if (!is.null(flags)) {
    if (length(flags) != length(values)) stop("Flags must align with scores.", call. = FALSE)
    data[, status := ifelse(as.logical(flags), "Flagged", "Not flagged")]
    group <- "status"
  }
  Plot.Histogram(dt = data, XVar = "anomaly_score", GroupVar = group,
    Title = Title, EchartsTheme = EchartsTheme, ...)
}

#' Plot ordered nearest-neighbor distances
#'
#' @param distances Numeric neighbor distances.
#' @param Title Plot title.
#' @param EchartsTheme AutoPlots theme.
#' @param ... Additional arguments passed to [Plot.Line()].
#' @return An `echarts4r` htmlwidget.
#' @export
Plot.NeighborDistances <- function(distances, Title = "Neighbor distances",
    EchartsTheme = "macarons", ...) {
  values <- sort(as.numeric(distances))
  if (!length(values) || any(!is.finite(values)))
    stop("Neighbor distances must contain finite numeric values.", call. = FALSE)
  data <- data.table::data.table(rank = seq_along(values), distance = values)
  Plot.Line(dt = data, XVar = "rank", YVar = "distance", Title = Title,
    EchartsTheme = EchartsTheme, ...)
}

autoplot_require_columns <- function(data, columns) {
  columns <- columns[!is.na(columns) & nzchar(columns)]
  missing <- setdiff(columns, names(data))
  if (length(missing)) stop("Missing required plot columns: ",
    paste(missing, collapse = ", "), call. = FALSE)
  invisible(TRUE)
}

#' Plot modeled-field missingness rates
#' @param missing_rates Named numeric proportions.
#' @param Title Plot title.
#' @param EchartsTheme AutoPlots theme.
#' @param ... Additional arguments passed to [Plot.Bar()].
#' @return An `echarts4r` htmlwidget.
#' @export
Plot.MissingnessRates <- function(missing_rates, Title = "Missingness by field",
    EchartsTheme = "macarons", ...) {
  values <- as.numeric(missing_rates)
  if (!length(values) || any(!is.finite(values)) || any(values < 0 | values > 1))
    stop("Missingness rates must be finite proportions.", call. = FALSE)
  labels <- names(missing_rates)
  if (is.null(labels) || any(!nzchar(labels))) labels <- paste0("Field ", seq_along(values))
  Plot.Bar(data.table::data.table(field = labels, missing_rate = values),
    PreAgg = TRUE, XVar = "field", YVar = "missing_rate", Title = Title,
    EchartsTheme = EchartsTheme, ...)
}

#' Plot a temporal signal with governed event markers
#' @param time,value Ordered time and numeric values.
#' @param events Optional positions marking motifs, discords, anomalies, or changes.
#' @param event_label Legend label for event markers.
#' @param Title Plot title.
#' @param EchartsTheme AutoPlots theme.
#' @param ... Additional arguments passed to [Plot.Line()].
#' @return An `echarts4r` htmlwidget.
#' @export
Plot.TemporalEvents <- function(time, value, events = integer(),
    event_label = "Detected event", Title = "Temporal diagnostics",
    EchartsTheme = "macarons", ...) {
  if (length(time) != length(value) || !length(value) || any(!is.finite(value)))
    stop("Time and finite values must align.", call. = FALSE)
  event <- rep("Signal", length(value))
  events <- unique(as.integer(events))
  events <- events[events >= 1L & events <= length(value)]
  event[events] <- event_label
  Plot.Line(data.table::data.table(time = time, value = as.numeric(value), event = event),
    XVar = "time", YVar = "value", GroupVar = "event", Title = Title,
    EchartsTheme = EchartsTheme, ...)
}
