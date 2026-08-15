# Reusable forecasting views. Forecast fitting and governance remain owned by
# the caller; these functions only visualize explicit forecast evidence.

#' Plot governed history, forecasts, and interval bounds
#'
#' @param dt Tabular forecast evidence.
#' @param DateVar Ordered date/time column.
#' @param ActualVar Optional observed-value column.
#' @param ForecastVar Forecast column.
#' @param LowerVar,UpperVar Optional predictive interval-bound columns.
#' @param GroupVar Optional series/entity column.
#' @param Title Plot title.
#' @param EchartsTheme AutoPlots theme.
#' @param ... Additional arguments passed to [Plot.Line()].
#' @return An `echarts4r` htmlwidget.
#' @export
Plot.Forecast <- function(dt, DateVar = "forecast_date", ActualVar = "actual",
    ForecastVar = "forecast", LowerVar = "lower_interval",
    UpperVar = "upper_interval", GroupVar = NULL,
    Title = "History and forecast", EchartsTheme = "macarons", ...) {
  data <- data.table::as.data.table(data.table::copy(dt))
  required <- c(DateVar, ForecastVar, GroupVar)
  autoplot_require_columns(data, required)
  value_columns <- c(if (ActualVar %in% names(data)) ActualVar,
    ForecastVar, if (LowerVar %in% names(data)) LowerVar,
    if (UpperVar %in% names(data)) UpperVar)
  if (any(!vapply(data[, ..value_columns], is.numeric, logical(1))))
    stop("Forecast values and interval bounds must be numeric.", call. = FALSE)
  identifiers <- c(DateVar, GroupVar)
  long <- data.table::melt(data, id.vars = identifiers,
    measure.vars = value_columns, variable.name = ".forecast_evidence",
    value.name = ".forecast_value", variable.factor = FALSE,
    na.rm = TRUE)
  if (!nrow(long)) stop("Forecast evidence contains no plottable values.", call. = FALSE)
  long[, .forecast_series := if (is.null(GroupVar)) .forecast_evidence else
    paste(get(GroupVar), .forecast_evidence, sep = " / ")]
  Plot.Line(dt = long, XVar = DateVar, YVar = ".forecast_value",
    GroupVar = ".forecast_series", Title = Title,
    EchartsTheme = EchartsTheme, ...)
}

#' Plot rolling-origin forecast errors by horizon
#'
#' @param dt Rolling-origin metrics containing horizon and metric values.
#' @param HorizonVar Horizon column.
#' @param ValueVar Metric-value column.
#' @param MetricVar Optional metric-name column.
#' @param Title Plot title.
#' @param EchartsTheme AutoPlots theme.
#' @param ... Additional arguments passed to [Plot.Line()].
#' @return An `echarts4r` htmlwidget.
#' @export
Plot.ForecastBacktest <- function(dt, HorizonVar = "horizon",
    ValueVar = "value", MetricVar = "metric",
    Title = "Rolling-origin error by horizon", EchartsTheme = "macarons", ...) {
  data <- data.table::as.data.table(data.table::copy(dt))
  autoplot_require_columns(data, c(HorizonVar, ValueVar))
  if (!is.numeric(data[[HorizonVar]]) || !is.numeric(data[[ValueVar]]))
    stop("Backtest horizon and metric values must be numeric.", call. = FALSE)
  group <- if (MetricVar %in% names(data)) MetricVar else NULL
  Plot.Line(dt = data, XVar = HorizonVar, YVar = ValueVar,
    GroupVar = group, Title = Title, EchartsTheme = EchartsTheme, ...)
}

#' Compare base and reconciled forecasts
#'
#' @param dt Reconciliation evidence.
#' @param DateVar Ordered date/time column.
#' @param BaseVar Base forecast column.
#' @param ReconciledVar Reconciled forecast column.
#' @param GroupVar Optional hierarchy/entity column.
#' @param Title Plot title.
#' @param EchartsTheme AutoPlots theme.
#' @param ... Additional arguments passed to [Plot.Line()].
#' @return An `echarts4r` htmlwidget.
#' @export
Plot.ForecastReconciliation <- function(dt, DateVar = "forecast_date",
    BaseVar = "base_forecast", ReconciledVar = "reconciled_forecast",
    GroupVar = NULL, Title = "Base and reconciled forecast",
    EchartsTheme = "macarons", ...) {
  data <- data.table::as.data.table(data.table::copy(dt))
  autoplot_require_columns(data, c(DateVar, BaseVar, ReconciledVar, GroupVar))
  if (!is.numeric(data[[BaseVar]]) || !is.numeric(data[[ReconciledVar]]))
    stop("Base and reconciled forecasts must be numeric.", call. = FALSE)
  identifiers <- c(DateVar, GroupVar)
  long <- data.table::melt(data, id.vars = identifiers,
    measure.vars = c(BaseVar, ReconciledVar), variable.name = ".forecast_kind",
    value.name = ".forecast_value", variable.factor = FALSE)
  long[, .forecast_series := if (is.null(GroupVar)) .forecast_kind else
    paste(get(GroupVar), .forecast_kind, sep = " / ")]
  Plot.Line(dt = long, XVar = DateVar, YVar = ".forecast_value",
    GroupVar = ".forecast_series", Title = Title,
    EchartsTheme = EchartsTheme, ...)
}

#' Plot nominal and empirical forecast coverage
#'
#' @param dt Calibration evidence.
#' @param NominalVar,EmpiricalVar Nominal and empirical coverage columns.
#' @param HorizonVar Optional horizon column.
#' @param Title Plot title.
#' @param EchartsTheme AutoPlots theme.
#' @param ... Additional arguments passed to [Plot.Line()].
#' @return An `echarts4r` htmlwidget.
#' @export
Plot.ForecastCalibration <- function(dt, NominalVar = "nominal_coverage",
    EmpiricalVar = "empirical_coverage", HorizonVar = NULL,
    Title = "Forecast interval calibration", EchartsTheme = "macarons", ...) {
  data <- data.table::as.data.table(data.table::copy(dt))
  autoplot_require_columns(data, c(NominalVar, EmpiricalVar, HorizonVar))
  if (!is.numeric(data[[NominalVar]]) || !is.numeric(data[[EmpiricalVar]]))
    stop("Nominal and empirical coverage must be numeric.", call. = FALSE)
  data[, .calibration_axis := if (is.null(HorizonVar)) get(NominalVar) else
    get(HorizonVar)]
  long <- data.table::melt(data, id.vars = ".calibration_axis",
    measure.vars = c(NominalVar, EmpiricalVar), variable.name = ".coverage_kind",
    value.name = ".coverage", variable.factor = FALSE)
  Plot.Line(long, XVar = ".calibration_axis", YVar = ".coverage",
    GroupVar = ".coverage_kind", Title = Title, EchartsTheme = EchartsTheme,
    ...)
}

#' Plot component forecasts and their governed combination
#'
#' @param dt Long component/combination forecast evidence.
#' @param HorizonVar,ForecastVar,ComponentVar Column names.
#' @param Title Plot title.
#' @param EchartsTheme AutoPlots theme.
#' @param ... Additional arguments passed to [Plot.Line()].
#' @return An `echarts4r` htmlwidget.
#' @export
Plot.ForecastCombination <- function(dt, HorizonVar = "horizon",
    ForecastVar = "forecast", ComponentVar = "component_id",
    Title = "Forecast components and combination",
    EchartsTheme = "macarons", ...) {
  data <- data.table::as.data.table(data.table::copy(dt))
  autoplot_require_columns(data, c(HorizonVar, ForecastVar, ComponentVar))
  if (!is.numeric(data[[HorizonVar]]) || !is.numeric(data[[ForecastVar]]))
    stop("Combination horizon and forecast values must be numeric.",
      call. = FALSE)
  Plot.Line(data, XVar = HorizonVar, YVar = ForecastVar,
    GroupVar = ComponentVar, Title = Title, EchartsTheme = EchartsTheme, ...)
}

#' Plot MSTL decomposition components
#'
#' @param dt Wide MSTL component evidence.
#' @param ObservationVar Observation index column.
#' @param ComponentVars Optional component columns.
#' @param Title Plot title.
#' @param EchartsTheme AutoPlots theme.
#' @param ... Additional arguments passed to [Plot.Line()].
#' @return An `echarts4r` htmlwidget.
#' @export
Plot.ForecastDecomposition <- function(dt, ObservationVar = "observation",
    ComponentVars = NULL, Title = "MSTL decomposition",
    EchartsTheme = "macarons", ...) {
  data <- data.table::as.data.table(data.table::copy(dt))
  autoplot_require_columns(data, ObservationVar)
  components <- if (is.null(ComponentVars)) setdiff(names(data),
    ObservationVar) else ComponentVars
  autoplot_require_columns(data, components)
  if (!length(components) || any(!vapply(data[, ..components], is.numeric,
      logical(1)))) stop("Decomposition components must be numeric.",
        call. = FALSE)
  long <- data.table::melt(data, id.vars = ObservationVar,
    measure.vars = components, variable.name = ".component",
    value.name = ".value", variable.factor = FALSE)
  Plot.Line(long, XVar = ObservationVar, YVar = ".value",
    GroupVar = ".component", Title = Title, EchartsTheme = EchartsTheme, ...)
}
