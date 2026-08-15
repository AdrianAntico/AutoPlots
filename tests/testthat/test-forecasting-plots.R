test_that("forecast views preserve forecast, interval, and group evidence", {
  data <- data.table::data.table(
    forecast_date = as.Date("2026-01-01") + 0:3,
    entity = rep(c("a", "b"), each = 2L),
    actual = c(1, 2, 3, 4), forecast = c(1.1, 2.1, 2.9, 4.1),
    lower_interval = c(.5, 1.5, 2.5, 3.5),
    upper_interval = c(1.5, 2.5, 3.5, 4.5))
  expect_s3_class(Plot.Forecast(data, GroupVar = "entity"), "htmlwidget")
  expect_error(Plot.Forecast(data[, forecast := as.character(forecast)]),
    "must be numeric")
})

test_that("forecast backtest and reconciliation views are reusable", {
  metrics <- data.table::data.table(horizon = 1:3, metric = "mae",
    value = c(1, 1.5, 2))
  expect_s3_class(Plot.ForecastBacktest(metrics), "htmlwidget")
  reconciled <- data.table::data.table(
    forecast_date = as.Date("2026-01-01") + 0:2,
    base_forecast = c(4, 5, 6), reconciled_forecast = c(4.5, 5.2, 5.8))
  expect_s3_class(Plot.ForecastReconciliation(reconciled), "htmlwidget")
})

test_that("Wave 1B views preserve calibration, combination, and decomposition", {
  calibration <- data.table::data.table(nominal_coverage = c(.8, .9),
    empirical_coverage = c(.78, .88))
  expect_s3_class(Plot.ForecastCalibration(calibration), "htmlwidget")
  components <- data.table::CJ(component_id = c("theta", "arima",
    "combination"), horizon = 1:3)
  components[, forecast := seq_len(.N)]
  expect_s3_class(Plot.ForecastCombination(components), "htmlwidget")
  decomposition <- data.table::data.table(observation = 1:6,
    Trend = as.numeric(1:6), Seasonal12 = rep(c(-1, 1), 3), Remainder = 0)
  expect_s3_class(Plot.ForecastDecomposition(decomposition), "htmlwidget")
})
