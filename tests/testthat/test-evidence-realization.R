# Evidence-bound realization. Expected classes come from the frozen domain.
# The geometry oracle in helper-geometry-oracle.R does not call AutoPlots.

ap_domain_path <- Sys.getenv(
  "AUTOPLOTS_DOMAIN",
  unset = "C:/Users/Bizon/Documents/AnalyticsEcosystem-evaluations/AutoPlots-evidence-bound-1.0/DOMAIN.json")
ap_domain <- NULL
ap_cases <- new.env(parent = emptyenv())
ap_cache <- new.env(parent = emptyenv())

qualify_need <- function(ok, message) {
  if (isTRUE(ok)) return(invisible(TRUE))
  if (identical(Sys.getenv("AUTOPLOTS_QUALIFICATION"), "1")) stop(message, call. = FALSE)
  testthat::skip(message)
}

case_spec <- function(id) {
  hit <- Filter(function(case) identical(case$id, id), ap_domain$cases)
  if (length(hit) != 1L) stop("Domain case missing: ", id, call. = FALSE)
  hit[[1]]
}

record_case <- function(id, status, code, release = FALSE) {
  ap_cases[[id]] <- list(status = status, code = code, release = isTRUE(release))
}

expect_outcome <- function(id, status, code, release = FALSE, reason = NULL) {
  spec <- case_spec(id)
  expect_identical(status, spec$expected_status, info = id)
  if (is.null(spec$expected_code)) expect_null(code, info = id) else expect_identical(code, spec$expected_code, info = id)
  expect_false(isTRUE(release), info = id)
  if (!is.null(reason)) expect_true(is.character(reason) && length(reason) == 1L && nzchar(reason), info = id)
  record_case(id, status, code, release)
}

expect_plot <- function(id, result) {
  expect_outcome(id, result$status, result$code, result$release_authorized, result$reason)
  expect_identical(result$release_authorized, FALSE, info = id)
  if (identical(result$status, "RENDERED")) {
    expect_identical(result$human_judgment, "UNRESOLVED", info = id)
    expect_identical(result$scientific_qualification, "NOT_GRANTED", info = id)
    label_text <- vapply(result$labels, function(label) paste(label$title, label$subtitle), character(1))
    expect_false(any(grepl("approved|qualified|causal", label_text, ignore.case = TRUE)), info = id)
    claims <- vapply(result$trace$omissions, function(item) item$claim_id, character(1))
    expect_true("owner-limit" %in% claims, info = id)
    signature <- readBin(result$file, "raw", n = 8L)
    expect_identical(signature, oracle_png_signature, info = id)
  }
}

expect_identity_figure <- function(figure, values) {
  geometry <- figure$geometry
  expect_identical(geometry$value, values)
  expect_identical(geometry$y0, pmin(0, values))
  expect_identical(geometry$y1, pmax(0, values))
  limits <- lapply(values, oracle_identity_limits)
  expect_identical(geometry$ymin_axis, vapply(limits, function(x) unname(x[["ymin"]]), numeric(1)))
  expect_identical(geometry$ymax_axis, vapply(limits, function(x) unname(x[["ymax"]]), numeric(1)))
  expect_true(all(geometry$ymin_axis <= 0 & geometry$ymax_axis >= 0))
  expect_identical(figure$trace$scale, "identity")
  expect_false(isTRUE(figure$trace$estimand_changed))
  expect_true(all(geometry$zero_included))
  device <- oracle_device(limits, lapply(values, function(value) c(min(0, value), max(0, value))),
    width = figure$width, height = figure$height, res = figure$res)
  expect_equal(geometry$device_y0, vapply(device, function(x) x$y[[1]], numeric(1)), tolerance = 1e-6)
  expect_equal(geometry$device_y1, vapply(device, function(x) x$y[[2]], numeric(1)), tolerance = 1e-6)
  expect_equal(geometry$device_mark, geometry$device_y1, tolerance = 1e-6)
  invisible(device)
}

expect_bar_pixels <- function(figure, device) {
  if (!requireNamespace("png", quietly = TRUE)) {
    if (identical(Sys.getenv("AUTOPLOTS_QUALIFICATION"), "1")) stop("png package is required to read the rendered file", call. = FALSE)
    return(invisible())
  }
  img <- png::readPNG(figure$file)
  height <- nrow(img)
  target <- c(44, 127, 184) / 255
  distance_at <- function(x, y) {
    row <- height - y
    col <- x
    rows <- pmax(1, pmin(height, round(row) + -2:2))
    cols <- pmax(1, pmin(ncol(img), round(col) + -2:2))
    chips <- img[rows, cols, 1:3, drop = FALSE]
    min(apply(chips, c(1, 2), function(px) sum((px - target) ^ 2)))
  }
  pixel_y0 <- device[[1]]$y[[1]]
  pixel_y1 <- device[[1]]$y[[2]]
  mid_pixel <- pixel_y0 + 0.5 * (pixel_y1 - pixel_y0)
  center_x <- device[[1]]$x[[2]]
  outside_x <- device[[1]]$x[[1]] + (0.55 - 0.5) / 0.5 * (center_x - device[[1]]$x[[1]])
  expect_lt(distance_at(center_x, mid_pixel), 0.05)
  expect_gt(distance_at(outside_x, mid_pixel), 0.2)
  expect_true(file.size(figure$file) > 100)
}

synthetic_inputs <- function(quantities, uncertainty, scope = "synthetic labeled evidence") {
  ci_binding <- CommunicationIntelligence::ci_binding
  as_of <- "2026-10-09T00:00:00Z"
  source <- list(
    issuer = "synthetic-owner", kind = "source", id = "synthetic::numbers", revision = "1",
    content_binding = list(algorithm = "sha256", encoding = "utf8", status = "AVAILABLE", digest = ci_binding(quantities)),
    locator = "synthetic::numbers", authority_assertion_ref = "synthetic:1:numbers",
    access_assertion_ref = "test-fixture")
  semantics_common <- list(
    units = "synthetic_unit", denominator = "not_labeled", population = "synthetic",
    grain = "one row", horizon = "not_a_forecast", transformations = list("identity"),
    uncertainty = uncertainty, missingness = "none supplied", ordering = "evidence order")
  sem <- semantics_common
  sem$quantities <- lapply(quantities, function(q) list(
    id = q$id, value = q$value, display = q$display, unit = q$unit, locator = q$id))
  text <- paste(vapply(quantities, function(q) paste(q$id, q$display, q$unit), character(1)), collapse = "; ")
  claim <- list(
    id = "numbers",
    ref = list(issuer = "synthetic-owner", kind = "claim", id = "numbers", revision = "1",
      content_binding = source$content_binding, locator = "numbers",
      authority_assertion_ref = source$authority_assertion_ref, access_assertion_ref = source$access_assertion_ref),
    source_refs = list(source), text = text, authority_class = "owner_return_not_approval",
    projection = list(contract_ref = "communication_intelligence_owner_adapter_1", owner_locator = "synthetic::numbers",
      kind = "TYPED_OWNER_PROJECTION", owner_assertion_binding = "pending"),
    semantics = sem, material = TRUE, task_roles = list("synthetic"))
  claim$projection$owner_assertion_binding <- ci_binding(list(
    text = claim$text, authority_class = claim$authority_class, semantics = claim$semantics))
  limit_sem <- list(quantities = list(), units = "not_applicable", denominator = "not_applicable",
    population = "not_applicable", grain = "not_applicable", horizon = "not_applicable",
    transformations = list("not_applicable"), uncertainty = uncertainty, missingness = "not_applicable",
    ordering = "not_applicable")
  limit <- list(
    id = "owner-limit",
    ref = list(issuer = "synthetic-owner", kind = "claim", id = "owner-limit", revision = "1",
      content_binding = source$content_binding, locator = "owner-limit",
      authority_assertion_ref = source$authority_assertion_ref, access_assertion_ref = source$access_assertion_ref),
    source_refs = list(source), text = paste("Limit:", uncertainty),
    authority_class = "communication_limit_not_approval",
    projection = list(contract_ref = "communication_intelligence_owner_adapter_1", owner_locator = "synthetic::numbers",
      kind = "TYPED_OWNER_PROJECTION", owner_assertion_binding = "pending"),
    semantics = limit_sem, material = TRUE, task_roles = list("synthetic"))
  limit$projection$owner_assertion_binding <- ci_binding(list(
    text = limit$text, authority_class = limit$authority_class, semantics = limit$semantics))
  claims <- list(claim, limit)
  receipt <- function(id, subject) list(
    issuer = "synthetic-owner", receipt_id = id, revision = "1", evidence_binding = ci_binding(id), as_of = as_of,
    subject_ref = subject, authority = "RESOLVED", access = "GRANTED", claim_warrant = "AS_ASSERTED_BY_OWNER")
  manifest <- c(
    list(list(id = "source", kind = "source", ref = source, disposition = "ADMITTED",
      reason = "Synthetic fixture with explicit values", material = FALSE, admission_receipt = receipt("source", source))),
    lapply(claims, function(item) list(id = item$id, kind = "claim", ref = item$ref, disposition = "ADMITTED",
      reason = "Synthetic fixture with explicit values", material = TRUE, admission_receipt = receipt(item$id, item$ref))))
  relations <- list(list(id = "limit-numbers", kind = "qualifies", from = "owner-limit", to = "numbers",
    material = TRUE, authority_ref = source, required_exposure = "with_assertion"))
  inventory <- list(manifest = manifest, claims = claims, relations = relations, scope = scope, as_of = as_of)
  inventory_receipt <- list(
    issuer = "communication-workflow", receipt_id = "synthetic-inventory", revision = "1",
    evidence_binding = ci_binding("synthetic-inventory"), as_of = as_of,
    subject_binding = ci_binding(inventory), completeness = "COMPLETE_SUPPLIED_SET")
  CommunicationIntelligence::ci_bind_inputs(
    manifest, claims, relations, scope, as_of, id = "synthetic", revision = "1", inventory_receipt = inventory_receipt)
}

bind_figure <- function(inputs, task_id) {
  expect_identical(inputs$operation, "COMPLETED", info = paste(inputs$findings[[1]]$code, inputs$findings[[1]]$reason))
  bound <- ap_bind_pattern_request(inputs$value, task_id)
  expect_identical(bound$status, "REQUEST_READY", info = paste(bound$code, bound$reason))
  expect_false(isTRUE(bound$release_authorized))
  bound
}

test_that("frozen domain hash is unchanged", {
  qualify_need(file.exists(ap_domain_path), "frozen domain file is not present")
  qualify_need(requireNamespace("digest", quietly = TRUE) && requireNamespace("jsonlite", quietly = TRUE), "digest or jsonlite missing")
  freeze <- file.path(dirname(ap_domain_path), "DOMAIN.sha256")
  qualify_need(file.exists(freeze), "DOMAIN.sha256 was not written before execution")
  expect_identical(digest::digest(file = ap_domain_path, algo = "sha256"), trimws(readLines(freeze, warn = FALSE)))
  ap_domain <<- jsonlite::fromJSON(ap_domain_path, simplifyVector = FALSE)
  classes <- vapply(ap_domain$cases, function(case) case$class, character(1))
  expect_false(any(classes %in% c("WRONG_SUCCESS", "FALSE_REFUSAL", "GOVERNANCE_VIOLATION")))
})

test_that("previous public exports and help topics remain", {
  old <- readLines(testthat::test_path("exports-1.5.0.txt"))
  exports <- getNamespaceExports("AutoPlots")
  expect_length(setdiff(old, exports), 0L)
  expect_true(all(c("ap_bind_pattern_request", "ap_realize_request") %in% exports))
  aliases <- names(readRDS(system.file("help", "aliases.rds", package = "AutoPlots")))
  expect_length(setdiff(exports, aliases), 0L)
})

test_that("package source does not refit owner models", {
  src <- Sys.getenv("AUTOPLOTS_SOURCE", unset = "C:/Users/Bizon/Documents/GitHub/AutoPlots/R/evidence_realization.R")
  qualify_need(file.exists(src), "realization source is not available")
  text <- paste(readLines(src, warn = FALSE), collapse = "\n")
  expect_false(grepl("sli_fit|fi_bs|fi_ewma|meas_cronbach|rk_|aq_opt|sli_predict|sli_shap", text))
})

test_that("a null request is refused", {
  qualify_need(!is.null(ap_domain), "domain not loaded")
  result <- ap_realize_request(NULL, NULL)
  expect_plot("null_request", result)
})

test_that("live owner evidence renders without changing the estimand", {
  qualify_need(!is.null(ap_domain), "domain not loaded")
  pkgs <- c("CommunicationIntelligence", "FinancialIntelligence", "ForecastIntelligence",
    "MeasurementIntelligence", "RankingIntelligence", "DecisionIntelligence",
    "SupervisedLearningIntelligence", "ProbabilityEngine", "data.table")
  qualify_need(all(vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)), paste("missing", paste(pkgs, collapse = ", ")))
  versions <- c(FinancialIntelligence = "0.4.8", ForecastIntelligence = "0.5.23",
    MeasurementIntelligence = "0.6.2", RankingIntelligence = "0.6.1",
    DecisionIntelligence = "0.1.10", SupervisedLearningIntelligence = "0.6.0")
  for (package in names(versions)) expect_identical(as.character(utils::packageVersion(package)), versions[[package]], info = package)
  as_of <- "2026-10-09T00:00:00Z"
  returns <- c(0.01, -0.02, 0.015, 0.005)
  items <- MeasurementIntelligence::meas_example("scale")
  items$respondent_id <- as.character(items$respondent_id)
  ranked <- RankingIntelligence::rk_items(c("A", "B"), point = c(1.25, 3.5))
  dist_items <- list(
    list(id = "normal", pe_revision = ProbabilityEngine::pe_revision("normal", 0.4, 1), point = 0.4),
    list(id = "gamma", pe_revision = ProbabilityEngine::pe_revision("gamma", 2, 1), point = 2))
  table_unit <- function(id, outcome) {
    DecisionIntelligence::aq_opt_unit(id, DecisionIntelligence::aq_opt_mapping_table(data.table::data.table(
      control = 0:2, resource = 0:2, outcome = outcome)), 0, 2)
  }
  spec <- DecisionIntelligence::aq_opt_spec(list(table_unit("A", c(0, 10, 11)), table_unit("B", c(0, 3, 5))),
    start_boundary = "low_resource", traversal = "add_resource", step = list(kind = "control_grid", value = 1), exact_resource = 1)
  frontier <- DecisionIntelligence::aq_opt_build_frontier(spec)
  model_data <- data.table::data.table(x = 1:40, y = 2 * (1:40) + 1)
  linear <- SupervisedLearningIntelligence::sli_fit(
    SupervisedLearningIntelligence::sli_model_spec("regression", "y", "x", engine = "linear"), model_data)
  boosted <- SupervisedLearningIntelligence::sli_fit(
    SupervisedLearningIntelligence::sli_model_spec("regression", "y", "x", engine = "xgboost"), model_data)
  owners <- list(
    list(id = "owner_fi_bs", package = "FinancialIntelligence", export = "fi_bs",
      args = list(spot = 100, strike = 100, rate = 0.01, vol = 0.2, horizon = 1),
      quantities = list(list(id = "value", path = "value")), scope = "one closed-form value"),
    list(id = "owner_fi_ewma", package = "ForecastIntelligence", export = "fi_ewma",
      args = list(x = returns, lambda = 0.94, horizon = 1L),
      quantities = list(list(id = "forecast_variance", path = "forecast_variance/1"), list(id = "n", path = "n")),
      scope = "one EWMA variance forecast"),
    list(id = "owner_meas_cronbach", package = "MeasurementIntelligence", export = "meas_cronbach",
      args = list(items = items, item_cols = c("item_a", "item_b", "item_c", "item_d"), respondent_id = "respondent_id"),
      quantities = list(list(id = "alpha", path = "payload/alpha"), list(id = "n", path = "payload/n"), list(id = "k", path = "payload/k")),
      scope = "Cronbach alpha on the installed fictional scale"),
    list(id = "owner_rk_rank", package = "RankingIntelligence", export = "rk_rank",
      args = list(items = ranked), quantities = list(list(id = "score_1", path = "scores/1")), scope = "point order"),
    list(id = "owner_rk_p_best", package = "RankingIntelligence", export = "rk_p_best",
      args = list(items = dist_items, n_grid = 32L), quantities = list(list(id = "p_normal", path = "probabilities/normal")), scope = "P(best)"),
    list(id = "owner_rk_rpm", package = "RankingIntelligence", export = "rk_rpm",
      args = list(items = dist_items, n_grid = 32L), quantities = list(list(id = "p_normal", path = "probabilities/normal")), scope = "RPM"),
    list(id = "owner_aq_opt_select", package = "DecisionIntelligence", export = "aq_opt_select",
      args = list(frontier = frontier, exact_resource = 1, allow_interpolation = FALSE),
      quantities = list(list(id = "outcome", path = "selected/outcome"), list(id = "resource", path = "selected/resource")),
      scope = "owner selection"),
    list(id = "owner_sli_predict", package = "SupervisedLearningIntelligence", export = "sli_predict",
      args = list(object = linear, new_data = model_data[1:2]),
      quantities = list(list(id = "prediction_1", path = "prediction/1")), scope = "one model prediction"),
    list(id = "owner_sli_shap", package = "SupervisedLearningIntelligence", export = "sli_shap",
      args = list(fit = boosted, data = model_data[1:2]),
      quantities = list(list(id = "contribution_1", path = "contribution/1")), scope = "one native contribution"))
  for (owner in owners) {
    admitted <- CommunicationIntelligence::ci_admit_owner_call(
      owner$package, owner$export, owner$args, owner$quantities, versions[[owner$package]], as_of, owner$scope)
    bound <- bind_figure(admitted, owner$id)
    timed <- system.time(figure <- ap_realize_request(bound$request, bound$task))
    expect_plot(owner$id, figure)
    table <- CommunicationIntelligence::ci_quantity_table(admitted$value)
    expect_identical(figure$geometry$quantity_id, table$quantity_id, info = owner$id)
    expect_identical(figure$geometry$value, table$value, info = owner$id)
    expect_identical(figure$geometry$display, table$display, info = owner$id)
    device <- expect_identity_figure(figure, table$value)
    if (identical(owner$id, "owner_sli_shap")) {
      expect_lt(table$value[[1]], 0)
      expect_identical(figure$geometry$y0[[1]], table$value[[1]])
      expect_identical(figure$geometry$y1[[1]], 0)
    }
    if (identical(owner$id, "owner_sli_predict")) expect_false(identical(figure$geometry$value[[1]], 3))
    if (identical(owner$id, "owner_fi_bs")) {
      again <- ap_realize_request(bound$request, bound$task)
      expect_identical(again$geometry$value, figure$geometry$value)
      expect_equal(again$geometry$device_mark, figure$geometry$device_mark, tolerance = 1e-6)
      restored <- readRDS({path <- tempfile(fileext = ".rds"); saveRDS(figure$geometry, path); path})
      expect_identical(restored$value, figure$geometry$value)
      expect_identical(restored$ymin_axis, figure$geometry$ymin_axis)
      expect_bar_pixels(figure, device)
      ap_cache$fi_bs <- list(admitted = admitted, bound = bound, figure = figure, seconds = timed[["elapsed"]])
    }
    if (identical(owner$id, "owner_fi_ewma")) ap_cache$ewma <- list(bound = bound, table = table)
    if (identical(owner$id, "owner_sli_shap")) ap_cache$shap <- list(bound = bound)
    evidence <- Sys.getenv("AUTOPLOTS_EVIDENCE_DIR")
    if (nzchar(evidence) && identical(figure$status, "RENDERED")) {
      dir.create(file.path(evidence, "png"), showWarnings = FALSE, recursive = TRUE)
      file.copy(figure$file, file.path(evidence, "png", paste0(owner$id, ".png")), overwrite = TRUE)
    }
    ap_cases[[owner$id]]$seconds <- unname(timed[["elapsed"]])
    ap_cases[[owner$id]]$bytes <- figure$bytes
  }
})

test_that("authorized log10 keeps the original value", {
  qualify_need(!is.null(ap_cache$fi_bs), "fi_bs figure was not rendered")
  table <- CommunicationIntelligence::ci_quantity_table(ap_cache$fi_bs$admitted$value)
  value <- table$value[[1]]
  receipt <- list(
    issuer = "scale-authority", receipt_id = "log-fi-bs", revision = "1",
    evidence_binding = CommunicationIntelligence::ci_binding("log-fi-bs"), as_of = "2026-10-09T00:00:00Z",
    status = "MET",
    subject_binding = CommunicationIntelligence::ci_binding(list(
      scale = "log10", quantity_ids = list(table$quantity_id[[1]]), values = list(value))))
  figure <- ap_realize_request(ap_cache$fi_bs$bound$request, ap_cache$fi_bs$bound$task,
    encoding = list(geom = "point", scale = "log10", authority_receipt = receipt))
  expect_plot("log10_authorized_point", figure)
  expect_identical(figure$geometry$value[[1]], value)
  expect_true(isTRUE(figure$trace$estimand_changed))
  expect_identical(figure$trace$scale, "log10")
  expect_true(grepl(table$display[[1]], figure$labels[[1]]$title, fixed = TRUE))
  expect_true(grepl("log10 scale", figure$labels[[1]]$subtitle, fixed = TRUE))
  expect_false(grepl("log10\\(", figure$labels[[1]]$title))
  expect_identical(figure$trace$transformed_values[[1]]$log10, log10(value))
  expect_false(figure$geometry$zero_included[[1]])
  limits <- oracle_log_limits(value)
  expect_identical(c(figure$geometry$ymin_axis[[1]], figure$geometry$ymax_axis[[1]]), unname(limits))
  expect_false(figure$geometry$ymin_axis[[1]] <= 0 && figure$geometry$ymax_axis[[1]] >= 0)
  device <- oracle_device(list(unname(limits)), list(log10(value)), width = figure$width, height = figure$height, res = figure$res)
  expect_equal(figure$geometry$device_mark[[1]], device[[1]]$y[[1]], tolerance = 1e-6)
})

test_that("a synthetic confidence interval is not relabeled", {
  qualify_need(!is.null(ap_domain), "domain not loaded")
  qualify_need(requireNamespace("CommunicationIntelligence", quietly = TRUE), "CommunicationIntelligence missing")
  uncertainty <- "95 percent confidence interval"
  admitted <- synthetic_inputs(list(
    list(id = "band_lower", value = 1, display = "1", unit = "synthetic_unit"),
    list(id = "band_upper", value = 2, display = "2", unit = "synthetic_unit"),
    list(id = "band", value = 1.5, display = "1.5", unit = "synthetic_unit")), uncertainty)
  bound <- bind_figure(admitted, "synthetic-interval")
  figure <- ap_realize_request(bound$request, bound$task, encoding = list(geom = "interval", interval_kind = "confidence"))
  expect_plot("synthetic_confidence_interval", figure)
  expect_identical(figure$geometry$y0[[1]], 1)
  expect_identical(figure$geometry$y1[[1]], 2)
  expect_identical(figure$geometry$value[[1]], 1.5)
  expect_identical(figure$geometry$uncertainty[[1]], uncertainty)
  limits <- oracle_identity_limits(c(1, 2))
  expect_identical(c(figure$geometry$ymin_axis[[1]], figure$geometry$ymax_axis[[1]]), unname(limits))
  device <- oracle_device(list(unname(limits)), list(c(1, 2)), width = figure$width, height = figure$height, res = figure$res)
  expect_equal(figure$geometry$device_y0[[1]], device[[1]]$y[[1]], tolerance = 1e-6)
  expect_equal(figure$geometry$device_y1[[1]], device[[1]]$y[[2]], tolerance = 1e-6)
})

test_that("an interval does not present one endpoint as the estimand", {
  qualify_need(!is.null(ap_domain), "domain not loaded")
  qualify_need(requireNamespace("CommunicationIntelligence", quietly = TRUE), "CommunicationIntelligence missing")
  uncertainty <- "95 percent confidence interval"
  endpoint <- function(id, value, display, unit = "synthetic_unit") {
    list(id = id, value = value, display = display, unit = unit)
  }
  admitted <- synthetic_inputs(list(endpoint("band_lower", 1, "1"), endpoint("band_upper", 2, "2")), uncertainty)
  bound <- bind_figure(admitted, "interval-range")
  figure <- ap_realize_request(bound$request, bound$task, encoding = list(geom = "interval", interval_kind = "confidence"))
  expect_identical(figure$status, "RENDERED")
  expect_false(isTRUE(figure$release_authorized))
  expect_identical(figure$geometry$display[[1]], "1 to 2")
  expect_true(is.na(figure$geometry$value[[1]]))
  expect_identical(figure$geometry$y0[[1]], 1)
  expect_identical(figure$geometry$y1[[1]], 2)
  expect_identical(figure$labels[[1]]$title, "band = 1 to 2")
  expect_false(grepl("^band = 1$", figure$labels[[1]]$title))
  extra <- synthetic_inputs(list(
    endpoint("band_lower", 1, "1"), endpoint("band_upper", 2, "2"), endpoint("other", 9, "9")), uncertainty)
  extra_bound <- bind_figure(extra, "interval-extra")
  dropped <- ap_realize_request(extra_bound$request, extra_bound$task, encoding = list(geom = "interval"))
  expect_identical(dropped$status, "REFUSED")
  expect_identical(dropped$code, "OMISSION_NOT_AUTHORIZED")
  expect_false(isTRUE(dropped$release_authorized))
  mixed <- synthetic_inputs(list(
    endpoint("band_lower", 1, "1", "meter"), endpoint("band_upper", 2, "2", "second")), uncertainty)
  mixed_bound <- bind_figure(mixed, "interval-units")
  units <- ap_realize_request(mixed_bound$request, mixed_bound$task, encoding = list(geom = "interval"))
  expect_identical(units$status, "REFUSED")
  expect_identical(units$code, "CONFLICTING_ESTIMANDS")
})

test_that("misleading encodings and bad authority are refused", {
  qualify_need(!is.null(ap_cache$fi_bs) && !is.null(ap_cache$ewma) && !is.null(ap_cache$shap), "owner fixtures missing")
  request <- ap_cache$fi_bs$bound$request
  task <- ap_cache$fi_bs$bound$task
  refuse <- function(id, ...) {
    result <- ap_realize_request(request, task, ...)
    expect_plot(id, result)
  }
  refuse("percent_without_denominator", encoding = list(percent = TRUE))
  refuse("dual_axis", encoding = list(dual_axis = TRUE))
  refuse("aggregation", encoding = list(aggregate = "sum"))
  refuse("reversed_axis", encoding = list(reverse_y = TRUE))
  refuse("zero_baseline_removed", encoding = list(zero_baseline = FALSE))
  refuse("caller_ylim", encoding = list(ylim = c(1, 2)))
  refuse("caller_xlim", encoding = list(xlim = c(0, 1)))
  refuse("category_reorder", encoding = list(category_order = c("value")))
  refuse("unit_conversion", encoding = list(unit_conversion = "rescale"))
  refuse("authorize_release", encoding = list(authorize_release = TRUE))
  refuse("drop_ids", encoding = list(drop_ids = "value"))
  refuse("duplicate_timestamps", encoding = list(time_axis = TRUE))
  refuse("irregular_intervals", encoding = list(time_axis = TRUE))
  refuse("oversized_width", width = 2000L)
  refuse("unknown_encoding", encoding = list(color = "red"))
  refuse("normalize_scale", encoding = list(scale = "normalize"))
  refuse("log_column", encoding = list(scale = "log10"))
  expect_plot("log_nonpositive", ap_realize_request(ap_cache$shap$bound$request, ap_cache$shap$bound$task,
    encoding = list(geom = "point", scale = "log10")))
  refuse("log_without_receipt", encoding = list(geom = "point", scale = "log10"))
  refuse("forged_scale_authority", encoding = list(geom = "point", scale = "log10", authority_receipt = list(
    status = "MET", subject_binding = CommunicationIntelligence::ci_binding("forged-scale"))))
  refuse("missing_interval", encoding = list(geom = "interval"))
  shared <- ap_realize_request(ap_cache$ewma$bound$request, ap_cache$ewma$bound$task, encoding = list(facet = "shared"))
  expect_plot("shared_facet", shared)
  tampered <- request
  tampered$record_binding <- NULL
  tampered$payload$status <- "FORGED"
  tampered$record_binding <- CommunicationIntelligence::ci_binding(tampered)
  expect_plot("tampered_rehashed_status", ap_realize_request(tampered, task))
  stripped <- request
  admission <- stripped$payload$runtime$host_admission
  runtime <- stripped$payload$runtime
  runtime$host_admission <- NULL
  runtime$visual_candidate <- list(id = "not-a-candidate")
  admission$subject_binding <- CommunicationIntelligence::ci_binding(runtime)
  runtime$host_admission <- admission
  stripped$payload$runtime <- runtime
  stripped$record_binding <- NULL
  stripped$record_binding <- CommunicationIntelligence::ci_binding(stripped)
  expect_plot("missing_visual_admission", ap_realize_request(stripped, task))
  missing <- request
  missing$payload$composition$payload$elements[[1]]$payload$quantities[[1]]$value <- NA_real_
  expect_plot("missing_value_record", ap_realize_request(missing, task))
})

test_that("text realization is not a plot and a stale owner is not drawn", {
  qualify_need(!is.null(ap_cache$fi_bs), "fi_bs admission missing")
  opened <- CommunicationIntelligence::ci_open_communication(ap_cache$fi_bs$admitted$value)
  expect_identical(opened$operation, "COMPLETED")
  name <- "FinancialIntelligence::fi_bs"
  observations <- c(
    inert_import = CommunicationIntelligence::ci_binding(name),
    complete_path_inventory = CommunicationIntelligence::ci_binding(name),
    effect_confinement = CommunicationIntelligence::ci_binding(name),
    literal_preservation = CommunicationIntelligence::ci_binding(name),
    runtime_binding = CommunicationIntelligence::ci_binding(name),
    host_execution_permission = CommunicationIntelligence::ci_binding(name))
  realized <- CommunicationIntelligence::ci_realize_text(opened, opened$value$lines, observations)
  expect_identical(realized$operation, "COMPLETED")
  expect_identical(realized$value$autoplots, "NOT_USED")
  expect_identical(realized$value$rendering, "TEXT_READBACK_NOT_A_PLOT")
  expect_plot("exact_lookup_text", ap_realize_request(realized$value$request, opened$value$task))
  stale <- CommunicationIntelligence::ci_admit_owner_call(
    "FinancialIntelligence", "fi_bs", list(spot = 100, strike = 100, rate = 0.01, vol = 0.2, horizon = 1),
    list(list(id = "value", path = "value")), "9.9.9", "2026-10-09T00:00:00Z", "stale probe")
  expect_outcome("stale_owner_version", stale$operation, stale$findings[[1]]$code, FALSE, stale$findings[[1]]$reason)
  mismatched <- synthetic_inputs(list(list(id = "level", value = 2, display = "1", unit = "synthetic_unit")), "labeled synthetic")
  expect_outcome("display_mismatch_admission", mismatched$operation, mismatched$findings[[1]]$code, FALSE, mismatched$findings[[1]]$reason)
})

test_that("interval, duplicate, unit, and size defects are refused", {
  qualify_need(requireNamespace("CommunicationIntelligence", quietly = TRUE), "CommunicationIntelligence missing")
  one <- function(id, quantities, uncertainty, encoding = NULL, task = id) {
    admitted <- synthetic_inputs(quantities, uncertainty)
    if (!identical(admitted$operation, "COMPLETED")) {
      expect_outcome(id, admitted$operation, admitted$findings[[1]]$code, FALSE, admitted$findings[[1]]$reason)
      return()
    }
    bound <- bind_figure(admitted, task)
    expect_plot(id, ap_realize_request(bound$request, bound$task, encoding = encoding))
  }
  endpoint <- function(id, value, display) list(id = id, value = value, display = display, unit = "synthetic_unit")
  one("invalid_interval", list(endpoint("band_lower", 3, "3"), endpoint("band_upper", 1, "1")),
    "95 percent confidence interval", list(geom = "interval"), "invalid-interval")
  one("interval_relabel", list(endpoint("band_lower", 1, "1"), endpoint("band_upper", 2, "2"), endpoint("band", 1.5, "1.5")),
    "95 percent confidence interval", list(geom = "interval", interval_kind = "prediction"), "relabel-interval")
  one("interval_excludes_point", list(endpoint("band_lower", 1, "1"), endpoint("band_upper", 2, "2"), endpoint("band", 5, "5")),
    "95 percent confidence interval", list(geom = "interval", interval_kind = "confidence"), "excluded-point")
  one("duplicate_quantity", list(endpoint("x", 1, "1"), endpoint("x", 2, "2")), "labeled synthetic", NULL, "duplicate-quantity")
  one("conflicting_units_shared", list(
    list(id = "length", value = 2, display = "2", unit = "meter"),
    list(id = "duration", value = 3, display = "3", unit = "second")),
    "labeled synthetic", list(facet = "shared"), "conflicting-units")
  many <- lapply(1:13, function(i) endpoint(sprintf("q%02d", i), as.numeric(i), as.character(i)))
  one("artifact_bound_13", many, "labeled synthetic", NULL, "thirteen-quantities")
})

test_that("open obligations stay classified and no release is granted", {
  qualify_need(!is.null(ap_domain), "domain not loaded")
  open <- vapply(ap_domain$cases, function(case) if (identical(case$class, "NOT_EXERCISED")) case$id else "", character(1))
  open <- open[nzchar(open)]
  expect_identical(sort(open), sort(c(
    "legacy_echarts_pixels", "human_visual_judgment", "ci_text_path_does_not_call_autoplots",
    "cross_machine_pixel_identity", "consistent_rebuilt_forged_record")))
  for (id in open) record_case(id, "NOT_EXERCISED", NULL, FALSE)
  for (case in ap_domain$cases) {
    got <- ap_cases[[case$id]]
    expect_false(is.null(got), info = case$id)
    if (identical(case$class, "NOT_EXERCISED")) {
      expect_identical(got$status, "NOT_EXERCISED", info = case$id)
    } else {
      expect_identical(got$status, case$expected_status, info = case$id)
      if (is.null(case$expected_code)) expect_null(got$code, info = case$id) else expect_identical(got$code, case$expected_code, info = case$id)
    }
    expect_false(isTRUE(got$release), info = case$id)
  }
  evidence <- Sys.getenv("AUTOPLOTS_EVIDENCE_DIR")
  if (nzchar(evidence)) {
    rows <- lapply(ls(ap_cases), function(id) c(list(id = id), ap_cases[[id]]))
    jsonlite::write_json(rows, file.path(evidence, "case-results.json"), auto_unbox = TRUE, pretty = TRUE, null = "null")
  }
})
