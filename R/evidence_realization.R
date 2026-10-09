# Evidence-bound realization. This file draws supplied numbers. It does not
# estimate, approve, or authorize publication.

ap_contract <- function() "autoplots-evidence-realization-1"

ap_refuse <- function(status, code, reason) list(
  status = status, code = code, reason = reason, release_authorized = FALSE,
  human_judgment = "UNRESOLVED", scientific_qualification = "NOT_GRANTED",
  file = NULL, png_sha256 = NULL, geometry = NULL, trace = NULL, labels = NULL)

ap_receipt <- function(issuer, receipt_id, as_of, extra) c(list(
  issuer = issuer, receipt_id = receipt_id, revision = "1",
  evidence_binding = CommunicationIntelligence::ci_binding(receipt_id), as_of = as_of), extra)

ap_identity_limits <- function(value) {
  lo <- min(0, value)
  hi <- max(0, value)
  span <- hi - lo
  if (span == 0) return(c(ymin_axis = 0, ymax_axis = 1))
  pad <- 0.05 * span
  c(ymin_axis = lo - if (lo < 0) pad else 0, ymax_axis = hi + if (hi > 0) pad else 0)
}

ap_display_faithful <- function(display, value) {
  if (!is.character(display) || length(display) != 1L || is.na(display)) return(FALSE)
  if (!grepl("^-?(?:0|[1-9][0-9]*)(?:\\.[0-9]+)?(?:[eE][+-]?[0-9]+)?$", display, perl = TRUE)) return(FALSE)
  parsed <- suppressWarnings(as.numeric(display))
  is.finite(parsed) && identical(CommunicationIntelligence::ci_binding(parsed),
    CommunicationIntelligence::ci_binding(value))
}

ap_find_record <- function(records, id) {
  hit <- which(vapply(records, function(record) identical(record$id, id), logical(1)))
  if (length(hit) == 1L) records[[hit]] else NULL
}

ap_plot_quantities <- function(composition) {
  elements <- composition$payload$elements
  expressions <- composition$payload$expressions
  rows <- list()
  omissions <- list()
  for (element in elements) {
    if (!identical(element$payload$representation, "plot")) {
      omissions[[length(omissions) + 1L]] <- list(
        element_id = element$id, claim_id = element$payload$claim_id,
        representation = element$payload$representation,
        reason = "Not a plot element. No mark is invented for it, and its numbers are not dropped from a plot element.")
      next
    }
    quantities <- element$payload$quantities
    if (!length(quantities)) {
      omissions[[length(omissions) + 1L]] <- list(element_id = element$id, reason = "plot element has no numeric quantity")
      next
    }
    expression <- ap_find_record(expressions, element$payload$expression_refs[[1]]$id)
    source <- if (is.null(expression)) NULL else expression$payload$source_refs[[1]]
    uncertainty <- element$payload$truth_conditions$uncertainty
    for (quantity in quantities) {
      rows[[length(rows) + 1L]] <- list(
        element_id = element$id, quantity_id = quantity$id, value = quantity$value,
        display = quantity$display, unit = quantity$unit, locator = quantity$locator,
        uncertainty = if (is.null(uncertainty)) "" else as.character(uncertainty)[1],
        source = source)
    }
  }
  list(rows = rows, omissions = omissions)
}

ap_encoding <- function(encoding, rows) {
  allowed <- c("geom", "scale", "zero_baseline", "reverse_y", "percent", "aggregate",
    "unit_conversion", "facet", "ylim", "xlim", "category_order", "authorize_release",
    "authority_receipt", "interval_kind", "drop_ids", "dual_axis", "time_axis")
  base <- list(geom = "column", scale = "identity", zero_baseline = TRUE, reverse_y = FALSE,
    percent = FALSE, aggregate = "none", unit_conversion = "identity", facet = "quantity",
    ylim = NULL, xlim = NULL, category_order = NULL, authorize_release = FALSE,
    authority_receipt = NULL, interval_kind = NULL, drop_ids = NULL, dual_axis = FALSE,
    time_axis = FALSE)
  if (is.null(encoding)) encoding <- list()
  if (!is.list(encoding) || is.null(names(encoding)) && length(encoding) ||
      any(!(names(encoding) %in% allowed)))
    return(ap_refuse("REFUSED", "UNKNOWN_ENCODING", "The rendering instruction contains a field this contract does not accept."))
  asked <- modifyList(base, encoding)
  if (isTRUE(asked$authorize_release))
    return(ap_refuse("REFUSED", "PUBLICATION_NOT_OWNED", "AutoPlots does not authorize publication or human approval."))
  if (isTRUE(asked$dual_axis))
    return(ap_refuse("REFUSED", "DUAL_AXIS", "Two numeric scales on one panel are refused."))
  if (isTRUE(asked$time_axis))
    return(ap_refuse("REFUSED", "UNSUPPORTED_TIME_AXIS", "A time axis is refused unless the admitted evidence itself supplies the times. This contract does not invent them."))
  if (!identical(asked$aggregate, "none"))
    return(ap_refuse("REFUSED", "UNSUPPORTED_AGGREGATION", "Aggregation would change the admitted estimand and is not performed."))
  if (isTRUE(asked$percent))
    return(ap_refuse("REFUSED", "PERCENT_WITHOUT_DENOMINATOR", "A percent mark is refused when the admitted unit is not already a percent."))
  if (!identical(asked$unit_conversion, "identity"))
    return(ap_refuse("REFUSED", "UNSUPPORTED_UNIT_CONVERSION", "AutoPlots does not convert units."))
  if (isTRUE(asked$reverse_y))
    return(ap_refuse("REFUSED", "REVERSED_AXIS", "The supported encoding is up-means-more. A reversed axis is refused."))
  if (!isTRUE(asked$zero_baseline))
    return(ap_refuse("REFUSED", "ZERO_BASELINE_REQUIRED", "A bar or point in this contract keeps zero in the quantitative frame."))
  if (!is.null(asked$ylim) || !is.null(asked$xlim))
    return(ap_refuse("REFUSED", "AXIS_TRUNCATION", "Caller axis limits are refused. Limits are derived from zero and the admitted values."))
  if (!is.null(asked$category_order))
    return(ap_refuse("REFUSED", "CATEGORY_REORDER", "Category order stays in admitted evidence order."))
  if (length(asked$drop_ids))
    return(ap_refuse("REFUSED", "OMISSION_NOT_AUTHORIZED", "Admitted quantities are not dropped to simplify a figure."))
  if (!asked$facet %in% c("quantity", "shared"))
    return(ap_refuse("REFUSED", "UNKNOWN_ENCODING", "Facet must be quantity or shared."))
  if (identical(asked$facet, "shared") && length(unique(vapply(rows, function(row) row$quantity_id, character(1)))) > 1L)
    return(ap_refuse("REFUSED", "CONFLICTING_ESTIMANDS", "Different admitted quantities do not share one scale."))
  if (!asked$geom %in% c("column", "point", "interval"))
    return(ap_refuse("REFUSED", "UNKNOWN_ENCODING", "Geom must be column, point, or interval."))
  values <- vapply(rows, function(row) row$value, numeric(1))
  if (identical(asked$scale, "log10")) {
    if (any(!is.finite(values) | values <= 0))
      return(ap_refuse("REFUSED", "LOG_NONPOSITIVE", "A log scale is refused when an admitted value is not strictly positive."))
    if (!identical(asked$geom, "point"))
      return(ap_refuse("REFUSED", "LOG_BAR_HAS_NO_ZERO", "A log scale is supported only for a point. A bar or interval length is measured from zero."))
    expected <- CommunicationIntelligence::ci_binding(list(scale = "log10",
      quantity_ids = lapply(rows, function(row) row$quantity_id), values = as.list(values)))
    receipt <- asked$authority_receipt
    if (is.null(receipt))
      return(ap_refuse("UNRESOLVED_AUTHORITY", "LOG_SCALE_AUTHORITY", "A log scale changes the visual estimand and needs an explicit authority receipt. None was supplied."))
    if (!is.list(receipt) || !identical(receipt$status, "MET") || !identical(receipt$subject_binding, expected))
      return(ap_refuse("REFUSED", "FORGED_SCALE_AUTHORITY", "The scale receipt does not bind this log request."))
  } else if (!identical(asked$scale, "identity")) {
    return(ap_refuse("REFUSED", "UNSUPPORTED_SCALE", "Only identity and an authorized log10 point scale are supported."))
  }
  if (!is.null(asked$interval_kind) && !asked$interval_kind %in% c("confidence", "prediction", "owner_supplied"))
    return(ap_refuse("REFUSED", "INTERVAL_RELABEL", "An interval kind must be confidence, prediction, or owner_supplied, and it must already be the admitted wording."))
  list(status = "READY", encoding = asked)
}

ap_interval_rows <- function(rows, kind) {
  ids <- vapply(rows, function(row) row$quantity_id, character(1))
  lowers <- grep("_lower$", ids)
  if (!length(lowers)) return(ap_refuse("REFUSED", "MISSING_INTERVAL", "An interval geom needs admitted <id>_lower and <id>_upper quantities."))
  out <- list()
  for (index in lowers) {
    stem <- sub("_lower$", "", ids[[index]])
    upper <- which(ids == paste0(stem, "_upper"))
    point <- which(ids == stem)
    if (length(upper) != 1L) return(ap_refuse("REFUSED", "MISSING_INTERVAL", paste("Missing upper endpoint for", stem)))
    lo <- rows[[index]]$value
    hi <- rows[[upper]]$value
    if (lo > hi) return(ap_refuse("REFUSED", "INVALID_INTERVAL", "The lower endpoint is above the upper endpoint."))
    if (length(point) == 1L && (rows[[point]]$value < lo || rows[[point]]$value > hi))
      return(ap_refuse("REFUSED", "INTERVAL_EXCLUDES_POINT", "The admitted point is outside the admitted interval."))
    parts <- c(list(rows[[index]], rows[[upper]]), if (length(point) == 1L) list(rows[[point]]) else list())
    if (length(unique(vapply(parts, function(part) part$unit, character(1)))) != 1L)
      return(ap_refuse("REFUSED", "CONFLICTING_ESTIMANDS", "Interval endpoints do not share one unit."))
    wording <- rows[[index]]$uncertainty
    if (!is.null(kind) && !grepl(kind, wording, fixed = TRUE))
      return(ap_refuse("REFUSED", "INTERVAL_RELABEL", "The requested interval kind is not the admitted uncertainty wording."))
    out[[length(out) + 1L]] <- list(stem = stem, lower = rows[[index]], upper = rows[[upper]],
      point = if (length(point) == 1L) rows[[point]] else NULL, uncertainty = wording)
  }
  used <- unlist(lapply(out, function(pair) c(pair$lower$quantity_id, pair$upper$quantity_id,
    if (!is.null(pair$point)) pair$point$quantity_id)), use.names = FALSE)
  unused <- setdiff(ids, used)
  if (length(unused))
    return(ap_refuse("REFUSED", "OMISSION_NOT_AUTHORIZED",
      paste("An interval figure does not drop admitted quantities:", paste(unused, collapse = ", "))))
  out
}

ap_bind_pattern_request <- function(inputs, task_id = "autoplots-pattern") {
  if (!requireNamespace("CommunicationIntelligence", quietly = TRUE))
    return(ap_refuse("REFUSED", "AUTHORITY_PACKAGE_UNAVAILABLE", "CommunicationIntelligence is not installed, so the request cannot be built or verified."))
  ci <- asNamespace("CommunicationIntelligence")
  if (!isTRUE(ci$ci_validate_record(inputs)) || !identical(inputs$record_type, "AdmittedInputSet"))
    return(ap_refuse("REFUSED", "MISSING_ADMITTED_INPUTS", "Pass an admitted CommunicationIntelligence input set. This function does not invent owner values."))
  claims <- inputs$payload$claims
  primary <- setdiff(vapply(claims, function(claim) claim$id, character(1)), "owner-limit")
  if (!length(primary)) return(ap_refuse("REFUSED", "NO_NUMERIC_EVIDENCE", "The admitted set has no governing claim."))
  as_of <- inputs$payload$as_of
  host <- "autoplots-local-host"
  generator <- "autoplots-host"
  selector <- "autoplots-review-handoff"
  core <- c("claim_fidelity", "selection_fidelity", "quantitative_fidelity", "units", "scope", "uncertainty",
    "source_integrity", "authority_labels", "artifact_binding", "access_release", "actual_artifact")
  context <- c("audience", "narrative", "representation", "visual_quality", "accessibility", "medium")
  obligations <- lapply(c(core, context), function(id) list(
    id = id, origin = "communication fidelity floor", mandatory = TRUE, applicability = "APPLICABLE",
    methods = if (id %in% core) list("mechanical", "independent") else list("independent"),
    rule = "Preserve owner values and do not treat a figure as approval",
    assessor = "independent human review remains unresolved", na = NULL))
  profile <- list(id = paste0(task_id, "-profile"), revision = "1", issuer = "autoplots-profile-owner",
    audience_ref = "local reader", medium_refs = list("executive_memo", "interactive_briefing"),
    design_ref = "evidence-bound-figure", obligations = obligations,
    judge_policy = list(judges = 2, same_family_disclosed = TRUE,
      identity_recognizability_rule = "a figure is not a judge"))
  grant <- function(action, subject) ap_receipt(host, paste(task_id, action, subject, sep = "-"), as_of, list(
    subject = subject, action = action, task_id = task_id, input_binding = inputs$record_binding,
    subject_binding = inputs$record_binding, audience = "local reader", channel = "local", decision = "GRANTED",
    valid_from = as_of, valid_until = as_of))
  envelope <- list(id = "autoplots-evidence-envelope", allowed_owner = "AutoPlots",
    operations = list("identity column from admitted quantities"))
  spec <- list(id = task_id, revision = "1", objective = "Show the admitted numbers without adding authority",
    audience = "local reader", task_type = "pattern_inspection",
    reader_question = "What quantities did the owner admit, and what must stay visible beside them?",
    input_binding = inputs$record_binding, scope = inputs$payload$scope,
    media = list("executive_memo", "interactive_briefing"), profile = profile,
    profile_receipt = ap_receipt("autoplots-profile-owner", paste0(task_id, "-profile"), as_of, list(
      profile_binding = ci$ci_binding(profile), authorized = TRUE, phase = "BEFORE_GENERATION")),
    generator = generator, selector = selector, mode = "automated",
    grants = list(grant("propose", generator), grant("select", selector), grant("compose", generator), grant("realize", generator)),
    as_of = as_of, budgets = list(max_candidates = 3, judges_per_candidate = 2, max_adjudications = 1,
      provider_calls = 0, tool_calls = 0, realizations = 2),
    needs = list(primary_claims = as.list(primary), required_claims = as.list(vapply(claims, function(claim) claim$id, character(1))),
      permitted_omissions = list(), reading_budget_seconds = 60, max_main_elements = length(primary),
      detail_path = "detail", visual_relation = "admitted quantities in evidence order"),
    output_slots = list(list(id = "memo", medium = "executive_memo"), list(id = "briefing", medium = "interactive_briefing")),
    host_issuer = host, inspection = list(states = list("figure"), viewports = list("png"),
      methods = list("graphics device"), task_questions = list("Does the figure encode the admitted value from zero?"),
      coverage_argument = "One controlled png device, not a publication"),
    realization_envelope = envelope)
  task <- ci$ci_bind_task(spec)
  if (!identical(task$operation, "COMPLETED")) return(ap_refuse("REFUSED", task$findings[[1]]$code, task$findings[[1]]$reason))
  proposal <- ci$ci_propose_plan(task$value, inputs)
  if (!identical(proposal$operation, "COMPLETED")) return(ap_refuse("REFUSED", proposal$findings[[1]]$code, proposal$findings[[1]]$reason))
  plan <- ci$ci_select_plan(proposal$value, proposal$value$payload$recommended, selector, task$value)
  if (!identical(plan$operation, "COMPLETED")) return(ap_refuse("REFUSED", plan$findings[[1]]$code, plan$findings[[1]]$reason))
  composition <- ci$ci_compose(plan$value, inputs, task$value)
  if (!identical(composition$operation, "COMPLETED")) return(ap_refuse("REFUSED", composition$findings[[1]]$code, composition$findings[[1]]$reason))
  collected <- ap_plot_quantities(composition$value)
  prepared <- lapply(collected$rows, function(row) list(id = row$quantity_id, value = row$value, display = row$display, unit = row$unit))
  if (!length(prepared)) return(ap_refuse("REFUSED", "NO_NUMERIC_EVIDENCE", "The plot elements contain no numeric quantity."))
  runtime_binding <- ci$ci_binding(list(owner = "AutoPlots", contract = ap_contract()))
  adapter_binding <- ci$ci_binding(ap_contract())
  behavior_spec <- list(id = paste0(task_id, "-behavior"), composition_ref = ci$ci_ref(composition$value),
    normalized_spec_binding = ci$ci_binding(prepared), runtime_binding = runtime_binding, adapter_binding = adapter_binding,
    asset_bindings = list(), implementation_binding = ci$ci_binding("ap_realize_request"),
    class = "DECLARATIVE_VISUAL_SPECIFICATION", inputs = list("admitted quantities"), triggers = list("host render"),
    effects = list("png and geometry trace"), destinations = list("local png"), permissions = list("local_png"),
    profile_binding = ci$ci_binding(task$value$payload$profile), scope = inputs$payload$scope, lifetime = "one figure",
    paths = list(list(id = "evidence-column", implementation_binding = ci$ci_binding("ap_realize_request"),
      trigger = "host render", effects = list("png and geometry trace"), permission_ref = "local_png")))
  obligations <- c("inert_import", "complete_path_inventory", "effect_confinement", "literal_preservation", "runtime_binding", "host_execution_permission")
  behavior_receipts <- lapply(obligations, function(obligation) ap_receipt(host, paste(task_id, obligation, sep = "-"), as_of, list(
    obligation = obligation, subject_binding = ci$ci_binding(behavior_spec), observation_ref = obligation, status = "MET")))
  behavior <- ci$ci_behavior_admission(behavior_spec, behavior_receipts)
  if (!identical(behavior$operation, "COMPLETED")) return(ap_refuse("REFUSED", behavior$findings[[1]]$code, behavior$findings[[1]]$reason))
  source <- list(issuer = "AutoPlots", kind = "source", id = "ap_realize_request",
    revision = as.character(utils::packageVersion("AutoPlots")),
    content_binding = list(algorithm = "sha256", encoding = "utf8", status = "AVAILABLE", digest = ci$ci_binding(ap_contract())),
    locator = "R/evidence_realization.R", authority_assertion_ref = "AutoPlots:ap_realize_request",
    access_assertion_ref = "exported_public_function")
  semantics <- claims[[1]]$semantics
  semantics$encodings <- list("column from zero in evidence order")
  semantics$selection_binding <- plan$value$payload$selection$record_binding
  visual_spec <- list(id = paste0(task_id, "-visual"), plan_ref = ci$ci_ref(plan$value),
    element_refs = lapply(composition$value$payload$elements, ci$ci_ref), task_ref = ci$ci_ref(task$value),
    input_ref = ci$ci_ref(inputs), prepared_refs = list(claims[[1]]$source_refs[[1]]), semantics = semantics,
    grammar = list(binding = ci$ci_binding(prepared), resolved_values = prepared,
      composition_writes = list("identity column"), obligations = list("preserve admitted value")),
    runtime_binding = runtime_binding, behavior = behavior$value)
  route <- list(route = "high_level", available = TRUE, selected = TRUE,
    reason = "The maintained public realization is ap_realize_request",
    source_path = "R/evidence_realization.R", source_ref = source, surface = "MAINTAINED_PUBLIC",
    authority_receipt = ap_receipt(host, paste0(task_id, "-route"), as_of, list(
      source_path = "R/evidence_realization.R", subject_ref = source, surface = "MAINTAINED_PUBLIC")))
  candidate <- ci$ci_discover(visual_spec, list(route))
  if (!identical(candidate$operation, "COMPLETED")) return(ap_refuse("REFUSED", candidate$findings[[1]]$code, candidate$findings[[1]]$reason))
  for (state in c("SEMANTICALLY_BOUND", "REALIZATION_ADMITTED")) {
    evidence <- ap_receipt(host, paste(task_id, state, sep = "-"), as_of, list(
      subject_binding = candidate$value$record_binding, status = "MET", observation_ref = state))
    candidate <- ci$ci_transition(candidate$value, state, evidence)
    if (!identical(candidate$operation, "COMPLETED")) return(ap_refuse("REFUSED", candidate$findings[[1]]$code, candidate$findings[[1]]$reason))
  }
  runtime <- list(owner = "AutoPlots", runtime_binding = runtime_binding, adapter_binding = adapter_binding,
    asset_bindings = list(), envelope_binding = ci$ci_binding(envelope), behavior = behavior$value,
    visual_candidate = candidate$value, prepared_data_binding = ci$ci_binding(prepared),
    resolved_grammar = list(id = ap_contract()))
  runtime$host_admission <- ap_receipt(host, paste0(task_id, "-runtime"), as_of, list(
    subject_binding = ci$ci_binding(runtime), status = "MET", composition_binding = composition$value$record_binding))
  request <- ci$ci_request_realization(composition$value, task$value, runtime)
  if (!identical(request$operation, "COMPLETED")) return(ap_refuse("REFUSED", request$findings[[1]]$code, request$findings[[1]]$reason))
  list(status = "REQUEST_READY", code = NULL, reason = "The request is CommunicationIntelligence's realization record. It is not approval.",
    release_authorized = FALSE, human_judgment = "UNRESOLVED", scientific_qualification = "NOT_GRANTED",
    request = request$value, task = task$value, prepared = prepared)
}

ap_layout_fits <- function(n, width, height, res, margins) {
  path <- tempfile(fileext = ".png")
  grDevices::png(path, width = width, height = height, units = "px", res = res, bg = "white")
  open <- TRUE
  on.exit({
    if (open) grDevices::dev.off()
    unlink(path)
  }, add = TRUE)
  ok <- TRUE
  tryCatch({
    graphics::par(mfrow = c(n, 1L), mar = margins, family = "sans")
    graphics::plot(NA, xlim = c(0.5, 1.5), ylim = c(0, 1), xlab = "", ylab = "",
      main = "probe", axes = FALSE, xaxs = "i", yaxs = "i")
  }, error = function(e) ok <<- FALSE)
  grDevices::dev.off()
  open <- FALSE
  ok
}

ap_realize_request <- function(request, task, file = NULL, encoding = NULL, width = 800L, height = 500L, res = 96L) {
  tryCatch(ap_realize_checked(request, task, file, encoding, width, height, res), error = function(e)
    ap_refuse("REFUSED", "REQUEST_BINDING_MISMATCH", paste("Verification failed closed:", conditionMessage(e))))
}

ap_realize_checked <- function(request, task, file, encoding, width, height, res) {
  if (!requireNamespace("CommunicationIntelligence", quietly = TRUE))
    return(ap_refuse("REFUSED", "AUTHORITY_PACKAGE_UNAVAILABLE", "CommunicationIntelligence is not installed, so the request cannot be verified."))
  ci <- asNamespace("CommunicationIntelligence")
  if (!isTRUE(ci$ci_validate_record(request)) || !identical(request$record_type, "RealizationRequest"))
    return(ap_refuse("REFUSED", "REQUEST_BINDING_MISMATCH", "An intact CommunicationIntelligence realization request is required."))
  if (!isTRUE(ci$ci_validate_record(task)) || !identical(request$payload$task, ci$ci_ref(task)))
    return(ap_refuse("REFUSED", "TASK_BINDING_MISMATCH", "The task record does not match the request."))
  if (!width %in% 200:1200 || !height %in% 200:1200 || !res %in% c(72, 96, 120, 144))
    return(ap_refuse("REFUSED", "RESOURCE_CEILING", "The qualified device is 200 to 1200 pixels on a side and 72, 96, 120, or 144 dpi."))
  replay <- ci$ci_request_realization(request$payload$composition, task, request$payload$runtime)
  if (!identical(replay$operation, "COMPLETED"))
    return(ap_refuse("REFUSED", replay$findings[[1]]$code, replay$findings[[1]]$reason))
  if (!identical(replay$value$record_binding, request$record_binding))
    return(ap_refuse("REFUSED", "REQUEST_REPLAY_MISMATCH", "Replaying the authority contract did not reproduce this request. A hash on a modified record is not authentication."))
  collected <- ap_plot_quantities(request$payload$composition)
  rows <- collected$rows
  if (!length(rows)) return(ap_refuse("REFUSED", "NO_PLOT_ELEMENT", "The admitted composition has no plot quantity. A text request is not a figure."))
  route <- request$payload$runtime$visual_candidate$payload$selected_route
  if (!identical(request$payload$runtime$owner, "AutoPlots") || is.null(route) ||
      !identical(route$source_path, "R/evidence_realization.R") || !identical(route$source_ref$id, "ap_realize_request") ||
      !identical(route$source_ref$content_binding$digest, ci$ci_binding(ap_contract())) ||
      !identical(route$surface, "MAINTAINED_PUBLIC"))
    return(ap_refuse("REFUSED", "UNBOUND_PLOT_ROUTE", "The admitted route is not AutoPlots::ap_realize_request."))
  if (length(rows) > 12L) return(ap_refuse("REFUSED", "ARTIFACT_BOUND", "More than 12 admitted quantities exceeds this figure contract."))
  prepared <- lapply(rows, function(row) list(id = row$quantity_id, value = row$value, display = row$display, unit = row$unit))
  if (!identical(request$payload$runtime$prepared_data_binding, ci$ci_binding(prepared)))
    return(ap_refuse("REFUSED", "PREPARED_DATA_MISMATCH", "The prepared-data binding is not the admitted plot quantities."))
  if (anyDuplicated(vapply(rows, function(row) row$quantity_id, character(1))))
    return(ap_refuse("REFUSED", "DUPLICATE_QUANTITY", "The same quantity id appears more than once."))
  for (row in rows) {
    if (!is.numeric(row$value) || length(row$value) != 1L || !is.finite(row$value))
      return(ap_refuse("REFUSED", "MISSING_VALUE", "Each admitted quantity must be one finite number."))
    if (!ap_display_faithful(row$display, row$value))
      return(ap_refuse("REFUSED", "DISPLAY_MISMATCH", "The display literal does not denote the admitted value."))
    source <- row$source
    if (is.null(source) || !nzchar(source$issuer) || source$revision %in% c("latest", "current", "UNKNOWN"))
      return(ap_refuse("REFUSED", "MISSING_PROVENANCE", "Each plotted quantity needs an exact source revision."))
  }
  decision <- ap_encoding(encoding, rows)
  if (!identical(decision$status, "READY")) return(decision)
  asked <- decision$encoding
  drawn <- rows
  if (identical(asked$geom, "interval")) {
    paired <- ap_interval_rows(rows, asked$interval_kind)
    if (is.list(paired) && !is.null(paired$status) && paired$status %in% c("REFUSED", "UNRESOLVED_AUTHORITY")) return(paired)
    drawn <- lapply(paired, function(pair) list(
      element_id = pair$lower$element_id, quantity_id = pair$stem, value = if (is.null(pair$point)) NA_real_ else pair$point$value,
      display = if (is.null(pair$point)) paste(pair$lower$display, "to", pair$upper$display) else pair$point$display,
      unit = pair$lower$unit, locator = pair$lower$locator, uncertainty = pair$uncertainty, source = pair$lower$source,
      y0 = pair$lower$value, y1 = pair$upper$value))
  }
  estimand_changed <- identical(asked$scale, "log10")
  panels <- lapply(seq_along(drawn), function(index) {
    row <- drawn[[index]]
    value <- row$value
    y0 <- if (!is.null(row$y0)) row$y0 else min(0, value)
    y1 <- if (!is.null(row$y1)) row$y1 else max(0, value)
    limits <- if (estimand_changed) {
      encoded <- log10(value)
      pad <- 0.05 * abs(encoded)
      if (!is.finite(pad) || pad == 0) pad <- 0.05
      c(ymin_axis = encoded - pad, ymax_axis = encoded + pad)
    } else ap_identity_limits(if (identical(asked$geom, "interval")) c(y0, y1) else value)
    data.table::data.table(
      quantity_id = row$quantity_id, value = value, display = row$display, unit = row$unit,
      panel = row$quantity_id, category_index = index, y0 = y0, y1 = y1,
      ymin_axis = unname(limits[["ymin_axis"]]), ymax_axis = unname(limits[["ymax_axis"]]),
      scale = asked$scale, zero_included = identical(asked$scale, "identity"),
      uncertainty = row$uncertainty, source_issuer = row$source$issuer, source_id = row$source$id,
      source_revision = row$source$revision, source_digest = row$source$content_binding$digest,
      locator = row$locator)
  })
  geometry <- data.table::rbindlist(panels)
  labels <- lapply(seq_len(nrow(geometry)), function(i) list(
    title = paste0(geometry$quantity_id[[i]], " = ", geometry$display[[i]]),
    subtitle = paste0(geometry$unit[[i]], " | ", geometry$source_issuer[[i]], " ",
      geometry$source_revision[[i]], " ", substr(geometry$source_digest[[i]], 1, 12),
      " | not an approval")))
  if (any(grepl("approved|qualified|causal", vapply(labels, function(label) paste(label$title, label$subtitle), character(1)), ignore.case = TRUE)))
    return(ap_refuse("REFUSED", "LABEL_AUTHORITY", "A figure label must not say approved, qualified, or causal."))
  if (is.null(file)) file <- tempfile(pattern = "autoplots-evidence-", fileext = ".png")
  device_y0 <- device_y1 <- device_mark <- numeric(nrow(geometry))
  # Bottom margin leaves a line under the category for unit, source revision, and "not an approval".
  margins <- c(5.2, 4.2, 2.8, 0.8)
  if (!ap_layout_fits(nrow(geometry), width, height, res, margins))
    return(ap_refuse("REFUSED", "RESOURCE_CEILING", "This device cannot show every panel and the provenance line. A taller height inside 200 to 1200 pixels may fit fewer panels; this call does not draw a clipped figure."))
  grDevices::png(file, width = width, height = height, units = "px", res = res, bg = "white")
  device_open <- TRUE
  on.exit(if (device_open) grDevices::dev.off(), add = TRUE)
  graphics::par(mfrow = c(nrow(geometry), 1L), mar = margins, family = "sans")
  for (i in seq_len(nrow(geometry))) {
    row <- geometry[i]
    graphics::plot(NA, xlim = c(0.5, 1.5), ylim = c(row$ymin_axis, row$ymax_axis),
      xlab = "", ylab = row$unit, main = labels[[i]]$title,
      axes = FALSE, xaxs = "i", yaxs = "i")
    graphics::axis(1, at = 1, labels = row$quantity_id, tick = FALSE)
    graphics::mtext(labels[[i]]$subtitle, side = 1, line = 3.6, cex = 0.58)
    graphics::axis(2)
    if (isTRUE(row$zero_included)) graphics::abline(h = 0, col = "gray40")
    if (identical(asked$geom, "point")) {
      yy <- if (identical(asked$scale, "log10")) log10(row$value) else row$value
      graphics::points(1, yy, pch = 16, cex = 1.4, col = "#2c7fb8")
      device_mark[[i]] <- graphics::grconvertY(yy, from = "user", to = "device")
      device_y0[[i]] <- device_mark[[i]]
      device_y1[[i]] <- device_mark[[i]]
    } else if (identical(asked$geom, "interval")) {
      graphics::segments(1, row$y0, 1, row$y1, lwd = 3, col = "#2c7fb8")
      device_y0[[i]] <- graphics::grconvertY(row$y0, from = "user", to = "device")
      device_y1[[i]] <- graphics::grconvertY(row$y1, from = "user", to = "device")
      if (is.finite(row$value)) {
        graphics::points(1, row$value, pch = 16, col = "#08306b")
        device_mark[[i]] <- graphics::grconvertY(row$value, from = "user", to = "device")
      } else {
        device_mark[[i]] <- NA_real_
      }
    } else {
      graphics::rect(0.7, row$y0, 1.3, row$y1, col = "#2c7fb8", border = NA)
      device_y0[[i]] <- graphics::grconvertY(row$y0, from = "user", to = "device")
      device_y1[[i]] <- graphics::grconvertY(row$y1, from = "user", to = "device")
      device_mark[[i]] <- device_y1[[i]]
    }
  }
  grDevices::dev.off()
  device_open <- FALSE
  signature <- readBin(file, "raw", n = 8L)
  if (!identical(signature, as.raw(c(0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a))))
    return(ap_refuse("REFUSED", "ARTIFACT_BOUND", "The graphics device did not write a PNG signature. No figure is returned."))
  data.table::set(geometry, j = "device_y0", value = device_y0)
  data.table::set(geometry, j = "device_y1", value = device_y1)
  data.table::set(geometry, j = "device_mark", value = device_mark)
  list(status = "RENDERED", code = NULL,
    reason = "The figure encodes the admitted values. It does not approve them or authorize publication.",
    release_authorized = FALSE, human_judgment = "UNRESOLVED", scientific_qualification = "NOT_GRANTED",
    file = file, png_sha256 = digest::digest(file = file, algo = "sha256"),
    bytes = file.size(file), width = width, height = height, res = res,
    geometry = geometry, labels = labels,
    trace = list(original_values = lapply(rows, function(row) list(id = row$quantity_id, value = row$value)),
      transformed_values = if (identical(asked$scale, "log10")) lapply(seq_len(nrow(geometry)), function(i) list(id = geometry$quantity_id[[i]], log10 = log10(geometry$value[[i]]))) else NULL,
      scale = asked$scale, estimand_changed = estimand_changed, omissions = collected$omissions,
      axis_limits = geometry[, list(quantity_id, ymin_axis, ymax_axis)],
      rendering = list(device = "png", width = width, height = height, res = res, margins = margins, family = "sans"),
      zero_included = unique(geometry$zero_included),
      reproducibility = "Device coordinates are for this controlled png device. They are not a cross-machine pixel certificate."))
}
