# @noRd
.ap_get_plot_theme <- function(p) {
  theme <- tryCatch(p$x$theme, error = function(e) NULL)

  if (
    !is.null(theme) &&
    is.character(theme) &&
    length(theme) == 1L &&
    !is.na(theme) &&
    nzchar(theme)
  ) {
    return(theme)
  }

  NULL
}

# @noRd
.ap_infer_theme_from_plots <- function(plots, fallback = "dark") {
  themes <- vapply(
    plots,
    function(p) {
      theme <- .ap_get_plot_theme(p)
      if (is.null(theme)) NA_character_ else theme
    },
    character(1L)
  )

  themes <- themes[!is.na(themes) & nzchar(themes)]

  if (!length(themes)) {
    return(fallback)
  }

  unique_themes <- unique(themes)

  if (length(unique_themes) > 1L) {
    warning(
      "Multiple plot themes detected: ",
      paste(unique_themes, collapse = ", "),
      ". Using the first detected theme: ",
      unique_themes[1L],
      call. = FALSE
    )
  }

  unique_themes[1L]
}

# @noRd
.ap_is_nonempty_chr <- function(x) {
  !is.null(x) && length(x) == 1L && !is.na(x) && nzchar(x)
}

# @noRd
.ap_css <- function(...) {
  vals <- list(...)
  vals <- vals[!vapply(vals, is.null, logical(1))]
  paste0(unlist(vals, use.names = FALSE), collapse = "")
}

# @noRd
.ap_validate_plot_list <- function(plots, arg = "plots") {
  if (!is.list(plots)) {
    stop(sprintf("`%s` must be a list of echarts4r plots, htmlwidgets, or htmltools tags.", arg), call. = FALSE)
  }

  if (length(plots) == 0L) {
    stop(sprintf("`%s` must contain at least one plot.", arg), call. = FALSE)
  }

  invisible(TRUE)
}

# @noRd
.ap_normalize_labels <- function(x, n, arg) {
  if (is.null(x)) {
    return(rep(NA_character_, n))
  }

  if (!is.character(x) || length(x) != n) {
    stop(sprintf("`%s` must be NULL or a character vector with length equal to the number of plots.", arg), call. = FALSE)
  }

  x
}

# @noRd
.ap_card_header <- function(
    title = NA_character_,
    subtitle = NA_character_,
    title_color = "#E5E7EB",
    subtitle_color = "#9CA3AF",
    title_size = "16px",
    subtitle_size = "13px",
    font_family = "Segoe UI, system-ui, -apple-system, BlinkMacSystemFont, sans-serif"
) {
  children <- list()

  if (!is.na(title) && nzchar(title)) {
    children[[length(children) + 1L]] <- htmltools::tags$div(
      style = paste0(
        "font-family:", font_family, ";",
        "font-size:", title_size, ";",
        "font-weight:700;",
        "color:", title_color, ";",
        "margin:0 0 4px 0;"
      ),
      title
    )
  }

  if (!is.na(subtitle) && nzchar(subtitle)) {
    children[[length(children) + 1L]] <- htmltools::tags$div(
      style = paste0(
        "font-family:", font_family, ";",
        "font-size:", subtitle_size, ";",
        "font-weight:400;",
        "color:", subtitle_color, ";",
        "margin:0 0 12px 0;"
      ),
      subtitle
    )
  }

  htmltools::tagList(children)
}

# @noRd
.ap_card_style <- function(
    card = TRUE,
    card_background = "#0F172A",
    card_border = "1px solid rgba(255,255,255,0.12)",
    card_radius = "14px",
    card_padding = "14px",
    card_shadow = "0 14px 35px rgba(0,0,0,0.25)"
) {
  if (isTRUE(card)) {
    paste0(
      "background:", card_background, ";",
      "border:", card_border, ";",
      "border-radius:", card_radius, ";",
      "padding:", card_padding, ";",
      "box-shadow:", card_shadow, ";",
      "overflow:hidden;"
    )
  } else {
    "overflow:hidden;"
  }
}

# @noRd
.ap_deparse_arg <- function(x) {
  paste(deparse(x), collapse = "")
}

# @noRd
.ap_display_resize_css <- function() {
  paste(
    ".aq-resizable-row{position:relative;min-height:var(--aq-row-min-height,320px);height:var(--aq-row-height,420px);resize:vertical;overflow:auto;border-radius:var(--aq-row-radius,14px)}",
    ".aq-resizable-row::after{content:\"\";position:absolute;right:8px;bottom:8px;width:16px;height:16px;border-right:2px solid rgba(148,163,184,.55);border-bottom:2px solid rgba(148,163,184,.55);pointer-events:none;opacity:.75}",
    ".aq-resizable-card{height:100%;min-height:0;display:flex;flex-direction:column}",
    ".aq-display-grid{display:grid;grid-template-columns:repeat(var(--aq-display-cols,2),minmax(0,1fr));gap:var(--aq-display-gap,20px);height:100%;min-height:0;width:100%}",
    ".aq-display-cell{min-width:0;min-height:180px;height:100%;display:flex;flex-direction:column;overflow:hidden}",
    ".aq-display-widget{flex:1 1 auto;min-height:0;height:100%;width:100%;overflow:hidden}",
    ".aq-display-widget>.html-widget,.aq-display-widget .html-widget,.aq-display-widget iframe{width:100%!important;height:100%!important;min-height:0!important}",
    "@media screen and (max-width:900px){.aq-display-grid{grid-template-columns:1fr!important}.aq-resizable-row{min-height:360px}}",
    sep = "\n"
  )
}

# @noRd
.ap_display_resize_js <- function() {
  "(function(){function resizeWidgets(root){try{window.dispatchEvent(new Event('resize'))}catch(e){}var scope=root||document;scope.querySelectorAll('.aq-display-widget,.aq-resizable-row').forEach(function(node){node.querySelectorAll('div').forEach(function(el){try{var inst=(window.echarts&&window.echarts.getInstanceByDom)?window.echarts.getInstanceByDom(el):null;if(inst&&inst.resize)inst.resize()}catch(e){}})})}document.addEventListener('DOMContentLoaded',function(){resizeWidgets(document);if('ResizeObserver'in window){var observer=new ResizeObserver(function(entries){entries.forEach(function(entry){resizeWidgets(entry.target)})});document.querySelectorAll('.aq-resizable-row').forEach(function(row){observer.observe(row)})}})})();"
}

# @noRd
.ap_display_resize_assets <- function() {
  htmltools::tagList(
    htmltools::tags$style(htmltools::HTML(.ap_display_resize_css())),
    htmltools::tags$script(htmltools::HTML(.ap_display_resize_js()))
  )
}

# @noRd
.ap_css_unit <- function(x, default = "420px") {
  if (is.null(x) || length(x) != 1L || is.na(x)) {
    return(default)
  }
  if (is.numeric(x)) {
    return(paste0(as.integer(x), "px"))
  }
  as.character(x)
}

# @noRd
.ap_css_units <- function(x, n, default = "420px") {
  if (is.null(x)) {
    x <- default
  }
  if (length(x) == 1L) {
    x <- rep(x, n)
  }
  if (length(x) != n) {
    stop(sprintf("CSS unit vector must have length 1 or %d.", n), call. = FALSE)
  }
  vapply(x, .ap_css_unit, character(1L), default = default)
}

# @noRd
.ap_wrap_display_widget <- function(plot) {
  htmltools::tags$div(class = "aq-display-widget", plot)
}

# @noRd
.ap_get_plot_theme <- function(p) {
  theme <- tryCatch(p$x$theme, error = function(e) NULL)

  if (
    !is.null(theme) &&
    is.character(theme) &&
    length(theme) == 1L &&
    !is.na(theme) &&
    nzchar(theme)
  ) {
    return(theme)
  }

  NULL
}

# @noRd
.ap_infer_theme_from_plots <- function(plots, fallback = "dark") {
  themes <- vapply(
    plots,
    function(p) {
      theme <- .ap_get_plot_theme(p)
      if (is.null(theme)) NA_character_ else theme
    },
    character(1L)
  )

  themes <- themes[!is.na(themes) & nzchar(themes)]

  if (!length(themes)) {
    return(fallback)
  }

  unique(themes)[1L]
}

# @noRd
.ap_flatten_plot_sections <- function(sections) {
  unlist(sections, recursive = FALSE, use.names = FALSE)
}

# @noRd
.ap_alpha_color <- function(color, alpha = 0.35) {
  if (is.null(color) || length(color) == 0L || is.na(color[1L])) {
    return(sprintf("rgba(255,255,255,%.3f)", alpha))
  }

  color <- color[1L]

  if (grepl("^rgba\\(", color, ignore.case = TRUE)) return(color)
  if (grepl("^rgb\\(", color, ignore.case = TRUE)) return(color)

  rgb <- tryCatch(grDevices::col2rgb(color), error = function(e) NULL)

  if (is.null(rgb)) return(color)

  sprintf(
    "rgba(%d,%d,%d,%.3f)",
    rgb[1L, 1L],
    rgb[2L, 1L],
    rgb[3L, 1L],
    alpha
  )
}

# @noRd
# @noRd
.ap_is_dark_layout_theme <- function(theme) {
  if (is.null(theme) || !length(theme) || is.na(theme[1L])) {
    return(TRUE)
  }

  theme <- tolower(theme[1L])

  # Direct rule
  if (grepl("dark", theme, fixed = TRUE)) {
    return(TRUE)
  }

  # AutoPlots custom themes that visually expect a dark shell
  theme %in% c(
    "chatgpt",
    "groq",
    "cyberpunk",
    "braves"
  )
}

# @noRd
.ap_get_layout_theme_defaults <- function(theme = "dark") {
  common <- tryCatch(
    get_theme_defaults_common(theme),
    error = function(e) list()
  )

  is_dark <- .ap_is_dark_layout_theme(theme)

  theme_title <- common[["title.textStyle.color"]]
  if (is.null(theme_title)) theme_title <- if (is_dark) "#E5E7EB" else "#111827"

  theme_subtitle <- common[["title.subtextStyle.color"]]
  if (is.null(theme_subtitle)) theme_subtitle <- if (is_dark) "#A7B0C0" else "#4B5563"

  accent_color <- common[["title.textStyle.textShadowColor"]]
  if (is.null(accent_color)) accent_color <- common[["toolbox.emphasis.iconStyle.borderColor"]]
  if (is.null(accent_color)) accent_color <- theme_title
  if (is.null(accent_color)) accent_color <- if (is_dark) "#38BDF8" else "#2563EB"

  if (is_dark) {

    list(
      card_background = "#020617",
      card_border = paste0("1px solid ", .ap_alpha_color(accent_color, 0.45)),
      card_shadow = paste0(
        "0 14px 35px rgba(0,0,0,0.40),",
        "0 0 22px ", .ap_alpha_color(accent_color, 0.12)
      ),

      title_color = "#EAF2FF",
      subtitle_color = "#AEBBD0",

      section_background = "#030B12",
      section_border = paste0("1px solid ", .ap_alpha_color(accent_color, 0.35)),

      tab_background = "#08111F",
      tab_border = paste0("1px solid ", .ap_alpha_color(accent_color, 0.35)),
      tab_active_background = .ap_alpha_color(accent_color, 0.35),
      tab_active_border = paste0("1px solid ", .ap_alpha_color(accent_color, 0.85)),
      tab_text_color = "#EAF2FF",
      tab_active_text_color = "#FFFFFF",

      accent_color = accent_color,
      is_dark = TRUE
    )

  } else {

    list(
      card_background = "#FFFFFF",
      card_border = paste0("1px solid ", .ap_alpha_color(accent_color, 0.30)),
      card_shadow = paste0(
        "0 12px 28px rgba(15,23,42,0.12),",
        "0 0 18px ", .ap_alpha_color(accent_color, 0.08)
      ),

      title_color = "#111827",
      subtitle_color = "#4B5563",

      section_background = "#F8FAFC",
      section_border = paste0("1px solid ", .ap_alpha_color(accent_color, 0.25)),

      tab_background = "#FFFFFF",
      tab_border = paste0("1px solid ", .ap_alpha_color(accent_color, 0.25)),
      tab_active_background = .ap_alpha_color(accent_color, 0.14),
      tab_active_border = paste0("1px solid ", .ap_alpha_color(accent_color, 0.60)),
      tab_text_color = "#111827",
      tab_active_text_color = "#111827",

      accent_color = accent_color,
      is_dark = FALSE
    )
  }
}

#' Save plot as image
#'
#' @param plot A plot object or display_plot_grid object
#' @param path Directory to save image
#' @param name name of output image. name only
#' @param height Default 1027 good for PowerPoint
#' @param width Default 1500 good for PowerPoint
#' @param delay Default 1 good for PowerPoint
#' @param zoom Default 2 good for PowerPoint
#' @return A browsable HTML grid for use in Rmarkdown, Shiny, or viewer pane.
#' @export
save_image <- function(plot, path, name, height = 1027, width = 1500, delay = 1, zoom = 2) {
  htmlwidgets::saveWidget(plot, file.path(path, paste0(name, ".html")), selfcontained = TRUE)
  webshot2::webshot(file.path(path, paste0(name, ".html")), file.path(path, paste0(name, ".png")), vwidth = width, vheight = height, delay = delay, zoom = zoom)
  file.remove(file.path(path, paste0(name, ".html")))
  unlink(file.path(path, paste0(name, "_files")), recursive = TRUE)
}


#' Display a Series of Plots in a Styled HTML Grid
#'
#' @description
#' Arrange a list of echarts4r/htmlwidgets plots into a styled HTML grid.
#' Supports optional card titles, subtitles, responsive layouts, last-row
#' behavior, HTML export, and optional code generation.
#'
#' @param plots A list of echarts4r plots, htmlwidgets, or htmltools tags.
#' @param cols Number of grid columns. Use NULL for auto-fit responsive layout.
#' @param titles Optional character vector of card titles, one per plot.
#' @param subtitles Optional character vector of card subtitles, one per plot.
#' @param gap CSS grid gap.
#' @param padding CSS padding around the full grid.
#' @param min_width Minimum card width when cols = NULL.
#' @param last_row Behavior when explicit cols leaves one plot in last row.
#'   One of "span", "left", "center", or "none".
#' @param card Logical. If TRUE, wrap each plot in a styled card.
#' @param card_background CSS background for cards.
#' @param card_border CSS border for cards.
#' @param card_radius CSS border-radius for cards.
#' @param card_padding CSS padding inside cards.
#' @param card_shadow CSS box-shadow for cards.
#' @param title_color CSS color for card titles.
#' @param subtitle_color CSS color for card subtitles.
#' @param title_size CSS font-size for card titles.
#' @param subtitle_size CSS font-size for card subtitles.
#' @param font_family CSS font-family used for titles/subtitles.
#' @param container_class CSS class for each plot container.
#' @param grid_class CSS class for the grid layout container.
#' @param background Optional CSS background for the full grid wrapper.
#' @param width CSS width of the full grid wrapper.
#' @param resizable Logical. If TRUE and `cols` is explicit, plots are grouped
#'   into vertically resizable rows and widgets fill the available row height.
#' @param row_height Default CSS height for resizable rows. May be length 1 or
#'   one value per generated row.
#' @param row_min_height Minimum CSS height for resizable rows. May be length 1
#'   or one value per generated row.
#' @param save_path Optional file path to save standalone HTML.
#' @param selfcontained Logical passed to htmlwidgets::saveWidget.
#' @param return_code Logical. If TRUE, return list(html = ..., code = ...).
#'
#' @return A browsable HTML grid, or list(html, code) when return_code = TRUE.
#'
#' @export
display_plots_grid <- function(
    plots,
    cols = NULL,
    titles = NULL,
    subtitles = NULL,
    gap = "20px",
    padding = "0px",
    min_width = "350px",
    last_row = c("span", "left", "center", "none"),
    card = TRUE,
    card_background = NULL,
    card_border = NULL,
    card_radius = "14px",
    card_padding = "14px",
    card_shadow = NULL,
    title_color = NULL,
    subtitle_color = NULL,
    title_size = "16px",
    subtitle_size = "13px",
    font_family = "Segoe UI, system-ui, -apple-system, BlinkMacSystemFont, sans-serif",
    container_class = "plot-card",
    grid_class = "plot-grid",
    background = NULL,
    width = "100%",
    resizable = TRUE,
    row_height = "420px",
    row_min_height = "320px",
    save_path = NULL,
    selfcontained = TRUE,
    return_code = FALSE
) {

  .ap_validate_plot_list(plots)

  nplots <- length(plots)
  last_row <- match.arg(last_row)

  explicit_cols <- !is.null(cols)

  if (explicit_cols) {
    if (!(is.numeric(cols) && length(cols) == 1L && is.finite(cols) && cols >= 1)) {
      stop("`cols` must be a single positive integer or NULL.", call. = FALSE)
    }
    cols <- as.integer(cols)
  }

  titles <- .ap_normalize_labels(titles, nplots, "titles")
  subtitles <- .ap_normalize_labels(subtitles, nplots, "subtitles")

  inferred_theme <- .ap_infer_theme_from_plots(plots, fallback = "dark")
  layout_theme <- .ap_get_layout_theme_defaults(inferred_theme)

  if (is.null(card_background)) card_background <- layout_theme$card_background
  if (is.null(card_border))     card_border     <- layout_theme$card_border
  if (is.null(card_shadow))     card_shadow     <- layout_theme$card_shadow
  if (is.null(title_color))     title_color     <- layout_theme$title_color
  if (is.null(subtitle_color))  subtitle_color  <- layout_theme$subtitle_color

  grid_template <- if (explicit_cols) {
    sprintf("repeat(%d,minmax(0,1fr))", cols)
  } else {
    sprintf("repeat(auto-fit,minmax(%s,1fr))", min_width)
  }

  grid_style <- paste0(
    "display:grid;",
    "grid-template-columns:", grid_template, ";",
    "gap:", gap, ";",
    "padding:", padding, ";",
    "width:", width, ";",
    if (!is.null(background)) paste0("background:", background, ";") else ""
  )

  plots_in_last_row <- if (explicit_cols) {
    r <- nplots %% cols
    if (r == 0L && nplots > 0L) cols else r
  } else {
    NA_integer_
  }

  base_card_style <- .ap_card_style(
    card = card,
    card_background = card_background,
    card_border = card_border,
    card_radius = card_radius,
    card_padding = card_padding,
    card_shadow = card_shadow
  )

  make_plot_cell <- function(i, extra_style = "") {

    header <- .ap_card_header(
      title = titles[i],
      subtitle = subtitles[i],
      title_color = title_color,
      subtitle_color = subtitle_color,
      title_size = title_size,
      subtitle_size = subtitle_size,
      font_family = font_family
    )

    htmltools::tags$div(
      class = paste(container_class, "aq-display-cell"),
      style = paste0(base_card_style, extra_style),
      header,
      .ap_wrap_display_widget(plots[[i]])
    )
  }

  wrapped_plots <- lapply(seq_len(nplots), function(i) {
    extra_style <- ""

    if (explicit_cols && plots_in_last_row == 1L && i == nplots) {
      if (identical(last_row, "span")) {
        extra_style <- sprintf("grid-column:span %d;", cols)
      } else if (identical(last_row, "center")) {
        center_col <- max(1L, ceiling(cols / 2))
        extra_style <- sprintf("grid-column:%d / span 1;", center_col)
      }
    }

    make_plot_cell(i, extra_style = extra_style)
  })

  grid_children <- htmltools::tagList(wrapped_plots)
  wrapper_class <- grid_class
  wrapper_style <- grid_style

  if (isTRUE(resizable) && explicit_cols) {
    row_starts <- seq(1L, nplots, by = cols)
    row_heights <- .ap_css_units(row_height, length(row_starts), default = "420px")
    row_min_heights <- .ap_css_units(row_min_height, length(row_starts), default = "320px")
    row_nodes <- lapply(seq_along(row_starts), function(row_i) {
      start_i <- row_starts[[row_i]]
      end_i <- min(start_i + cols - 1L, nplots)
      row_indices <- seq.int(start_i, end_i)
      row_items <- lapply(row_indices, function(plot_i) {
        extra_style <- ""
        if (length(row_indices) == 1L && cols > 1L) {
          if (identical(last_row, "span")) {
            extra_style <- sprintf("grid-column:span %d;", cols)
          } else if (identical(last_row, "center")) {
            center_col <- max(1L, ceiling(cols / 2))
            extra_style <- sprintf("grid-column:%d / span 1;", center_col)
          }
        }
        make_plot_cell(plot_i, extra_style = extra_style)
      })
      htmltools::tags$div(
        class = "aq-resizable-row",
        style = paste0(
          "--aq-row-height:", row_heights[[row_i]], ";",
          "--aq-row-min-height:", row_min_heights[[row_i]], ";",
          "--aq-display-gap:", gap, ";",
          "--aq-display-cols:", cols, ";"
        ),
        htmltools::tags$div(
          class = "aq-resizable-card",
          htmltools::tags$div(
            class = paste(grid_class, "aq-display-grid"),
            style = paste0(
              "grid-template-columns:", grid_template, ";",
              "--aq-display-cols:", cols, ";"
            ),
            htmltools::tagList(row_items)
          )
        )
      )
    })
    grid_children <- htmltools::tagList(.ap_display_resize_assets(), row_nodes)
    wrapper_class <- paste(grid_class, "aq-display-grid-wrapper")
    wrapper_style <- paste0(
      "display:flex;",
      "flex-direction:column;",
      "gap:", gap, ";",
      "padding:", padding, ";",
      "width:", width, ";",
      if (!is.null(background)) paste0("background:", background, ";") else ""
    )
  }

  out <- htmltools::browsable(
    htmltools::tags$div(
      class = wrapper_class,
      style = wrapper_style,
      grid_children
    )
  )

  if (!is.null(save_path)) {
    htmlwidgets::saveWidget(
      widget = out,
      file = save_path,
      selfcontained = selfcontained
    )
  }

  code_lines <- c(
    "AutoPlots::display_plots_grid(",
    paste0("  plots = list(", paste0("p", seq_len(nplots), collapse = ", "), "),"),
    if (!is.null(cols)) paste0("  cols = ", cols, ",") else "  cols = NULL,",
    if (!all(is.na(titles))) paste0("  titles = ", .ap_deparse_arg(titles), ",") else NULL,
    if (!all(is.na(subtitles))) paste0("  subtitles = ", .ap_deparse_arg(subtitles), ",") else NULL,
    paste0("  gap = ", .ap_deparse_arg(gap), ","),
    paste0("  card = ", if (isTRUE(card)) "TRUE" else "FALSE", ","),
    paste0("  resizable = ", if (isTRUE(resizable)) "TRUE" else "FALSE", ","),
    paste0("  last_row = ", .ap_deparse_arg(last_row)),
    ")"
  )

  generated_code <- paste(code_lines, collapse = "\n")

  if (isTRUE(return_code)) {
    return(list(
      html = out,
      code = generated_code
    ))
  }

  out
}


#' Display Plots in HTML Tabs
#'
#' @description
#' Arrange a list of plots into a tabbed HTML interface.
#'
#' @param plots A list of echarts4r plots, htmlwidgets, or htmltools tags.
#' @param tab_titles Optional tab labels. If NULL, names(plots) are used when available.
#' @param titles Optional card titles, one per plot.
#' @param subtitles Optional card subtitles, one per plot.
#' @param selected Initial selected tab index.
#' @param card Logical. If TRUE, wrap each plot in a styled card.
#' @param card_background CSS background for cards.
#' @param card_border CSS border for cards.
#' @param card_radius CSS border-radius for cards.
#' @param card_padding CSS padding inside cards.
#' @param card_shadow CSS box-shadow for cards.
#' @param title_color CSS color for card titles.
#' @param subtitle_color CSS color for card subtitles.
#' @param title_size CSS font-size for card titles.
#' @param subtitle_size CSS font-size for card subtitles.
#' @param font_family CSS font-family.
#' @param tabs_class CSS class for wrapper.
#' @param save_path Optional file path to save standalone HTML.
#' @param selfcontained Logical passed to htmlwidgets::saveWidget.
#' @param return_code Logical. If TRUE, return list(html = ..., code = ...).
#'
#' @return A browsable HTML tab set, or list(html, code) when return_code = TRUE.
#'
#' @export
display_plots_tabs <- function(
    plots,
    tab_titles = NULL,
    titles = NULL,
    subtitles = NULL,
    selected = 1L,
    card = TRUE,
    card_background = NULL,
    card_border = NULL,
    card_radius = "14px",
    card_padding = "14px",
    card_shadow = NULL,
    title_color = NULL,
    subtitle_color = NULL,
    title_size = "16px",
    subtitle_size = "13px",
    font_family = "Segoe UI, system-ui, -apple-system, BlinkMacSystemFont, sans-serif",
    tabs_class = "plot-tabs",
    save_path = NULL,
    selfcontained = TRUE,
    return_code = FALSE
) {

  .ap_validate_plot_list(plots)

  nplots <- length(plots)

  if (is.null(tab_titles)) {
    nms <- names(plots)
    if (!is.null(nms) && all(nzchar(nms))) {
      tab_titles <- nms
    } else {
      tab_titles <- paste0("Plot ", seq_len(nplots))
    }
  }

  tab_titles <- .ap_normalize_labels(tab_titles, nplots, "tab_titles")
  titles <- .ap_normalize_labels(titles, nplots, "titles")
  subtitles <- .ap_normalize_labels(subtitles, nplots, "subtitles")

  inferred_theme <- .ap_infer_theme_from_plots(plots, fallback = "dark")
  layout_theme <- .ap_get_layout_theme_defaults(inferred_theme)

  if (is.null(card_background)) card_background <- layout_theme$card_background
  if (is.null(card_border))     card_border     <- layout_theme$card_border
  if (is.null(card_shadow))     card_shadow     <- layout_theme$card_shadow
  if (is.null(title_color))     title_color     <- layout_theme$title_color
  if (is.null(subtitle_color))  subtitle_color  <- layout_theme$subtitle_color

  tab_background <- layout_theme$tab_background
  tab_border <- layout_theme$tab_border
  tab_active_background <- layout_theme$tab_active_background
  tab_active_border <- layout_theme$tab_active_border
  tab_text_color <- layout_theme$tab_text_color

  if (!(is.numeric(selected) && length(selected) == 1L && selected >= 1L && selected <= nplots)) {
    stop("`selected` must be a single integer between 1 and length(plots).", call. = FALSE)
  }
  selected <- as.integer(selected)

  uid <- paste0("ap_tabs_", as.integer(stats::runif(1L, 1e6, 9e6)))

  base_card_style <- .ap_card_style(
    card = card,
    card_background = card_background,
    card_border = card_border,
    card_radius = card_radius,
    card_padding = card_padding,
    card_shadow = card_shadow
  )

  css <- htmltools::tags$style(htmltools::HTML(sprintf("
    #%s .ap-tab-buttons {
      display:flex;
      flex-wrap:wrap;
      gap:8px;
      margin-bottom:14px;
      font-family:%s;
    }
    #%s .ap-tab-button {
      cursor:pointer;
      border:%s;
      background:%s;
      color:%s;
      border-radius:999px;
      padding:8px 13px;
      font-size:13px;
      line-height:1;
    }
    #%s .ap-tab-button.active {
      background:%s;
      border:%s;
      color:%s;
    }
    #%s .ap-tab-panel {
      display:none;
    }
    #%s .ap-tab-panel.active {
      display:block;
    }
  ",
                                                       uid, font_family,
                                                       uid, tab_border, tab_background, tab_text_color,
                                                       uid, tab_active_background, tab_active_border, tab_text_color,
                                                       uid, uid
  )))

  js <- htmltools::tags$script(htmltools::HTML(sprintf("
    (function() {
      var root = document.getElementById('%s');
      if (!root) return;

      var buttons = root.querySelectorAll('.ap-tab-button');
      var panels = root.querySelectorAll('.ap-tab-panel');

      function activate(idx) {
        buttons.forEach(function(btn, i) {
          btn.classList.toggle('active', i === idx);
        });
        panels.forEach(function(panel, i) {
          panel.classList.toggle('active', i === idx);
        });
      }

      buttons.forEach(function(btn, i) {
        btn.addEventListener('click', function() {
          activate(i);
          setTimeout(function() {
            window.dispatchEvent(new Event('resize'));
          }, 50);
        });
      });

      activate(%d);
    })();
  ", uid, selected - 1L)))

  buttons <- lapply(seq_len(nplots), function(i) {
    htmltools::tags$button(
      type = "button",
      class = paste("ap-tab-button", if (i == selected) "active" else ""),
      `data-index` = i - 1L,
      tab_titles[i]
    )
  })

  panels <- lapply(seq_len(nplots), function(i) {
    header <- .ap_card_header(
      title = titles[i],
      subtitle = subtitles[i],
      title_color = title_color,
      subtitle_color = subtitle_color,
      title_size = title_size,
      subtitle_size = subtitle_size,
      font_family = font_family
    )

    htmltools::tags$div(
      class = paste("ap-tab-panel", if (i == selected) "active" else ""),
      htmltools::tags$div(
        style = base_card_style,
        header,
        plots[[i]]
      )
    )
  })

  out <- htmltools::browsable(
    htmltools::tags$div(
      id = uid,
      class = tabs_class,
      css,
      htmltools::tags$div(class = "ap-tab-buttons", htmltools::tagList(buttons)),
      htmltools::tagList(panels),
      js
    )
  )

  if (!is.null(save_path)) {
    htmlwidgets::saveWidget(
      widget = out,
      file = save_path,
      selfcontained = selfcontained
    )
  }

  code_lines <- c(
    "AutoPlots::display_plots_tabs(",
    paste0("  plots = list(", paste0("p", seq_len(nplots), collapse = ", "), "),"),
    paste0("  tab_titles = ", .ap_deparse_arg(tab_titles), ","),
    if (!all(is.na(titles))) paste0("  titles = ", .ap_deparse_arg(titles), ",") else NULL,
    if (!all(is.na(subtitles))) paste0("  subtitles = ", .ap_deparse_arg(subtitles), ",") else NULL,
    paste0("  selected = ", selected, ","),
    paste0("  card = ", if (isTRUE(card)) "TRUE" else "FALSE"),
    ")"
  )

  generated_code <- paste(code_lines, collapse = "\n")

  if (isTRUE(return_code)) {
    return(list(
      html = out,
      code = generated_code
    ))
  }

  out
}


#' Display Plots in Styled Sections
#'
#' @description
#' Arrange plots into named report-style sections. Each section uses
#' display_plots_grid() internally.
#'
#' @param sections A named list. Each element should be a list of plots.
#' @param cols Number of columns per section.
#' @param collapsible Logical. If TRUE, sections use HTML details/summary.
#' @param open Logical. If TRUE, collapsible sections are open by default.
#' @param section_titles Optional section titles. Defaults to names(sections).
#' @param section_subtitles Optional section subtitles.
#' @param plot_titles Optional named list of title vectors by section.
#' @param plot_subtitles Optional named list of subtitle vectors by section.
#' @param gap CSS grid gap inside sections.
#' @param section_gap CSS gap between sections.
#' @param padding CSS padding around the full report.
#' @param card Logical. If TRUE, wrap plots in cards.
#' @param card_background CSS background for plot cards. Defaults to inferred theme.
#' @param card_border CSS border for plot cards. Defaults to inferred theme.
#' @param card_shadow CSS box-shadow for plot cards. Defaults to inferred theme.
#' @param section_background CSS background for section wrappers.
#' @param section_border CSS border for section wrappers.
#' @param section_radius CSS border-radius for section wrappers.
#' @param section_padding CSS padding for section wrappers.
#' @param title_color CSS color for section titles.
#' @param subtitle_color CSS color for section subtitles.
#' @param font_family CSS font-family.
#' @param resizable Logical. If TRUE, each section's grid is grouped into
#'   vertically resizable rows by `cols`.
#' @param row_height Default CSS height for resizable rows. May be length 1 or
#'   one value per generated row inside each section.
#' @param row_min_height Minimum CSS height for resizable rows. May be length 1
#'   or one value per generated row inside each section.
#' @param save_path Optional file path to save standalone HTML.
#' @param selfcontained Logical passed to htmlwidgets::saveWidget.
#' @param return_code Logical. If TRUE, return list(html = ..., code = ...).
#'
#' @return A browsable sectioned HTML report, or list(html, code) when return_code = TRUE.
#'
#' @export
display_plots_sections <- function(
    sections,
    cols = 2,
    collapsible = TRUE,
    open = TRUE,
    section_titles = NULL,
    section_subtitles = NULL,
    plot_titles = NULL,
    plot_subtitles = NULL,
    gap = "20px",
    section_gap = "22px",
    padding = "0px",
    card = TRUE,
    card_background = NULL,
    card_border = NULL,
    card_shadow = NULL,
    section_background = NULL,
    section_border = NULL,
    section_radius = "16px",
    section_padding = "18px",
    title_color = NULL,
    subtitle_color = NULL,
    font_family = "Segoe UI, system-ui, -apple-system, BlinkMacSystemFont, sans-serif",
    resizable = TRUE,
    row_height = "420px",
    row_min_height = "320px",
    save_path = NULL,
    selfcontained = TRUE,
    return_code = FALSE
) {

  if (!is.list(sections) || length(sections) == 0L) {
    stop("`sections` must be a non-empty named list of plot lists.", call. = FALSE)
  }

  nsections <- length(sections)

  if (is.null(section_titles)) {
    nms <- names(sections)
    if (!is.null(nms) && all(nzchar(nms))) {
      section_titles <- nms
    } else {
      section_titles <- paste0("Section ", seq_len(nsections))
    }
  }

  section_titles <- .ap_normalize_labels(section_titles, nsections, "section_titles")
  section_subtitles <- .ap_normalize_labels(section_subtitles, nsections, "section_subtitles")

  section_names <- names(sections)
  if (is.null(section_names) || any(!nzchar(section_names))) {
    section_names <- section_titles
  }

  flat_plots <- .ap_flatten_plot_sections(sections)
  inferred_theme <- .ap_infer_theme_from_plots(flat_plots, fallback = "dark")
  layout_theme <- .ap_get_layout_theme_defaults(inferred_theme)

  if (is.null(section_background)) section_background <- layout_theme$section_background
  if (is.null(section_border))     section_border     <- layout_theme$section_border
  if (is.null(card_background))    card_background    <- layout_theme$card_background
  if (is.null(card_border))        card_border        <- layout_theme$card_border
  if (is.null(card_shadow))        card_shadow        <- layout_theme$card_shadow
  if (is.null(title_color))        title_color        <- layout_theme$title_color
  if (is.null(subtitle_color))     subtitle_color     <- layout_theme$subtitle_color

  section_style <- paste0(
    "background:", section_background, ";",
    "border:", section_border, ";",
    "border-radius:", section_radius, ";",
    "padding:", section_padding, ";",
    "overflow:hidden;"
  )

  wrapper_style <- paste0(
    "display:flex;",
    "flex-direction:column;",
    "gap:", section_gap, ";",
    "padding:", padding, ";"
  )

  section_header_style <- paste0(
    "font-family:", font_family, ";",
    "font-size:20px;",
    "font-weight:800;",
    "color:", title_color, ";",
    "margin:0 0 4px 0;"
  )

  section_subtitle_style <- paste0(
    "font-family:", font_family, ";",
    "font-size:13px;",
    "font-weight:400;",
    "color:", subtitle_color, ";",
    "margin:0 0 16px 0;"
  )

  section_nodes <- lapply(seq_len(nsections), function(i) {

    section_plot_list <- sections[[i]]
    .ap_validate_plot_list(section_plot_list, arg = paste0("sections[[", i, "]]"))

    sec_name <- section_names[i]

    p_titles <- NULL
    p_subtitles <- NULL

    if (!is.null(plot_titles)) {
      p_titles <- plot_titles[[sec_name]]
      if (is.null(p_titles)) p_titles <- plot_titles[[i]]
    }

    if (!is.null(plot_subtitles)) {
      p_subtitles <- plot_subtitles[[sec_name]]
      if (is.null(p_subtitles)) p_subtitles <- plot_subtitles[[i]]
    }

    grid <- display_plots_grid(
      plots = section_plot_list,
      cols = cols,
      titles = p_titles,
      subtitles = p_subtitles,
      gap = gap,
      padding = "0px",
      card = card,
      card_background = card_background,
      card_border = card_border,
      card_shadow = card_shadow,
      title_color = title_color,
      subtitle_color = subtitle_color,
      resizable = resizable,
      row_height = row_height,
      row_min_height = row_min_height,
      return_code = FALSE
    )

    header <- htmltools::tagList(
      htmltools::tags$div(
        style = section_header_style,
        section_titles[i]
      ),
      if (!is.na(section_subtitles[i]) && nzchar(section_subtitles[i])) {
        htmltools::tags$div(
          style = section_subtitle_style,
          section_subtitles[i]
        )
      }
    )

    content <- htmltools::tags$div(
      style = section_style,
      header,
      grid
    )

    if (isTRUE(collapsible)) {
      htmltools::tags$details(
        open = if (isTRUE(open)) NA else NULL,
        style = section_style,
        htmltools::tags$summary(
          style = paste0(
            "font-family:", font_family, ";",
            "font-size:20px;",
            "font-weight:800;",
            "color:", title_color, ";",
            "cursor:pointer;",
            "margin-bottom:14px;"
          ),
          section_titles[i]
        ),
        if (!is.na(section_subtitles[i]) && nzchar(section_subtitles[i])) {
          htmltools::tags$div(
            style = section_subtitle_style,
            section_subtitles[i]
          )
        },
        grid
      )
    } else {
      content
    }
  })

  out <- htmltools::browsable(
    htmltools::tags$div(
      class = "plot-sections",
      style = wrapper_style,
      htmltools::tagList(section_nodes)
    )
  )

  if (!is.null(save_path)) {
    htmlwidgets::saveWidget(
      widget = out,
      file = save_path,
      selfcontained = selfcontained
    )
  }

  code_lines <- c(
    "AutoPlots::display_plots_sections(",
    "  sections = list(",
    paste0(
      "    ", .ap_deparse_arg(section_titles), " = list(...)",
      collapse = ",\n"
    ),
    "  ),",
    paste0("  cols = ", cols, ","),
    paste0("  collapsible = ", if (isTRUE(collapsible)) "TRUE" else "FALSE", ","),
    paste0("  open = ", if (isTRUE(open)) "TRUE" else "FALSE", ","),
    paste0("  card = ", if (isTRUE(card)) "TRUE" else "FALSE"),
    ")"
  )

  generated_code <- paste(code_lines, collapse = "\n")

  if (isTRUE(return_code)) {
    return(list(
      html = out,
      code = generated_code
    ))
  }

  out
}

qa_resizable_display_plots <- function() {
  plots <- list(
    htmltools::tags$div(class = "html-widget", "plot one"),
    htmltools::tags$div(class = "html-widget", "plot two"),
    htmltools::tags$div(class = "html-widget", "plot three"),
    htmltools::tags$div(class = "html-widget", "plot four"),
    htmltools::tags$div(class = "html-widget", "plot five")
  )

  grid <- display_plots_grid(
    plots = plots,
    cols = 2L,
    titles = c("One", "Two", "Three", "Four", "Five"),
    subtitles = c("bar / qa", "line / qa", "heatmap / qa", "box / qa", "scatter / qa"),
    resizable = TRUE,
    row_height = c("420px", "520px", "360px"),
    row_min_height = "340px"
  )
  grid_markup <- paste(as.character(grid), collapse = "\n")

  sections <- display_plots_sections(
    sections = list(QA = plots[1:2]),
    cols = 2L,
    collapsible = FALSE,
    resizable = TRUE,
    row_height = "480px"
  )
  section_markup <- paste(as.character(sections), collapse = "\n")
  css <- .ap_display_resize_css()
  js <- .ap_display_resize_js()

  data.table::data.table(
    check = c(
      "grid_has_resizable_rows",
      "grid_splits_artifacts_into_rows",
      "grid_row_height_vector",
      "grid_has_display_cells",
      "single_final_row_spans",
      "widgets_fill_height_css",
      "resize_observer_js",
      "window_resize_js",
      "sections_forward_resizable_rows",
      "resizable_can_be_disabled"
    ),
    status = c(
      if (grepl("aq-resizable-row", grid_markup, fixed = TRUE)) "success" else "error",
      if (length(gregexpr("--aq-row-height:", grid_markup, fixed = TRUE)[[1L]]) == 3L) "success" else "error",
      if (grepl("--aq-row-height:420px", grid_markup, fixed = TRUE) && grepl("--aq-row-height:520px", grid_markup, fixed = TRUE) && grepl("--aq-row-height:360px", grid_markup, fixed = TRUE)) "success" else "error",
      if (length(gregexpr("aq-display-cell", grid_markup, fixed = TRUE)[[1L]]) >= 5L) "success" else "error",
      if (grepl("grid-column:span 2", grid_markup, fixed = TRUE)) "success" else "error",
      if (grepl("height:100%!important", css, fixed = TRUE)) "success" else "error",
      if (grepl("ResizeObserver", js, fixed = TRUE)) "success" else "error",
      if (grepl("dispatchEvent", js, fixed = TRUE)) "success" else "error",
      if (grepl("aq-resizable-row", section_markup, fixed = TRUE)) "success" else "error",
      {
        plain <- display_plots_grid(plots = plots[1:2], cols = 2L, resizable = FALSE)
        plain_markup <- paste(as.character(plain), collapse = "\n")
        if (!grepl("aq-resizable-row", plain_markup, fixed = TRUE)) "success" else "error"
      }
    ),
    message = c(
      "display_plots_grid() creates vertically resizable rows.",
      "More than one row of plots is split into separate row-level resize containers.",
      "A row_height vector can set different default heights per row.",
      "Multiple plots render inside display cells.",
      "A single final plot spans the full row by default.",
      "HTML widget containers fill available card height.",
      "ResizeObserver is included to react to manual row resizing.",
      "Window resize events are dispatched after row size changes.",
      "display_plots_sections() forwards resizable row behavior to section grids.",
      "Resizable behavior remains optional."
    )
  )
}

