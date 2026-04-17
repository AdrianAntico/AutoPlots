get_theme_defaults_common <- function(theme = "dark") {
  switch(
    theme,
    dark = list(
      title.textStyle.color = "#DEEAFC",
      title.textStyle.textShadowColor = "#5298FF",
      title.textStyle.textShadowBlur = 15,
      title.textStyle.textShadowOffsetX = 0,
      title.textStyle.textShadowOffsetY = 0,
      title.top = 12,
      title.left = 12,
      title.padding = c(4, 4, 10, 4),
      title.itemGap = 6,
      title.subtextStyle.color = "#9BAAC2",
      title.subtextStyle.textShadowColor = "#5298FF",
      title.subtextStyle.textShadowBlur = 15,
      title.subtextStyle.textShadowOffsetX = 0,
      title.subtextStyle.textShadowOffsetY = 0,
      title.subtextStyle.fontWeight = "bold",
      xAxis.nameTextStyle.fontSize = 20,
      xAxis.nameTextStyle.color = "#DEEAFC",
      xAxis.axisLabel.color = "#ABD5FF",
      xAxis.axisLabel.overflow = "truncate",
      yAxis.nameTextStyle.fontSize = 20,
      yAxis.nameTextStyle.padding = 60,
      yAxis.nameTextStyle.color = "#DEEAFC",
      yAxis.axisLabel.color = "#ABD5FF",
      tooltip.backgroundColor = "#002259",
      tooltip.textStyle.color = "#91C1FF",
      toolbox.iconStyle.borderColor = "#DEEAFC",
      toolbox.emphasis.iconStyle.borderColor = "#5E8DD1"
    ),
    NULL
  )
}

get_theme_defaults_plot <- function(theme = "dark", plot_type = NULL) {
  if (is.null(plot_type)) return(NULL)

  switch(
    theme,
    dark = switch(
      plot_type,
      Area = list(
        areaStyle.color = c("#DEEAFC", "#91C1FF", "#1474FF"),
        areaStyle.opacity = c(1.0, 0.75, 0.35)
      ),
      Line = list(
        lineStyle.color = c("#DEEAFC", "#91C1FF", "#1474FF")
      ),
      Step = list(
        lineStyle.color = c("#DEEAFC", "#91C1FF", "#1474FF")
      ),
      Parallel = list(
        lineStyle.color = c("#91C1FF"),
        lineStyle.width = 0.2
      ),
      Radar = list(
        lineStyle.color = c("#00BFFF", "#FF69B4", "#32CD32")
      ),
      Bar = list(
        backgroundStyle.color = c("#DEEAFC", "#91C1FF", "#1474FF"),
        backgroundStyle.opacity = c(1.0, 0.75, 0.35)
      ),
      Histogram = list(
        backgroundStyle.color = c("#DEEAFC", "#91C1FF", "#1474FF"),
        backgroundStyle.opacity = c(1.0, 0.75, 0.35)
      ),
      Density = list(
        areaStyle.color = c("#DEEAFC", "#91C1FF", "#1474FF"),
        areaStyle.opacity = c(1.0, 0.75, 0.35)
      ),
      Box = list(
        itemStyle.color = c("#DEEAFC", "#91C1FF", "#1474FF"),
        itemStyle.opacity = c(1.0, 0.75, 0.35)
      ),
      River = list(
        itemStyle.color = c("#FF4C4C", "#00BFFF", "#FFD700", "#32CD32", "#FF69B4")
      ),
      CorrMatrix = list(
        visualMap.InRange.color = c("white", "gray", "darkblue"),
        ShowLabels <- TRUE
      ),
      HeatMap = list(
        label.show = TRUE,
        label.fontWeight = "bolder",
        emphasis.shadowColor = "white",
        emphasis.shadowBlur = 10,
        visualMap.InRange.color = c("blue", "white", "red")
      ),
      NULL
    ),
    NULL
  )
}

set_null_defaults <- function(defaults, env) {
  if (is.null(defaults)) {
    return(invisible(NULL))
  }

  stopifnot(is.list(defaults))
  stopifnot(is.environment(env))

  nms <- names(defaults)
  if (is.null(nms)) {
    stop("`defaults` must be a named list.")
  }

  valid <- !is.na(nms) & nzchar(nms)

  for (i in which(valid)) {
    nm <- nms[[i]]

    if (!exists(nm, envir = env, inherits = FALSE)) next

    current_value <- get(nm, envir = env, inherits = FALSE)

    if (is.null(current_value)) {
      assign(nm, defaults[[i]], envir = env)
    }
  }

  invisible(NULL)
}

apply_theme_defaults <- function(theme, plot_type = NULL, env = parent.frame()) {
  set_null_defaults(get_theme_defaults_common(theme), env = env)
  set_null_defaults(get_theme_defaults_plot(theme, plot_type), env = env)
  invisible(NULL)
}
