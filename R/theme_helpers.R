get_theme_defaults_common <- function(theme = "dark") {
  switch(
    theme,
    dark = list(
      title.textStyle.color = "#DEEAFC",
      title.textStyle.fontWeight = "bolder",
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
    macarons = list(
      title.textStyle.color = "#006066",
      title.textStyle.fontWeight = "bolder",
      title.textStyle.textShadowColor = "#00969E",
      title.textStyle.textShadowBlur = 15,
      title.textStyle.textShadowOffsetX = 0,
      title.textStyle.textShadowOffsetY = 0,
      title.top = 12,
      title.left = 12,
      title.padding = c(4, 4, 10, 4),
      title.itemGap = 6,
      title.subtextStyle.color = "#426B6E",
      title.subtextStyle.textShadowColor = "#00969E",
      title.subtextStyle.textShadowBlur = 15,
      title.subtextStyle.textShadowOffsetX = 0,
      title.subtextStyle.textShadowOffsetY = 0,
      title.subtextStyle.fontWeight = "bold",
      xAxis.nameTextStyle.fontSize = 20,
      xAxis.nameTextStyle.color = "#006769",
      xAxis.axisLabel.color = "#004142",
      xAxis.axisLabel.overflow = "truncate",
      yAxis.nameTextStyle.fontSize = 20,
      yAxis.nameTextStyle.padding = 60,
      yAxis.nameTextStyle.color = "#006769",
      yAxis.axisLabel.color = "#004142",
      tooltip.backgroundColor = "#004142",
      tooltip.textStyle.color = "#C4FEFF",
      toolbox.iconStyle.borderColor = "#00C1C4",
      toolbox.emphasis.iconStyle.borderColor = "#006466"
    ),
    NULL
  )
}

get_theme_defaults_plot <- function(theme = "dark", plot_type = NULL, grouped = FALSE) {
  if (is.null(plot_type)) return(NULL)

  switch(
    theme,
    macarons = switch(
      plot_type,
      Area = if(grouped) {
        NULL
      } else {
        list(
          areaStyle.color = c("#00A5A8", "#00D5D9", "#38F8FF"),
          areaStyle.opacity = c(1.0, 0.75, 0.25)
        )
      },
      Line = if(grouped) {
        NULL
      } else {
        list(
          lineStyle.color = c("#00A5A8", "#00D5D9", "#38F8FF")
        )
      },
      Step = if(grouped) {
        NULL
      } else {
        list(
          lineStyle.color = c("#00A5A8", "#00D5D9", "#38F8FF")
        )
      },
      Parallel = list(
        lineStyle.color = c("#00D5D9")
      ),
      Bar = if(grouped) {
        NULL
      } else {
        list(
          backgroundStyle.color = c("#00A5A8", "#00D5D9", "#38F8FF"),
          backgroundStyle.opacity = c(1.0, 0.75, 0.25)
        )
      },
      Histogram = if(grouped) {
        NULL
      } else {
        list(
          backgroundStyle.color = c("#00A5A8", "#00D5D9", "#38F8FF"),
          backgroundStyle.opacity = c(1.0, 0.75, 0.25)
        )
      },
      Density = if(grouped) {
        NULL
      } else {
        list(
          areaStyle.color = c("#00A5A8", "#00D5D9", "#38F8FF"),
          areaStyle.opacity = c(1.0, 0.75, 0.25)
        )
      },
      Box = list(
        itemStyle.color = c("#00A5A8", "#00D5D9", "#38F8FF"),
        itemStyle.opacity = c(1.0, 0.75, 0.25)
      ),
      NULL
    ),
    dark = switch(
      plot_type,
      Area = if(grouped) {
        NULL
      } else {
        list(
          areaStyle.color = c("#DEEAFC", "#91C1FF", "#1474FF"),
          areaStyle.opacity = c(1.0, 0.75, 0.35)
        )
      },
      Line = if(grouped) {
        NULL
      } else {
        list(
          lineStyle.color = c("#DEEAFC", "#91C1FF", "#1474FF")
        )
      },
      Step = if(grouped) {
        NULL
      } else {
        list(
          lineStyle.color = c("#DEEAFC", "#91C1FF", "#1474FF")
        )
      },
      Bar = if(grouped) {
        NULL
      } else {
        list(
          backgroundStyle.color = c("#DEEAFC", "#91C1FF", "#1474FF"),
          backgroundStyle.opacity = c(1.0, 0.75, 0.35)
        )
      },
      Histogram = if(grouped) {
        NULL
      } else {
        list(
          backgroundStyle.color = c("#DEEAFC", "#91C1FF", "#1474FF"),
          backgroundStyle.opacity = c(1.0, 0.75, 0.35)
        )
      },
      Density = if(grouped) {
        NULL
      } else {
        list(
          areaStyle.color = c("#DEEAFC", "#91C1FF", "#1474FF"),
          areaStyle.opacity = c(1.0, 0.75, 0.35)
        )
      },
      Box = list(
        itemStyle.color = c("#DEEAFC", "#91C1FF", "#1474FF"),
        itemStyle.opacity = c(1.0, 0.75, 0.35)
      ),
      Parallel = list(
        lineStyle.color = c("#91C1FF"),
        lineStyle.width = 0.2
      ),
      Radar = list(
        lineStyle.color = c("#00BFFF", "#FF69B4", "#32CD32")
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

apply_theme_defaults <- function(theme, plot_type = NULL, grouped = FALSE, env = parent.frame()) {
  set_null_defaults(get_theme_defaults_common(theme), env = env)
  set_null_defaults(get_theme_defaults_plot(theme, plot_type, grouped = grouped), env = env)
  invisible(NULL)
}
