# compact helper to drop NULLs
.compact <- function(x) x[!vapply(x, is.null, is.logical(1))]

#' Enhanced X-Axis Setter for echarts4r
#'
#' Exposes every xAxis.* option so you don't have to hand-craft the JSON.
#'
#' @param e An echarts4r object
#' @param axis "x"
#' @param serie NULL
#' @param index Which x axis to target (zero-based). Default: 0
#' @param xAxis.title Axis title
#' @param xAxis.min Min value
#' @param xAxis.max Max value
#' @param xAxis.position 'bottom', 'top'
#' @param xAxis.nameLocation 'center', 'start', 'middle'
#' @param xAxis.axisTick.customValues Custom values for the x-axis
#' @param xAxis.nameTextStyle.color hex or named color
#' @param xAxis.nameTextStyle.padding numeric
#' @param xAxis.nameTextStyle.align 'center', 'left', 'right'
#' @param xAxis.nameTextStyle.fontStyle 'italic', 'normal' 'oblique'
#' @param xAxis.nameTextStyle.fontWeight 'normal', 'bold', 'bolder', 'lighter'
#' @param xAxis.nameTextStyle.fontSize numeric
#' @param xAxis.nameTextStyle.fontFamily 'sans-serif', 'serif', 'monospace', 'Arial', 'Times New Roman', 'Roboto', 'Open Sans', 'Lato', 'Helvetica', 'Georgia', 'Verdana', 'Arial', 'Tahoma', 'Courier New'
#' @param xAxis.splitNumber numeric. Increments for numeric axis labels
#' @param xAxis.axisLabel.rotate numeric
#' @param xAxis.axisLabel.margin numeric
#' @param xAxis.axisLabel.color hex or named
#' @param xAxis.axisLabel.fontStyle 'italic', 'normal' 'oblique'
#' @param xAxis.axisLabel.fontWeight 'normal', 'bold', 'bolder', 'lighter'
#' @param xAxis.axisLabel.fontFamily 'sans-serif', 'serif', 'monospace', 'Arial', 'Times New Roman', 'Roboto', 'Open Sans', 'Lato', 'Helvetica', 'Georgia', 'Verdana', 'Arial', 'Tahoma', 'Courier New'
#' @param xAxis.axisLabel.fontSize numeric
#' @param xAxis.axisLabel.align 'center', 'left', 'right'
#' @param xAxis.axisLabel.verticalAlign 'top', 'bottom'
#' @param xAxis.axisLabel.backgroundColor hex or named
#' @param xAxis.axisLabel.borderColor hex or named
#' @param xAxis.axisLabel.borderWidth numeric
#' @param xAxis.axisLabel.borderType 'solid', 'dashed', 'dotted'
#' @param xAxis.axisLabel.borderRadius numeric
#' @param xAxis.axisLabel.padding numeric
#' @param xAxis.axisLabel.shadowColor hex or named
#' @param xAxis.axisLabel.shadowBlur numeric
#' @param xAxis.axisLabel.shadowOffsetX numeric
#' @param xAxis.axisLabel.shadowOffsetY numeric
#' @param xAxis.axisLabel.textBorderColor hex or named
#' @param xAxis.axisLabel.textBorderWidth numeric
#' @param xAxis.axisLabel.textBorderType 'solid', 'dashed', 'dotted'
#' @param xAxis.axisLabel.textShadowColor hex or named
#' @param xAxis.axisLabel.textShadowBlur numeric
#' @param xAxis.axisLabel.textShadowOffsetX numeric
#' @param xAxis.axisLabel.textShadowOffsetY numeric
#' @param xAxis.axisLabel.overflow 'truncate'
#'
#' @return The modified echarts4r object
#' @export
#'
e_x_axis_full <- function(
    e,
    index = 0,
    serie = NULL,
    axis = "x",
    xAxis.title = NULL,
    xAxis.position = "bottom",
    xAxis.nameLocation = "center",
    xAxis.axisTick.customValues = NULL,
    xAxis.nameTextStyle.color = NULL,
    xAxis.nameTextStyle.padding = 15,
    xAxis.nameTextStyle.align = NULL,
    xAxis.nameTextStyle.fontStyle = NULL,
    xAxis.nameTextStyle.fontWeight = NULL,
    xAxis.nameTextStyle.fontSize = NULL,
    xAxis.nameTextStyle.fontFamily = NULL,
    xAxis.min = NULL,
    xAxis.max = NULL,
    xAxis.splitNumber = NULL,
    xAxis.axisLabel.rotate = NULL,
    xAxis.axisLabel.margin = NULL,
    xAxis.axisLabel.color = NULL,
    xAxis.axisLabel.fontStyle = NULL,
    xAxis.axisLabel.fontWeight = NULL,
    xAxis.axisLabel.fontFamily = NULL,
    xAxis.axisLabel.fontSize = NULL,
    xAxis.axisLabel.align = NULL,
    xAxis.axisLabel.verticalAlign = NULL,
    xAxis.axisLabel.backgroundColor = NULL,
    xAxis.axisLabel.borderColor = NULL,
    xAxis.axisLabel.borderWidth = NULL,
    xAxis.axisLabel.borderType = NULL,
    xAxis.axisLabel.borderRadius = NULL,
    xAxis.axisLabel.padding = NULL,
    xAxis.axisLabel.shadowColor = NULL,
    xAxis.axisLabel.shadowBlur = NULL,
    xAxis.axisLabel.shadowOffsetX = NULL,
    xAxis.axisLabel.shadowOffsetY = NULL,
    xAxis.axisLabel.textBorderColor = NULL,
    xAxis.axisLabel.textBorderWidth = NULL,
    xAxis.axisLabel.textBorderType = NULL,
    xAxis.axisLabel.textShadowColor = NULL,
    xAxis.axisLabel.textShadowBlur = NULL,
    xAxis.axisLabel.textShadowOffsetX = NULL,
    xAxis.axisLabel.textShadowOffsetY = NULL,
    xAxis.axisLabel.overflow = NULL) {

  # Build the nameTextStyle list
  nts <- .compact(list(
    color      = xAxis.nameTextStyle.color,
    padding    = xAxis.nameTextStyle.padding,
    align      = xAxis.nameTextStyle.align,
    fontStyle  = xAxis.nameTextStyle.fontStyle,
    fontWeight = xAxis.nameTextStyle.fontWeight,
    fontSize   = xAxis.nameTextStyle.fontSize,
    fontFamily = xAxis.nameTextStyle.fontFamily
  ))

  # Build the axisLabel list
  al <- .compact(list(
    rotate        = xAxis.axisLabel.rotate,
    margin        = xAxis.axisLabel.margin,
    color         = xAxis.axisLabel.color,
    fontStyle     = xAxis.axisLabel.fontStyle,
    fontWeight    = xAxis.axisLabel.fontWeight,
    fontFamily    = xAxis.axisLabel.fontFamily,
    fontSize      = xAxis.axisLabel.fontSize,
    align         = xAxis.axisLabel.align,
    verticalAlign = xAxis.axisLabel.verticalAlign,
    backgroundColor = xAxis.axisLabel.backgroundColor,
    borderColor     = xAxis.axisLabel.borderColor,
    borderWidth     = xAxis.axisLabel.borderWidth,
    borderType      = xAxis.axisLabel.borderType,
    borderRadius    = xAxis.axisLabel.borderRadius,
    padding         = xAxis.axisLabel.padding,
    shadowColor     = xAxis.axisLabel.shadowColor,
    shadowBlur      = xAxis.axisLabel.shadowBlur,
    shadowOffsetX   = xAxis.axisLabel.shadowOffsetX,
    shadowOffsetY   = xAxis.axisLabel.shadowOffsetY,
    textBorderColor = xAxis.axisLabel.textBorderColor,
    textBorderWidth = xAxis.axisLabel.textBorderWidth,
    textBorderType  = xAxis.axisLabel.textBorderType,
    textShadowColor = xAxis.axisLabel.textShadowColor,
    textShadowBlur  = xAxis.axisLabel.textShadowBlur,
    textShadowOffsetX = xAxis.axisLabel.textShadowOffsetX,
    textShadowOffsetY = xAxis.axisLabel.textShadowOffsetY,
    overflow        = xAxis.axisLabel.overflow
  ))

  at <- .compact(list(
    customValues = xAxis.axisTick.customValues
  ))

  # Assemble the final opts
  opts <- .compact(list(
    nameTextStyle = if (length(nts)) nts,
    axisTick      = if (length(at)) at,
    min           = xAxis.min,
    max           = xAxis.max,
    position      = xAxis.position,
    splitNumber   = xAxis.splitNumber,
    axisLabel     = if (length(al)) al
  ))

  # Call the internal helper
  do.call(echarts4r::e_axis_, c(
    list(
      e = e,
      axis = axis,
      index = index,
      name = xAxis.title,
      nameLocation = xAxis.nameLocation
    ),
    opts
  ))
}


#' Enhanced Y-Axis Setter for echarts4r
#'
#' Exposes every yAxis.* option so you don't have to hand-craft the JSON.
#'
#' @param e An echarts4r object
#' @param axis "y"
#' @param serie NULL
#' @param index Which y axis to target (zero-based). Default: 0
#' @param yAxis.title Axis title
#' @param yAxis.min Min value
#' @param yAxis.max Max value
#' @param yAxis.position 'bottom', 'top'
#' @param yAxis.axisTick.customValues Custom values for the y-axis
#' @param yAxis.nameLocation 'center', 'start', 'middle'
#' @param yAxis.nameTextStyle.color hex or named color
#' @param yAxis.nameTextStyle.padding numeric
#' @param yAxis.nameTextStyle.align 'center', 'left', 'right'
#' @param yAxis.nameTextStyle.fontStyle 'italic', 'normal' 'oblique'
#' @param yAxis.nameTextStyle.fontWeight 'normal', 'bold', 'bolder', 'lighter'
#' @param yAxis.nameTextStyle.fontSize numeric
#' @param yAxis.nameTextStyle.fontFamily 'sans-serif', 'serif', 'monospace', 'Arial', 'Times New Roman', 'Roboto', 'Open Sans', 'Lato', 'Helvetica', 'Georgia', 'Verdana', 'Arial', 'Tahoma', 'Courier New'
#' @param yAxis.splitNumber numeric. Increments for numeric axis labels
#' @param yAxis.axisLabel.rotate numeric
#' @param yAxis.axisLabel.margin numeric
#' @param yAxis.axisLabel.color hex or named
#' @param yAxis.axisLabel.fontStyle 'italic', 'normal' 'oblique'
#' @param yAxis.axisLabel.fontWeight 'normal', 'bold', 'bolder', 'lighter'
#' @param yAxis.axisLabel.fontFamily 'sans-serif', 'serif', 'monospace', 'Arial', 'Times New Roman', 'Roboto', 'Open Sans', 'Lato', 'Helvetica', 'Georgia', 'Verdana', 'Arial', 'Tahoma', 'Courier New'
#' @param yAxis.axisLabel.fontSize numeric
#' @param yAxis.axisLabel.align 'center', 'left', 'right'
#' @param yAxis.axisLabel.verticalAlign 'top', 'bottom'
#' @param yAxis.axisLabel.backgroundColor hex or named
#' @param yAxis.axisLabel.borderColor hex or named
#' @param yAxis.axisLabel.borderWidth numeric
#' @param yAxis.axisLabel.borderType 'solid', 'dashed', 'dotted'
#' @param yAxis.axisLabel.borderRadius numeric
#' @param yAxis.axisLabel.padding numeric
#' @param yAxis.axisLabel.shadowColor hex or named
#' @param yAxis.axisLabel.shadowBlur numeric
#' @param yAxis.axisLabel.shadowOffsetX numeric
#' @param yAxis.axisLabel.shadowOffsetY numeric
#' @param yAxis.axisLabel.textBorderColor hex or named
#' @param yAxis.axisLabel.textBorderWidth numeric
#' @param yAxis.axisLabel.textBorderType 'solid', 'dashed', 'dotted'
#' @param yAxis.axisLabel.textShadowColor hex or named
#' @param yAxis.axisLabel.textShadowBlur numeric
#' @param yAxis.axisLabel.textShadowOffsetX numeric
#' @param yAxis.axisLabel.textShadowOffsetY numeric
#' @param yAxis.axisLabel.overflow 'truncate'
#'
#' @return The modified echarts4r object
#' @export
#'
e_y_axis_full <- function(
    e,
    index = 0,
    serie = NULL,
    axis = "y",
    yAxis.title = NULL,
    yAxis.position = "bottom",
    yAxis.nameLocation = "center",
    yAxis.axisTick.customValues = NULL,
    yAxis.nameTextStyle.color = NULL,
    yAxis.nameTextStyle.padding = 15,
    yAxis.nameTextStyle.align = NULL,
    yAxis.nameTextStyle.fontStyle = NULL,
    yAxis.nameTextStyle.fontWeight = NULL,
    yAxis.nameTextStyle.fontSize = NULL,
    yAxis.nameTextStyle.fontFamily = NULL,
    yAxis.min = NULL,
    yAxis.max = NULL,
    yAxis.splitNumber = NULL,
    yAxis.axisLabel.rotate = NULL,
    yAxis.axisLabel.margin = NULL,
    yAxis.axisLabel.color = NULL,
    yAxis.axisLabel.fontStyle = NULL,
    yAxis.axisLabel.fontWeight = NULL,
    yAxis.axisLabel.fontFamily = NULL,
    yAxis.axisLabel.fontSize = NULL,
    yAxis.axisLabel.align = NULL,
    yAxis.axisLabel.verticalAlign = NULL,
    yAxis.axisLabel.backgroundColor = NULL,
    yAxis.axisLabel.borderColor = NULL,
    yAxis.axisLabel.borderWidth = NULL,
    yAxis.axisLabel.borderType = NULL,
    yAxis.axisLabel.borderRadius = NULL,
    yAxis.axisLabel.padding = NULL,
    yAxis.axisLabel.shadowColor = NULL,
    yAxis.axisLabel.shadowBlur = NULL,
    yAxis.axisLabel.shadowOffsetX = NULL,
    yAxis.axisLabel.shadowOffsetY = NULL,
    yAxis.axisLabel.textBorderColor = NULL,
    yAxis.axisLabel.textBorderWidth = NULL,
    yAxis.axisLabel.textBorderType = NULL,
    yAxis.axisLabel.textShadowColor = NULL,
    yAxis.axisLabel.textShadowBlur = NULL,
    yAxis.axisLabel.textShadowOffsetX = NULL,
    yAxis.axisLabel.textShadowOffsetY = NULL,
    yAxis.axisLabel.overflow = NULL) {

  # Build the nameTextStyle list
  nts <- .compact(list(
    color      = yAxis.nameTextStyle.color,
    padding    = yAxis.nameTextStyle.padding,
    align      = yAxis.nameTextStyle.align,
    fontStyle  = yAxis.nameTextStyle.fontStyle,
    fontWeight = yAxis.nameTextStyle.fontWeight,
    fontSize   = yAxis.nameTextStyle.fontSize,
    fontFamily = yAxis.nameTextStyle.fontFamily
  ))

  # Build the axisLabel list
  al <- .compact(list(
    rotate        = yAxis.axisLabel.rotate,
    margin        = yAxis.axisLabel.margin,
    color         = yAxis.axisLabel.color,
    fontStyle     = yAxis.axisLabel.fontStyle,
    fontWeight    = yAxis.axisLabel.fontWeight,
    fontFamily    = yAxis.axisLabel.fontFamily,
    fontSize      = yAxis.axisLabel.fontSize,
    align         = yAxis.axisLabel.align,
    verticalAlign = yAxis.axisLabel.verticalAlign,
    backgroundColor = yAxis.axisLabel.backgroundColor,
    borderColor     = yAxis.axisLabel.borderColor,
    borderWidth     = yAxis.axisLabel.borderWidth,
    borderType      = yAxis.axisLabel.borderType,
    borderRadius    = yAxis.axisLabel.borderRadius,
    padding         = yAxis.axisLabel.padding,
    shadowColor     = yAxis.axisLabel.shadowColor,
    shadowBlur      = yAxis.axisLabel.shadowBlur,
    shadowOffsetX   = yAxis.axisLabel.shadowOffsetX,
    shadowOffsetY   = yAxis.axisLabel.shadowOffsetY,
    textBorderColor = yAxis.axisLabel.textBorderColor,
    textBorderWidth = yAxis.axisLabel.textBorderWidth,
    textBorderType  = yAxis.axisLabel.textBorderType,
    textShadowColor = yAxis.axisLabel.textShadowColor,
    textShadowBlur  = yAxis.axisLabel.textShadowBlur,
    textShadowOffsetX = yAxis.axisLabel.textShadowOffsetX,
    textShadowOffsetY = yAxis.axisLabel.textShadowOffsetY,
    overflow        = yAxis.axisLabel.overflow
  ))

  at <- .compact(list(
    customValues = yAxis.axisTick.customValues
  ))

  # Assemble the final opts
  opts <- .compact(list(
    nameTextStyle = if (length(nts)) nts,
    axisTick      = if (length(at)) at,
    min           = yAxis.min,
    max           = yAxis.max,
    position      = yAxis.position,
    splitNumber   = yAxis.splitNumber,
    axisLabel     = if (length(al)) al
  ))

  # Call the internal helper
  do.call(echarts4r::e_axis_, c(
    list(
      e = e,
      axis = axis,
      index = index,
      name = yAxis.title,
      nameLocation = yAxis.nameLocation
    ),
    opts
  ))
}

#' Enhanced Tooltip Setter for echarts4r
#'
#' Exposes every tooltip.* option so you don't have to hand-craft the JSON.
#'
#' @param e An echarts4r object
#' @param tooltip.show logical
#' @param tooltip.trigger "cross" "axis" "item" "none"
#' @param tooltip.backgroundColor hex or name
#' @param tooltip.borderColor numeric
#' @param tooltip.borderWidth numeric
#' @param tooltip.padding numeric
#' @param tooltip.axisPointer.type "line" or "shadow"
#' @param tooltip.axisPointer.lineStyle.color hex or name
#' @param tooltip.axisPointer.shadowStyle.color hex or name
#' @param tooltip.axisPointer.shadowStyle.shadowBlur numeric
#' @param tooltip.axisPointer.shadowStyle.shadowOffsetX numeric
#' @param tooltip.axisPointer.shadowStyle.shadowOffsetY numeric
#' @param tooltip.axisPointer.shadowStyle.opacity numeric between 0 and 1
#' @param tooltip.textStyle.color hex or name
#' @param tooltip.textStyle.fontStyle "normal" "italic" "oblique"
#' @param tooltip.textStyle.fontWeight "normal" "bold" "bolder" "lighter"
#' @param tooltip.textStyle.fontFamily 'sans-serif', 'serif', 'monospace', 'Arial', 'Times New Roman', 'Roboto', 'Open Sans', 'Lato', 'Helvetica', 'Georgia', 'Verdana', 'Arial', 'Tahoma', 'Courier New'
#' @param tooltip.textStyle.lineHeight numeric
#' @param tooltip.textStyle.width numeric
#' @param tooltip.textStyle.height numeric
#' @param tooltip.textStyle.textBorderColor hex or name
#' @param tooltip.textStyle.textBorderWidth numeric
#' @param tooltip.textStyle.textBorderType "solid" "dashed" "dotted"
#' @param tooltip.textStyle.textShadowColor hex or name
#' @param tooltip.textStyle.textShadowBlur numeric
#' @param tooltip.textStyle.textShadowOffsetX numeric
#' @param tooltip.textStyle.textShadowOffsetY numeric
#' @return The modified echarts4r object
#' @export
e_tooltip_full <- function(
    e,
    tooltip.show = TRUE,
    tooltip.trigger = "axis",
    tooltip.backgroundColor = NULL,
    tooltip.borderColor = NULL,
    tooltip.borderWidth = NULL,
    tooltip.padding = NULL,
    tooltip.axisPointer.type = "cross", # 'line', 'shadow', 'cross', 'none'
    tooltip.axisPointer.lineStyle.color = NULL,
    tooltip.axisPointer.shadowStyle.color = NULL,
    tooltip.axisPointer.shadowStyle.shadowBlur = NULL,
    tooltip.axisPointer.shadowStyle.shadowOffsetX = NULL,
    tooltip.axisPointer.shadowStyle.shadowOffsetY = NULL,
    tooltip.axisPointer.shadowStyle.opacity = NULL,
    tooltip.textStyle.color = NULL,
    tooltip.textStyle.fontStyle = NULL,
    tooltip.textStyle.fontWeight = NULL,
    tooltip.textStyle.fontFamily = NULL,
    tooltip.textStyle.lineHeight = NULL,
    tooltip.textStyle.width = NULL,
    tooltip.textStyle.height = NULL,
    tooltip.textStyle.textBorderColor = NULL,
    tooltip.textStyle.textBorderWidth = NULL,
    tooltip.textStyle.textBorderType = NULL,
    tooltip.textStyle.textShadowColor = NULL,
    tooltip.textStyle.textShadowBlur = NULL,
    tooltip.textStyle.textShadowOffsetX = NULL,
    tooltip.textStyle.textShadowOffsetY = NULL) {

  ap <- .compact(list(
    type = tooltip.axisPointer.type,
    lineStyle = .compact(list(
      color = tooltip.axisPointer.lineStyle.color
    )),
    shadowStyle = .compact(list(
      color = tooltip.axisPointer.shadowStyle.color,
      shadowBlur = tooltip.axisPointer.shadowStyle.shadowBlur,
      shadowOffsetX = tooltip.axisPointer.shadowStyle.shadowOffsetX,
      shadowOffsetY = tooltip.axisPointer.shadowStyle.shadowOffsetY,
      opacity = tooltip.axisPointer.shadowStyle.opacity
    ))
  ))

  ts <- .compact(list(
    color = tooltip.textStyle.color,
    fontStyle = tooltip.textStyle.fontStyle,
    fontWeight = tooltip.textStyle.fontWeight,
    fontFamily = tooltip.textStyle.fontFamily,
    lineHeight = tooltip.textStyle.lineHeight,
    width = tooltip.textStyle.width,
    height = tooltip.textStyle.height,
    textBorderColor = tooltip.textStyle.textBorderColor,
    textBorderWidth = tooltip.textStyle.textBorderWidth,
    textBorderType = tooltip.textStyle.textBorderType,
    textShadowColor = tooltip.textStyle.textShadowColor,
    textShadowBlur = tooltip.textStyle.textShadowBlur,
    textShadowOffsetX = tooltip.textStyle.textShadowOffsetX,
    textShadowOffsetY = tooltip.textStyle.textShadowOffsetY
  ))

  # Assemble the final opts
  opts <- .compact(list(
    axisPointer = if (length(ap)) ap,
    textStyle = if (length(ts)) ts
  ))

  standard <- list()
  standard[["e"]] <- e
  standard[["show"]] <- tooltip.show
  standard[["trigger"]] <- tooltip.trigger
  standard[["backgroundColor"]] <- tooltip.backgroundColor
  standard[["borderWidth"]] <- tooltip.borderColor
  standard[["padding"]] <- tooltip.padding

  do.call(echarts4r::e_tooltip, c(
    standard,
    opts
  ))
}


#' Enhanced Title Setter for echarts4r
#'
#' Exposes every title.* option so you don't have to hand-craft the JSON.
#'
#' @param e An echarts4r object
#' @param title.text Title name
#' @param title.subtext Subtitle name
#' @param title.link Title as a link
#' @param title.sublink Subtitle as a link
#' @param title.Align 'auto' 'left' 'right' 'center'
#' @param title.top 'auto' '20' 'top' 'middle' 'bottom'
#' @param title.left distance between title and left side of container
#' @param title.right distance between title and right side of container
#' @param title.bottom 'auto' '20' 'top' 'middle' 'bottom'
#' @param title.padding numeric
#' @param title.itemGap space between title and subtitle
#' @param title.backgroundColor hex or name
#' @param title.borderColor hex or name
#' @param title.borderWidth numeric
#' @param title.borderRadius numeric
#' @param title.shadowColor hex or name
#' @param title.shadowBlur numeric
#' @param title.shadowOffsetX numeric
#' @param title.shadowOffsetY numeric
#' @param title.textStyle.color hex or name
#' @param title.textStyle.fontStyle 'normal' 'italic' 'oblique'
#' @param title.textStyle.fontWeight 'normal' 'bold' 'bolder' 'lighter'
#' @param title.textStyle.fontFamily 'sans-serif', 'serif', 'monospace', 'Arial', 'Times New Roman', 'Roboto', 'Open Sans', 'Lato', 'Helvetica', 'Georgia', 'Verdana', 'Arial', 'Tahoma', 'Courier New'
#' @param title.textStyle.fontSize numeric
#' @param title.textStyle.lineHeight numeric
#' @param title.textStyle.width numeric
#' @param title.textStyle.height numeric
#' @param title.textStyle.textBorderColor hex or name
#' @param title.textStyle.textBorderWidth numeric
#' @param title.textStyle.textBorderType 'solid' 'dashed' 'dotted'
#' @param title.textStyle.textBorderDashOffset numeric
#' @param title.textStyle.textShadowColor hex or name
#' @param title.textStyle.textShadowBlur numeric
#' @param title.textStyle.textShadowOffsetX numeric
#' @param title.textStyle.textShadowOffsetY numeric
#' @param title.subtextStyle.color hex or name
#' @param title.subtextStyle.align 'auto' 'left' 'right' 'center'
#' @param title.subtextStyle.fontStyle 'normal' 'italic' 'oblique'
#' @param title.subtextStyle.fontWeight 'normal' 'bold' 'bolder' 'lighter'
#' @param title.subtextStyle.fontFamily 'sans-serif', 'serif', 'monospace', 'Arial', 'Times New Roman', 'Roboto', 'Open Sans', 'Lato', 'Helvetica', 'Georgia', 'Verdana', 'Arial', 'Tahoma', 'Courier New'
#' @param title.subtextStyle.fontSize numeric
#' @param title.subtextStyle.lineHeight numeric
#' @param title.subtextStyle.width numeric
#' @param title.subtextStyle.height numeric
#' @param title.subtextStyle.textBorderColor hex or name
#' @param title.subtextStyle.textBorderWidth numeric
#' @param title.subtextStyle.textBorderType 'solid' 'dashed' 'dotted'
#' @param title.subtextStyle.textBorderDashOffset numeric
#' @param title.subtextStyle.textShadowColor numeric
#' @param title.subtextStyle.textShadowBlur numeric
#' @param title.subtextStyle.textShadowOffsetX numeric
#' @param title.subtextStyle.textShadowOffsetY numeric
#' @return The modified echarts4r object
#' @export
e_title_full <- function(
    e = NULL,
    title.text = NULL,
    title.subtext = NULL,
    title.link = NULL,
    title.sublink = NULL,
    title.Align = NULL,
    title.top = NULL,
    title.left = NULL,
    title.right = NULL,
    title.bottom = NULL,
    title.padding = NULL,
    title.itemGap = NULL,
    title.backgroundColor = NULL,
    title.borderColor = NULL,
    title.borderWidth = NULL,
    title.borderRadius = NULL,
    title.shadowColor = NULL,
    title.shadowBlur = NULL,
    title.shadowOffsetX = NULL,
    title.shadowOffsetY = NULL,
    title.textStyle.color = NULL,
    title.textStyle.fontStyle = NULL,
    title.textStyle.fontWeight = NULL,
    title.textStyle.fontFamily = NULL,
    title.textStyle.fontSize = NULL,
    title.textStyle.lineHeight = NULL,
    title.textStyle.width = NULL,
    title.textStyle.height = NULL,
    title.textStyle.textBorderColor = NULL,
    title.textStyle.textBorderWidth = NULL,
    title.textStyle.textBorderType = NULL,
    title.textStyle.textBorderDashOffset = NULL,
    title.textStyle.textShadowColor = NULL,
    title.textStyle.textShadowBlur = NULL,
    title.textStyle.textShadowOffsetX = NULL,
    title.textStyle.textShadowOffsetY = NULL,
    title.subtextStyle.color = NULL,
    title.subtextStyle.align = NULL,
    title.subtextStyle.fontStyle = NULL,
    title.subtextStyle.fontWeight = NULL,
    title.subtextStyle.fontFamily = NULL,
    title.subtextStyle.fontSize = NULL,
    title.subtextStyle.lineHeight = NULL,
    title.subtextStyle.width = NULL,
    title.subtextStyle.height = NULL,
    title.subtextStyle.textBorderColor = NULL,
    title.subtextStyle.textBorderWidth = NULL,
    title.subtextStyle.textBorderType = NULL,
    title.subtextStyle.textBorderDashOffset = NULL,
    title.subtextStyle.textShadowColor = NULL,
    title.subtextStyle.textShadowBlur = NULL,
    title.subtextStyle.textShadowOffsetX = NULL,
    title.subtextStyle.textShadowOffsetY = NULL) {

  ts <- .compact(list(
    color = title.textStyle.color,
    fontStyle = title.textStyle.fontStyle, # 'normal' 'italic' 'oblique'
    fontWeight = title.textStyle.fontWeight, # 'normal' 'bold' 'bolder' 'lighter'
    fontFamily = title.textStyle.fontFamily,
    fontSize = title.textStyle.fontSize,
    lineHeight = title.textStyle.lineHeight,
    width = title.textStyle.width,
    height = title.textStyle.height,
    textBorderColor = title.textStyle.textBorderColor,
    textBorderWidth = title.textStyle.textBorderWidth,
    textBorderType = title.textStyle.textBorderType, # 'solid' 'dashed' 'dotted'
    textBorderDashOffset = title.textStyle.textBorderDashOffset,
    textShadowColor = title.textStyle.textShadowColor,
    textShadowBlur = title.textStyle.textShadowBlur,
    textShadowOffsetX = title.textStyle.textShadowOffsetX,
    textShadowOffsetY = title.textStyle.textShadowOffsetY
  ))

  sts <- .compact(list(
    color = title.subtextStyle.color,
    align = title.subtextStyle.align,
    fontStyle = title.subtextStyle.fontStyle, # 'normal' 'italic' 'oblique'
    fontWeight = title.subtextStyle.fontWeight, # 'normal' 'bold' 'bolder' 'lighter'
    fontFamily = title.subtextStyle.fontFamily,
    fontSize = title.subtextStyle.fontSize,
    lineHeight = title.subtextStyle.lineHeight,
    width = title.subtextStyle.width,
    height = title.subtextStyle.height,
    textBorderColor = title.subtextStyle.textBorderColor,
    textBorderWidth = title.subtextStyle.textBorderWidth,
    textBorderType = title.subtextStyle.textBorderType, # 'solid' 'dashed' 'dotted'
    textBorderDashOffset = title.subtextStyle.textBorderDashOffset,
    textShadowColor = title.subtextStyle.textShadowColor,
    textShadowBlur = title.subtextStyle.textShadowBlur,
    textShadowOffsetX = title.subtextStyle.textShadowOffsetX,
    textShadowOffsetY = title.subtextStyle.textShadowOffsetY
  ))

  # Assemble the final opts
  opts <- .compact(list(
    textStyle = if (length(ts)) ts,
    subtextStyle = if (length(sts)) sts
  ))

  standard <- list()
  standard[["e"]] <- e
  standard[["text"]] <- title.text
  standard[["subtext"]] <- title.subtext
  standard[["link"]] <- title.link
  standard[["sublink"]] <- title.sublink
  standard[["textAlign"]] <- title.Align # 'auto' 'left' 'right' 'center'
  standard[["top"]] <- title.top # 'auto' '20' '20%' 'top' 'middle' 'bottom'
  standard[["left"]] <- title.left # distance between title and left side of container
  standard[["right"]] <- title.right # distance between title and right side of container
  standard[["bottom"]] <- title.bottom # 'auto' '20' '20%' 'top' 'middle' 'bottom'
  standard[["padding"]] <- title.padding
  standard[["itemGap"]] <- title.itemGap # space between title and subtitle
  standard[["backgroundColor"]] <- title.backgroundColor
  standard[["borderColor"]] <- title.borderColor
  standard[["borderWidth"]] <- title.borderWidth
  standard[["borderRadius"]] <- title.borderRadius
  standard[["shadowColor"]] <- title.shadowColor
  standard[["shadowBlur"]] <- title.shadowBlur
  standard[["shadowOffsetX"]] <- title.shadowOffsetX
  standard[["shadowOffsetY"]] <- title.shadowOffsetY

  do.call(echarts4r::e_title, c(
    standard,
    opts
  ))
}


#' Enhanced Legend Setter for echarts4r
#'
#' Exposes every legend.* option so you don't have to hand-craft the JSON.
#'
#' @param e An echarts4r object
#' @param legend.show logical
#' @param legend.type 'scroll' 'plain'
#' @param legend.selector logical
#' @param legend.icon 'circle', 'rect', 'roundRect', 'triangle', 'diamond', 'pin', 'arrow', 'none'
#' @param legend.align 'auto' 'left' 'right'
#' @param legend.padding numeric
#' @param legend.itemGap numeric
#' @param legend.itemWidth numeric
#' @param legend.orient 'vertical' 'horizontal'
#' @param legend.width numeric
#' @param legend.height numeric
#' @param legend.left numeric
#' @param legend.right numeric
#' @param legend.top numeric
#' @param legend.bottom numeric
#' @param legend.backgroundColor hex or color name
#' @param legend.borderColor hex or color name
#' @param legend.borderWidth numeric
#' @param legend.borderRadius numeric
#' @param legend.shadowBlur numeric
#' @param legend.shadowColor hex or color name
#' @param legend.shadowOffsetX numeric
#' @param legend.shadowOffsetY numeric
#' @param legend.itemStyle.color hex or color name
#' @param legend.itemStyle.borderColor hex or color name
#' @param legend.itemStyle.borderWidth numeric
#' @param legend.itemStyle.borderType 'solid' 'dashed' 'dotted'
#' @param legend.itemStyle.shadowBlur numeric
#' @param legend.itemStyle.shadowColor hex or color name
#' @param legend.itemStyle.shadowOffsetX numeric
#' @param legend.itemStyle.shadowOffsetY numeric
#' @param legend.itemStyle.opacity numeric 0 to 1
#' @param legend.lineStyle.color hex or color name
#' @param legend.lineStyle.width numeric
#' @param legend.lineStyle.type 'solid' 'dashed' 'dotted'
#' @param legend.lineStyle.shadowBlur numeric
#' @param legend.lineStyle.shadowColor hex or color name
#' @param legend.lineStyle.shadowOffsetX numeric
#' @param legend.lineStyle.shadowOffsetY numeric
#' @param legend.lineStyle.opacity numeric 0 to 1
#' @param legend.lineStyle.inactiveColor hex or color name
#' @param legend.lineStyle.inactiveWidth numeric
#' @param legend.textStyle.color hex or color name
#' @param legend.textStyle.fontStyle 'normal' 'italic' 'oblique'
#' @param legend.textStyle.fontWeight 'normal' 'bold' 'bolder' 'lighter'
#' @param legend.textStyle.fontFamily 'sans-serif', 'serif', 'monospace', 'Arial', 'Times New Roman', 'Roboto', 'Open Sans', 'Lato', 'Helvetica', 'Georgia', 'Verdana', 'Arial', 'Tahoma', 'Courier New'
#' @param legend.textStyle.fontSize numeric
#' @param legend.textStyle.backgroundColor hex or color name
#' @param legend.textStyle.borderColor hex or color name
#' @param legend.textStyle.borderWidth numeric
#' @param legend.textStyle.borderType 'solid' 'dashed' 'dotted'
#' @param legend.textStyle.borderRadius numeric
#' @param legend.textStyle.padding numeric
#' @param legend.textStyle.shadowColor hex or color name
#' @param legend.textStyle.shadowBlur numeric
#' @param legend.textStyle.shadowOffsetX numeric
#' @param legend.textStyle.shadowOffsetY numeric
#' @param legend.textStyle.width numeric
#' @param legend.textStyle.height numeric
#' @param legend.textStyle.textBorderColor hex or color name
#' @param legend.textStyle.textBorderWidth numeric
#' @param legend.textStyle.textBorderType 'solid' 'dashed' 'dotted'
#' @param legend.textStyle.textShadowColor hex or color name
#' @param legend.textStyle.textShadowBlur numeric
#' @param legend.textStyle.textShadowOffsetX numeric
#' @param legend.textStyle.textShadowOffsetY numeric
#' @param legend.pageTextStyle.color hex or color name
#' @param legend.pageTextStyle.fontStyle 'normal' 'italic' 'oblique'
#' @param legend.pageTextStyle.fontWeight 'normal' 'bold' 'bolder' 'lighter'
#' @param legend.pageTextStyle.fontFamily 'sans-serif', 'serif', 'monospace', 'Arial', 'Times New Roman', 'Roboto', 'Open Sans', 'Lato', 'Helvetica', 'Georgia', 'Verdana', 'Arial', 'Tahoma', 'Courier New'
#' @param legend.pageTextStyle.fontSize numeric
#' @param legend.pageTextStyle.lineHeight numeric
#' @param legend.pageTextStyle.width numeric
#' @param legend.pageTextStyle.height numeric
#' @param legend.pageTextStyle.textBorderColor hex or color name
#' @param legend.pageTextStyle.textBorderWidth numeric
#' @param legend.pageTextStyle.textBorderType 'solid' 'dashed' 'dotted'
#' @param legend.pageTextStyle.textShadowColor hex or color name
#' @param legend.pageTextStyle.textShadowBlur numeric
#' @param legend.pageTextStyle.textShadowOffsetX numeric
#' @param legend.pageTextStyle.textShadowOffsetY numeric
#' @param legend.emphasis.selectorLabel.show logical
#' @param legend.emphasis.selectorLabel.distance numeric
#' @param legend.emphasis.selectorLabel.rotate numeric
#' @param legend.emphasis.selectorLabel.color hex or color name
#' @param legend.emphasis.selectorLabel.fontStyle 'normal' 'italic' 'oblique'
#' @param legend.emphasis.selectorLabel.fontWeight 'normal' 'bold' 'bolder' 'lighter'
#' @param legend.emphasis.selectorLabel.fontFamily 'sans-serif', 'serif', 'monospace', 'Arial', 'Times New Roman', 'Roboto', 'Open Sans', 'Lato', 'Helvetica', 'Georgia', 'Verdana', 'Arial', 'Tahoma', 'Courier New'
#' @param legend.emphasis.selectorLabel.fontSize numeric
#' @param legend.emphasis.selectorLabel.align 'left' 'center' 'right'
#' @param legend.emphasis.selectorLabel.verticalAlign 'top' 'middle' 'bottom'
#' @param legend.emphasis.selectorLabel.lineHeight numeric
#' @param legend.emphasis.selectorLabel.backgroundColor hex or color name
#' @param legend.emphasis.selectorLabel.borderColor hex or color name
#' @param legend.emphasis.selectorLabel.borderWidth numeric
#' @param legend.emphasis.selectorLabel.borderType 'solid' 'dashed' 'dotted'
#' @param legend.emphasis.selectorLabel.borderRadius numeric
#' @param legend.emphasis.selectorLabel.padding numeric
#' @param legend.emphasis.selectorLabel.shadowColor hex or color name
#' @param legend.emphasis.selectorLabel.shadowBlur numeric
#' @param legend.emphasis.selectorLabel.shadowOffsetX numeric
#' @param legend.emphasis.selectorLabel.shadowOffsetY numeric
#' @param legend.emphasis.selectorLabel.width numeric
#' @param legend.emphasis.selectorLabel.height numeric
#' @param legend.emphasis.selectorLabel.textBorderColor hex or color name
#' @param legend.emphasis.selectorLabel.textBorderWidth numeric
#' @param legend.emphasis.selectorLabel.textBorderType 'solid' 'dashed' 'dotted'
#' @param legend.emphasis.selectorLabel.textShadowColor hex or color name
#' @param legend.emphasis.selectorLabel.textShadowBlur numeric
#' @param legend.emphasis.selectorLabel.textShadowOffsetX numeric
#' @param legend.emphasis.selectorLabel.textShadowOffsetY numeric
#' @return The modified echarts4r object
#' @export
e_legend_full <- function(
    e = NULL,
    legend.show = FALSE,
    legend.type = "scroll", # 'plain' 'scroll'
    legend.selector = "all", # 'all' 'inverse'
    legend.icon = NULL, # 'circle', 'rect', 'roundRect', 'triangle', 'diamond', 'pin', 'arrow', 'none'
    legend.align = "auto", # 'auto' 'left' 'right'
    legend.padding = c(5,10,5,10), # Supply 1, 2 (top bottom, left right), or 4 numbers (top, right, bottom, left)
    legend.itemGap = 10,
    legend.itemWidth = 25, # image width of legend symbol-0o9
    legend.orient = "horizontal",
    legend.width = NULL,
    legend.height = "240px",
    legend.left = NULL,
    legend.right = 50,
    legend.top = 40,
    legend.bottom = NULL,
    legend.backgroundColor = NULL,
    legend.borderColor = NULL,
    legend.borderWidth = NULL,
    legend.borderRadius = NULL,
    legend.shadowBlur = NULL,
    legend.shadowColor = NULL,
    legend.shadowOffsetX = NULL,
    legend.shadowOffsetY = NULL,
    legend.itemStyle.color = NULL, # color of legend items
    legend.itemStyle.borderColor = NULL,
    legend.itemStyle.borderWidth = NULL,
    legend.itemStyle.borderType = NULL, # 'solid' 'dashed' 'dotted'
    legend.itemStyle.shadowBlur = NULL, # numeric
    legend.itemStyle.shadowColor = NULL,
    legend.itemStyle.shadowOffsetX = NULL,
    legend.itemStyle.shadowOffsetY = NULL,
    legend.itemStyle.opacity = NULL,
    legend.lineStyle.color = NULL,
    legend.lineStyle.width = NULL,
    legend.lineStyle.type = NULL, # 'solid' 'dashed' 'dotted'
    legend.lineStyle.shadowBlur = NULL,
    legend.lineStyle.shadowColor = NULL,
    legend.lineStyle.shadowOffsetX = NULL,
    legend.lineStyle.shadowOffsetY = NULL,
    legend.lineStyle.opacity = NULL,
    legend.lineStyle.inactiveColor = NULL,
    legend.lineStyle.inactiveWidth = NULL,
    legend.textStyle.color = NULL,
    legend.textStyle.fontStyle = NULL,
    legend.textStyle.fontWeight = NULL, # 'normal' 'bold' 'bolder' 'lighter'
    legend.textStyle.fontFamily = NULL,
    legend.textStyle.fontSize = NULL,
    legend.textStyle.backgroundColor = NULL,
    legend.textStyle.borderColor = NULL,
    legend.textStyle.borderWidth = NULL,
    legend.textStyle.borderType = NULL, # 'solid' 'dashed' 'dotted'
    legend.textStyle.borderRadius = NULL,
    legend.textStyle.padding = NULL,
    legend.textStyle.shadowColor = NULL,
    legend.textStyle.shadowBlur = NULL,
    legend.textStyle.shadowOffsetX = NULL,
    legend.textStyle.shadowOffsetY = NULL,
    legend.textStyle.width = NULL,
    legend.textStyle.height = NULL,
    legend.textStyle.textBorderColor = NULL,
    legend.textStyle.textBorderWidth = NULL,
    legend.textStyle.textBorderType = NULL, # 'solid' 'dashed' 'dotted'
    legend.textStyle.textShadowColor = NULL,
    legend.textStyle.textShadowBlur = NULL,
    legend.textStyle.textShadowOffsetX = NULL,
    legend.textStyle.textShadowOffsetY = NULL,
    legend.pageTextStyle.color = NULL, # = #333
    legend.pageTextStyle.fontStyle = NULL, # = 'normal' 'italic' 'oblique'
    legend.pageTextStyle.fontWeight = NULL, # = 'normal' 'bold' 'bolder' 'lighter'
    legend.pageTextStyle.fontFamily = NULL, # = 'sans-serif'
    legend.pageTextStyle.fontSize = NULL, # = 12
    legend.pageTextStyle.lineHeight = NULL, #
    legend.pageTextStyle.width = NULL, #
    legend.pageTextStyle.height = NULL, #
    legend.pageTextStyle.textBorderColor = NULL, #
    legend.pageTextStyle.textBorderWidth = NULL, #
    legend.pageTextStyle.textBorderType = NULL, # = 'solid' 'dashed' 'dotted'
    legend.pageTextStyle.textShadowColor = NULL, # = 'transparent'
    legend.pageTextStyle.textShadowBlur = NULL, #
    legend.pageTextStyle.textShadowOffsetX = NULL, #
    legend.pageTextStyle.textShadowOffsetY = NULL,
    legend.emphasis.selectorLabel.show = NULL,
    legend.emphasis.selectorLabel.distance = NULL,
    legend.emphasis.selectorLabel.rotate = NULL,
    legend.emphasis.selectorLabel.color = NULL,
    legend.emphasis.selectorLabel.fontStyle = NULL,
    legend.emphasis.selectorLabel.fontWeight = NULL,
    legend.emphasis.selectorLabel.fontFamily = NULL,
    legend.emphasis.selectorLabel.fontSize = NULL,
    legend.emphasis.selectorLabel.align = NULL,
    legend.emphasis.selectorLabel.verticalAlign = NULL,
    legend.emphasis.selectorLabel.lineHeight = NULL,
    legend.emphasis.selectorLabel.backgroundColor = NULL,
    legend.emphasis.selectorLabel.borderColor = NULL,
    legend.emphasis.selectorLabel.borderWidth = NULL,
    legend.emphasis.selectorLabel.borderType = NULL,
    legend.emphasis.selectorLabel.borderRadius = NULL,
    legend.emphasis.selectorLabel.padding = NULL,
    legend.emphasis.selectorLabel.shadowColor = NULL,
    legend.emphasis.selectorLabel.shadowBlur = NULL,
    legend.emphasis.selectorLabel.shadowOffsetX = NULL,
    legend.emphasis.selectorLabel.shadowOffsetY = NULL,
    legend.emphasis.selectorLabel.width = NULL,
    legend.emphasis.selectorLabel.height = NULL,
    legend.emphasis.selectorLabel.textBorderColor = NULL,
    legend.emphasis.selectorLabel.textBorderWidth = NULL,
    legend.emphasis.selectorLabel.textBorderType = NULL,
    legend.emphasis.selectorLabel.textShadowColor = NULL,
    legend.emphasis.selectorLabel.textShadowBlur = NULL,
    legend.emphasis.selectorLabel.textShadowOffsetX = NULL,
    legend.emphasis.selectorLabel.textShadowOffsetY = NULL) {

  is <- .compact(list(
    color = legend.itemStyle.color,
    borderColor = legend.itemStyle.borderColor,
    borderWidth = legend.itemStyle.borderWidth,
    borderType = legend.itemStyle.borderType,
    shadowBlur = legend.itemStyle.shadowBlur,
    shadowColor = legend.itemStyle.shadowColor,
    shadowOffsetX = legend.itemStyle.shadowOffsetX,
    shadowOffsetY = legend.itemStyle.shadowOffsetY,
    opacity = legend.itemStyle.opacity
  ))

  ls <- .compact(list(
    color = legend.lineStyle.color,
    width = legend.lineStyle.width,
    type = legend.lineStyle.type,
    shadowBlur = legend.lineStyle.shadowBlur,
    shadowColor = legend.lineStyle.shadowColor,
    shadowOffsetX = legend.lineStyle.shadowOffsetX,
    shadowOffsetY = legend.lineStyle.shadowOffsetY,
    opacity = legend.lineStyle.opacity,
    inactiveColor = legend.lineStyle.inactiveColor,
    inactiveWidth = legend.lineStyle.inactiveWidth
  ))

  ts <- .compact(list(
    color = legend.textStyle.color,
    fontStyle = legend.textStyle.fontStyle,
    fontWeight = legend.textStyle.fontWeight,
    fontFamily = legend.textStyle.fontFamily,
    fontSize = legend.textStyle.fontSize,
    backgroundColor = legend.textStyle.backgroundColor,
    borderColor = legend.textStyle.borderColor,
    borderWidth = legend.textStyle.borderWidth,
    borderType = legend.textStyle.borderType,
    borderRadius = legend.textStyle.borderRadius,
    padding = legend.textStyle.padding,
    shadowColor = legend.textStyle.shadowColor,
    shadowBlur = legend.textStyle.shadowBlur,
    shadowOffsetX = legend.textStyle.shadowOffsetX,
    shadowOffsetY = legend.textStyle.shadowOffsetY,
    width = legend.textStyle.width,
    height = legend.textStyle.height,
    textBorderColor = legend.textStyle.textBorderColor,
    textBorderWidth = legend.textStyle.textBorderWidth,
    textBorderType = legend.textStyle.textBorderType,
    textShadowColor = legend.textStyle.textShadowColor,
    textShadowBlur = legend.textStyle.textShadowBlur,
    textShadowOffsetX = legend.textStyle.textShadowOffsetX,
    textShadowOffsetY = legend.textStyle.textShadowOffsetY
  ))

  pts <- .compact(list(
    color = legend.pageTextStyle.color,
    fontStyle = legend.pageTextStyle.fontStyle,
    fontWeight = legend.pageTextStyle.fontWeight,
    fontFamily = legend.pageTextStyle.fontFamily,
    fontSize = legend.pageTextStyle.fontSize,
    lineHeight = legend.pageTextStyle.lineHeight,
    width = legend.pageTextStyle.width,
    height = legend.pageTextStyle.height,
    textBorderColor = legend.pageTextStyle.textBorderColor,
    textBorderWidth = legend.pageTextStyle.textBorderWidth,
    textBorderType = legend.pageTextStyle.textBorderType,
    textShadowColor = legend.pageTextStyle.textShadowColor,
    textShadowBlur = legend.pageTextStyle.textShadowBlur,
    textShadowOffsetX = legend.pageTextStyle.textShadowOffsetX,
    textShadowOffsetY = legend.pageTextStyle.textShadowOffsetY
  ))

  esl <- .compact(list(selectorLabel = list(
    show = legend.emphasis.selectorLabel.show,
    distance = legend.emphasis.selectorLabel.distance,
    rotate = legend.emphasis.selectorLabel.rotate,
    color = legend.emphasis.selectorLabel.color,
    fontStyle = legend.emphasis.selectorLabel.fontStyle,
    fontWeight = legend.emphasis.selectorLabel.fontWeight,
    fontFamily = legend.emphasis.selectorLabel.fontFamily,
    fontSize = legend.emphasis.selectorLabel.fontSize,
    align = legend.emphasis.selectorLabel.align,
    verticalAlign = legend.emphasis.selectorLabel.verticalAlign,
    lineHeight = legend.emphasis.selectorLabel.lineHeight,
    backgroundColor = legend.emphasis.selectorLabel.backgroundColor,
    borderColor = legend.emphasis.selectorLabel.borderColor,
    borderWidth = legend.emphasis.selectorLabel.borderWidth,
    borderType = legend.emphasis.selectorLabel.borderType,
    borderRadius = legend.emphasis.selectorLabel.borderRadius,
    padding = legend.emphasis.selectorLabel.padding,
    shadowColor = legend.emphasis.selectorLabel.shadowColor,
    shadowBlur = legend.emphasis.selectorLabel.shadowBlur,
    shadowOffsetX = legend.emphasis.selectorLabel.shadowOffsetX,
    shadowOffsetY = legend.emphasis.selectorLabel.shadowOffsetY,
    width = legend.emphasis.selectorLabel.width,
    height = legend.emphasis.selectorLabel.height,
    textBorderColor = legend.emphasis.selectorLabel.textBorderColor,
    textBorderWidth = legend.emphasis.selectorLabel.textBorderWidth,
    textBorderType = legend.emphasis.selectorLabel.textBorderType,
    textShadowColor = legend.emphasis.selectorLabel.textShadowColor,
    textShadowBlur = legend.emphasis.selectorLabel.textShadowBlur,
    textShadowOffsetX = legend.emphasis.selectorLabel.textShadowOffsetX,
    textShadowOffsetY = legend.emphasis.selectorLabel.textShadowOffsetY
  )))


  # Assemble the final opts
  opts <- .compact(list(
    itemStyle = if (length(is)) is,
    lineStyle = if (length(ls)) ls,
    textStyle = if (length(ts)) ts,
    pageTextStyle = if (length(pts)) pts,
    emphasis = if (length(esl)) esl
  ))

  standard <- list()
  standard[["e"]] <- e
  standard[["show"]] <- legend.show
  standard[["type"]] <- legend.type
  standard[["selector"]] <- legend.selector
  standard[["icon"]] <- legend.icon
  standard[["align"]] <- legend.align
  standard[["padding"]] <- legend.padding
  standard[["itemGap"]] <- legend.itemGap
  standard[["itemWidth"]] <- legend.itemWidth
  standard[["orient"]] <- legend.orient
  standard[["width"]] <- legend.width
  standard[["height"]] <- legend.height
  standard[["left"]] <- legend.left
  standard[["right"]] <- legend.right
  standard[["top"]] <- legend.top
  standard[["bottom"]] <- legend.bottom
  standard[["backgroundColor"]] <- legend.backgroundColor
  standard[["borderColor"]] <- legend.borderColor
  standard[["borderWidth"]] <- legend.borderWidth
  standard[["borderRadius"]] <- legend.borderRadius
  standard[["shadowBlur"]] <- legend.shadowBlur
  standard[["shadowColor"]] <- legend.shadowColor
  standard[["shadowOffsetX"]] <- legend.shadowOffsetX
  standard[["shadowOffsetY"]] <- legend.shadowOffsetY

  do.call(echarts4r::e_legend, c(
    standard,
    opts
  ))
}


#' Enhanced area Setter for echarts4r
#'
#' Exposes every area* option so you don't have to hand-craft the JSON.
#'
#' @param e plot object
#' @param serie Variable
#' @param smooth Smooth line
#' @param showSymbol Logical
#' @param areaStyle.color Fill color. Can be a single color or vector of multiple colors for gradient.
#' @param areaStyle.opacity transparency
#' @return The modified echarts4r object
#' @export
e_area_full <- function(e = NULL,
                        serie = NULL,
                        smooth = NULL,
                        showSymbol = NULL,
                        areaStyle.color = NULL,
                        areaStyle.opacity = NULL) {

  # Helper to convert hex color to rgba with opacity
  hex_to_rgba <- function(hex, alpha = 1) {
    rgb <- grDevices::col2rgb(hex) / 255
    sprintf("rgba(%d,%d,%d,%.2f)", rgb[1]*255, rgb[2]*255, rgb[3]*255, alpha)
  }

  # If multiple colors, build a vertical linear gradient
  if (!is.null(areaStyle.color) && length(areaStyle.color) > 1) {
    n_colors <- length(areaStyle.color)

    # Handle opacity: single value or vector
    alpha <- areaStyle.opacity
    if (is.null(alpha)) {
      alpha <- rep(1, n_colors)
    } else if (length(alpha) == 1) {
      alpha <- rep(alpha, n_colors)
    } else if (length(alpha) != n_colors) {
      stop("Length of areaStyle.opacity must be 1 or match length of areaStyle.color")
    }

    colorStops <- lapply(seq_along(areaStyle.color), function(i) {
      list(
        offset = (i - 1) / (n_colors - 1),
        color = hex_to_rgba(areaStyle.color[[i]], alpha[i])
      )
    })

    areaStyle.color <- list(
      type = "linear",
      x = 0, y = 0, x2 = 0, y2 = 1,
      colorStops = colorStops
    )

    # Remove top-level opacity to avoid conflict
    areaStyle.opacity <- NULL
  }

  # areaStyle list
  as <- .compact(list(
    color = areaStyle.color,
    opacity = areaStyle.opacity
  ))

  # opts
  opts <- .compact(list(
    itemStyle = if (length(as)) as
  ))

  # standard e_area_ args
  standard <- list()
  standard[["e"]] <- e
  standard[["serie"]] <- serie
  standard[["smooth"]] <- smooth
  standard[["showSymbol"]] <- showSymbol

  # final call
  do.call(echarts4r::e_area_, c(standard, opts))
}

#' Enhanced area Setter for echarts4r
#'
#' Exposes every area* option so you don't have to hand-craft the JSON.
#'
#' @param e plot object
#' @param serie Variable
#' @param label Logical
#' @param backgroundStyle.color Fill color. Can be a single color or vector of multiple colors for gradient.
#' @param backgroundStyle.opacity transparency
#' @return The modified echarts4r object
#' @export
e_bar_full <- function(e = NULL,
                       serie = NULL,
                       label = FALSE,
                       backgroundStyle.color = NULL,
                       backgroundStyle.opacity = NULL) {

  # Helper to convert hex color to rgba with opacity
  hex_to_rgba <- function(hex, alpha = 1) {
    rgb <- grDevices::col2rgb(hex) / 255
    sprintf("rgba(%d,%d,%d,%.2f)", rgb[1]*255, rgb[2]*255, rgb[3]*255, alpha)
  }

  # If multiple colors, build a vertical linear gradient
  if (!is.null(backgroundStyle.color) && length(backgroundStyle.color) > 1) {
    n_colors <- length(backgroundStyle.color)

    # Handle opacity: single value or vector
    alpha <- backgroundStyle.opacity
    if (is.null(alpha)) {
      alpha <- rep(1, n_colors)
    } else if (length(alpha) == 1) {
      alpha <- rep(alpha, n_colors)
    } else if (length(alpha) != n_colors) {
      stop("Length of backgroundStyle.opacity must be 1 or match length of backgroundStyle.color")
    }

    colorStops <- lapply(seq_along(backgroundStyle.color), function(i) {
      list(
        offset = (i - 1) / (n_colors - 1),
        color = hex_to_rgba(backgroundStyle.color[[i]], alpha[i])
      )
    })

    backgroundStyle.color <- list(
      type = "linear",
      x = 0, y = 0, x2 = 0, y2 = 1,
      colorStops = colorStops
    )

    # Remove top-level opacity to avoid conflict
    backgroundStyle.opacity <- NULL
  }

  # backgroundStyle list
  as <- .compact(list(
    color = backgroundStyle.color,
    opacity = backgroundStyle.opacity
  ))

  # opts
  opts <- .compact(list(
    itemStyle = if (length(as)) as
  ))

  # standard e_area_ args
  standard <- list()
  standard[["e"]] <- e
  standard[["serie"]] <- serie
  standard[["label"]] <- list(show = label)

  # final call
  do.call(echarts4r::e_bar_, c(standard, opts))
}


#' Enhanced area Setter for echarts4r
#'
#' Exposes every area* option so you don't have to hand-craft the JSON.
#'
#' @param e plot object
#' @param serie Variable
#' @param label Logical
#' @param itemStyle.color Fill color. Can be a single color or vector of multiple colors for gradient.
#' @param itemStyle.opacity transparency
#' @param itemStyle.borderColor hex or color name
#' @param itemStyle.borderWidth numeric
#' @param itemStyle.borderType 'solid' 'dashed' 'dotted'
#' @param itemStyle.borderCap 'butt' 'round' 'square'
#' @param itemStyle.shadowBlur numeric
#' @param itemStyle.shadowColor hex or name
#' @param itemStyle.shadowOffsetX numeric
#' @param itemStyle.shadowOffsetY numeric
#' @return The modified echarts4r object
#' @export
e_boxplot_full <- function(e = NULL,
                           serie = NULL,
                           label = FALSE,
                           itemStyle.color = NULL,
                           itemStyle.opacity = NULL,
                           itemStyle.borderColor = NULL,
                           itemStyle.borderWidth = NULL,
                           itemStyle.borderType = NULL,
                           itemStyle.borderCap = NULL,
                           itemStyle.shadowBlur = NULL,
                           itemStyle.shadowColor = NULL,
                           itemStyle.shadowOffsetX = NULL,
                           itemStyle.shadowOffsetY = NULL) {

  # Helper to convert hex color to rgba with opacity
  hex_to_rgba <- function(hex, alpha = 1) {
    rgb <- grDevices::col2rgb(hex) / 255
    sprintf("rgba(%d,%d,%d,%.2f)", rgb[1]*255, rgb[2]*255, rgb[3]*255, alpha)
  }

  # If multiple colors, build a vertical linear gradient
  if (!is.null(itemStyle.color) && length(itemStyle.color) > 1) {
    n_colors <- length(itemStyle.color)

    # Handle opacity: single value or vector
    alpha <- itemStyle.opacity
    if (is.null(alpha)) {
      alpha <- rep(1, n_colors)
    } else if (length(alpha) == 1) {
      alpha <- rep(alpha, n_colors)
    } else if (length(alpha) != n_colors) {
      stop("Length of itemStyle.opacity must be 1 or match length of itemStyle.color")
    }

    colorStops <- lapply(seq_along(itemStyle.color), function(i) {
      list(
        offset = (i - 1) / (n_colors - 1),
        color = hex_to_rgba(itemStyle.color[[i]], alpha[i])
      )
    })

    itemStyle.color <- list(
      type = "linear",
      x = 0, y = 0, x2 = 0, y2 = 1,
      colorStops = colorStops
    )

    # Remove top-level opacity to avoid conflict
    itemStyle.opacity <- NULL
  }

  # backgroundStyle list
  is <- .compact(list(
    color = itemStyle.color,
    opacity = itemStyle.opacity,
    borderColor = itemStyle.borderColor,
    borderWidth = itemStyle.borderWidth,
    borderType = itemStyle.borderType,
    borderCap = itemStyle.borderCap,
    shadowBlur = itemStyle.shadowBlur,
    shadowColor = itemStyle.shadowColor,
    shadowOffsetX = itemStyle.shadowOffsetX,
    shadowOffsetY = itemStyle.shadowOffsetY
  ))

  # opts
  opts <- .compact(list(
    itemStyle = if (length(is)) is
  ))

  # standard e_area_ args
  standard <- list()
  standard[["e"]] <- e
  standard[["serie"]] <- serie
  standard[["label"]] <- list(show = label)

  # final call
  do.call(echarts4r::e_boxplot_, c(standard, opts))
}



#' Display a Series of Plots in a Styled HTML Grid with Columns
#'
#' @param plots A list of echarts4r plots (or htmlwidgets).
#' @param cols Number of columns.
#' @param container_class CSS class for each plot container.
#' @param grid_class CSS class for the grid layout container.
#' @return A browsable HTML grid for use in Rmarkdown, Shiny, or viewer pane.
#' @export
display_plots_grid <- function(
    plots,
    cols = NULL,
    container_class = "plot-card",
    grid_class = "plot-grid") {

  nplots <- length(plots)

  if (is.null(cols)) {
    # Auto layout using auto-fit
    grid_style <- "display: grid; grid-template-columns: repeat(auto-fit, minmax(350px, 1fr)); gap: 20px;"
  } else {
    grid_style <- paste0(
      "display: grid; ",
      "grid-template-columns: repeat(", cols, ", 1fr); ",
      "gap: 20px;"
    )
  }

  # Determine leftover in last row
  plots_in_last_row <- nplots %% cols
  if (plots_in_last_row == 0) plots_in_last_row <- cols

  wrapped_plots <- purrr::imap(plots, function(p, i) {
    # If it's the only plot in last row, span all columns
    if (!is.null(cols) && i > nplots - plots_in_last_row && plots_in_last_row == 1) {
      htmltools::div(
        class = container_class,
        style = paste0("grid-column: span ", cols, ";"),
        p
      )
    } else {
      htmltools::div(class = container_class, p)
    }
  })

  htmltools::browsable(
    htmltools::tags$div(
      class = grid_class,
      style = grid_style,
      wrapped_plots
    )
  )
}
