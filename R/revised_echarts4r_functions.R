# compact helper to drop NULLs
.compact <- function(x) x[!vapply(x, is.null, is.logical(1))]

#' Enhanced X-Axis Setter for echarts4r
#'
#' Exposes every xAxis.* option so you don't have to hand-craft the JSON.
#'
#' @param e An echarts4r object
#' @param index Which x axis to target (zero-based). Default: 0
#' @param xAxis.nameTextStyle.fontStyle,fontWeight,fontSize Font settings for the axis name
#' @param xAxis.min,xAxis.max Numeric data min/max
#' @param xAxis.splitNumber Numeric: how many segments between min & max
#' @param xAxis.axisLabel.* See list below
#' @param xAxis.axisPointer.* See list below
#'
#' @return The modified echarts4r object
#' @export
#'
e_x_axis_full <- function(
    e,
    index = 0,
    serie = NULL,
    axis = "x",
    name = NULL,
    xAxis.position = "bottom",
    xAxis.nameLocation = "center",
    xAxis.axisTick.customValues = NULL,
    xAxis.nameTextStyle.color = NULL,
    xAxis.nameTextStyle.backgroundColor = NULL,
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
    xAxis.axisLabel.overflow = NULL,
    xAxis.axisPointer.axis = NULL,
    xAxis.axisPointer.type = NULL,
    xAxis.axisPointer.triggerEmphasis = NULL,
    xAxis.axisPointer.triggerTooltip = NULL ) {

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
  do.call(echarts4r:::e_axis_, c(
    list(
      e = e,
      axis = axis,
      index = index,
      name = name,
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
#' @param index Which y axis to target (zero-based). Default: 0
#' @param yAxis.nameTextStyle.fontStyle,fontWeight,fontSize Font settings for the axis name
#' @param yAxis.min,yAxis.max Numeric data min/max
#' @param yAxis.splitNumber Numeric: how many segments between min & max
#' @param yAxis.axisLabel.* See list below
#' @param yAxis.axisPointer.* See list below
#'
#' @return The modified echarts4r object
#' @export
#'
e_y_axis_full <- function(
    e,
    index = 0,
    serie = NULL,
    axis = "y",
    name = NULL,
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
    yAxis.axisLabel.overflow = NULL,
    yAxis.axisPointer.axis = NULL,
    yAxis.axisPointer.type = NULL,
    yAxis.axisPointer.triggerEmphasis = NULL,
    yAxis.axisPointer.triggerTooltip = NULL ) {

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
  do.call(echarts4r:::e_axis_, c(
    list(
      e = e,
      axis = axis,
      index = index,
      name = name,
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

