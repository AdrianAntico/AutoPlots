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


#' Enhanced Title Setter for echarts4r
#'
#' Exposes every title.* option so you don't have to hand-craft the JSON.
#'
#' @param e An echarts4r object
#' @return The modified echarts4r object
#' @export
e_title_full <- function(
    e = NULL,
    title.text = NULL,
    tooltip.show = NULL,
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
  standard[["show"]] <- tooltip.show
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


#' Enhanced Title Setter for echarts4r
#'
#' Exposes every legend.* option so you don't have to hand-craft the JSON.
#'
#' @param e An echarts4r object
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
    opacity = legend.itemStyle.opacity,
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





