get_theme_defaults_common <- function(theme = "dark") {
  switch(
    theme,
    auritus = list(
      title.textStyle.color = "#414557",
      title.textStyle.fontWeight = "bolder",
      title.textStyle.textShadowColor = "#414557",
      title.textStyle.textShadowBlur = 15,
      title.textStyle.textShadowOffsetX = 0,
      title.textStyle.textShadowOffsetY = 0,
      title.top = 12,
      title.left = 12,
      title.padding = c(4, 4, 10, 4),
      title.itemGap = 6,
      title.subtextStyle.color = "#7E8396",
      title.subtextStyle.textShadowColor = "#414557",
      title.subtextStyle.textShadowBlur = 15,
      title.subtextStyle.textShadowOffsetX = 0,
      title.subtextStyle.textShadowOffsetY = 0,
      title.subtextStyle.fontWeight = "bold",
      xAxis.nameTextStyle.fontSize = 20,
      xAxis.nameTextStyle.color = "#7E8396",
      xAxis.axisLabel.color = "#727DAB",
      xAxis.axisLabel.overflow = "truncate",
      xAxis.nameTextStyle.textShadowColor = "#414557",
      xAxis.nameTextStyle.textShadowBlur = 15,
      xAxis.nameTextStyle.textShadowOffsetX = 0,
      xAxis.nameTextStyle.textShadowOffsetY = 0,
      yAxis.nameTextStyle.fontSize = 20,
      yAxis.nameTextStyle.padding = 60,
      yAxis.nameTextStyle.color = "#7E8396",
      yAxis.axisLabel.color = "#727DAB",
      yAxis.nameTextStyle.textShadowColor = "#414557",
      yAxis.nameTextStyle.textShadowBlur = 15,
      yAxis.nameTextStyle.textShadowOffsetX = 0,
      yAxis.nameTextStyle.textShadowOffsetY = 0,
      tooltip.backgroundColor = "#0F173B",
      tooltip.textStyle.color = "#ABB5DE",
      toolbox.iconStyle.borderColor = "#7E8396",
      toolbox.emphasis.iconStyle.borderColor = "#0F173B"
    ),
    azul = list(
      title.textStyle.color = "#FC4E84",
      title.textStyle.fontWeight = "bolder",
      title.textStyle.textShadowColor = "#FC4E84",
      title.textStyle.textShadowBlur = 15,
      title.textStyle.textShadowOffsetX = 0,
      title.textStyle.textShadowOffsetY = 0,
      title.top = 12,
      title.left = 12,
      title.padding = c(4, 4, 10, 4),
      title.itemGap = 6,
      title.subtextStyle.color = "#7E8396",
      title.subtextStyle.textShadowColor = "#FC4E84",
      title.subtextStyle.textShadowBlur = 15,
      title.subtextStyle.textShadowOffsetX = 0,
      title.subtextStyle.textShadowOffsetY = 0,
      title.subtextStyle.fontWeight = "bold",
      xAxis.nameTextStyle.fontSize = 20,
      xAxis.nameTextStyle.color = "#7E8396",
      xAxis.axisLabel.color = "#727DAB",
      xAxis.axisLabel.overflow = "truncate",
      xAxis.nameTextStyle.textShadowColor = "#FC4E84",
      xAxis.nameTextStyle.textShadowBlur = 15,
      xAxis.nameTextStyle.textShadowOffsetX = 0,
      xAxis.nameTextStyle.textShadowOffsetY = 0,
      yAxis.nameTextStyle.fontSize = 20,
      yAxis.nameTextStyle.padding = 60,
      yAxis.nameTextStyle.color = "#7E8396",
      yAxis.axisLabel.color = "#727DAB",
      yAxis.nameTextStyle.textShadowColor = "#FC4E84",
      yAxis.nameTextStyle.textShadowBlur = 15,
      yAxis.nameTextStyle.textShadowOffsetX = 0,
      yAxis.nameTextStyle.textShadowOffsetY = 0,
      tooltip.backgroundColor = "#0F173B",
      tooltip.textStyle.color = "#ABB5DE",
      toolbox.iconStyle.borderColor = "#7E8396",
      toolbox.emphasis.iconStyle.borderColor = "#0F173B"
    ),
    blue = list(
      title.textStyle.color = "#005ABF",
      title.textStyle.fontWeight = "bolder",
      title.textStyle.textShadowColor = "#005ABF",
      title.textStyle.textShadowBlur = 15,
      title.textStyle.textShadowOffsetX = 0,
      title.textStyle.textShadowOffsetY = 0,
      title.top = 12,
      title.left = 12,
      title.padding = c(4, 4, 10, 4),
      title.itemGap = 6,
      title.subtextStyle.color = "#7E8396",
      title.subtextStyle.textShadowColor = "#005ABF",
      title.subtextStyle.textShadowBlur = 15,
      title.subtextStyle.textShadowOffsetX = 0,
      title.subtextStyle.textShadowOffsetY = 0,
      title.subtextStyle.fontWeight = "bold",
      xAxis.nameTextStyle.fontSize = 20,
      xAxis.nameTextStyle.color = "#005ABF",
      xAxis.axisLabel.color = "#727DAB",
      xAxis.axisLabel.overflow = "truncate",
      xAxis.nameTextStyle.textShadowColor = "#005ABF",
      xAxis.nameTextStyle.textShadowBlur = 15,
      xAxis.nameTextStyle.textShadowOffsetX = 0,
      xAxis.nameTextStyle.textShadowOffsetY = 0,
      yAxis.nameTextStyle.fontSize = 20,
      yAxis.nameTextStyle.padding = 60,
      yAxis.nameTextStyle.color = "#005ABF",
      yAxis.axisLabel.color = "#727DAB",
      yAxis.nameTextStyle.textShadowColor = "#005ABF",
      yAxis.nameTextStyle.textShadowBlur = 15,
      yAxis.nameTextStyle.textShadowOffsetX = 0,
      yAxis.nameTextStyle.textShadowOffsetY = 0,
      tooltip.backgroundColor = "#003169",
      tooltip.textStyle.color = "#ABB5DE",
      toolbox.iconStyle.borderColor = "#005ABF",
      toolbox.emphasis.iconStyle.borderColor = "#003169"
    ),
    `bee-inspired` = list(
      title.textStyle.color = "#494959",
      title.textStyle.fontWeight = "bolder",
      title.textStyle.textShadowColor = "#DEDE00",
      title.textStyle.textShadowBlur = 15,
      title.textStyle.textShadowOffsetX = 0,
      title.textStyle.textShadowOffsetY = 0,
      title.top = 12,
      title.left = 12,
      title.padding = c(4, 4, 10, 4),
      title.itemGap = 6,
      title.subtextStyle.color = "#494959",
      title.subtextStyle.textShadowColor = "#DEDE00",
      title.subtextStyle.textShadowBlur = 15,
      title.subtextStyle.textShadowOffsetX = 0,
      title.subtextStyle.textShadowOffsetY = 0,
      title.subtextStyle.fontWeight = "bold",
      xAxis.nameTextStyle.fontSize = 20,
      xAxis.nameTextStyle.color = "#2C2C36",
      xAxis.nameTextStyle.textShadowColor = "#DEDE00",
      xAxis.nameTextStyle.textShadowBlur = 15,
      xAxis.nameTextStyle.textShadowOffsetX = 0,
      xAxis.nameTextStyle.textShadowOffsetY = 0,
      xAxis.axisLabel.color = "#494959",
      xAxis.axisLabel.overflow = "truncate",
      yAxis.nameTextStyle.fontSize = 20,
      yAxis.nameTextStyle.padding = 60,
      yAxis.nameTextStyle.color = "#2C2C36",
      yAxis.nameTextStyle.textShadowColor = "#DEDE00",
      yAxis.nameTextStyle.textShadowBlur = 15,
      yAxis.nameTextStyle.textShadowOffsetX = 0,
      yAxis.nameTextStyle.textShadowOffsetY = 0,
      yAxis.axisLabel.color = "#494959",
      tooltip.backgroundColor = "#494959",
      tooltip.textStyle.color = "#DEDE00",
      toolbox.iconStyle.borderColor = "#2C2C36",
      toolbox.emphasis.iconStyle.borderColor = "#DEDE00"
    ),
    caravan = list(
      title.textStyle.color = "#FC6600",
      title.textStyle.fontWeight = "bolder",
      title.textStyle.textShadowColor = "#494959",
      title.textStyle.textShadowBlur = 15,
      title.textStyle.textShadowOffsetX = 0,
      title.textStyle.textShadowOffsetY = 0,
      title.top = 12,
      title.left = 12,
      title.padding = c(4, 4, 10, 4),
      title.itemGap = 6,
      title.subtextStyle.color = "#BD4D00",
      title.subtextStyle.textShadowColor = "#FC6600",
      title.subtextStyle.textShadowBlur = 15,
      title.subtextStyle.textShadowOffsetX = 0,
      title.subtextStyle.textShadowOffsetY = 0,
      title.subtextStyle.fontWeight = "bold",
      xAxis.nameTextStyle.fontSize = 20,
      xAxis.nameTextStyle.color = "#2C2C36",
      xAxis.nameTextStyle.textShadowColor = "#FC6600",
      xAxis.nameTextStyle.textShadowBlur = 15,
      xAxis.nameTextStyle.textShadowOffsetX = 0,
      xAxis.nameTextStyle.textShadowOffsetY = 0,
      xAxis.axisLabel.color = "#494959",
      xAxis.axisLabel.overflow = "truncate",
      yAxis.nameTextStyle.fontSize = 20,
      yAxis.nameTextStyle.padding = 60,
      yAxis.nameTextStyle.color = "#2C2C36",
      yAxis.nameTextStyle.textShadowColor = "#FC6600",
      yAxis.nameTextStyle.textShadowBlur = 15,
      yAxis.nameTextStyle.textShadowOffsetX = 0,
      yAxis.nameTextStyle.textShadowOffsetY = 0,
      yAxis.axisLabel.color = "#494959",
      tooltip.backgroundColor = "#494959",
      tooltip.textStyle.color = "#FC6600",
      toolbox.iconStyle.borderColor = "#2C2C36",
      toolbox.emphasis.iconStyle.borderColor = "#FC6600"
    ),
    carp = list(
      title.textStyle.color = "#FFB761",
      title.textStyle.fontWeight = "bolder",
      title.textStyle.textShadowColor = "#494959",
      title.textStyle.textShadowBlur = 15,
      title.textStyle.textShadowOffsetX = 0,
      title.textStyle.textShadowOffsetY = 0,
      title.top = 12,
      title.left = 12,
      title.padding = c(4, 4, 10, 4),
      title.itemGap = 6,
      title.subtextStyle.color = "#FFA93D",
      title.subtextStyle.textShadowColor = "#FFB761",
      title.subtextStyle.textShadowBlur = 15,
      title.subtextStyle.textShadowOffsetX = 0,
      title.subtextStyle.textShadowOffsetY = 0,
      title.subtextStyle.fontWeight = "bold",
      xAxis.nameTextStyle.fontSize = 20,
      xAxis.nameTextStyle.color = "#2C2C36",
      xAxis.nameTextStyle.textShadowColor = "#FFB761",
      xAxis.nameTextStyle.textShadowBlur = 15,
      xAxis.nameTextStyle.textShadowOffsetX = 0,
      xAxis.nameTextStyle.textShadowOffsetY = 0,
      xAxis.axisLabel.color = "#494959",
      xAxis.axisLabel.overflow = "truncate",
      yAxis.nameTextStyle.fontSize = 20,
      yAxis.nameTextStyle.padding = 60,
      yAxis.nameTextStyle.color = "#2C2C36",
      yAxis.nameTextStyle.textShadowColor = "#FFB761",
      yAxis.nameTextStyle.textShadowBlur = 15,
      yAxis.nameTextStyle.textShadowOffsetX = 0,
      yAxis.nameTextStyle.textShadowOffsetY = 0,
      yAxis.axisLabel.color = "#494959",
      tooltip.backgroundColor = "#494959",
      tooltip.textStyle.color = "#FFB761",
      toolbox.iconStyle.borderColor = "#2C2C36",
      toolbox.emphasis.iconStyle.borderColor = "#FFB761"
    ),
    chalk = list(
      title.textStyle.color = "#FF699C",
      title.textStyle.fontWeight = "bolder",
      title.textStyle.textShadowColor = "#B8B8B8",
      title.textStyle.textShadowBlur = 15,
      title.textStyle.textShadowOffsetX = 0,
      title.textStyle.textShadowOffsetY = 0,
      title.top = 12,
      title.left = 12,
      title.padding = c(4, 4, 10, 4),
      title.itemGap = 6,
      title.subtextStyle.color = "#FF4281",
      title.subtextStyle.textShadowColor = "#FF699C",
      title.subtextStyle.textShadowBlur = 15,
      title.subtextStyle.textShadowOffsetX = 0,
      title.subtextStyle.textShadowOffsetY = 0,
      title.subtextStyle.fontWeight = "bold",
      xAxis.nameTextStyle.fontSize = 20,
      xAxis.nameTextStyle.color = "#B8B8B8",
      xAxis.nameTextStyle.textShadowColor = "#FF699C",
      xAxis.nameTextStyle.textShadowBlur = 15,
      xAxis.nameTextStyle.textShadowOffsetX = 0,
      xAxis.nameTextStyle.textShadowOffsetY = 0,
      xAxis.axisLabel.color = "#D1D1D1",
      xAxis.axisLabel.overflow = "truncate",
      yAxis.nameTextStyle.fontSize = 20,
      yAxis.nameTextStyle.padding = 60,
      yAxis.nameTextStyle.color = "#B8B8B8",
      yAxis.nameTextStyle.textShadowColor = "#FF699C",
      yAxis.nameTextStyle.textShadowBlur = 15,
      yAxis.nameTextStyle.textShadowOffsetX = 0,
      yAxis.nameTextStyle.textShadowOffsetY = 0,
      yAxis.axisLabel.color = "#D1D1D1",
      tooltip.backgroundColor = "#D1D1D1",
      tooltip.textStyle.color = "#FF699C",
      toolbox.iconStyle.borderColor = "#B8B8B8",
      toolbox.emphasis.iconStyle.borderColor = "#FF699C"
    ),
    cool = list(
      title.textStyle.color = "#00BFFF",
      title.textStyle.fontWeight = "bolder",
      title.textStyle.textShadowColor = "#494959",
      title.textStyle.textShadowBlur = 15,
      title.textStyle.textShadowOffsetX = 0,
      title.textStyle.textShadowOffsetY = 0,
      title.top = 12,
      title.left = 12,
      title.padding = c(4, 4, 10, 4),
      title.itemGap = 6,
      title.subtextStyle.color = "#00BFFF",
      title.subtextStyle.textShadowColor = "#494959",
      title.subtextStyle.textShadowBlur = 15,
      title.subtextStyle.textShadowOffsetX = 0,
      title.subtextStyle.textShadowOffsetY = 0,
      title.subtextStyle.fontWeight = "bold",
      xAxis.nameTextStyle.fontSize = 20,
      xAxis.nameTextStyle.color = "#2C2C36",
      xAxis.nameTextStyle.textShadowColor = "#00BFFF",
      xAxis.nameTextStyle.textShadowBlur = 15,
      xAxis.nameTextStyle.textShadowOffsetX = 0,
      xAxis.nameTextStyle.textShadowOffsetY = 0,
      xAxis.axisLabel.color = "#494959",
      xAxis.axisLabel.overflow = "truncate",
      yAxis.nameTextStyle.fontSize = 20,
      yAxis.nameTextStyle.padding = 60,
      yAxis.nameTextStyle.color = "#2C2C36",
      yAxis.nameTextStyle.textShadowColor = "#00BFFF",
      yAxis.nameTextStyle.textShadowBlur = 15,
      yAxis.nameTextStyle.textShadowOffsetX = 0,
      yAxis.nameTextStyle.textShadowOffsetY = 0,
      yAxis.axisLabel.color = "#494959",
      tooltip.backgroundColor = "#494959",
      tooltip.textStyle.color = "#00BFFF",
      toolbox.iconStyle.borderColor = "#2C2C36",
      toolbox.emphasis.iconStyle.borderColor = "#00BFFF"
    ),
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
      legend.textStyle.color = "#DEEAFC",
      xAxis.nameTextStyle.fontSize = 20,
      xAxis.nameTextStyle.color = "#DEEAFC",
      xAxis.nameTextStyle.textShadowColor = "#5298FF",
      xAxis.nameTextStyle.textShadowBlur = 15,
      xAxis.nameTextStyle.textShadowOffsetX = 0,
      xAxis.nameTextStyle.textShadowOffsetY = 0,
      xAxis.axisLabel.color = "#ABD5FF",
      xAxis.axisLabel.overflow = "truncate",
      yAxis.nameTextStyle.fontSize = 20,
      yAxis.nameTextStyle.padding = 60,
      yAxis.nameTextStyle.color = "#DEEAFC",
      yAxis.nameTextStyle.textShadowColor = "#5298FF",
      yAxis.nameTextStyle.textShadowBlur = 15,
      yAxis.nameTextStyle.textShadowOffsetX = 0,
      yAxis.nameTextStyle.textShadowOffsetY = 0,
      yAxis.axisLabel.color = "#ABD5FF",
      tooltip.backgroundColor = "#002259",
      tooltip.textStyle.color = "#91C1FF",
      toolbox.iconStyle.borderColor = "#DEEAFC",
      toolbox.emphasis.iconStyle.borderColor = "#5E8DD1"
    ),
    `dark-blue` = list(
      title.textStyle.color = "#C9DAFF",
      title.textStyle.fontWeight = "bolder",
      title.textStyle.textShadowColor = "#C9DAFF",
      title.textStyle.textShadowBlur = 15,
      title.textStyle.textShadowOffsetX = 0,
      title.textStyle.textShadowOffsetY = 0,
      title.top = 12,
      title.left = 12,
      title.padding = c(4, 4, 10, 4),
      title.itemGap = 6,
      title.subtextStyle.color = "#87ADFF",
      title.subtextStyle.textShadowColor = "#C9DAFF",
      title.subtextStyle.textShadowBlur = 15,
      title.subtextStyle.textShadowOffsetX = 0,
      title.subtextStyle.textShadowOffsetY = 0,
      title.subtextStyle.fontWeight = "bold",
      xAxis.nameTextStyle.fontSize = 20,
      xAxis.nameTextStyle.color = "#C9DAFF",
      xAxis.nameTextStyle.textShadowColor = "#C9DAFF",
      xAxis.nameTextStyle.textShadowBlur = 15,
      xAxis.nameTextStyle.textShadowOffsetX = 0,
      xAxis.nameTextStyle.textShadowOffsetY = 0,
      xAxis.axisLabel.color = "#C9DAFF",
      xAxis.axisLabel.overflow = "truncate",
      yAxis.nameTextStyle.fontSize = 20,
      yAxis.nameTextStyle.padding = 60,
      yAxis.nameTextStyle.color = "#C9DAFF",
      yAxis.nameTextStyle.textShadowColor = "#C9DAFF",
      yAxis.nameTextStyle.textShadowBlur = 15,
      yAxis.nameTextStyle.textShadowOffsetX = 0,
      yAxis.nameTextStyle.textShadowOffsetY = 0,
      yAxis.axisLabel.color = "#C9DAFF",
      tooltip.backgroundColor = "#494959",
      tooltip.textStyle.color = "#C9DAFF",
      toolbox.iconStyle.borderColor = "#C9DAFF",
      toolbox.emphasis.iconStyle.borderColor = "#3A78FC"
    ),
    `dark-bold` = list(
      title.textStyle.color = "#D1FFDB",
      title.textStyle.fontWeight = "bolder",
      title.textStyle.textShadowColor = "#D1FFDB",
      title.textStyle.textShadowBlur = 15,
      title.textStyle.textShadowOffsetX = 0,
      title.textStyle.textShadowOffsetY = 0,
      title.top = 12,
      title.left = 12,
      title.padding = c(4, 4, 10, 4),
      title.itemGap = 6,
      title.subtextStyle.color = "#A8FFBB",
      title.subtextStyle.textShadowColor = "#D1FFDB",
      title.subtextStyle.textShadowBlur = 15,
      title.subtextStyle.textShadowOffsetX = 0,
      title.subtextStyle.textShadowOffsetY = 0,
      title.subtextStyle.fontWeight = "bold",
      xAxis.nameTextStyle.fontSize = 20,
      xAxis.nameTextStyle.color = "#D1FFDB",
      xAxis.nameTextStyle.textShadowColor = "#D1FFDB",
      xAxis.nameTextStyle.textShadowBlur = 15,
      xAxis.nameTextStyle.textShadowOffsetX = 0,
      xAxis.nameTextStyle.textShadowOffsetY = 0,
      xAxis.axisLabel.color = "#D1FFDB",
      xAxis.axisLabel.overflow = "truncate",
      yAxis.nameTextStyle.fontSize = 20,
      yAxis.nameTextStyle.padding = 60,
      yAxis.nameTextStyle.color = "#D1FFDB",
      yAxis.nameTextStyle.textShadowColor = "#D1FFDB",
      yAxis.nameTextStyle.textShadowBlur = 15,
      yAxis.nameTextStyle.textShadowOffsetX = 0,
      yAxis.nameTextStyle.textShadowOffsetY = 0,
      yAxis.axisLabel.color = "#D1FFDB",
      tooltip.backgroundColor = "#494959",
      tooltip.textStyle.color = "#D1FFDB",
      toolbox.iconStyle.borderColor = "#D1FFDB",
      toolbox.emphasis.iconStyle.borderColor = "#7DFF99"
    ),
    `dark-digerati` = list(
      title.textStyle.color = "#C9DAFF",
      title.textStyle.fontWeight = "bolder",
      title.textStyle.textShadowColor = "#C9DAFF",
      title.textStyle.textShadowBlur = 15,
      title.textStyle.textShadowOffsetX = 0,
      title.textStyle.textShadowOffsetY = 0,
      title.top = 12,
      title.left = 12,
      title.padding = c(4, 4, 10, 4),
      title.itemGap = 6,
      title.subtextStyle.color = "#7B9AAD",
      title.subtextStyle.textShadowColor = "#C9DAFF",
      title.subtextStyle.textShadowBlur = 15,
      title.subtextStyle.textShadowOffsetX = 0,
      title.subtextStyle.textShadowOffsetY = 0,
      title.subtextStyle.fontWeight = "bold",
      xAxis.nameTextStyle.fontSize = 20,
      xAxis.nameTextStyle.color = "#C9DAFF",
      xAxis.nameTextStyle.textShadowColor = "#C9DAFF",
      xAxis.nameTextStyle.textShadowBlur = 15,
      xAxis.nameTextStyle.textShadowOffsetX = 0,
      xAxis.nameTextStyle.textShadowOffsetY = 0,
      xAxis.axisLabel.color = "#C9DAFF",
      xAxis.axisLabel.overflow = "truncate",
      yAxis.nameTextStyle.fontSize = 20,
      yAxis.nameTextStyle.padding = 60,
      yAxis.nameTextStyle.color = "#C9DAFF",
      yAxis.nameTextStyle.textShadowColor = "#C9DAFF",
      yAxis.nameTextStyle.textShadowBlur = 15,
      yAxis.nameTextStyle.textShadowOffsetX = 0,
      yAxis.nameTextStyle.textShadowOffsetY = 0,
      yAxis.axisLabel.color = "#C9DAFF",
      tooltip.backgroundColor = "#494959",
      tooltip.textStyle.color = "#C9DAFF",
      toolbox.iconStyle.borderColor = "#C9DAFF",
      toolbox.emphasis.iconStyle.borderColor = "#77AAD1"
    ),
    `dark-fresh-cut` = list(
      title.textStyle.color = "#CFF5FF",
      title.textStyle.fontWeight = "bolder",
      title.textStyle.textShadowColor = "#CFF5FF",
      title.textStyle.textShadowBlur = 15,
      title.textStyle.textShadowOffsetX = 0,
      title.textStyle.textShadowOffsetY = 0,
      title.top = 12,
      title.left = 12,
      title.padding = c(4, 4, 10, 4),
      title.itemGap = 6,
      title.subtextStyle.color = "#73DCFF",
      title.subtextStyle.textShadowColor = "#CFF5FF",
      title.subtextStyle.textShadowBlur = 15,
      title.subtextStyle.textShadowOffsetX = 0,
      title.subtextStyle.textShadowOffsetY = 0,
      title.subtextStyle.fontWeight = "bold",
      xAxis.nameTextStyle.fontSize = 20,
      xAxis.nameTextStyle.color = "#CFF5FF",
      xAxis.nameTextStyle.textShadowColor = "#CFF5FF",
      xAxis.nameTextStyle.textShadowBlur = 15,
      xAxis.nameTextStyle.textShadowOffsetX = 0,
      xAxis.nameTextStyle.textShadowOffsetY = 0,
      xAxis.axisLabel.color = "#CFF5FF",
      xAxis.axisLabel.overflow = "truncate",
      yAxis.nameTextStyle.fontSize = 20,
      yAxis.nameTextStyle.padding = 60,
      yAxis.nameTextStyle.color = "#CFF5FF",
      yAxis.nameTextStyle.textShadowColor = "#CFF5FF",
      yAxis.nameTextStyle.textShadowBlur = 15,
      yAxis.nameTextStyle.textShadowOffsetX = 0,
      yAxis.nameTextStyle.textShadowOffsetY = 0,
      yAxis.axisLabel.color = "#CFF5FF",
      tooltip.backgroundColor = "#494959",
      tooltip.textStyle.color = "#CFF5FF",
      toolbox.iconStyle.borderColor = "#CFF5FF",
      toolbox.emphasis.iconStyle.borderColor = "#1FD2FF"
    ),
    `dark-mushroom` = list(
      title.textStyle.color = "#FFEAE5",
      title.textStyle.fontWeight = "bolder",
      title.textStyle.textShadowColor = "#FFEAE5",
      title.textStyle.textShadowBlur = 15,
      title.textStyle.textShadowOffsetX = 0,
      title.textStyle.textShadowOffsetY = 0,
      title.top = 12,
      title.left = 12,
      title.padding = c(4, 4, 10, 4),
      title.itemGap = 6,
      title.subtextStyle.color = "#FFDCD4",
      title.subtextStyle.textShadowColor = "#FFEAE5",
      title.subtextStyle.textShadowBlur = 15,
      title.subtextStyle.textShadowOffsetX = 0,
      title.subtextStyle.textShadowOffsetY = 0,
      title.subtextStyle.fontWeight = "bold",
      xAxis.nameTextStyle.fontSize = 20,
      xAxis.nameTextStyle.color = "#FFEAE5",
      xAxis.nameTextStyle.textShadowColor = "#FFEAE5",
      xAxis.nameTextStyle.textShadowBlur = 15,
      xAxis.nameTextStyle.textShadowOffsetX = 0,
      xAxis.nameTextStyle.textShadowOffsetY = 0,
      xAxis.axisLabel.color = "#FFEAE5",
      xAxis.axisLabel.overflow = "truncate",
      yAxis.nameTextStyle.fontSize = 20,
      yAxis.nameTextStyle.padding = 60,
      yAxis.nameTextStyle.color = "#FFEAE5",
      yAxis.nameTextStyle.textShadowColor = "#FFEAE5",
      yAxis.nameTextStyle.textShadowBlur = 15,
      yAxis.nameTextStyle.textShadowOffsetX = 0,
      yAxis.nameTextStyle.textShadowOffsetY = 0,
      yAxis.axisLabel.color = "#FFEAE5",
      tooltip.backgroundColor = "#FCEAE6",
      tooltip.textStyle.color = "#FF2F00",
      toolbox.iconStyle.borderColor = "#FFEAE5",
      toolbox.emphasis.iconStyle.borderColor = "#FC9681"
    ),
    eduardo = list(
      title.textStyle.color = "#4F4F4F",
      title.textStyle.fontWeight = "bolder",
      title.textStyle.textShadowColor = "#3B32B8",
      title.textStyle.textShadowBlur = 15,
      title.textStyle.textShadowOffsetX = 0,
      title.textStyle.textShadowOffsetY = 0,
      title.top = 12,
      title.left = 12,
      title.padding = c(4, 4, 10, 4),
      title.itemGap = 6,
      title.subtextStyle.color = "#212121",
      title.subtextStyle.textShadowColor = "#3B32B8",
      title.subtextStyle.textShadowBlur = 15,
      title.subtextStyle.textShadowOffsetX = 0,
      title.subtextStyle.textShadowOffsetY = 0,
      title.subtextStyle.fontWeight = "bold",
      xAxis.nameTextStyle.fontSize = 20,
      xAxis.nameTextStyle.color = "#212121",
      xAxis.nameTextStyle.textShadowColor = "#D9D6FF",
      xAxis.nameTextStyle.textShadowBlur = 15,
      xAxis.nameTextStyle.textShadowOffsetX = 0,
      xAxis.nameTextStyle.textShadowOffsetY = 0,
      xAxis.axisLabel.color = "#4F4F4F",
      xAxis.axisLabel.overflow = "truncate",
      yAxis.nameTextStyle.fontSize = 20,
      yAxis.nameTextStyle.padding = 60,
      yAxis.nameTextStyle.color = "#212121",
      yAxis.nameTextStyle.textShadowColor = "#D9D6FF",
      yAxis.nameTextStyle.textShadowBlur = 15,
      yAxis.nameTextStyle.textShadowOffsetX = 0,
      yAxis.nameTextStyle.textShadowOffsetY = 0,
      yAxis.axisLabel.color = "#4F4F4F",
      tooltip.backgroundColor = "#E4E1FC",
      tooltip.textStyle.color = "#271ABD",
      toolbox.iconStyle.borderColor = "#6066BD",
      toolbox.emphasis.iconStyle.borderColor = "#000885"
    ),
    essos = list(
      title.textStyle.color = "#661010",
      title.textStyle.fontWeight = "bolder",
      title.textStyle.textShadowColor = "#FF7D03",
      title.textStyle.textShadowBlur = 15,
      title.textStyle.textShadowOffsetX = 0,
      title.textStyle.textShadowOffsetY = 0,
      title.top = 12,
      title.left = 12,
      title.padding = c(4, 4, 10, 4),
      title.itemGap = 6,
      title.subtextStyle.color = "#3D0000",
      title.subtextStyle.textShadowColor = "#FF7D03",
      title.subtextStyle.textShadowBlur = 15,
      title.subtextStyle.textShadowOffsetX = 0,
      title.subtextStyle.textShadowOffsetY = 0,
      title.subtextStyle.fontWeight = "bold",
      xAxis.nameTextStyle.fontSize = 20,
      xAxis.nameTextStyle.color = "#3D0000",
      xAxis.nameTextStyle.textShadowColor = "#FF7D03",
      xAxis.nameTextStyle.textShadowBlur = 15,
      xAxis.nameTextStyle.textShadowOffsetX = 0,
      xAxis.nameTextStyle.textShadowOffsetY = 0,
      xAxis.axisLabel.color = "#3D0000",
      xAxis.axisLabel.overflow = "truncate",
      yAxis.nameTextStyle.fontSize = 20,
      yAxis.nameTextStyle.padding = 60,
      yAxis.nameTextStyle.color = "#3D0000",
      yAxis.nameTextStyle.textShadowColor = "#FF7D03",
      yAxis.nameTextStyle.textShadowBlur = 15,
      yAxis.nameTextStyle.textShadowOffsetX = 0,
      yAxis.nameTextStyle.textShadowOffsetY = 0,
      yAxis.axisLabel.color = "#3D0000",
      tooltip.backgroundColor = "#FFECD6",
      tooltip.textStyle.color = "#3D0000",
      toolbox.iconStyle.borderColor = "#824040",
      toolbox.emphasis.iconStyle.borderColor = "#FF7D03"
    ),
    forest = list(
      title.textStyle.color = "#034D03",
      title.textStyle.fontWeight = "bolder",
      title.textStyle.textShadowColor = "#325C33",
      title.textStyle.textShadowBlur = 15,
      title.textStyle.textShadowOffsetX = 0,
      title.textStyle.textShadowOffsetY = 0,
      title.top = 12,
      title.left = 12,
      title.padding = c(4, 4, 10, 4),
      title.itemGap = 6,
      title.subtextStyle.color = "#014A01",
      title.subtextStyle.textShadowColor = "#325C33",
      title.subtextStyle.textShadowBlur = 15,
      title.subtextStyle.textShadowOffsetX = 0,
      title.subtextStyle.textShadowOffsetY = 0,
      title.subtextStyle.fontWeight = "bold",
      xAxis.nameTextStyle.fontSize = 20,
      xAxis.nameTextStyle.color = "#034D03",
      xAxis.nameTextStyle.textShadowColor = "#325C33",
      xAxis.nameTextStyle.textShadowBlur = 15,
      xAxis.nameTextStyle.textShadowOffsetX = 0,
      xAxis.nameTextStyle.textShadowOffsetY = 0,
      xAxis.axisLabel.color = "#033803",
      xAxis.axisLabel.overflow = "truncate",
      yAxis.nameTextStyle.fontSize = 20,
      yAxis.nameTextStyle.padding = 60,
      yAxis.nameTextStyle.color = "#034D03",
      yAxis.nameTextStyle.textShadowColor = "#325C33",
      yAxis.nameTextStyle.textShadowBlur = 15,
      yAxis.nameTextStyle.textShadowOffsetX = 0,
      yAxis.nameTextStyle.textShadowOffsetY = 0,
      yAxis.axisLabel.color = "#033803",
      tooltip.backgroundColor = "#E0FFE0",
      tooltip.textStyle.color = "#034D03",
      toolbox.iconStyle.borderColor = "#546654",
      toolbox.emphasis.iconStyle.borderColor = "#1D731D"
    ),
    `fresh-cut` = list(
      title.textStyle.color = "#0075AD",
      title.textStyle.fontWeight = "bolder",
      title.textStyle.textShadowColor = "#0075AD",
      title.textStyle.textShadowBlur = 15,
      title.textStyle.textShadowOffsetX = 0,
      title.textStyle.textShadowOffsetY = 0,
      title.top = 12,
      title.left = 12,
      title.padding = c(4, 4, 10, 4),
      title.itemGap = 6,
      title.subtextStyle.color = "#005882",
      title.subtextStyle.textShadowColor = "#0075AD",
      title.subtextStyle.textShadowBlur = 15,
      title.subtextStyle.textShadowOffsetX = 0,
      title.subtextStyle.textShadowOffsetY = 0,
      title.subtextStyle.fontWeight = "bold",
      xAxis.nameTextStyle.fontSize = 20,
      xAxis.nameTextStyle.color = "#13384D",
      xAxis.nameTextStyle.textShadowColor = "#0075AD",
      xAxis.nameTextStyle.textShadowBlur = 15,
      xAxis.nameTextStyle.textShadowOffsetX = 0,
      xAxis.nameTextStyle.textShadowOffsetY = 0,
      xAxis.axisLabel.color = "#13384D",
      xAxis.axisLabel.overflow = "truncate",
      yAxis.nameTextStyle.fontSize = 20,
      yAxis.nameTextStyle.padding = 60,
      yAxis.nameTextStyle.color = "#13384D",
      yAxis.nameTextStyle.textShadowColor = "#0075AD",
      yAxis.nameTextStyle.textShadowBlur = 15,
      yAxis.nameTextStyle.textShadowOffsetX = 0,
      yAxis.nameTextStyle.textShadowOffsetY = 0,
      yAxis.axisLabel.color = "#13384D",
      tooltip.backgroundColor = "#DEF4FF",
      tooltip.textStyle.color = "#13384D",
      toolbox.iconStyle.borderColor = "#0099E0",
      toolbox.emphasis.iconStyle.borderColor = "#001924"
    ),
    fruit = list(
      title.textStyle.color = "#FF9800",
      title.textStyle.fontWeight = "bolder",
      title.textStyle.textShadowColor = "#633B00",
      title.textStyle.textShadowBlur = 15,
      title.textStyle.textShadowOffsetX = 0,
      title.textStyle.textShadowOffsetY = 0,
      title.top = 12,
      title.left = 12,
      title.padding = c(4, 4, 10, 4),
      title.itemGap = 6,
      title.subtextStyle.color = "#C27600",
      title.subtextStyle.textShadowColor = "#633B00",
      title.subtextStyle.textShadowBlur = 15,
      title.subtextStyle.textShadowOffsetX = 0,
      title.subtextStyle.textShadowOffsetY = 0,
      title.subtextStyle.fontWeight = "bold",
      xAxis.nameTextStyle.fontSize = 20,
      xAxis.nameTextStyle.color = "#694000",
      xAxis.nameTextStyle.textShadowColor = "#633B00",
      xAxis.nameTextStyle.textShadowBlur = 15,
      xAxis.nameTextStyle.textShadowOffsetX = 0,
      xAxis.nameTextStyle.textShadowOffsetY = 0,
      xAxis.axisLabel.color = "#694000",
      xAxis.axisLabel.overflow = "truncate",
      yAxis.nameTextStyle.fontSize = 20,
      yAxis.nameTextStyle.padding = 60,
      yAxis.nameTextStyle.color = "#694000",
      yAxis.nameTextStyle.textShadowColor = "#633B00",
      yAxis.nameTextStyle.textShadowBlur = 15,
      yAxis.nameTextStyle.textShadowOffsetX = 0,
      yAxis.nameTextStyle.textShadowOffsetY = 0,
      yAxis.axisLabel.color = "#694000",
      tooltip.backgroundColor = "#FFF6E8",
      tooltip.textStyle.color = "#694000",
      toolbox.iconStyle.borderColor = "#FFC05E",
      toolbox.emphasis.iconStyle.borderColor = "#7A4900"
    ),
    gray = list(
      title.textStyle.color = "#454545",
      title.textStyle.fontWeight = "bolder",
      title.textStyle.textShadowColor = "#262626",
      title.textStyle.textShadowBlur = 15,
      title.textStyle.textShadowOffsetX = 0,
      title.textStyle.textShadowOffsetY = 0,
      title.top = 12,
      title.left = 12,
      title.padding = c(4, 4, 10, 4),
      title.itemGap = 6,
      title.subtextStyle.color = "#262626",
      title.subtextStyle.textShadowColor = "#262626",
      title.subtextStyle.textShadowBlur = 15,
      title.subtextStyle.textShadowOffsetX = 0,
      title.subtextStyle.textShadowOffsetY = 0,
      title.subtextStyle.fontWeight = "bold",
      xAxis.nameTextStyle.fontSize = 20,
      xAxis.nameTextStyle.color = "#454545",
      xAxis.nameTextStyle.textShadowColor = "#262626",
      xAxis.nameTextStyle.textShadowBlur = 15,
      xAxis.nameTextStyle.textShadowOffsetX = 0,
      xAxis.nameTextStyle.textShadowOffsetY = 0,
      xAxis.axisLabel.color = "#454545",
      xAxis.axisLabel.overflow = "truncate",
      yAxis.nameTextStyle.fontSize = 20,
      yAxis.nameTextStyle.padding = 60,
      yAxis.nameTextStyle.color = "#454545",
      yAxis.nameTextStyle.textShadowColor = "#262626",
      yAxis.nameTextStyle.textShadowBlur = 15,
      yAxis.nameTextStyle.textShadowOffsetX = 0,
      yAxis.nameTextStyle.textShadowOffsetY = 0,
      yAxis.axisLabel.color = "#454545",
      tooltip.backgroundColor = "#DBDBDB",
      tooltip.textStyle.color = "#454545",
      toolbox.iconStyle.borderColor = "#828282",
      toolbox.emphasis.iconStyle.borderColor = "#1950B5"
    ),
    green = list(
      title.textStyle.color = "#025910",
      title.textStyle.fontWeight = "bolder",
      title.textStyle.textShadowColor = "#002E08",
      title.textStyle.textShadowBlur = 15,
      title.textStyle.textShadowOffsetX = 0,
      title.textStyle.textShadowOffsetY = 0,
      title.top = 12,
      title.left = 12,
      title.padding = c(4, 4, 10, 4),
      title.itemGap = 6,
      title.subtextStyle.color = "#002E08",
      title.subtextStyle.textShadowColor = "#002E08",
      title.subtextStyle.textShadowBlur = 15,
      title.subtextStyle.textShadowOffsetX = 0,
      title.subtextStyle.textShadowOffsetY = 0,
      title.subtextStyle.fontWeight = "bold",
      xAxis.nameTextStyle.fontSize = 20,
      xAxis.nameTextStyle.color = "#025910",
      xAxis.nameTextStyle.textShadowColor = "#002E08",
      xAxis.nameTextStyle.textShadowBlur = 15,
      xAxis.nameTextStyle.textShadowOffsetX = 0,
      xAxis.nameTextStyle.textShadowOffsetY = 0,
      xAxis.axisLabel.color = "#025910",
      xAxis.axisLabel.overflow = "truncate",
      yAxis.nameTextStyle.fontSize = 20,
      yAxis.nameTextStyle.padding = 60,
      yAxis.nameTextStyle.color = "#025910",
      yAxis.nameTextStyle.textShadowColor = "#002E08",
      yAxis.nameTextStyle.textShadowBlur = 15,
      yAxis.nameTextStyle.textShadowOffsetX = 0,
      yAxis.nameTextStyle.textShadowOffsetY = 0,
      yAxis.axisLabel.color = "#025910",
      tooltip.backgroundColor = "#D9FFDF",
      tooltip.textStyle.color = "#025910",
      toolbox.iconStyle.borderColor = "#017A14",
      toolbox.emphasis.iconStyle.borderColor = "#002E08"
    ),
    halloween = list(
      title.textStyle.color = "#FFA629",
      title.textStyle.fontWeight = "bolder",
      title.textStyle.textShadowColor = "#6B3F00",
      title.textStyle.textShadowBlur = 15,
      title.textStyle.textShadowOffsetX = 0,
      title.textStyle.textShadowOffsetY = 0,
      title.top = 12,
      title.left = 12,
      title.padding = c(4, 4, 10, 4),
      title.itemGap = 6,
      title.subtextStyle.color = "#C27100",
      title.subtextStyle.textShadowColor = "#6B3F00",
      title.subtextStyle.textShadowBlur = 15,
      title.subtextStyle.textShadowOffsetX = 0,
      title.subtextStyle.textShadowOffsetY = 0,
      title.subtextStyle.fontWeight = "bold",
      xAxis.nameTextStyle.fontSize = 20,
      xAxis.nameTextStyle.color = "#424242",
      xAxis.nameTextStyle.textShadowColor = "#6B3F00",
      xAxis.nameTextStyle.textShadowBlur = 15,
      xAxis.nameTextStyle.textShadowOffsetX = 0,
      xAxis.nameTextStyle.textShadowOffsetY = 0,
      xAxis.axisLabel.color = "#424242",
      xAxis.axisLabel.overflow = "truncate",
      yAxis.nameTextStyle.fontSize = 20,
      yAxis.nameTextStyle.padding = 60,
      yAxis.nameTextStyle.color = "#424242",
      yAxis.nameTextStyle.textShadowColor = "#6B3F00",
      yAxis.nameTextStyle.textShadowBlur = 15,
      yAxis.nameTextStyle.textShadowOffsetX = 0,
      yAxis.nameTextStyle.textShadowOffsetY = 0,
      yAxis.axisLabel.color = "#424242",
      tooltip.backgroundColor = "#FFE2C7",
      tooltip.textStyle.color = "#424242",
      toolbox.iconStyle.borderColor = "#FF7C00",
      toolbox.emphasis.iconStyle.borderColor = "#6B3F00"
    ),
    helianthus = list(
      title.textStyle.color = "#858585",
      title.textStyle.fontWeight = "bolder",
      title.textStyle.textShadowColor = "#007ECC",
      title.textStyle.textShadowBlur = 15,
      title.textStyle.textShadowOffsetX = 0,
      title.textStyle.textShadowOffsetY = 0,
      title.top = 12,
      title.left = 12,
      title.padding = c(4, 4, 10, 4),
      title.itemGap = 6,
      title.subtextStyle.color = "#3D3D3D",
      title.subtextStyle.textShadowColor = "#007ECC",
      title.subtextStyle.textShadowBlur = 15,
      title.subtextStyle.textShadowOffsetX = 0,
      title.subtextStyle.textShadowOffsetY = 0,
      title.subtextStyle.fontWeight = "bold",
      xAxis.nameTextStyle.fontSize = 20,
      xAxis.nameTextStyle.color = "#3D3D3D",
      xAxis.nameTextStyle.textShadowColor = "#007ECC",
      xAxis.nameTextStyle.textShadowBlur = 15,
      xAxis.nameTextStyle.textShadowOffsetX = 0,
      xAxis.nameTextStyle.textShadowOffsetY = 0,
      xAxis.axisLabel.color = "#3D3D3D",
      xAxis.axisLabel.overflow = "truncate",
      yAxis.nameTextStyle.fontSize = 20,
      yAxis.nameTextStyle.padding = 60,
      yAxis.nameTextStyle.color = "#3D3D3D",
      yAxis.nameTextStyle.textShadowColor = "#007ECC",
      yAxis.nameTextStyle.textShadowBlur = 15,
      yAxis.nameTextStyle.textShadowOffsetX = 0,
      yAxis.nameTextStyle.textShadowOffsetY = 0,
      yAxis.axisLabel.color = "#3D3D3D",
      tooltip.backgroundColor = "#66DAFF",
      tooltip.textStyle.color = "#3D3D3D",
      toolbox.iconStyle.borderColor = "#3D3D3D",
      toolbox.emphasis.iconStyle.borderColor = "#007ECC"
    ),
    infographic = list(
      title.textStyle.color = "#3B95A3",
      title.textStyle.fontWeight = "bolder",
      title.textStyle.textShadowColor = "#005B69",
      title.textStyle.textShadowBlur = 15,
      title.textStyle.textShadowOffsetX = 0,
      title.textStyle.textShadowOffsetY = 0,
      title.top = 12,
      title.left = 12,
      title.padding = c(4, 4, 10, 4),
      title.itemGap = 6,
      title.subtextStyle.color = "#046F80",
      title.subtextStyle.textShadowColor = "#005B69",
      title.subtextStyle.textShadowBlur = 15,
      title.subtextStyle.textShadowOffsetX = 0,
      title.subtextStyle.textShadowOffsetY = 0,
      title.subtextStyle.fontWeight = "bold",
      xAxis.nameTextStyle.fontSize = 20,
      xAxis.nameTextStyle.color = "#046F80",
      xAxis.nameTextStyle.textShadowColor = "#005B69",
      xAxis.nameTextStyle.textShadowBlur = 15,
      xAxis.nameTextStyle.textShadowOffsetX = 0,
      xAxis.nameTextStyle.textShadowOffsetY = 0,
      xAxis.axisLabel.color = "#046F80",
      xAxis.axisLabel.overflow = "truncate",
      yAxis.nameTextStyle.fontSize = 20,
      yAxis.nameTextStyle.padding = 60,
      yAxis.nameTextStyle.color = "#046F80",
      yAxis.nameTextStyle.textShadowColor = "#005B69",
      yAxis.nameTextStyle.textShadowBlur = 15,
      yAxis.nameTextStyle.textShadowOffsetX = 0,
      yAxis.nameTextStyle.textShadowOffsetY = 0,
      yAxis.axisLabel.color = "#046F80",
      tooltip.backgroundColor = "#66DAFF",
      tooltip.textStyle.color = "#046F80",
      toolbox.iconStyle.borderColor = "#D60606",
      toolbox.emphasis.iconStyle.borderColor = "#005B69"
    ),
    inspired = list(
      title.textStyle.color = "#AB0000",
      title.textStyle.fontWeight = "bolder",
      title.textStyle.textShadowColor = "#4D0000",
      title.textStyle.textShadowBlur = 15,
      title.textStyle.textShadowOffsetX = 0,
      title.textStyle.textShadowOffsetY = 0,
      title.top = 12,
      title.left = 12,
      title.padding = c(4, 4, 10, 4),
      title.itemGap = 6,
      title.subtextStyle.color = "#300000",
      title.subtextStyle.textShadowColor = "#4D0000",
      title.subtextStyle.textShadowBlur = 15,
      title.subtextStyle.textShadowOffsetX = 0,
      title.subtextStyle.textShadowOffsetY = 0,
      title.subtextStyle.fontWeight = "bold",
      xAxis.nameTextStyle.fontSize = 20,
      xAxis.nameTextStyle.color = "#363636",
      xAxis.nameTextStyle.textShadowColor = "#4D0000",
      xAxis.nameTextStyle.textShadowBlur = 15,
      xAxis.nameTextStyle.textShadowOffsetX = 0,
      xAxis.nameTextStyle.textShadowOffsetY = 0,
      xAxis.axisLabel.color = "#363636",
      xAxis.axisLabel.overflow = "truncate",
      yAxis.nameTextStyle.fontSize = 20,
      yAxis.nameTextStyle.padding = 60,
      yAxis.nameTextStyle.color = "#363636",
      yAxis.nameTextStyle.textShadowColor = "#4D0000",
      yAxis.nameTextStyle.textShadowBlur = 15,
      yAxis.nameTextStyle.textShadowOffsetX = 0,
      yAxis.nameTextStyle.textShadowOffsetY = 0,
      yAxis.axisLabel.color = "#363636",
      tooltip.backgroundColor = "#BDBDBD",
      tooltip.textStyle.color = "#363636",
      toolbox.iconStyle.borderColor = "#AB0000",
      toolbox.emphasis.iconStyle.borderColor = "#000000"
    ),
    jazz = list(
      title.textStyle.color = "#81856F",
      title.textStyle.fontWeight = "bolder",
      title.textStyle.textShadowColor = "#6C6E51",
      title.textStyle.textShadowBlur = 15,
      title.textStyle.textShadowOffsetX = 0,
      title.textStyle.textShadowOffsetY = 0,
      title.top = 12,
      title.left = 12,
      title.padding = c(4, 4, 10, 4),
      title.itemGap = 6,
      title.subtextStyle.color = "#3C3D33",
      title.subtextStyle.textShadowColor = "#6C6E51",
      title.subtextStyle.textShadowBlur = 15,
      title.subtextStyle.textShadowOffsetX = 0,
      title.subtextStyle.textShadowOffsetY = 0,
      title.subtextStyle.fontWeight = "bold",
      xAxis.nameTextStyle.fontSize = 20,
      xAxis.nameTextStyle.color = "#3C3D33",
      xAxis.nameTextStyle.textShadowColor = "#6C6E51",
      xAxis.nameTextStyle.textShadowBlur = 15,
      xAxis.nameTextStyle.textShadowOffsetX = 0,
      xAxis.nameTextStyle.textShadowOffsetY = 0,
      xAxis.axisLabel.color = "#3C3D33",
      xAxis.axisLabel.overflow = "truncate",
      yAxis.nameTextStyle.fontSize = 20,
      yAxis.nameTextStyle.padding = 60,
      yAxis.nameTextStyle.color = "#3C3D33",
      yAxis.nameTextStyle.textShadowColor = "#6C6E51",
      yAxis.nameTextStyle.textShadowBlur = 15,
      yAxis.nameTextStyle.textShadowOffsetX = 0,
      yAxis.nameTextStyle.textShadowOffsetY = 0,
      yAxis.axisLabel.color = "#3C3D33",
      tooltip.backgroundColor = "#DFE0C1",
      tooltip.textStyle.color = "#7B7D54",
      toolbox.iconStyle.borderColor = "#4C609C",
      toolbox.emphasis.iconStyle.borderColor = "#132457"
    ),
    london = list(
      title.textStyle.color = "#506666",
      title.textStyle.fontWeight = "bolder",
      title.textStyle.textShadowColor = "#506666",
      title.textStyle.textShadowBlur = 15,
      title.textStyle.textShadowOffsetX = 0,
      title.textStyle.textShadowOffsetY = 0,
      title.top = 12,
      title.left = 12,
      title.padding = c(4, 4, 10, 4),
      title.itemGap = 6,
      title.subtextStyle.color = "#2F4040",
      title.subtextStyle.textShadowColor = "#506666",
      title.subtextStyle.textShadowBlur = 15,
      title.subtextStyle.textShadowOffsetX = 0,
      title.subtextStyle.textShadowOffsetY = 0,
      title.subtextStyle.fontWeight = "bold",
      xAxis.nameTextStyle.fontSize = 20,
      xAxis.nameTextStyle.color = "#2F4040",
      xAxis.nameTextStyle.textShadowColor = "#506666",
      xAxis.nameTextStyle.textShadowBlur = 15,
      xAxis.nameTextStyle.textShadowOffsetX = 0,
      xAxis.nameTextStyle.textShadowOffsetY = 0,
      xAxis.axisLabel.color = "#2F4040",
      xAxis.axisLabel.overflow = "truncate",
      yAxis.nameTextStyle.fontSize = 20,
      yAxis.nameTextStyle.padding = 60,
      yAxis.nameTextStyle.color = "#2F4040",
      yAxis.nameTextStyle.textShadowColor = "#506666",
      yAxis.nameTextStyle.textShadowBlur = 15,
      yAxis.nameTextStyle.textShadowOffsetX = 0,
      yAxis.nameTextStyle.textShadowOffsetY = 0,
      yAxis.axisLabel.color = "#2F4040",
      tooltip.backgroundColor = "#C8E3E3",
      tooltip.textStyle.color = "#2F4040",
      toolbox.iconStyle.borderColor = "#598080",
      toolbox.emphasis.iconStyle.borderColor = "#378080"
    ),
    v5 = list(
      title.textStyle.color = "#647073",
      title.textStyle.fontWeight = "bolder",
      title.textStyle.textShadowColor = "#353B3D",
      title.textStyle.textShadowBlur = 15,
      title.textStyle.textShadowOffsetX = 0,
      title.textStyle.textShadowOffsetY = 0,
      title.top = 12,
      title.left = 12,
      title.padding = c(4, 4, 10, 4),
      title.itemGap = 6,
      title.subtextStyle.color = "#647073",
      title.subtextStyle.textShadowColor = "#353B3D",
      title.subtextStyle.textShadowBlur = 15,
      title.subtextStyle.textShadowOffsetX = 0,
      title.subtextStyle.textShadowOffsetY = 0,
      title.subtextStyle.fontWeight = "bold",
      xAxis.nameTextStyle.fontSize = 20,
      xAxis.nameTextStyle.color = "#353B3D",
      xAxis.nameTextStyle.textShadowColor = "#353B3D",
      xAxis.nameTextStyle.textShadowBlur = 15,
      xAxis.nameTextStyle.textShadowOffsetX = 0,
      xAxis.nameTextStyle.textShadowOffsetY = 0,
      xAxis.axisLabel.color = "#647073",
      xAxis.axisLabel.overflow = "truncate",
      yAxis.nameTextStyle.fontSize = 20,
      yAxis.nameTextStyle.padding = 60,
      yAxis.nameTextStyle.color = "#353B3D",
      yAxis.nameTextStyle.textShadowColor = "#353B3D",
      yAxis.nameTextStyle.textShadowBlur = 15,
      yAxis.nameTextStyle.textShadowOffsetX = 0,
      yAxis.nameTextStyle.textShadowOffsetY = 0,
      yAxis.axisLabel.color = "#647073",
      tooltip.backgroundColor = "#647073",
      tooltip.textStyle.color = "#0010FF",
      toolbox.iconStyle.borderColor = "#353B3D",
      toolbox.emphasis.iconStyle.borderColor = "#0010FF"
    ),
    walden = list(
      title.textStyle.color = "#647073",
      title.textStyle.fontWeight = "bolder",
      title.textStyle.textShadowColor = "#353B3D",
      title.textStyle.textShadowBlur = 15,
      title.textStyle.textShadowOffsetX = 0,
      title.textStyle.textShadowOffsetY = 0,
      title.top = 12,
      title.left = 12,
      title.padding = c(4, 4, 10, 4),
      title.itemGap = 6,
      title.subtextStyle.color = "#647073",
      title.subtextStyle.textShadowColor = "#353B3D",
      title.subtextStyle.textShadowBlur = 15,
      title.subtextStyle.textShadowOffsetX = 0,
      title.subtextStyle.textShadowOffsetY = 0,
      title.subtextStyle.fontWeight = "bold",
      xAxis.nameTextStyle.fontSize = 20,
      xAxis.nameTextStyle.color = "#353B3D",
      xAxis.nameTextStyle.textShadowColor = "#353B3D",
      xAxis.nameTextStyle.textShadowBlur = 15,
      xAxis.nameTextStyle.textShadowOffsetX = 0,
      xAxis.nameTextStyle.textShadowOffsetY = 0,
      xAxis.axisLabel.color = "#647073",
      xAxis.axisLabel.overflow = "truncate",
      yAxis.nameTextStyle.fontSize = 20,
      yAxis.nameTextStyle.padding = 60,
      yAxis.nameTextStyle.color = "#353B3D",
      yAxis.nameTextStyle.textShadowColor = "#353B3D",
      yAxis.nameTextStyle.textShadowBlur = 15,
      yAxis.nameTextStyle.textShadowOffsetX = 0,
      yAxis.nameTextStyle.textShadowOffsetY = 0,
      yAxis.axisLabel.color = "#647073",
      tooltip.backgroundColor = "#647073",
      tooltip.textStyle.color = "#C4FEFF",
      toolbox.iconStyle.borderColor = "#353B3D",
      toolbox.emphasis.iconStyle.borderColor = "#00CCFF"
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
      legend.textStyle.color = "#006769",
      xAxis.nameTextStyle.fontSize = 20,
      xAxis.nameTextStyle.color = "#006769",
      xAxis.nameTextStyle.textShadowColor = "#00969E",
      xAxis.nameTextStyle.textShadowBlur = 15,
      xAxis.nameTextStyle.textShadowOffsetX = 0,
      xAxis.nameTextStyle.textShadowOffsetY = 0,
      xAxis.axisLabel.color = "#004142",
      xAxis.axisLabel.overflow = "truncate",
      yAxis.nameTextStyle.fontSize = 20,
      yAxis.nameTextStyle.padding = 60,
      yAxis.nameTextStyle.color = "#006769",
      yAxis.nameTextStyle.textShadowColor = "#00969E",
      yAxis.nameTextStyle.textShadowBlur = 15,
      yAxis.nameTextStyle.textShadowOffsetX = 0,
      yAxis.nameTextStyle.textShadowOffsetY = 0,
      yAxis.axisLabel.color = "#004142",
      tooltip.backgroundColor = "#004142",
      tooltip.textStyle.color = "#C4FEFF",
      toolbox.iconStyle.borderColor = "#00C1C4",
      toolbox.emphasis.iconStyle.borderColor = "#006466"
    ),
    macarons2 = list(
      title.textStyle.color = "#E6805E",
      title.textStyle.fontWeight = "bolder",
      title.textStyle.textShadowColor = "#FF9A7D",
      title.textStyle.textShadowBlur = 15,
      title.textStyle.textShadowOffsetX = 0,
      title.textStyle.textShadowOffsetY = 0,
      title.top = 12,
      title.left = 12,
      title.padding = c(4, 4, 10, 4),
      title.itemGap = 6,
      title.subtextStyle.color = "#B06046",
      title.subtextStyle.textShadowColor = "#FF9A7D",
      title.subtextStyle.textShadowBlur = 15,
      title.subtextStyle.textShadowOffsetX = 0,
      title.subtextStyle.textShadowOffsetY = 0,
      title.subtextStyle.fontWeight = "bold",
      xAxis.nameTextStyle.fontSize = 20,
      xAxis.nameTextStyle.color = "#591300",
      xAxis.nameTextStyle.textShadowColor = "#FF9A7D",
      xAxis.nameTextStyle.textShadowBlur = 15,
      xAxis.nameTextStyle.textShadowOffsetX = 0,
      xAxis.nameTextStyle.textShadowOffsetY = 0,
      xAxis.axisLabel.color = "#591300",
      xAxis.axisLabel.overflow = "truncate",
      yAxis.nameTextStyle.fontSize = 20,
      yAxis.nameTextStyle.padding = 60,
      yAxis.nameTextStyle.color = "#591300",
      yAxis.nameTextStyle.textShadowColor = "#FF9A7D",
      yAxis.nameTextStyle.textShadowBlur = 15,
      yAxis.nameTextStyle.textShadowOffsetX = 0,
      yAxis.nameTextStyle.textShadowOffsetY = 0,
      yAxis.axisLabel.color = "#591300",
      tooltip.backgroundColor = "#FFE3DB",
      tooltip.textStyle.color = "#591300",
      toolbox.iconStyle.borderColor = "#1D57A1",
      toolbox.emphasis.iconStyle.borderColor = "#4A1D0F"
    ),
    mint = list(
      title.textStyle.color = "#00966C",
      title.textStyle.fontWeight = "bolder",
      title.textStyle.textShadowColor = "#005E3E",
      title.textStyle.textShadowBlur = 15,
      title.textStyle.textShadowOffsetX = 0,
      title.textStyle.textShadowOffsetY = 0,
      title.top = 12,
      title.left = 12,
      title.padding = c(4, 4, 10, 4),
      title.itemGap = 6,
      title.subtextStyle.color = "#006348",
      title.subtextStyle.textShadowColor = "#005E3E",
      title.subtextStyle.textShadowBlur = 15,
      title.subtextStyle.textShadowOffsetX = 0,
      title.subtextStyle.textShadowOffsetY = 0,
      title.subtextStyle.fontWeight = "bold",
      xAxis.nameTextStyle.fontSize = 20,
      xAxis.nameTextStyle.color = "#003B28",
      xAxis.nameTextStyle.textShadowColor = "#005E3E",
      xAxis.nameTextStyle.textShadowBlur = 15,
      xAxis.nameTextStyle.textShadowOffsetX = 0,
      xAxis.nameTextStyle.textShadowOffsetY = 0,
      xAxis.axisLabel.color = "#003B28",
      xAxis.axisLabel.overflow = "truncate",
      yAxis.nameTextStyle.fontSize = 20,
      yAxis.nameTextStyle.padding = 60,
      yAxis.nameTextStyle.color = "#003B28",
      yAxis.nameTextStyle.textShadowColor = "#005E3E",
      yAxis.nameTextStyle.textShadowBlur = 15,
      yAxis.nameTextStyle.textShadowOffsetX = 0,
      yAxis.nameTextStyle.textShadowOffsetY = 0,
      yAxis.axisLabel.color = "#003B28",
      tooltip.backgroundColor = "#E0FFF6",
      tooltip.textStyle.color = "#003B28",
      toolbox.iconStyle.borderColor = "#00FFB5",
      toolbox.emphasis.iconStyle.borderColor = "#003B28"
    ),
    `purple-passion` = list(
      title.textStyle.color = "#F4E3FF",
      title.textStyle.fontWeight = "bolder",
      title.textStyle.textShadowColor = "#A46CC4",
      title.textStyle.textShadowBlur = 15,
      title.textStyle.textShadowOffsetX = 0,
      title.textStyle.textShadowOffsetY = 0,
      title.top = 12,
      title.left = 12,
      title.padding = c(4, 4, 10, 4),
      title.itemGap = 6,
      title.subtextStyle.color = "#ECCCFF",
      title.subtextStyle.textShadowColor = "#A46CC4",
      title.subtextStyle.textShadowBlur = 15,
      title.subtextStyle.textShadowOffsetX = 0,
      title.subtextStyle.textShadowOffsetY = 0,
      title.subtextStyle.fontWeight = "bold",
      xAxis.nameTextStyle.fontSize = 20,
      xAxis.nameTextStyle.color = "#F4E3FF",
      xAxis.nameTextStyle.textShadowColor = "#A46CC4",
      xAxis.nameTextStyle.textShadowBlur = 15,
      xAxis.nameTextStyle.textShadowOffsetX = 0,
      xAxis.nameTextStyle.textShadowOffsetY = 0,
      xAxis.axisLabel.color = "#F4E3FF",
      xAxis.axisLabel.overflow = "truncate",
      yAxis.nameTextStyle.fontSize = 20,
      yAxis.nameTextStyle.padding = 60,
      yAxis.nameTextStyle.color = "#F4E3FF",
      yAxis.nameTextStyle.textShadowColor = "#A46CC4",
      yAxis.nameTextStyle.textShadowBlur = 15,
      yAxis.nameTextStyle.textShadowOffsetX = 0,
      yAxis.nameTextStyle.textShadowOffsetY = 0,
      yAxis.axisLabel.color = "#F4E3FF",
      tooltip.backgroundColor = "#E0FFF6",
      tooltip.textStyle.color = "#F4E3FF",
      toolbox.iconStyle.borderColor = "#CBAFDB",
      toolbox.emphasis.iconStyle.borderColor = "#500080"
    ),
    rainbow = list(
      title.textStyle.color = "#00A5FF",
      title.textStyle.fontWeight = "bolder",
      title.textStyle.textShadowColor = "#002130",
      title.textStyle.textShadowBlur = 15,
      title.textStyle.textShadowOffsetX = 0,
      title.textStyle.textShadowOffsetY = 0,
      title.top = 12,
      title.left = 12,
      title.padding = c(4, 4, 10, 4),
      title.itemGap = 6,
      title.subtextStyle.color = "#0084C4",
      title.subtextStyle.textShadowColor = "#002130",
      title.subtextStyle.textShadowBlur = 15,
      title.subtextStyle.textShadowOffsetX = 0,
      title.subtextStyle.textShadowOffsetY = 0,
      title.subtextStyle.fontWeight = "bold",
      xAxis.nameTextStyle.fontSize = 20,
      xAxis.nameTextStyle.color = "#00A5FF",
      xAxis.nameTextStyle.textShadowColor = "#002130",
      xAxis.nameTextStyle.textShadowBlur = 15,
      xAxis.nameTextStyle.textShadowOffsetX = 0,
      xAxis.nameTextStyle.textShadowOffsetY = 0,
      xAxis.axisLabel.color = "#006596",
      xAxis.axisLabel.overflow = "truncate",
      yAxis.nameTextStyle.fontSize = 20,
      yAxis.nameTextStyle.padding = 60,
      yAxis.nameTextStyle.color = "#00A5FF",
      yAxis.nameTextStyle.textShadowColor = "#002130",
      yAxis.nameTextStyle.textShadowBlur = 15,
      yAxis.nameTextStyle.textShadowOffsetX = 0,
      yAxis.nameTextStyle.textShadowOffsetY = 0,
      yAxis.axisLabel.color = "#006596",
      tooltip.backgroundColor = "#A9E2FC",
      tooltip.textStyle.color = "#006596",
      toolbox.iconStyle.borderColor = "#006596",
      toolbox.emphasis.iconStyle.borderColor = "#002130"
    ),
    red = list(
      title.textStyle.color = "#FC0000",
      title.textStyle.fontWeight = "bolder",
      title.textStyle.textShadowColor = "#9E0000",
      title.textStyle.textShadowBlur = 15,
      title.textStyle.textShadowOffsetX = 0,
      title.textStyle.textShadowOffsetY = 0,
      title.top = 12,
      title.left = 12,
      title.padding = c(4, 4, 10, 4),
      title.itemGap = 6,
      title.subtextStyle.color = "#9E0000",
      title.subtextStyle.textShadowColor = "#9E0000",
      title.subtextStyle.textShadowBlur = 15,
      title.subtextStyle.textShadowOffsetX = 0,
      title.subtextStyle.textShadowOffsetY = 0,
      title.subtextStyle.fontWeight = "bold",
      xAxis.nameTextStyle.fontSize = 20,
      xAxis.nameTextStyle.color = "#FC0000",
      xAxis.nameTextStyle.textShadowColor = "#9E0000",
      xAxis.nameTextStyle.textShadowBlur = 15,
      xAxis.nameTextStyle.textShadowOffsetX = 0,
      xAxis.nameTextStyle.textShadowOffsetY = 0,
      xAxis.axisLabel.color = "#9E0000",
      xAxis.axisLabel.overflow = "truncate",
      yAxis.nameTextStyle.fontSize = 20,
      yAxis.nameTextStyle.padding = 60,
      yAxis.nameTextStyle.color = "#FC0000",
      yAxis.nameTextStyle.textShadowColor = "#9E0000",
      yAxis.nameTextStyle.textShadowBlur = 15,
      yAxis.nameTextStyle.textShadowOffsetX = 0,
      yAxis.nameTextStyle.textShadowOffsetY = 0,
      yAxis.axisLabel.color = "#9E0000",
      tooltip.backgroundColor = "#FFD1D1",
      tooltip.textStyle.color = "#9E0000",
      toolbox.iconStyle.borderColor = "#FF5959",
      toolbox.emphasis.iconStyle.borderColor = "#9E0000"
    ),
    `red-velvet` = list(
      title.textStyle.color = "#570000",
      title.textStyle.fontWeight = "bolder",
      title.textStyle.textShadowColor = "#004BFF",
      title.textStyle.textShadowBlur = 15,
      title.textStyle.textShadowOffsetX = 0,
      title.textStyle.textShadowOffsetY = 0,
      title.top = 12,
      title.left = 12,
      title.padding = c(4, 4, 10, 4),
      title.itemGap = 6,
      title.subtextStyle.color = "#3B0000",
      title.subtextStyle.textShadowColor = "#004BFF",
      title.subtextStyle.textShadowBlur = 15,
      title.subtextStyle.textShadowOffsetX = 0,
      title.subtextStyle.textShadowOffsetY = 0,
      title.subtextStyle.fontWeight = "bold",
      xAxis.nameTextStyle.fontSize = 20,
      xAxis.nameTextStyle.color = "#3B0000",
      xAxis.nameTextStyle.textShadowColor = "#004BFF",
      xAxis.nameTextStyle.textShadowBlur = 15,
      xAxis.nameTextStyle.textShadowOffsetX = 0,
      xAxis.nameTextStyle.textShadowOffsetY = 0,
      xAxis.axisLabel.color = "#002373",
      xAxis.axisLabel.overflow = "truncate",
      yAxis.nameTextStyle.fontSize = 20,
      yAxis.nameTextStyle.padding = 60,
      yAxis.nameTextStyle.color = "#3B0000",
      yAxis.nameTextStyle.textShadowColor = "#004BFF",
      yAxis.nameTextStyle.textShadowBlur = 15,
      yAxis.nameTextStyle.textShadowOffsetX = 0,
      yAxis.nameTextStyle.textShadowOffsetY = 0,
      yAxis.axisLabel.color = "#002373",
      tooltip.backgroundColor = "#DBE6FF",
      tooltip.textStyle.color = "#002373",
      toolbox.iconStyle.borderColor = "#3C538C",
      toolbox.emphasis.iconStyle.borderColor = "#004BFF"
    ),
    roma = list(
      title.textStyle.color = "#AB0035",
      title.textStyle.fontWeight = "bolder",
      title.textStyle.textShadowColor = "#FF78A4",
      title.textStyle.textShadowBlur = 15,
      title.textStyle.textShadowOffsetX = 0,
      title.textStyle.textShadowOffsetY = 0,
      title.top = 12,
      title.left = 12,
      title.padding = c(4, 4, 10, 4),
      title.itemGap = 6,
      title.subtextStyle.color = "#85002A",
      title.subtextStyle.textShadowColor = "#FF78A4",
      title.subtextStyle.textShadowBlur = 15,
      title.subtextStyle.textShadowOffsetX = 0,
      title.subtextStyle.textShadowOffsetY = 0,
      title.subtextStyle.fontWeight = "bold",
      xAxis.nameTextStyle.fontSize = 20,
      xAxis.nameTextStyle.color = "#5E001D",
      xAxis.nameTextStyle.textShadowColor = "#FF78A4",
      xAxis.nameTextStyle.textShadowBlur = 15,
      xAxis.nameTextStyle.textShadowOffsetX = 0,
      xAxis.nameTextStyle.textShadowOffsetY = 0,
      xAxis.axisLabel.color = "#002373",
      xAxis.axisLabel.overflow = "truncate",
      yAxis.nameTextStyle.fontSize = 20,
      yAxis.nameTextStyle.padding = 60,
      yAxis.nameTextStyle.color = "#5E001D",
      yAxis.nameTextStyle.textShadowColor = "#FF78A4",
      yAxis.nameTextStyle.textShadowBlur = 15,
      yAxis.nameTextStyle.textShadowOffsetX = 0,
      yAxis.nameTextStyle.textShadowOffsetY = 0,
      yAxis.axisLabel.color = "#002373",
      tooltip.backgroundColor = "#DBE6FF",
      tooltip.textStyle.color = "#002373",
      toolbox.iconStyle.borderColor = "#3C538C",
      toolbox.emphasis.iconStyle.borderColor = "#004BFF"
    ),
    royal = list(
      title.textStyle.color = "#154D70",
      title.textStyle.fontWeight = "bolder",
      title.textStyle.textShadowColor = "#0063A1",
      title.textStyle.textShadowBlur = 15,
      title.textStyle.textShadowOffsetX = 0,
      title.textStyle.textShadowOffsetY = 0,
      title.top = 12,
      title.left = 12,
      title.padding = c(4, 4, 10, 4),
      title.itemGap = 6,
      title.subtextStyle.color = "#0A3652",
      title.subtextStyle.textShadowColor = "#0063A1",
      title.subtextStyle.textShadowBlur = 15,
      title.subtextStyle.textShadowOffsetX = 0,
      title.subtextStyle.textShadowOffsetY = 0,
      title.subtextStyle.fontWeight = "bold",
      xAxis.nameTextStyle.fontSize = 20,
      xAxis.nameTextStyle.color = "#035182",
      xAxis.nameTextStyle.textShadowColor = "#0063A1",
      xAxis.nameTextStyle.textShadowBlur = 15,
      xAxis.nameTextStyle.textShadowOffsetX = 0,
      xAxis.nameTextStyle.textShadowOffsetY = 0,
      xAxis.axisLabel.color = "#006287",
      xAxis.axisLabel.overflow = "truncate",
      yAxis.nameTextStyle.fontSize = 20,
      yAxis.nameTextStyle.padding = 60,
      yAxis.nameTextStyle.color = "#035182",
      yAxis.nameTextStyle.textShadowColor = "#0063A1",
      yAxis.nameTextStyle.textShadowBlur = 15,
      yAxis.nameTextStyle.textShadowOffsetX = 0,
      yAxis.nameTextStyle.textShadowOffsetY = 0,
      yAxis.axisLabel.color = "#006287",
      tooltip.backgroundColor = "#C5EDFC",
      tooltip.textStyle.color = "#006287",
      toolbox.iconStyle.borderColor = "#21AAFF",
      toolbox.emphasis.iconStyle.borderColor = "#0063A1"
    ),
    sakura = list(
      title.textStyle.color = "#B0042B",
      title.textStyle.fontWeight = "bolder",
      title.textStyle.textShadowColor = "#223B85",
      title.textStyle.textShadowBlur = 15,
      title.textStyle.textShadowOffsetX = 0,
      title.textStyle.textShadowOffsetY = 0,
      title.top = 12,
      title.left = 12,
      title.padding = c(4, 4, 10, 4),
      title.itemGap = 6,
      title.subtextStyle.color = "#910323",
      title.subtextStyle.textShadowColor = "#223B85",
      title.subtextStyle.textShadowBlur = 15,
      title.subtextStyle.textShadowOffsetX = 0,
      title.subtextStyle.textShadowOffsetY = 0,
      title.subtextStyle.fontWeight = "bold",
      xAxis.nameTextStyle.fontSize = 20,
      xAxis.nameTextStyle.color = "#3B3B3B",
      xAxis.nameTextStyle.textShadowColor = "#223B85",
      xAxis.nameTextStyle.textShadowBlur = 15,
      xAxis.nameTextStyle.textShadowOffsetX = 0,
      xAxis.nameTextStyle.textShadowOffsetY = 0,
      xAxis.axisLabel.color = "#3B3B3B",
      xAxis.axisLabel.overflow = "truncate",
      yAxis.nameTextStyle.fontSize = 20,
      yAxis.nameTextStyle.padding = 60,
      yAxis.nameTextStyle.color = "#3B3B3B",
      yAxis.nameTextStyle.textShadowColor = "#223B85",
      yAxis.nameTextStyle.textShadowBlur = 15,
      yAxis.nameTextStyle.textShadowOffsetX = 0,
      yAxis.nameTextStyle.textShadowOffsetY = 0,
      yAxis.axisLabel.color = "#3B3B3B",
      tooltip.backgroundColor = "#DEE6FF",
      tooltip.textStyle.color = "#13339E",
      toolbox.iconStyle.borderColor = "#8790D4"
    ),
    `tech-blue` = list(
      title.textStyle.color = "#003EFF",
      title.textStyle.fontWeight = "bolder",
      title.textStyle.textShadowColor = "#3565FC",
      title.textStyle.textShadowBlur = 15,
      title.textStyle.textShadowOffsetX = 0,
      title.textStyle.textShadowOffsetY = 0,
      title.top = 12,
      title.left = 12,
      title.padding = c(4, 4, 10, 4),
      title.itemGap = 6,
      title.subtextStyle.color = "#0031C7",
      title.subtextStyle.textShadowColor = "#3565FC",
      title.subtextStyle.textShadowBlur = 15,
      title.subtextStyle.textShadowOffsetX = 0,
      title.subtextStyle.textShadowOffsetY = 0,
      title.subtextStyle.fontWeight = "bold",
      xAxis.nameTextStyle.fontSize = 20,
      xAxis.nameTextStyle.color = "#3B3B3B",
      xAxis.nameTextStyle.textShadowColor = "#3565FC",
      xAxis.nameTextStyle.textShadowBlur = 15,
      xAxis.nameTextStyle.textShadowOffsetX = 0,
      xAxis.nameTextStyle.textShadowOffsetY = 0,
      xAxis.axisLabel.color = "#3B3B3B",
      xAxis.axisLabel.overflow = "truncate",
      yAxis.nameTextStyle.fontSize = 20,
      yAxis.nameTextStyle.padding = 60,
      yAxis.nameTextStyle.color = "#3B3B3B",
      yAxis.nameTextStyle.textShadowColor = "#3565FC",
      yAxis.nameTextStyle.textShadowBlur = 15,
      yAxis.nameTextStyle.textShadowOffsetX = 0,
      yAxis.nameTextStyle.textShadowOffsetY = 0,
      yAxis.axisLabel.color = "#3B3B3B",
      tooltip.backgroundColor = "#DBE9FF",
      tooltip.textStyle.color = "#13339E",
      toolbox.iconStyle.borderColor = "#333333",
      toolbox.emphasis.iconStyle.borderColor = "#0031C7"
    ),
    weforum = list(
      title.textStyle.color = "#204780",
      title.textStyle.fontWeight = "bolder",
      title.textStyle.textShadowColor = "#4D6D9E",
      title.textStyle.textShadowBlur = 15,
      title.textStyle.textShadowOffsetX = 0,
      title.textStyle.textShadowOffsetY = 0,
      title.top = 12,
      title.left = 12,
      title.padding = c(4, 4, 10, 4),
      title.itemGap = 6,
      title.subtextStyle.color = "#103469",
      title.subtextStyle.textShadowColor = "#4D6D9E",
      title.subtextStyle.textShadowBlur = 15,
      title.subtextStyle.textShadowOffsetX = 0,
      title.subtextStyle.textShadowOffsetY = 0,
      title.subtextStyle.fontWeight = "bold",
      xAxis.nameTextStyle.fontSize = 20,
      xAxis.nameTextStyle.color = "#3B3B3B",
      xAxis.nameTextStyle.textShadowColor = "#4D6D9E",
      xAxis.nameTextStyle.textShadowBlur = 15,
      xAxis.nameTextStyle.textShadowOffsetX = 0,
      xAxis.nameTextStyle.textShadowOffsetY = 0,
      xAxis.axisLabel.color = "#3B3B3B",
      xAxis.axisLabel.overflow = "truncate",
      yAxis.nameTextStyle.fontSize = 20,
      yAxis.nameTextStyle.padding = 60,
      yAxis.nameTextStyle.color = "#3B3B3B",
      yAxis.nameTextStyle.textShadowColor = "#4D6D9E",
      yAxis.nameTextStyle.textShadowBlur = 15,
      yAxis.nameTextStyle.textShadowOffsetX = 0,
      yAxis.nameTextStyle.textShadowOffsetY = 0,
      yAxis.axisLabel.color = "#3B3B3B",
      tooltip.backgroundColor = "#BED1ED",
      tooltip.textStyle.color = "#204780",
      toolbox.iconStyle.borderColor = "#333333",
      toolbox.emphasis.iconStyle.borderColor = "#204780"
    ),
    wef = list(
      title.textStyle.color = "#4D5054",
      title.textStyle.fontWeight = "bolder",
      title.textStyle.textShadowColor = "#005ABF",
      title.textStyle.textShadowBlur = 15,
      title.textStyle.textShadowOffsetX = 0,
      title.textStyle.textShadowOffsetY = 0,
      title.top = 12,
      title.left = 12,
      title.padding = c(4, 4, 10, 4),
      title.itemGap = 6,
      title.subtextStyle.color = "#7E8396",
      title.subtextStyle.textShadowColor = "#005ABF",
      title.subtextStyle.textShadowBlur = 15,
      title.subtextStyle.textShadowOffsetX = 0,
      title.subtextStyle.textShadowOffsetY = 0,
      title.subtextStyle.fontWeight = "bold",
      xAxis.nameTextStyle.fontSize = 20,
      xAxis.nameTextStyle.color = "#4D5054",
      xAxis.axisLabel.color = "#727DAB",
      xAxis.axisLabel.overflow = "truncate",
      xAxis.nameTextStyle.textShadowColor = "#005ABF",
      xAxis.nameTextStyle.textShadowBlur = 15,
      xAxis.nameTextStyle.textShadowOffsetX = 0,
      xAxis.nameTextStyle.textShadowOffsetY = 0,
      yAxis.nameTextStyle.fontSize = 20,
      yAxis.nameTextStyle.padding = 60,
      yAxis.nameTextStyle.color = "#4D5054",
      yAxis.axisLabel.color = "#727DAB",
      yAxis.nameTextStyle.textShadowColor = "#005ABF",
      yAxis.nameTextStyle.textShadowBlur = 15,
      yAxis.nameTextStyle.textShadowOffsetX = 0,
      yAxis.nameTextStyle.textShadowOffsetY = 0,
      tooltip.backgroundColor = "#003169",
      tooltip.textStyle.color = "#ABB5DE",
      toolbox.iconStyle.borderColor = "#005ABF",
      toolbox.emphasis.iconStyle.borderColor = "#003169"
    ),
    NULL
  )
}

get_theme_defaults_plot <- function(theme = "dark", plot_type = NULL, grouped = FALSE) {
  switch(
    theme,
    caravan = switch(
      plot_type,
      Area = if(grouped) {
        NULL
      } else {
        list(
          areaStyle.color = c("#FFB17D","#FF8B3B","#FC6600"),
          areaStyle.opacity = c(1,0.8,0.35)
        )
      },
      Line = if(grouped) {
        NULL
      } else {
        list(
          lineStyle.color = c("#FFB17D","#FF8B3B","#FC6600"),
          lineStyle.width = 2,
          lineStyle.shadowColor = "#FF8B3B",
          lineStyle.shadowBlur = 10,
          lineStyle.shadowOffsetX = 2,
          lineStyle.shadowOffsetY = 2
        )
      },
      Step = if(grouped) {
        NULL
      } else {
        list(
          lineStyle.color = c("#FFB17D","#FF8B3B","#FC6600"),
          lineStyle.width = 2,
          lineStyle.shadowColor = "#FF8B3B",
          lineStyle.shadowBlur = 10,
          lineStyle.shadowOffsetX = 2,
          lineStyle.shadowOffsetY = 2
        )
      },
      Parallel = list(
        lineStyle.color = c("#FF8B3B")
      ),
      Bar = if(grouped) {
        NULL
      } else {
        list(
          backgroundStyle.color = c("#FFB17D","#FF8B3B","#FC6600"),
          backgroundStyle.opacity = c(1.0, 0.75, 0.25)
        )
      },
      Histogram = if(grouped) {
        NULL
      } else {
        list(
          backgroundStyle.color = c("#FFB17D","#FF8B3B","#FC6600"),
          backgroundStyle.opacity = c(1.0, 0.75, 0.25)
        )
      },
      Density = if(grouped) {
        NULL
      } else {
        list(
          areaStyle.color = c("#FFB17D","#FF8B3B","#FC6600"),
          areaStyle.opacity = c(1.0, 0.75, 0.25)
        )
      },
      Box = list(
        itemStyle.color = c("#FFB17D","#FF8B3B","#FC6600"),
        itemStyle.opacity = c(1.0, 0.75, 0.25)
      ),
      NULL
    ),
    carp = switch(
      plot_type,
      Area = if(grouped) {
        NULL
      } else {
        list(
          areaStyle.color = c("#FCD5A7","#FFC887","#FFB761"),
          areaStyle.opacity = c(1,0.8,0.35)
        )
      },
      Line = if(grouped) {
        NULL
      } else {
        list(
          lineStyle.color = c("#FCD5A7","#FFC887","#FFB761"),
          lineStyle.width = 2,
          lineStyle.shadowColor = "#FFB761",
          lineStyle.shadowBlur = 10,
          lineStyle.shadowOffsetX = 2,
          lineStyle.shadowOffsetY = 2
        )
      },
      Step = if(grouped) {
        NULL
      } else {
        list(
          lineStyle.color = c("#FCD5A7","#FFC887","#FFB761"),
          lineStyle.width = 2,
          lineStyle.shadowColor = "#FFB761",
          lineStyle.shadowBlur = 10,
          lineStyle.shadowOffsetX = 2,
          lineStyle.shadowOffsetY = 2
        )
      },
      Parallel = list(
        lineStyle.color = c("#FFB761")
      ),
      Bar = if(grouped) {
        NULL
      } else {
        list(
          backgroundStyle.color = c("#FCD5A7","#FFC887","#FFB761"),
          backgroundStyle.opacity = c(1.0, 0.75, 0.25)
        )
      },
      Histogram = if(grouped) {
        NULL
      } else {
        list(
          backgroundStyle.color = c("#FCD5A7","#FFC887","#FFB761"),
          backgroundStyle.opacity = c(1.0, 0.75, 0.25)
        )
      },
      Density = if(grouped) {
        NULL
      } else {
        list(
          areaStyle.color = c("#FCD5A7","#FFC887","#FFB761"),
          areaStyle.opacity = c(1.0, 0.75, 0.25)
        )
      },
      Box = list(
        itemStyle.color = c("#FCD5A7","#FFC887","#FFB761"),
        itemStyle.opacity = c(1.0, 0.75, 0.25)
      ),
      NULL
    ),
    chalk = switch(
      plot_type,
      Area = if(grouped) {
        NULL
      } else {
        list(
          areaStyle.color = c("#FF9CBD","#FF8AB2","#FF699C"),
          areaStyle.opacity = c(1,0.8,0.35)
        )
      },
      Line = if(grouped) {
        NULL
      } else {
        list(
          lineStyle.color = c("#FF9CBD","#FF8AB2","#FF699C"),
          lineStyle.width = 2,
          lineStyle.shadowColor = "#FF8AB2",
          lineStyle.shadowBlur = 10,
          lineStyle.shadowOffsetX = 2,
          lineStyle.shadowOffsetY = 2
        )
      },
      Step = if(grouped) {
        NULL
      } else {
        list(
          lineStyle.color = c("#FF9CBD","#FF8AB2","#FF699C"),
          lineStyle.width = 2,
          lineStyle.shadowColor = "#FF8AB2",
          lineStyle.shadowBlur = 10,
          lineStyle.shadowOffsetX = 2,
          lineStyle.shadowOffsetY = 2
        )
      },
      Parallel = list(
        lineStyle.color = c("#FF8AB2")
      ),
      Bar = if(grouped) {
        NULL
      } else {
        list(
          backgroundStyle.color = c("#FF9CBD","#FF8AB2","#FF699C"),
          backgroundStyle.opacity = c(1.0, 0.75, 0.25)
        )
      },
      Histogram = if(grouped) {
        NULL
      } else {
        list(
          backgroundStyle.color = c("#FF9CBD","#FF8AB2","#FF699C"),
          backgroundStyle.opacity = c(1.0, 0.75, 0.25)
        )
      },
      Density = if(grouped) {
        NULL
      } else {
        list(
          areaStyle.color = c("#FF9CBD","#FF8AB2","#FF699C"),
          areaStyle.opacity = c(1.0, 0.75, 0.25)
        )
      },
      Box = list(
        itemStyle.color = c("#FF9CBD","#FF8AB2","#FF699C"),
        itemStyle.opacity = c(1.0, 0.75, 0.25)
      ),
      NULL
    ),
    cool = switch(
      plot_type,
      Area = if(grouped) {
        NULL
      } else {
        list(
          areaStyle.color = c("#F03DFF","#ED0DFF","#C400D4"),
          areaStyle.opacity = c(1,0.8,0.35)
        )
      },
      Line = if(grouped) {
        NULL
      } else {
        list(
          lineStyle.color = c("#F03DFF","#ED0DFF","#C400D4"),
          lineStyle.width = 2,
          lineStyle.shadowColor = "#ED0DFF",
          lineStyle.shadowBlur = 10,
          lineStyle.shadowOffsetX = 2,
          lineStyle.shadowOffsetY = 2
        )
      },
      Step = if(grouped) {
        NULL
      } else {
        list(
          lineStyle.color = c("#F03DFF","#ED0DFF","#C400D4"),
          lineStyle.width = 2,
          lineStyle.shadowColor = "#ED0DFF",
          lineStyle.shadowBlur = 10,
          lineStyle.shadowOffsetX = 2,
          lineStyle.shadowOffsetY = 2
        )
      },
      Parallel = list(
        lineStyle.color = c("#ED0DFF")
      ),
      Bar = if(grouped) {
        NULL
      } else {
        list(
          backgroundStyle.color = c("#F03DFF","#ED0DFF","#C400D4"),
          backgroundStyle.opacity = c(1.0, 0.75, 0.25)
        )
      },
      Histogram = if(grouped) {
        NULL
      } else {
        list(
          backgroundStyle.color = c("#F03DFF","#ED0DFF","#C400D4"),
          backgroundStyle.opacity = c(1.0, 0.75, 0.25)
        )
      },
      Density = if(grouped) {
        NULL
      } else {
        list(
          areaStyle.color = c("#F03DFF","#ED0DFF","#C400D4"),
          areaStyle.opacity = c(1.0, 0.75, 0.25)
        )
      },
      Box = list(
        itemStyle.color = c("#F03DFF","#ED0DFF","#C400D4"),
        itemStyle.opacity = c(1.0, 0.75, 0.25)
      ),
      NULL
    ),
    `dark-blue` = switch(
      plot_type,
      Area = if(grouped) {
        NULL
      } else {
        list(
          areaStyle.color = c("#0033A1","#0033A1","#0033A1"),
          areaStyle.opacity = c(1,0.8,0.35)
        )
      },
      Line = if(grouped) {
        NULL
      } else {
        list(
          lineStyle.color = c("#0033A1","#0033A1","#0033A1"),
          lineStyle.width = 2,
          lineStyle.shadowColor = "#0033A1",
          lineStyle.shadowBlur = 10,
          lineStyle.shadowOffsetX = 2,
          lineStyle.shadowOffsetY = 2
        )
      },
      Step = if(grouped) {
        NULL
      } else {
        list(
          lineStyle.color = c("#0033A1","#0033A1","#0033A1"),
          lineStyle.width = 2,
          lineStyle.shadowColor = "#0033A1",
          lineStyle.shadowBlur = 10,
          lineStyle.shadowOffsetX = 2,
          lineStyle.shadowOffsetY = 2
        )
      },
      Parallel = list(
        lineStyle.color = c("#0033A1")
      ),
      Bar = if(grouped) {
        NULL
      } else {
        list(
          backgroundStyle.color = c("#0033A1","#0033A1","#0033A1"),
          backgroundStyle.opacity = c(1.0, 0.75, 0.25)
        )
      },
      Histogram = if(grouped) {
        NULL
      } else {
        list(
          backgroundStyle.color = c("#0033A1","#0033A1","#0033A1"),
          backgroundStyle.opacity = c(1.0, 0.75, 0.25)
        )
      },
      Density = if(grouped) {
        NULL
      } else {
        list(
          areaStyle.color = c("#0033A1","#0033A1","#0033A1"),
          areaStyle.opacity = c(1.0, 0.75, 0.25)
        )
      },
      Box = list(
        itemStyle.color = c("#0033A1","#0033A1","#0033A1"),
        itemStyle.opacity = c(1.0, 0.75, 0.25)
      ),
      NULL
    ),
    `dark-bold` = switch(
      plot_type,
      Area = if(grouped) {
        NULL
      } else {
        list(
          areaStyle.color = c("#8DFCA4","#69FF87","#47FF6C"),
          areaStyle.opacity = c(1,0.8,0.35)
        )
      },
      Line = if(grouped) {
        NULL
      } else {
        list(
          lineStyle.color = c("#8DFCA4","#69FF87","#47FF6C"),
          lineStyle.width = 2,
          lineStyle.shadowColor = "#69FF87",
          lineStyle.shadowBlur = 10,
          lineStyle.shadowOffsetX = 2,
          lineStyle.shadowOffsetY = 2
        )
      },
      Step = if(grouped) {
        NULL
      } else {
        list(
          lineStyle.color = c("#8DFCA4","#69FF87","#47FF6C"),
          lineStyle.width = 2,
          lineStyle.shadowColor = "#69FF87",
          lineStyle.shadowBlur = 10,
          lineStyle.shadowOffsetX = 2,
          lineStyle.shadowOffsetY = 2
        )
      },
      Parallel = list(
        lineStyle.color = c("#69FF87")
      ),
      Bar = if(grouped) {
        NULL
      } else {
        list(
          backgroundStyle.color = c("#8DFCA4","#69FF87","#47FF6C"),
          backgroundStyle.opacity = c(1.0, 0.75, 0.25)
        )
      },
      Histogram = if(grouped) {
        NULL
      } else {
        list(
          backgroundStyle.color = c("#8DFCA4","#69FF87","#47FF6C"),
          backgroundStyle.opacity = c(1.0, 0.75, 0.25)
        )
      },
      Density = if(grouped) {
        NULL
      } else {
        list(
          areaStyle.color = c("#8DFCA4","#69FF87","#47FF6C"),
          areaStyle.opacity = c(1.0, 0.75, 0.25)
        )
      },
      Box = list(
        itemStyle.color = c("#8DFCA4","#69FF87","#47FF6C"),
        itemStyle.opacity = c(1.0, 0.75, 0.25)
      ),
      NULL
    ),
    `dark-digerati` = switch(
      plot_type,
      Area = if(grouped) {
        NULL
      } else {
        list(
          areaStyle.color = c("#A1BBCF","#8FB3CF","#77AAD1"),
          areaStyle.opacity = c(1,0.8,0.35)
        )
      },
      Line = if(grouped) {
        NULL
      } else {
        list(
          lineStyle.color = c("#A1BBCF","#8FB3CF","#77AAD1"),
          lineStyle.width = 2,
          lineStyle.shadowColor = "#77AAD1",
          lineStyle.shadowBlur = 10,
          lineStyle.shadowOffsetX = 2,
          lineStyle.shadowOffsetY = 2
        )
      },
      Step = if(grouped) {
        NULL
      } else {
        list(
          lineStyle.color = c("#A1BBCF","#8FB3CF","#77AAD1"),
          lineStyle.width = 2,
          lineStyle.shadowColor = "#77AAD1",
          lineStyle.shadowBlur = 10,
          lineStyle.shadowOffsetX = 2,
          lineStyle.shadowOffsetY = 2
        )
      },
      Parallel = list(
        lineStyle.color = c("#77AAD1")
      ),
      Bar = if(grouped) {
        NULL
      } else {
        list(
          backgroundStyle.color = c("#A1BBCF","#8FB3CF","#77AAD1"),
          backgroundStyle.opacity = c(1.0, 0.75, 0.25)
        )
      },
      Histogram = if(grouped) {
        NULL
      } else {
        list(
          backgroundStyle.color = c("#A1BBCF","#8FB3CF","#77AAD1"),
          backgroundStyle.opacity = c(1.0, 0.75, 0.25)
        )
      },
      Density = if(grouped) {
        NULL
      } else {
        list(
          areaStyle.color = c("#A1BBCF","#8FB3CF","#77AAD1"),
          areaStyle.opacity = c(1.0, 0.75, 0.25)
        )
      },
      Box = list(
        itemStyle.color = c("#A1BBCF","#8FB3CF","#77AAD1"),
        itemStyle.opacity = c(1.0, 0.75, 0.25)
      ),
      NULL
    ),
    `dark-fresh-cut` = switch(
      plot_type,
      Area = if(grouped) {
        NULL
      } else {
        list(
          areaStyle.color = c("#42D9FF","#1FD2FF","#00CCFF"),
          areaStyle.opacity = c(1,0.8,0.35)
        )
      },
      Line = if(grouped) {
        NULL
      } else {
        list(
          lineStyle.color = c("#42D9FF","#1FD2FF","#00CCFF"),
          lineStyle.width = 2,
          lineStyle.shadowColor = "#1FD2FF",
          lineStyle.shadowBlur = 10,
          lineStyle.shadowOffsetX = 2,
          lineStyle.shadowOffsetY = 2
        )
      },
      Step = if(grouped) {
        NULL
      } else {
        list(
          lineStyle.color = c("#42D9FF","#1FD2FF","#00CCFF"),
          lineStyle.width = 2,
          lineStyle.shadowColor = "#1FD2FF",
          lineStyle.shadowBlur = 10,
          lineStyle.shadowOffsetX = 2,
          lineStyle.shadowOffsetY = 2
        )
      },
      Parallel = list(
        lineStyle.color = c("#1FD2FF")
      ),
      Bar = if(grouped) {
        NULL
      } else {
        list(
          backgroundStyle.color = c("#42D9FF","#1FD2FF","#00CCFF"),
          backgroundStyle.opacity = c(1.0, 0.75, 0.25)
        )
      },
      Histogram = if(grouped) {
        NULL
      } else {
        list(
          backgroundStyle.color = c("#42D9FF","#1FD2FF","#00CCFF"),
          backgroundStyle.opacity = c(1.0, 0.75, 0.25)
        )
      },
      Density = if(grouped) {
        NULL
      } else {
        list(
          areaStyle.color = c("#42D9FF","#1FD2FF","#00CCFF"),
          areaStyle.opacity = c(1.0, 0.75, 0.25)
        )
      },
      Box = list(
        itemStyle.color = c("#42D9FF","#1FD2FF","#00CCFF"),
        itemStyle.opacity = c(1.0, 0.75, 0.25)
      ),
      NULL
    ),
    `dark-mushroom` = switch(
      plot_type,
      Area = if(grouped) {
        NULL
      } else {
        list(
          areaStyle.color = c("#FF2F00","#FF4E26","#FC6C4C"),
          areaStyle.opacity = c(1,0.8,0.35)
        )
      },
      Line = if(grouped) {
        NULL
      } else {
        list(
          lineStyle.color = c("#FF2F00","#FF4E26","#FC6C4C"),
          lineStyle.width = 2,
          lineStyle.shadowColor = "#FF2F00",
          lineStyle.shadowBlur = 10,
          lineStyle.shadowOffsetX = 2,
          lineStyle.shadowOffsetY = 2
        )
      },
      Step = if(grouped) {
        NULL
      } else {
        list(
          lineStyle.color = c("#FF2F00","#FF4E26","#FC6C4C"),
          lineStyle.width = 2,
          lineStyle.shadowColor = "#FF2F00",
          lineStyle.shadowBlur = 10,
          lineStyle.shadowOffsetX = 2,
          lineStyle.shadowOffsetY = 2
        )
      },
      Parallel = list(
        lineStyle.color = c("#FF2F00")
      ),
      Bar = if(grouped) {
        NULL
      } else {
        list(
          backgroundStyle.color = c("#FF2F00","#FF4E26","#FC6C4C"),
          backgroundStyle.opacity = c(1.0, 0.75, 0.25)
        )
      },
      Histogram = if(grouped) {
        NULL
      } else {
        list(
          backgroundStyle.color = c("#FF2F00","#FF4E26","#FC6C4C"),
          backgroundStyle.opacity = c(1.0, 0.75, 0.25)
        )
      },
      Density = if(grouped) {
        NULL
      } else {
        list(
          areaStyle.color = c("#FF2F00","#FF4E26","#FC6C4C"),
          areaStyle.opacity = c(1.0, 0.75, 0.25)
        )
      },
      Box = list(
        itemStyle.color = c("#FF2F00","#FF4E26","#FC6C4C"),
        itemStyle.opacity = c(1.0, 0.75, 0.25)
      ),
      NULL
    ),
    eduardo = switch(
      plot_type,
      Area = if(grouped) {
        NULL
      } else {
        list(
          areaStyle.color = c("#5F627D","#51567D","#41487D"),
          areaStyle.opacity = c(1,0.8,0.35)
        )
      },
      Line = if(grouped) {
        NULL
      } else {
        list(
          lineStyle.color = c("#5F627D","#51567D","#41487D"),
          lineStyle.width = 2,
          lineStyle.shadowColor = "#51567D",
          lineStyle.shadowBlur = 10,
          lineStyle.shadowOffsetX = 2,
          lineStyle.shadowOffsetY = 2
        )
      },
      Step = if(grouped) {
        NULL
      } else {
        list(
          lineStyle.color = c("#5F627D","#51567D","#41487D"),
          lineStyle.width = 2,
          lineStyle.shadowColor = "#51567D",
          lineStyle.shadowBlur = 10,
          lineStyle.shadowOffsetX = 2,
          lineStyle.shadowOffsetY = 2
        )
      },
      Parallel = list(
        lineStyle.color = c("#51567D")
      ),
      Bar = if(grouped) {
        NULL
      } else {
        list(
          backgroundStyle.color = c("#5F627D","#51567D","#41487D"),
          backgroundStyle.opacity = c(1.0, 0.75, 0.25)
        )
      },
      Histogram = if(grouped) {
        NULL
      } else {
        list(
          backgroundStyle.color = c("#5F627D","#51567D","#41487D"),
          backgroundStyle.opacity = c(1.0, 0.75, 0.25)
        )
      },
      Density = if(grouped) {
        NULL
      } else {
        list(
          areaStyle.color = c("#5F627D","#51567D","#41487D"),
          areaStyle.opacity = c(1.0, 0.75, 0.25)
        )
      },
      Box = list(
        itemStyle.color = c("#5F627D","#51567D","#41487D"),
        itemStyle.opacity = c(1.0, 0.75, 0.25)
      ),
      NULL
    ),
    essos = switch(
      plot_type,
      Area = if(grouped) {
        NULL
      } else {
        list(
          areaStyle.color = c("#823535","#823535","#823535"),
          areaStyle.opacity = c(1,0.8,0.35)
        )
      },
      Line = if(grouped) {
        NULL
      } else {
        list(
          lineStyle.color = c("#823535","#823535","#823535"),
          lineStyle.width = 2,
          lineStyle.shadowColor = "#823535",
          lineStyle.shadowBlur = 10,
          lineStyle.shadowOffsetX = 2,
          lineStyle.shadowOffsetY = 2
        )
      },
      Step = if(grouped) {
        NULL
      } else {
        list(
          lineStyle.color = c("#823535","#823535","#823535"),
          lineStyle.width = 2,
          lineStyle.shadowColor = "#823535",
          lineStyle.shadowBlur = 10,
          lineStyle.shadowOffsetX = 2,
          lineStyle.shadowOffsetY = 2
        )
      },
      Parallel = list(
        lineStyle.color = c("#823535")
      ),
      Bar = if(grouped) {
        NULL
      } else {
        list(
          backgroundStyle.color = c("#823535","#823535","#823535"),
          backgroundStyle.opacity = c(1.0, 0.75, 0.25)
        )
      },
      Histogram = if(grouped) {
        NULL
      } else {
        list(
          backgroundStyle.color = c("#823535","#823535","#823535"),
          backgroundStyle.opacity = c(1.0, 0.75, 0.25)
        )
      },
      Density = if(grouped) {
        NULL
      } else {
        list(
          areaStyle.color = c("#823535","#823535","#823535"),
          areaStyle.opacity = c(1.0, 0.75, 0.25)
        )
      },
      Box = list(
        itemStyle.color = c("#823535","#823535","#823535"),
        itemStyle.opacity = c(1.0, 0.75, 0.25)
      ),
      NULL
    ),
    forest = switch(
      plot_type,
      Area = if(grouped) {
        NULL
      } else {
        list(
          areaStyle.color = c("#095E09","#1D591D","#2E5E2E"),
          areaStyle.opacity = c(1,0.8,0.35)
        )
      },
      Line = if(grouped) {
        NULL
      } else {
        list(
          lineStyle.color = c("#095E09","#1D591D","#2E5E2E"),
          lineStyle.width = 2,
          lineStyle.shadowColor = "#823535",
          lineStyle.shadowBlur = 10,
          lineStyle.shadowOffsetX = 2,
          lineStyle.shadowOffsetY = 2
        )
      },
      Step = if(grouped) {
        NULL
      } else {
        list(
          lineStyle.color = c("#095E09","#1D591D","#2E5E2E"),
          lineStyle.width = 2,
          lineStyle.shadowColor = "#823535",
          lineStyle.shadowBlur = 10,
          lineStyle.shadowOffsetX = 2,
          lineStyle.shadowOffsetY = 2
        )
      },
      Parallel = list(
        lineStyle.color = c("#823535")
      ),
      Bar = if(grouped) {
        NULL
      } else {
        list(
          backgroundStyle.color = c("#095E09","#1D591D","#2E5E2E"),
          backgroundStyle.opacity = c(1.0, 0.75, 0.25)
        )
      },
      Histogram = if(grouped) {
        NULL
      } else {
        list(
          backgroundStyle.color = c("#095E09","#1D591D","#2E5E2E"),
          backgroundStyle.opacity = c(1.0, 0.75, 0.25)
        )
      },
      Density = if(grouped) {
        NULL
      } else {
        list(
          areaStyle.color = c("#095E09","#1D591D","#2E5E2E"),
          areaStyle.opacity = c(1.0, 0.75, 0.25)
        )
      },
      Box = list(
        itemStyle.color = c("#095E09","#1D591D","#2E5E2E"),
        itemStyle.opacity = c(1.0, 0.75, 0.25)
      ),
      NULL
    ),
    `fresh-cut` = switch(
      plot_type,
      Area = if(grouped) {
        NULL
      } else {
        list(
          areaStyle.color = c("#00BFFF","#009FD4","#0086B3"),
          areaStyle.opacity = c(1,0.8,0.35)
        )
      },
      Line = if(grouped) {
        NULL
      } else {
        list(
          lineStyle.color = c("#00BFFF","#009FD4","#0086B3"),
          lineStyle.width = 2,
          lineStyle.shadowColor = "#00BFFF",
          lineStyle.shadowBlur = 10,
          lineStyle.shadowOffsetX = 2,
          lineStyle.shadowOffsetY = 2
        )
      },
      Step = if(grouped) {
        NULL
      } else {
        list(
          lineStyle.color = c("#00BFFF","#009FD4","#0086B3"),
          lineStyle.width = 2,
          lineStyle.shadowColor = "#00BFFF",
          lineStyle.shadowBlur = 10,
          lineStyle.shadowOffsetX = 2,
          lineStyle.shadowOffsetY = 2
        )
      },
      Parallel = list(
        lineStyle.color = c("#00BFFF")
      ),
      Bar = if(grouped) {
        NULL
      } else {
        list(
          backgroundStyle.color = c("#00BFFF","#009FD4","#0086B3"),
          backgroundStyle.opacity = c(1.0, 0.75, 0.25)
        )
      },
      Histogram = if(grouped) {
        NULL
      } else {
        list(
          backgroundStyle.color = c("#00BFFF","#009FD4","#0086B3"),
          backgroundStyle.opacity = c(1.0, 0.75, 0.25)
        )
      },
      Density = if(grouped) {
        NULL
      } else {
        list(
          areaStyle.color = c("#00BFFF","#009FD4","#0086B3"),
          areaStyle.opacity = c(1.0, 0.75, 0.25)
        )
      },
      Box = list(
        itemStyle.color = c("#00BFFF","#009FD4","#0086B3"),
        itemStyle.opacity = c(1.0, 0.75, 0.25)
      ),
      NULL
    ),
    fruit = switch(
      plot_type,
      Area = if(grouped) {
        NULL
      } else {
        list(
          areaStyle.color = c("#FF9800","#FFAE36","#FFC163"),
          areaStyle.opacity = c(1,0.8,0.35)
        )
      },
      Line = if(grouped) {
        NULL
      } else {
        list(
          lineStyle.color = c("#FF9800","#FFAE36","#FFC163"),
          lineStyle.width = 2,
          lineStyle.shadowColor = "#FF9800",
          lineStyle.shadowBlur = 10,
          lineStyle.shadowOffsetX = 2,
          lineStyle.shadowOffsetY = 2
        )
      },
      Step = if(grouped) {
        NULL
      } else {
        list(
          lineStyle.color = c("#FF9800","#FFAE36","#FFC163"),
          lineStyle.width = 2,
          lineStyle.shadowColor = "#FF9800",
          lineStyle.shadowBlur = 10,
          lineStyle.shadowOffsetX = 2,
          lineStyle.shadowOffsetY = 2
        )
      },
      Parallel = list(
        lineStyle.color = c("#FF9800")
      ),
      Bar = if(grouped) {
        NULL
      } else {
        list(
          backgroundStyle.color = c("#FF9800","#FFAE36","#FFC163"),
          backgroundStyle.opacity = c(1.0, 0.75, 0.25)
        )
      },
      Histogram = if(grouped) {
        NULL
      } else {
        list(
          backgroundStyle.color = c("#FF9800","#FFAE36","#FFC163"),
          backgroundStyle.opacity = c(1.0, 0.75, 0.25)
        )
      },
      Density = if(grouped) {
        NULL
      } else {
        list(
          areaStyle.color = c("#FF9800","#FFAE36","#FFC163"),
          areaStyle.opacity = c(1.0, 0.75, 0.25)
        )
      },
      Box = list(
        itemStyle.color = c("#FF9800","#FFAE36","#FFC163"),
        itemStyle.opacity = c(1.0, 0.75, 0.25)
      ),
      NULL
    ),
    gray = switch(
      plot_type,
      Area = if(grouped) {
        NULL
      } else {
        list(
          areaStyle.color = c("#707070","#8C8C8C","#ADADAD"),
          areaStyle.opacity = c(1,0.8,0.35)
        )
      },
      Line = if(grouped) {
        NULL
      } else {
        list(
          lineStyle.color = c("#707070","#8C8C8C","#ADADAD"),
          lineStyle.width = 2,
          lineStyle.shadowColor = "#707070",
          lineStyle.shadowBlur = 10,
          lineStyle.shadowOffsetX = 2,
          lineStyle.shadowOffsetY = 2
        )
      },
      Step = if(grouped) {
        NULL
      } else {
        list(
          lineStyle.color = c("#707070","#8C8C8C","#ADADAD"),
          lineStyle.width = 2,
          lineStyle.shadowColor = "#707070",
          lineStyle.shadowBlur = 10,
          lineStyle.shadowOffsetX = 2,
          lineStyle.shadowOffsetY = 2
        )
      },
      Parallel = list(
        lineStyle.color = c("#707070")
      ),
      Bar = if(grouped) {
        NULL
      } else {
        list(
          backgroundStyle.color = c("#707070","#8C8C8C","#ADADAD"),
          backgroundStyle.opacity = c(1.0, 0.75, 0.25)
        )
      },
      Histogram = if(grouped) {
        NULL
      } else {
        list(
          backgroundStyle.color = c("#707070","#8C8C8C","#ADADAD"),
          backgroundStyle.opacity = c(1.0, 0.75, 0.25)
        )
      },
      Density = if(grouped) {
        NULL
      } else {
        list(
          areaStyle.color = c("#707070","#8C8C8C","#ADADAD"),
          areaStyle.opacity = c(1.0, 0.75, 0.25)
        )
      },
      Box = list(
        itemStyle.color = c("#707070","#8C8C8C","#ADADAD"),
        itemStyle.opacity = c(1.0, 0.75, 0.25)
      ),
      NULL
    ),
    green = switch(
      plot_type,
      Area = if(grouped) {
        NULL
      } else {
        list(
          areaStyle.color = c("#008C18","#00B51F","#00FF2D"),
          areaStyle.opacity = c(1,0.8,0.35)
        )
      },
      Line = if(grouped) {
        NULL
      } else {
        list(
          lineStyle.color = c("#008C18","#00B51F","#00FF2D"),
          lineStyle.width = 2,
          lineStyle.shadowColor = "#008C18",
          lineStyle.shadowBlur = 10,
          lineStyle.shadowOffsetX = 2,
          lineStyle.shadowOffsetY = 2
        )
      },
      Step = if(grouped) {
        NULL
      } else {
        list(
          lineStyle.color = c("#008C18","#00B51F","#00FF2D"),
          lineStyle.width = 2,
          lineStyle.shadowColor = "#008C18",
          lineStyle.shadowBlur = 10,
          lineStyle.shadowOffsetX = 2,
          lineStyle.shadowOffsetY = 2
        )
      },
      Parallel = list(
        lineStyle.color = c("#008C18")
      ),
      Bar = if(grouped) {
        NULL
      } else {
        list(
          backgroundStyle.color = c("#008C18","#00B51F","#00FF2D"),
          backgroundStyle.opacity = c(1.0, 0.75, 0.25)
        )
      },
      Histogram = if(grouped) {
        NULL
      } else {
        list(
          backgroundStyle.color = c("#008C18","#00B51F","#00FF2D"),
          backgroundStyle.opacity = c(1.0, 0.75, 0.25)
        )
      },
      Density = if(grouped) {
        NULL
      } else {
        list(
          areaStyle.color = c("#008C18","#00B51F","#00FF2D"),
          areaStyle.opacity = c(1.0, 0.75, 0.25)
        )
      },
      Box = list(
        itemStyle.color = c("#008C18","#00B51F","#00FF2D"),
        itemStyle.opacity = c(1.0, 0.75, 0.25)
      ),
      NULL
    ),
    halloween = switch(
      plot_type,
      Area = if(grouped) {
        NULL
      } else {
        list(
          areaStyle.color = c("#FF4A29","#FF6347","#FF745C"),
          areaStyle.opacity = c(1,0.8,0.35)
        )
      },
      Line = if(grouped) {
        NULL
      } else {
        list(
          lineStyle.color = c("#FF4A29","#FF6347","#FF745C"),
          lineStyle.width = 2,
          lineStyle.shadowColor = "#FF4A29",
          lineStyle.shadowBlur = 10,
          lineStyle.shadowOffsetX = 2,
          lineStyle.shadowOffsetY = 2
        )
      },
      Step = if(grouped) {
        NULL
      } else {
        list(
          lineStyle.color = c("#FF4A29","#FF6347","#FF745C"),
          lineStyle.width = 2,
          lineStyle.shadowColor = "#FF4A29",
          lineStyle.shadowBlur = 10,
          lineStyle.shadowOffsetX = 2,
          lineStyle.shadowOffsetY = 2
        )
      },
      Parallel = list(
        lineStyle.color = c("#FF4A29")
      ),
      Bar = if(grouped) {
        NULL
      } else {
        list(
          backgroundStyle.color = c("#FF4A29","#FF6347","#FF745C"),
          backgroundStyle.opacity = c(1.0, 0.75, 0.25)
        )
      },
      Histogram = if(grouped) {
        NULL
      } else {
        list(
          backgroundStyle.color = c("#FF4A29","#FF6347","#FF745C"),
          backgroundStyle.opacity = c(1.0, 0.75, 0.25)
        )
      },
      Density = if(grouped) {
        NULL
      } else {
        list(
          areaStyle.color = c("#FF4A29","#FF6347","#FF745C"),
          areaStyle.opacity = c(1.0, 0.75, 0.25)
        )
      },
      Box = list(
        itemStyle.color = c("#FF4A29","#FF6347","#FF745C"),
        itemStyle.opacity = c(1.0, 0.75, 0.25)
      ),
      NULL
    ),
    helianthus = switch(
      plot_type,
      Area = if(grouped) {
        NULL
      } else {
        list(
          areaStyle.color = c("#21C9FF","#4DD4FF","#66DAFF"),
          areaStyle.opacity = c(1,0.8,0.35)
        )
      },
      Line = if(grouped) {
        NULL
      } else {
        list(
          lineStyle.color = c("#21C9FF","#4DD4FF","#66DAFF"),
          lineStyle.width = 2,
          lineStyle.shadowColor = "#21C9FF",
          lineStyle.shadowBlur = 10,
          lineStyle.shadowOffsetX = 2,
          lineStyle.shadowOffsetY = 2
        )
      },
      Step = if(grouped) {
        NULL
      } else {
        list(
          lineStyle.color = c("#21C9FF","#4DD4FF","#66DAFF"),
          lineStyle.width = 2,
          lineStyle.shadowColor = "#21C9FF",
          lineStyle.shadowBlur = 10,
          lineStyle.shadowOffsetX = 2,
          lineStyle.shadowOffsetY = 2
        )
      },
      Parallel = list(
        lineStyle.color = c("#21C9FF")
      ),
      Bar = if(grouped) {
        NULL
      } else {
        list(
          backgroundStyle.color = c("#21C9FF","#4DD4FF","#66DAFF"),
          backgroundStyle.opacity = c(1.0, 0.75, 0.25)
        )
      },
      Histogram = if(grouped) {
        NULL
      } else {
        list(
          backgroundStyle.color = c("#21C9FF","#4DD4FF","#66DAFF"),
          backgroundStyle.opacity = c(1.0, 0.75, 0.25)
        )
      },
      Density = if(grouped) {
        NULL
      } else {
        list(
          areaStyle.color = c("#21C9FF","#4DD4FF","#66DAFF"),
          areaStyle.opacity = c(1.0, 0.75, 0.25)
        )
      },
      Box = list(
        itemStyle.color = c("#21C9FF","#4DD4FF","#66DAFF"),
        itemStyle.opacity = c(1.0, 0.75, 0.25)
      ),
      NULL
    ),
    infographic = switch(
      plot_type,
      Area = if(grouped) {
        NULL
      } else {
        list(
          areaStyle.color = c("#D60606","#E02B2B","#D64747"),
          areaStyle.opacity = c(1,0.8,0.35)
        )
      },
      Line = if(grouped) {
        NULL
      } else {
        list(
          lineStyle.color = c("#D60606","#E02B2B","#D64747"),
          lineStyle.width = 2,
          lineStyle.shadowColor = "#D60606",
          lineStyle.shadowBlur = 10,
          lineStyle.shadowOffsetX = 2,
          lineStyle.shadowOffsetY = 2
        )
      },
      Step = if(grouped) {
        NULL
      } else {
        list(
          lineStyle.color = c("#D60606","#E02B2B","#D64747"),
          lineStyle.width = 2,
          lineStyle.shadowColor = "#D60606",
          lineStyle.shadowBlur = 10,
          lineStyle.shadowOffsetX = 2,
          lineStyle.shadowOffsetY = 2
        )
      },
      Parallel = list(
        lineStyle.color = c("#D60606")
      ),
      Bar = if(grouped) {
        NULL
      } else {
        list(
          backgroundStyle.color = c("#D60606","#E02B2B","#D64747"),
          backgroundStyle.opacity = c(1.0, 0.75, 0.25)
        )
      },
      Histogram = if(grouped) {
        NULL
      } else {
        list(
          backgroundStyle.color = c("#D60606","#E02B2B","#D64747"),
          backgroundStyle.opacity = c(1.0, 0.75, 0.25)
        )
      },
      Density = if(grouped) {
        NULL
      } else {
        list(
          areaStyle.color = c("#D60606","#E02B2B","#D64747"),
          areaStyle.opacity = c(1.0, 0.75, 0.25)
        )
      },
      Box = list(
        itemStyle.color = c("#D60606","#E02B2B","#D64747"),
        itemStyle.opacity = c(1.0, 0.75, 0.25)
      ),
      NULL
    ),
    inspired = switch(
      plot_type,
      Area = if(grouped) {
        NULL
      } else {
        list(
          areaStyle.color = c("#AB0000","#D10000","#FF0000"),
          areaStyle.opacity = c(1,0.8,0.35)
        )
      },
      Line = if(grouped) {
        NULL
      } else {
        list(
          lineStyle.color = c("#AB0000","#D10000","#FF0000"),
          lineStyle.width = 2,
          lineStyle.shadowColor = "#AB0000",
          lineStyle.shadowBlur = 10,
          lineStyle.shadowOffsetX = 2,
          lineStyle.shadowOffsetY = 2
        )
      },
      Step = if(grouped) {
        NULL
      } else {
        list(
          lineStyle.color = c("#AB0000","#D10000","#FF0000"),
          lineStyle.width = 2,
          lineStyle.shadowColor = "#AB0000",
          lineStyle.shadowBlur = 10,
          lineStyle.shadowOffsetX = 2,
          lineStyle.shadowOffsetY = 2
        )
      },
      Parallel = list(
        lineStyle.color = c("#AB0000")
      ),
      Bar = if(grouped) {
        NULL
      } else {
        list(
          backgroundStyle.color = c("#AB0000","#D10000","#FF0000"),
          backgroundStyle.opacity = c(1.0, 0.75, 0.25)
        )
      },
      Histogram = if(grouped) {
        NULL
      } else {
        list(
          backgroundStyle.color = c("#AB0000","#D10000","#FF0000"),
          backgroundStyle.opacity = c(1.0, 0.75, 0.25)
        )
      },
      Density = if(grouped) {
        NULL
      } else {
        list(
          areaStyle.color = c("#AB0000","#D10000","#FF0000"),
          areaStyle.opacity = c(1.0, 0.75, 0.25)
        )
      },
      Box = list(
        itemStyle.color = c("#AB0000","#D10000","#FF0000"),
        itemStyle.opacity = c(1.0, 0.75, 0.25)
      ),
      NULL
    ),
    jazz = switch(
      plot_type,
      Area = if(grouped) {
        NULL
      } else {
        list(
          areaStyle.color = c("#C1C79B","#D2D6B8","#E1E3D1"),
          areaStyle.opacity = c(1,0.8,0.35)
        )
      },
      Line = if(grouped) {
        NULL
      } else {
        list(
          lineStyle.color = c("#C1C79B","#D2D6B8","#E1E3D1"),
          lineStyle.width = 2,
          lineStyle.shadowColor = "#C1C79B",
          lineStyle.shadowBlur = 10,
          lineStyle.shadowOffsetX = 2,
          lineStyle.shadowOffsetY = 2
        )
      },
      Step = if(grouped) {
        NULL
      } else {
        list(
          lineStyle.color = c("#C1C79B","#D2D6B8","#E1E3D1"),
          lineStyle.width = 2,
          lineStyle.shadowColor = "#C1C79B",
          lineStyle.shadowBlur = 10,
          lineStyle.shadowOffsetX = 2,
          lineStyle.shadowOffsetY = 2
        )
      },
      Parallel = list(
        lineStyle.color = c("#C1C79B")
      ),
      Bar = if(grouped) {
        NULL
      } else {
        list(
          backgroundStyle.color = c("#C1C79B","#D2D6B8","#E1E3D1"),
          backgroundStyle.opacity = c(1.0, 0.75, 0.25)
        )
      },
      Histogram = if(grouped) {
        NULL
      } else {
        list(
          backgroundStyle.color = c("#C1C79B","#D2D6B8","#E1E3D1"),
          backgroundStyle.opacity = c(1.0, 0.75, 0.25)
        )
      },
      Density = if(grouped) {
        NULL
      } else {
        list(
          areaStyle.color = c("#C1C79B","#D2D6B8","#E1E3D1"),
          areaStyle.opacity = c(1.0, 0.75, 0.25)
        )
      },
      Box = list(
        itemStyle.color = c("#C1C79B","#D2D6B8","#E1E3D1"),
        itemStyle.opacity = c(1.0, 0.75, 0.25)
      ),
      NULL
    ),
    london = switch(
      plot_type,
      Area = if(grouped) {
        NULL
      } else {
        list(
          areaStyle.color = c("#2F4040","#3C4F4F","#506666"),
          areaStyle.opacity = c(1,0.8,0.35)
        )
      },
      Line = if(grouped) {
        NULL
      } else {
        list(
          lineStyle.color = c("#2F4040","#3C4F4F","#506666"),
          lineStyle.width = 2,
          lineStyle.shadowColor = "#2F4040",
          lineStyle.shadowBlur = 10,
          lineStyle.shadowOffsetX = 2,
          lineStyle.shadowOffsetY = 2
        )
      },
      Step = if(grouped) {
        NULL
      } else {
        list(
          lineStyle.color = c("#2F4040","#3C4F4F","#506666"),
          lineStyle.width = 2,
          lineStyle.shadowColor = "#2F4040",
          lineStyle.shadowBlur = 10,
          lineStyle.shadowOffsetX = 2,
          lineStyle.shadowOffsetY = 2
        )
      },
      Parallel = list(
        lineStyle.color = c("#2F4040")
      ),
      Bar = if(grouped) {
        NULL
      } else {
        list(
          backgroundStyle.color = c("#2F4040","#3C4F4F","#506666"),
          backgroundStyle.opacity = c(1.0, 0.75, 0.25)
        )
      },
      Histogram = if(grouped) {
        NULL
      } else {
        list(
          backgroundStyle.color = c("#2F4040","#3C4F4F","#506666"),
          backgroundStyle.opacity = c(1.0, 0.75, 0.25)
        )
      },
      Density = if(grouped) {
        NULL
      } else {
        list(
          areaStyle.color = c("#2F4040","#3C4F4F","#506666"),
          areaStyle.opacity = c(1.0, 0.75, 0.25)
        )
      },
      Box = list(
        itemStyle.color = c("#2F4040","#3C4F4F","#506666"),
        itemStyle.opacity = c(1.0, 0.75, 0.25)
      ),
      NULL
    ),
    macarons2 = switch(
      plot_type,
      Area = if(grouped) {
        NULL
      } else {
        list(
          areaStyle.color = c("#E6805E","#ED9274","#FFB299"),
          areaStyle.opacity = c(1,0.8,0.35)
        )
      },
      Line = if(grouped) {
        NULL
      } else {
        list(
          lineStyle.color = c("#E6805E","#ED9274","#FFB299"),
          lineStyle.width = 2,
          lineStyle.shadowColor = "#E6805E",
          lineStyle.shadowBlur = 10,
          lineStyle.shadowOffsetX = 2,
          lineStyle.shadowOffsetY = 2
        )
      },
      Step = if(grouped) {
        NULL
      } else {
        list(
          lineStyle.color = c("#E6805E","#ED9274","#FFB299"),
          lineStyle.width = 2,
          lineStyle.shadowColor = "#E6805E",
          lineStyle.shadowBlur = 10,
          lineStyle.shadowOffsetX = 2,
          lineStyle.shadowOffsetY = 2
        )
      },
      Parallel = list(
        lineStyle.color = c("#E6805E")
      ),
      Bar = if(grouped) {
        NULL
      } else {
        list(
          backgroundStyle.color = c("#E6805E","#ED9274","#FFB299"),
          backgroundStyle.opacity = c(1.0, 0.75, 0.25)
        )
      },
      Histogram = if(grouped) {
        NULL
      } else {
        list(
          backgroundStyle.color = c("#E6805E","#ED9274","#FFB299"),
          backgroundStyle.opacity = c(1.0, 0.75, 0.25)
        )
      },
      Density = if(grouped) {
        NULL
      } else {
        list(
          areaStyle.color = c("#E6805E","#ED9274","#FFB299"),
          areaStyle.opacity = c(1.0, 0.75, 0.25)
        )
      },
      Box = list(
        itemStyle.color = c("#E6805E","#ED9274","#FFB299"),
        itemStyle.opacity = c(1.0, 0.75, 0.25)
      ),
      NULL
    ),
    mint = switch(
      plot_type,
      Area = if(grouped) {
        NULL
      } else {
        list(
          areaStyle.color = c("#00C984","#00FFA8","#26FFB6"),
          areaStyle.opacity = c(1,0.8,0.35)
        )
      },
      Line = if(grouped) {
        NULL
      } else {
        list(
          lineStyle.color = c("#00C984","#00FFA8","#26FFB6"),
          lineStyle.width = 2,
          lineStyle.shadowColor = "#00C984",
          lineStyle.shadowBlur = 10,
          lineStyle.shadowOffsetX = 2,
          lineStyle.shadowOffsetY = 2
        )
      },
      Step = if(grouped) {
        NULL
      } else {
        list(
          lineStyle.color = c("#00C984","#00FFA8","#26FFB6"),
          lineStyle.width = 2,
          lineStyle.shadowColor = "#00C984",
          lineStyle.shadowBlur = 10,
          lineStyle.shadowOffsetX = 2,
          lineStyle.shadowOffsetY = 2
        )
      },
      Parallel = list(
        lineStyle.color = c("#00C984")
      ),
      Bar = if(grouped) {
        NULL
      } else {
        list(
          backgroundStyle.color = c("#00C984","#00FFA8","#26FFB6"),
          backgroundStyle.opacity = c(1.0, 0.75, 0.25)
        )
      },
      Histogram = if(grouped) {
        NULL
      } else {
        list(
          backgroundStyle.color = c("#00C984","#00FFA8","#26FFB6"),
          backgroundStyle.opacity = c(1.0, 0.75, 0.25)
        )
      },
      Density = if(grouped) {
        NULL
      } else {
        list(
          areaStyle.color = c("#00C984","#00FFA8","#26FFB6"),
          areaStyle.opacity = c(1.0, 0.75, 0.25)
        )
      },
      Box = list(
        itemStyle.color = c("#00C984","#00FFA8","#26FFB6"),
        itemStyle.opacity = c(1.0, 0.75, 0.25)
      ),
      NULL
    ),
    `purple-passion` = switch(
      plot_type,
      Area = if(grouped) {
        NULL
      } else {
        list(
          areaStyle.color = c("#B053E6","#B283D4","#B283D4"),
          areaStyle.opacity = c(1,0.8,0.35)
        )
      },
      Line = if(grouped) {
        NULL
      } else {
        list(
          lineStyle.color = c("#B053E6","#B283D4","#B283D4"),
          lineStyle.width = 2,
          lineStyle.shadowColor = "#B053E6",
          lineStyle.shadowBlur = 10,
          lineStyle.shadowOffsetX = 2,
          lineStyle.shadowOffsetY = 2
        )
      },
      Step = if(grouped) {
        NULL
      } else {
        list(
          lineStyle.color = c("#B053E6","#B283D4","#B283D4"),
          lineStyle.width = 2,
          lineStyle.shadowColor = "#B053E6",
          lineStyle.shadowBlur = 10,
          lineStyle.shadowOffsetX = 2,
          lineStyle.shadowOffsetY = 2
        )
      },
      Parallel = list(
        lineStyle.color = c("#B053E6")
      ),
      Bar = if(grouped) {
        NULL
      } else {
        list(
          backgroundStyle.color = c("#B053E6","#B283D4","#B283D4"),
          backgroundStyle.opacity = c(1.0, 0.75, 0.25)
        )
      },
      Histogram = if(grouped) {
        NULL
      } else {
        list(
          backgroundStyle.color = c("#B053E6","#B283D4","#B283D4"),
          backgroundStyle.opacity = c(1.0, 0.75, 0.25)
        )
      },
      Density = if(grouped) {
        NULL
      } else {
        list(
          areaStyle.color = c("#B053E6","#B283D4","#B283D4"),
          areaStyle.opacity = c(1.0, 0.75, 0.25)
        )
      },
      Box = list(
        itemStyle.color = c("#B053E6","#B283D4","#B283D4"),
        itemStyle.opacity = c(1.0, 0.75, 0.25)
      ),
      NULL
    ),
    rainbow = switch(
      plot_type,
      Area = if(grouped) {
        NULL
      } else {
        list(
          areaStyle.color = c("#0078BA","#00A5FF","#38B9FF"),
          areaStyle.opacity = c(1,0.8,0.35)
        )
      },
      Line = if(grouped) {
        NULL
      } else {
        list(
          lineStyle.color = c("#0078BA","#00A5FF","#38B9FF"),
          lineStyle.width = 2,
          lineStyle.shadowColor = "#0078BA",
          lineStyle.shadowBlur = 10,
          lineStyle.shadowOffsetX = 2,
          lineStyle.shadowOffsetY = 2
        )
      },
      Step = if(grouped) {
        NULL
      } else {
        list(
          lineStyle.color = c("#0078BA","#00A5FF","#38B9FF"),
          lineStyle.width = 2,
          lineStyle.shadowColor = "#0078BA",
          lineStyle.shadowBlur = 10,
          lineStyle.shadowOffsetX = 2,
          lineStyle.shadowOffsetY = 2
        )
      },
      Parallel = list(
        lineStyle.color = c("#0078BA")
      ),
      Bar = if(grouped) {
        NULL
      } else {
        list(
          backgroundStyle.color = c("#0078BA","#00A5FF","#38B9FF"),
          backgroundStyle.opacity = c(1.0, 0.75, 0.25)
        )
      },
      Histogram = if(grouped) {
        NULL
      } else {
        list(
          backgroundStyle.color = c("#0078BA","#00A5FF","#38B9FF"),
          backgroundStyle.opacity = c(1.0, 0.75, 0.25)
        )
      },
      Density = if(grouped) {
        NULL
      } else {
        list(
          areaStyle.color = c("#0078BA","#00A5FF","#38B9FF"),
          areaStyle.opacity = c(1.0, 0.75, 0.25)
        )
      },
      Box = list(
        itemStyle.color = c("#0078BA","#00A5FF","#38B9FF"),
        itemStyle.opacity = c(1.0, 0.75, 0.25)
      ),
      NULL
    ),
    red = switch(
      plot_type,
      Area = if(grouped) {
        NULL
      } else {
        list(
          areaStyle.color = c("#9E0000","#FC0000","#FC4C4C"),
          areaStyle.opacity = c(1,0.8,0.35)
        )
      },
      Line = if(grouped) {
        NULL
      } else {
        list(
          lineStyle.color = c("#9E0000","#FC0000","#FC4C4C"),
          lineStyle.width = 2,
          lineStyle.shadowColor = "#9E0000",
          lineStyle.shadowBlur = 10,
          lineStyle.shadowOffsetX = 2,
          lineStyle.shadowOffsetY = 2
        )
      },
      Step = if(grouped) {
        NULL
      } else {
        list(
          lineStyle.color = c("#9E0000","#FC0000","#FC4C4C"),
          lineStyle.width = 2,
          lineStyle.shadowColor = "#9E0000",
          lineStyle.shadowBlur = 10,
          lineStyle.shadowOffsetX = 2,
          lineStyle.shadowOffsetY = 2
        )
      },
      Parallel = list(
        lineStyle.color = c("#9E0000")
      ),
      Bar = if(grouped) {
        NULL
      } else {
        list(
          backgroundStyle.color = c("#9E0000","#FC0000","#FC4C4C"),
          backgroundStyle.opacity = c(1.0, 0.75, 0.25)
        )
      },
      Histogram = if(grouped) {
        NULL
      } else {
        list(
          backgroundStyle.color = c("#9E0000","#FC0000","#FC4C4C"),
          backgroundStyle.opacity = c(1.0, 0.75, 0.25)
        )
      },
      Density = if(grouped) {
        NULL
      } else {
        list(
          areaStyle.color = c("#9E0000","#FC0000","#FC4C4C"),
          areaStyle.opacity = c(1.0, 0.75, 0.25)
        )
      },
      Box = list(
        itemStyle.color = c("#9E0000","#FC0000","#FC4C4C"),
        itemStyle.opacity = c(1.0, 0.75, 0.25)
      ),
      NULL
    ),
    `red-velvet` = switch(
      plot_type,
      Area = if(grouped) {
        NULL
      } else {
        list(
          areaStyle.color = c("#570000","#800000","#AD0000"),
          areaStyle.opacity = c(1,0.8,0.35)
        )
      },
      Line = if(grouped) {
        NULL
      } else {
        list(
          lineStyle.color = c("#570000","#800000","#AD0000"),
          lineStyle.width = 2,
          lineStyle.shadowColor = "#570000",
          lineStyle.shadowBlur = 10,
          lineStyle.shadowOffsetX = 2,
          lineStyle.shadowOffsetY = 2
        )
      },
      Step = if(grouped) {
        NULL
      } else {
        list(
          lineStyle.color = c("#570000","#800000","#AD0000"),
          lineStyle.width = 2,
          lineStyle.shadowColor = "#570000",
          lineStyle.shadowBlur = 10,
          lineStyle.shadowOffsetX = 2,
          lineStyle.shadowOffsetY = 2
        )
      },
      Parallel = list(
        lineStyle.color = c("#570000")
      ),
      Bar = if(grouped) {
        NULL
      } else {
        list(
          backgroundStyle.color = c("#570000","#800000","#AD0000"),
          backgroundStyle.opacity = c(1.0, 0.75, 0.25)
        )
      },
      Histogram = if(grouped) {
        NULL
      } else {
        list(
          backgroundStyle.color = c("#570000","#800000","#AD0000"),
          backgroundStyle.opacity = c(1.0, 0.75, 0.25)
        )
      },
      Density = if(grouped) {
        NULL
      } else {
        list(
          areaStyle.color = c("#570000","#800000","#AD0000"),
          areaStyle.opacity = c(1.0, 0.75, 0.25)
        )
      },
      Box = list(
        itemStyle.color = c("#570000","#800000","#AD0000"),
        itemStyle.opacity = c(1.0, 0.75, 0.25)
      ),
      NULL
    ),
    roma = switch(
      plot_type,
      Area = if(grouped) {
        NULL
      } else {
        list(
          areaStyle.color = c("#AB0035","#D90044","#FF0051"),
          areaStyle.opacity = c(1,0.8,0.35)
        )
      },
      Line = if(grouped) {
        NULL
      } else {
        list(
          lineStyle.color = c("#AB0035","#D90044","#FF0051"),
          lineStyle.width = 2,
          lineStyle.shadowColor = "#AB0035",
          lineStyle.shadowBlur = 10,
          lineStyle.shadowOffsetX = 2,
          lineStyle.shadowOffsetY = 2
        )
      },
      Step = if(grouped) {
        NULL
      } else {
        list(
          lineStyle.color = c("#AB0035","#D90044","#FF0051"),
          lineStyle.width = 2,
          lineStyle.shadowColor = "#AB0035",
          lineStyle.shadowBlur = 10,
          lineStyle.shadowOffsetX = 2,
          lineStyle.shadowOffsetY = 2
        )
      },
      Parallel = list(
        lineStyle.color = c("#AB0035")
      ),
      Bar = if(grouped) {
        NULL
      } else {
        list(
          backgroundStyle.color = c("#AB0035","#D90044","#FF0051"),
          backgroundStyle.opacity = c(1.0, 0.75, 0.25)
        )
      },
      Histogram = if(grouped) {
        NULL
      } else {
        list(
          backgroundStyle.color = c("#AB0035","#D90044","#FF0051"),
          backgroundStyle.opacity = c(1.0, 0.75, 0.25)
        )
      },
      Density = if(grouped) {
        NULL
      } else {
        list(
          areaStyle.color = c("#AB0035","#D90044","#FF0051"),
          areaStyle.opacity = c(1.0, 0.75, 0.25)
        )
      },
      Box = list(
        itemStyle.color = c("#AB0035","#D90044","#FF0051"),
        itemStyle.opacity = c(1.0, 0.75, 0.25)
      ),
      NULL
    ),
    royal = switch(
      plot_type,
      Area = if(grouped) {
        NULL
      } else {
        list(
          areaStyle.color = c("#004573","#0069B0","#0096FC"),
          areaStyle.opacity = c(1,0.8,0.35)
        )
      },
      Line = if(grouped) {
        NULL
      } else {
        list(
          lineStyle.color = c("#004573","#0069B0","#0096FC"),
          lineStyle.width = 2,
          lineStyle.shadowColor = "#AB0035",
          lineStyle.shadowBlur = 10,
          lineStyle.shadowOffsetX = 2,
          lineStyle.shadowOffsetY = 2
        )
      },
      Step = if(grouped) {
        NULL
      } else {
        list(
          lineStyle.color = c("#004573","#0069B0","#0096FC"),
          lineStyle.width = 2,
          lineStyle.shadowColor = "#AB0035",
          lineStyle.shadowBlur = 10,
          lineStyle.shadowOffsetX = 2,
          lineStyle.shadowOffsetY = 2
        )
      },
      Parallel = list(
        lineStyle.color = c("#AB0035")
      ),
      Bar = if(grouped) {
        NULL
      } else {
        list(
          backgroundStyle.color = c("#004573","#0069B0","#0096FC"),
          backgroundStyle.opacity = c(1.0, 0.75, 0.25)
        )
      },
      Histogram = if(grouped) {
        NULL
      } else {
        list(
          backgroundStyle.color = c("#004573","#0069B0","#0096FC"),
          backgroundStyle.opacity = c(1.0, 0.75, 0.25)
        )
      },
      Density = if(grouped) {
        NULL
      } else {
        list(
          areaStyle.color = c("#004573","#0069B0","#0096FC"),
          areaStyle.opacity = c(1.0, 0.75, 0.25)
        )
      },
      Box = list(
        itemStyle.color = c("#004573","#0069B0","#0096FC"),
        itemStyle.opacity = c(1.0, 0.75, 0.25)
      ),
      NULL
    ),
    sakura = switch(
      plot_type,
      Area = if(grouped) {
        NULL
      } else {
        list(
          areaStyle.color = c("#EB0C3E","#DE2F57","#D64B6B"),
          areaStyle.opacity = c(1,0.8,0.35)
        )
      },
      Line = if(grouped) {
        NULL
      } else {
        list(
          lineStyle.color = c("#EB0C3E","#DE2F57","#D64B6B"),
          lineStyle.width = 2,
          lineStyle.shadowColor = "#EB0C3E",
          lineStyle.shadowBlur = 10,
          lineStyle.shadowOffsetX = 2,
          lineStyle.shadowOffsetY = 2
        )
      },
      Step = if(grouped) {
        NULL
      } else {
        list(
          lineStyle.color = c("#EB0C3E","#DE2F57","#D64B6B"),
          lineStyle.width = 2,
          lineStyle.shadowColor = "#EB0C3E",
          lineStyle.shadowBlur = 10,
          lineStyle.shadowOffsetX = 2,
          lineStyle.shadowOffsetY = 2
        )
      },
      Parallel = list(
        lineStyle.color = c("#EB0C3E")
      ),
      Bar = if(grouped) {
        NULL
      } else {
        list(
          backgroundStyle.color = c("#EB0C3E","#DE2F57","#D64B6B"),
          backgroundStyle.opacity = c(1.0, 0.75, 0.25)
        )
      },
      Histogram = if(grouped) {
        NULL
      } else {
        list(
          backgroundStyle.color = c("#EB0C3E","#DE2F57","#D64B6B"),
          backgroundStyle.opacity = c(1.0, 0.75, 0.25)
        )
      },
      Density = if(grouped) {
        NULL
      } else {
        list(
          areaStyle.color = c("#EB0C3E","#DE2F57","#D64B6B"),
          areaStyle.opacity = c(1.0, 0.75, 0.25)
        )
      },
      Box = list(
        itemStyle.color = c("#EB0C3E","#DE2F57","#D64B6B"),
        itemStyle.opacity = c(1.0, 0.75, 0.25)
      ),
      NULL
    ),
    `tech-blue` = switch(
      plot_type,
      Area = if(grouped) {
        NULL
      } else {
        list(
          areaStyle.color = c("#0031C7","#003EFF","#6195FF"),
          areaStyle.opacity = c(1,0.8,0.35)
        )
      },
      Line = if(grouped) {
        NULL
      } else {
        list(
          lineStyle.color = c("#0031C7","#003EFF","#6195FF"),
          lineStyle.width = 2,
          lineStyle.shadowColor = "#0031C7",
          lineStyle.shadowBlur = 10,
          lineStyle.shadowOffsetX = 2,
          lineStyle.shadowOffsetY = 2
        )
      },
      Step = if(grouped) {
        NULL
      } else {
        list(
          lineStyle.color = c("#0031C7","#003EFF","#6195FF"),
          lineStyle.width = 2,
          lineStyle.shadowColor = "#0031C7",
          lineStyle.shadowBlur = 10,
          lineStyle.shadowOffsetX = 2,
          lineStyle.shadowOffsetY = 2
        )
      },
      Parallel = list(
        lineStyle.color = c("#0031C7")
      ),
      Bar = if(grouped) {
        NULL
      } else {
        list(
          backgroundStyle.color = c("#0031C7","#003EFF","#6195FF"),
          backgroundStyle.opacity = c(1.0, 0.75, 0.25)
        )
      },
      Histogram = if(grouped) {
        NULL
      } else {
        list(
          backgroundStyle.color = c("#0031C7","#003EFF","#6195FF"),
          backgroundStyle.opacity = c(1.0, 0.75, 0.25)
        )
      },
      Density = if(grouped) {
        NULL
      } else {
        list(
          areaStyle.color = c("#0031C7","#003EFF","#6195FF"),
          areaStyle.opacity = c(1.0, 0.75, 0.25)
        )
      },
      Box = list(
        itemStyle.color = c("#0031C7","#003EFF","#6195FF"),
        itemStyle.opacity = c(1.0, 0.75, 0.25)
      ),
      NULL
    ),
    weforum = switch(
      plot_type,
      Area = if(grouped) {
        NULL
      } else {
        list(
          areaStyle.color = c("#204780","#375C91","#4D6D9E"),
          areaStyle.opacity = c(1,0.8,0.35)
        )
      },
      Line = if(grouped) {
        NULL
      } else {
        list(
          lineStyle.color = c("#204780","#375C91","#4D6D9E"),
          lineStyle.width = 2,
          lineStyle.shadowColor = "#204780",
          lineStyle.shadowBlur = 10,
          lineStyle.shadowOffsetX = 2,
          lineStyle.shadowOffsetY = 2
        )
      },
      Step = if(grouped) {
        NULL
      } else {
        list(
          lineStyle.color = c("#204780","#375C91","#4D6D9E"),
          lineStyle.width = 2,
          lineStyle.shadowColor = "#204780",
          lineStyle.shadowBlur = 10,
          lineStyle.shadowOffsetX = 2,
          lineStyle.shadowOffsetY = 2
        )
      },
      Parallel = list(
        lineStyle.color = c("#204780")
      ),
      Bar = if(grouped) {
        NULL
      } else {
        list(
          backgroundStyle.color = c("#204780","#375C91","#4D6D9E"),
          backgroundStyle.opacity = c(1.0, 0.75, 0.25)
        )
      },
      Histogram = if(grouped) {
        NULL
      } else {
        list(
          backgroundStyle.color = c("#204780","#375C91","#4D6D9E"),
          backgroundStyle.opacity = c(1.0, 0.75, 0.25)
        )
      },
      Density = if(grouped) {
        NULL
      } else {
        list(
          areaStyle.color = c("#204780","#375C91","#4D6D9E"),
          areaStyle.opacity = c(1.0, 0.75, 0.25)
        )
      },
      Box = list(
        itemStyle.color = c("#204780","#375C91","#4D6D9E"),
        itemStyle.opacity = c(1.0, 0.75, 0.25)
      ),
      NULL
    ),
    v5 = switch(
      plot_type,
      Area = if(grouped) {
        NULL
      } else {
        list(
          areaStyle.color = c("#000CB3","#0010FF","#4753FF"),
          areaStyle.opacity = c(1,0.8,0.35)
        )
      },
      Line = if(grouped) {
        NULL
      } else {
        list(
          lineStyle.color = c("#000CB3","#0010FF","#4753FF"),
          lineStyle.width = 2,
          lineStyle.shadowColor = "#0010FF",
          lineStyle.shadowBlur = 10,
          lineStyle.shadowOffsetX = 2,
          lineStyle.shadowOffsetY = 2
        )
      },
      Step = if(grouped) {
        NULL
      } else {
        list(
          lineStyle.color = c("#000CB3","#0010FF","#4753FF"),
          lineStyle.width = 2,
          lineStyle.shadowColor = "#0010FF",
          lineStyle.shadowBlur = 10,
          lineStyle.shadowOffsetX = 2,
          lineStyle.shadowOffsetY = 2
        )
      },
      Parallel = list(
        lineStyle.color = c("#0010FF")
      ),
      Bar = if(grouped) {
        NULL
      } else {
        list(
          backgroundStyle.color = c("#000CB3","#0010FF","#4753FF"),
          backgroundStyle.opacity = c(1.0, 0.75, 0.25)
        )
      },
      Histogram = if(grouped) {
        NULL
      } else {
        list(
          backgroundStyle.color = c("#000CB3","#0010FF","#4753FF"),
          backgroundStyle.opacity = c(1.0, 0.75, 0.25)
        )
      },
      Density = if(grouped) {
        NULL
      } else {
        list(
          areaStyle.color = c("#000CB3","#0010FF","#4753FF"),
          areaStyle.opacity = c(1.0, 0.75, 0.25)
        )
      },
      Box = list(
        itemStyle.color = c("#000CB3","#0010FF","#4753FF"),
        itemStyle.opacity = c(1.0, 0.75, 0.25)
      ),
      NULL
    ),
    `bee-inspired` = switch(
      plot_type,
      Area = if(grouped) {
        NULL
      } else {
        list(
          areaStyle.color = c("#B8B800","#B8B800","#B8B800"),
          areaStyle.opacity = c(1,0.8,0.35)
        )
      },
      Line = if(grouped) {
        NULL
      } else {
        list(
          lineStyle.color = c("#B8B800","#B8B800","#B8B800"),
          lineStyle.width = 2,
          lineStyle.shadowColor = "#B8B800",
          lineStyle.shadowBlur = 10,
          lineStyle.shadowOffsetX = 2,
          lineStyle.shadowOffsetY = 2
        )
      },
      Step = if(grouped) {
        NULL
      } else {
        list(
          lineStyle.color = c("#B8B800","#B8B800","#B8B800"),
          lineStyle.width = 2,
          lineStyle.shadowColor = "#B8B800",
          lineStyle.shadowBlur = 10,
          lineStyle.shadowOffsetX = 2,
          lineStyle.shadowOffsetY = 2
        )
      },
      Parallel = list(
        lineStyle.color = c("#B8B800")
      ),
      Bar = if(grouped) {
        NULL
      } else {
        list(
          backgroundStyle.color = c("#B8B800","#B8B800","#B8B800"),
          backgroundStyle.opacity = c(1.0, 0.75, 0.25)
        )
      },
      Histogram = if(grouped) {
        NULL
      } else {
        list(
          backgroundStyle.color = c("#B8B800","#B8B800","#B8B800"),
          backgroundStyle.opacity = c(1.0, 0.75, 0.25)
        )
      },
      Density = if(grouped) {
        NULL
      } else {
        list(
          areaStyle.color = c("#B8B800","#B8B800","#B8B800"),
          areaStyle.opacity = c(1.0, 0.75, 0.25)
        )
      },
      Box = list(
        itemStyle.color = c("#B8B800","#B8B800","#B8B800"),
        itemStyle.opacity = c(1.0, 0.75, 0.25)
      ),
      NULL
    ),
    walden = switch(
      plot_type,
      Area = if(grouped) {
        NULL
      } else {
        list(
          areaStyle.color = c("#47D3FF","#00C3FF","#008CBA"),
          areaStyle.opacity = c(1,0.8,0.35)
        )
      },
      Line = if(grouped) {
        NULL
      } else {
        list(
          lineStyle.color = c("#47D3FF","#00C3FF","#008CBA"),
          lineStyle.width = 2,
          lineStyle.shadowColor = "#008CBA",
          lineStyle.shadowBlur = 10,
          lineStyle.shadowOffsetX = 2,
          lineStyle.shadowOffsetY = 2
        )
      },
      Step = if(grouped) {
        NULL
      } else {
        list(
          lineStyle.color = c("#47D3FF","#00C3FF","#008CBA"),
          lineStyle.width = 2,
          lineStyle.shadowColor = "#008CBA",
          lineStyle.shadowBlur = 10,
          lineStyle.shadowOffsetX = 2,
          lineStyle.shadowOffsetY = 2
        )
      },
      Parallel = list(
        lineStyle.color = c("#008CBA")
      ),
      Bar = if(grouped) {
        NULL
      } else {
        list(
          backgroundStyle.color = c("#47D3FF","#00C3FF","#008CBA"),
          backgroundStyle.opacity = c(1.0, 0.75, 0.25)
        )
      },
      Histogram = if(grouped) {
        NULL
      } else {
        list(
          backgroundStyle.color = c("#47D3FF","#00C3FF","#008CBA"),
          backgroundStyle.opacity = c(1.0, 0.75, 0.25)
        )
      },
      Density = if(grouped) {
        NULL
      } else {
        list(
          areaStyle.color = c("#47D3FF","#00C3FF","#008CBA"),
          areaStyle.opacity = c(1.0, 0.75, 0.25)
        )
      },
      Box = list(
        itemStyle.color = c("#47D3FF","#00C3FF","#008CBA"),
        itemStyle.opacity = c(1.0, 0.75, 0.25)
      ),
      NULL
    ),
    wef = switch(
      plot_type,
      Area = if(grouped) {
        NULL
      } else {
        list(
          areaStyle.color = c("#429AFF","#0078FF","#005ABF"),
          areaStyle.opacity = c(1,0.8,0.35)
        )
      },
      Line = if(grouped) {
        NULL
      } else {
        list(
          lineStyle.color = c("#429AFF","#0078FF","#005ABF"),
          lineStyle.width = 2,
          lineStyle.shadowColor = "#429AFF",
          lineStyle.shadowBlur = 10,
          lineStyle.shadowOffsetX = 2,
          lineStyle.shadowOffsetY = 2
        )
      },
      Step = if(grouped) {
        NULL
      } else {
        list(
          lineStyle.color = c("#429AFF","#0078FF","#005ABF"),
          lineStyle.width = 2,
          lineStyle.shadowColor = "#429AFF",
          lineStyle.shadowBlur = 10,
          lineStyle.shadowOffsetX = 2,
          lineStyle.shadowOffsetY = 2
        )
      },
      Parallel = list(
        lineStyle.color = c("#005ABF")
      ),
      Bar = if(grouped) {
        NULL
      } else {
        list(
          backgroundStyle.color = c("#429AFF","#0078FF","#005ABF"),
          backgroundStyle.opacity = c(1.0, 0.75, 0.25)
        )
      },
      Histogram = if(grouped) {
        NULL
      } else {
        list(
          backgroundStyle.color = c("#429AFF","#0078FF","#005ABF"),
          backgroundStyle.opacity = c(1.0, 0.75, 0.25)
        )
      },
      Density = if(grouped) {
        NULL
      } else {
        list(
          areaStyle.color = c("#429AFF","#0078FF","#005ABF"),
          areaStyle.opacity = c(1.0, 0.75, 0.25)
        )
      },
      Box = list(
        itemStyle.color = c("#429AFF","#0078FF","#005ABF"),
        itemStyle.opacity = c(1.0, 0.75, 0.25)
      ),
      NULL
    ),
    azul = switch(
      plot_type,
      Area = if(grouped) {
        NULL
      } else {
        list(
          areaStyle.color = c("#429AFF","#0078FF","#005ABF"),
          areaStyle.opacity = c(1,0.8,0.35)
        )
      },
      Line = if(grouped) {
        NULL
      } else {
        list(
          lineStyle.color = c("#429AFF","#0078FF","#005ABF"),
          lineStyle.width = 2,
          lineStyle.shadowColor = "#429AFF",
          lineStyle.shadowBlur = 10,
          lineStyle.shadowOffsetX = 2,
          lineStyle.shadowOffsetY = 2
        )
      },
      Step = if(grouped) {
        NULL
      } else {
        list(
          lineStyle.color = c("#429AFF","#0078FF","#005ABF"),
          lineStyle.width = 2,
          lineStyle.shadowColor = "#429AFF",
          lineStyle.shadowBlur = 10,
          lineStyle.shadowOffsetX = 2,
          lineStyle.shadowOffsetY = 2
        )
      },
      Parallel = list(
        lineStyle.color = c("#005ABF")
      ),
      Bar = if(grouped) {
        NULL
      } else {
        list(
          backgroundStyle.color = c("#429AFF","#0078FF","#005ABF"),
          backgroundStyle.opacity = c(1.0, 0.75, 0.25)
        )
      },
      Histogram = if(grouped) {
        NULL
      } else {
        list(
          backgroundStyle.color = c("#429AFF","#0078FF","#005ABF"),
          backgroundStyle.opacity = c(1.0, 0.75, 0.25)
        )
      },
      Density = if(grouped) {
        NULL
      } else {
        list(
          areaStyle.color = c("#429AFF","#0078FF","#005ABF"),
          areaStyle.opacity = c(1.0, 0.75, 0.25)
        )
      },
      Box = list(
        itemStyle.color = c("#429AFF","#0078FF","#005ABF"),
        itemStyle.opacity = c(1.0, 0.75, 0.25)
      ),
      NULL
    ),
    auritus = switch(
      plot_type,
      Area = if(grouped) {
        NULL
      } else {
        list(
          areaStyle.color = c("#FF004C","#FC4E84","#B87B8E"),
          areaStyle.opacity = c(1,0.8,0.35)
        )
      },
      Line = if(grouped) {
        NULL
      } else {
        list(
          lineStyle.color = c("#FF004C","#FC4E84","#B87B8E"),
          lineStyle.width = 2,
          lineStyle.shadowColor = "#FF004C",
          lineStyle.shadowBlur = 10,
          lineStyle.shadowOffsetX = 2,
          lineStyle.shadowOffsetY = 2
        )
      },
      Step = if(grouped) {
        NULL
      } else {
        list(
          lineStyle.color = c("#FF004C","#FC4E84","#B87B8E"),
          lineStyle.width = 2,
          lineStyle.shadowColor = "#FF004C",
          lineStyle.shadowBlur = 10,
          lineStyle.shadowOffsetX = 2,
          lineStyle.shadowOffsetY = 2
        )
      },
      Parallel = list(
        lineStyle.color = c("#414557")
      ),
      Bar = if(grouped) {
        NULL
      } else {
        list(
          backgroundStyle.color = c("#FF004C","#FC4E84","#B87B8E"),
          backgroundStyle.opacity = c(1.0, 0.75, 0.25)
        )
      },
      Histogram = if(grouped) {
        NULL
      } else {
        list(
          backgroundStyle.color = c("#FF004C","#FC4E84","#B87B8E"),
          backgroundStyle.opacity = c(1.0, 0.75, 0.25)
        )
      },
      Density = if(grouped) {
        NULL
      } else {
        list(
          areaStyle.color = c("#FF004C","#FC4E84","#B87B8E"),
          areaStyle.opacity = c(1.0, 0.75, 0.25)
        )
      },
      Box = list(
        itemStyle.color = c("#FF004C","#FC4E84","#B87B8E"),
        itemStyle.opacity = c(1.0, 0.75, 0.25)
      ),
      NULL
    ),
    macarons = switch(
      plot_type,
      Area = if(grouped) {
        NULL
      } else {
        list(
          areaStyle.color = c("#00FFC3","#00BA8F","#008F6F"),
          areaStyle.opacity = c(1.0, 0.75, 0.25)
        )
      },
      Line = if(grouped) {
        NULL
      } else {
        list(
          lineStyle.color = c("#00FFC3","#00BA8F","#008F6F"),
          lineStyle.width = 2,
          lineStyle.shadowColor = "#00FFC3",
          lineStyle.shadowBlur = 10,
          lineStyle.shadowOffsetX = 2,
          lineStyle.shadowOffsetY = 2
        )
      },
      Step = if(grouped) {
        NULL
      } else {
        list(
          lineStyle.color = c("#00FFC3","#00BA8F","#008F6F"),
          lineStyle.width = 2,
          lineStyle.shadowColor = "#00FFC3",
          lineStyle.shadowBlur = 10,
          lineStyle.shadowOffsetX = 2,
          lineStyle.shadowOffsetY = 2
        )
      },
      Parallel = list(
        lineStyle.color = c("#00D5D9")
      ),
      Bar = if(grouped) {
        NULL
      } else {
        list(
          backgroundStyle.color = c("#00FFC3","#00BA8F","#008F6F"),
          backgroundStyle.opacity = c(1.0, 0.75, 0.25)
        )
      },
      Histogram = if(grouped) {
        NULL
      } else {
        list(
          backgroundStyle.color = c("#00FFC3","#00BA8F","#008F6F"),
          backgroundStyle.opacity = c(1.0, 0.75, 0.25)
        )
      },
      Density = if(grouped) {
        NULL
      } else {
        list(
          areaStyle.color = c("#00FFC3","#00BA8F","#008F6F"),
          areaStyle.opacity = c(1.0, 0.75, 0.25)
        )
      },
      Box = list(
        itemStyle.color = c("#00FFC3","#00BA8F","#008F6F"),
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
          lineStyle.color = c("#DEEAFC", "#91C1FF", "#1474FF"),
          lineStyle.width = 2,
          lineStyle.shadowColor = "#DEEAFC",
          lineStyle.shadowBlur = 10,
          lineStyle.shadowOffsetX = 2,
          lineStyle.shadowOffsetY = 2
        )
      },
      Step = if(grouped) {
        NULL
      } else {
        list(
          lineStyle.color = c("#DEEAFC", "#91C1FF", "#1474FF"),
          lineStyle.width = 2,
          lineStyle.shadowColor = "#DEEAFC",
          lineStyle.shadowBlur = 10,
          lineStyle.shadowOffsetX = 2,
          lineStyle.shadowOffsetY = 2
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
        lineStyle.color = c("#00BFFF", "#FF69B4", "#32CD32"),
        legend.top = 45
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
  nms <- names(defaults)
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
