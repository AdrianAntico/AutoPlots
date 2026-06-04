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
