# AggMethod "mean", "median", "sd", "skewnewss", "kurtosis", "CoeffVar"

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
# :: Helper Functions ::                                                      ----
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

#' @noRd
SummaryFunction <- function(AggMethod) {
  if(AggMethod == "count") {
    aggFunc <- function(x) .N
  } else if(AggMethod == "mean") {
    aggFunc <- function(x) mean(x, na.rm = TRUE)
  } else if(AggMethod == "log(mean(x))") {
    aggFunc <- function(x) log(mean(x, na.rm = TRUE))
  } else if(AggMethod == "mean(abs(x))") {
    aggFunc <- function(x) mean(abs(x), na.rm = TRUE)
  } else if(AggMethod == "sum") {
    aggFunc <- function(x) sum(x, na.rm = TRUE)
  } else if(AggMethod == "log(sum(x))") {
    aggFunc <- function(x) log(sum(x, na.rm = TRUE))
  } else if(AggMethod == "sum(abs(x))") {
    aggFunc <- function(x) sum(abs(x), na.rm = TRUE)
  } else if(AggMethod == "median") {
    aggFunc <- function(x) median(x, na.rm = TRUE)
  } else if(AggMethod == "log(median(x))") {
    aggFunc <- function(x) log(median(x, na.rm = TRUE))
  } else if(AggMethod == "median(abs(x))") {
    aggFunc <- function(x) median(abs(x), na.rm = TRUE)
  } else if(AggMethod == "sd") {
    aggFunc <- function(x) sd(x, na.rm = TRUE)
  } else if(AggMethod == "log(sd(x))") {
    aggFunc <- function(x) log(sd(x, na.rm = TRUE))
  } else if(AggMethod == "sd(abs(x))") {
    aggFunc <- function(x) sd(abs(x), na.rm = TRUE)
  } else if(AggMethod == "skewness") {
    aggFunc <- function(x) e1071::skewness(x, na.rm = TRUE)
  } else if(AggMethod == "skewness(abs(x))") {
    aggFunc <- function(x) e1071::skewness(abs(x), na.rm = TRUE)
  } else if(AggMethod == "kurtosis") {
    aggFunc <- function(x) e1071::kurtosis(x, na.rm = TRUE)
  } else if(AggMethod == "kurtosis(abs(x))") {
    aggFunc <- function(x) e1071::kurtosis(abs(x), na.rm = TRUE)
  } else if(AggMethod == "CoeffVar") {
    aggFunc <- function(x) sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE)
  } else if(AggMethod == "CoeffVar(abs(x))") {
    aggFunc <- function(x) sd(abs(x), na.rm = TRUE) / mean(abs(x), na.rm = TRUE)
  }
  return(aggFunc)
}

#' @noRd
ColTypes <- function(data) {
  CT <- c()
  for(Col in names(data)) CT <- c(CT, class(data[[Col]])[1L])
  CT
}

#' @noRd
bold_ <- function(x) paste0('<b>',x,'</b>')

#' @noRd
font_ <- function(family = "Segoe UI Symbol", size = 12, color = 'white') list(family = family, size = size, color = color)

#' @title ChartTheme
#'
#' @description This function helps your ggplots look professional with the choice of the two main colors that will dominate the theme
#'
#' @author Adrian Antico
#' @family helper
#'
#' @param Size The size of the axis labels and title
#' @param AngleX The angle of the x axis labels
#' @param AngleY The angle of the Y axis labels
#' @param ChartColor "lightsteelblue1",
#' @param BorderColor "darkblue"
#' @param SubTitleColor 'blue'
#' @param TextColor "darkblue"
#' @param GridColor "white"
#' @param BackGroundColor "gray95"
#' @param LegendPosition Where to place legend
#' @param LegendBorderSize 0.50
#' @param LegendLineType 'solid'
#' @examples
#' \dontrun{
#' data <- data.table::data.table(DateTime = as.Date(Sys.time()),
#'   Target = stats::filter(rnorm(1000,
#'                                mean = 50,
#'                                sd = 20),
#'                          filter=rep(1,10),
#'                          circular=TRUE))
#' data[, temp := seq(1:1000)][, DateTime := DateTime - temp][
#'   , temp := NULL]
#' data <- data[order(DateTime)]
#' p <- ggplot2::ggplot(data, ggplot2::aes(x = DateTime, y = Target)) +
#'   ggplot2::geom_line()
#' p <- p + ChartTheme(Size = 12)
#' }
#' @return An object to pass along to ggplot objects following the "+" sign
#' @export
ChartTheme <- function(Size = 12,
                       AngleX = 90,
                       AngleY = 0,
                       BackGroundColor = "#6a6969", #"#1b1959", #'#00060b',
                       ChartColor = '#001534',
                       GridColor = 'white',
                       TextColor = 'white',
                       SubTitleColor = 'white',
                       BorderColor = 'white',
                       LegendPosition = 'bottom',
                       LegendBorderSize = 0.01,
                       LegendLineType = 'solid') {
  chart_theme <- ggplot2::theme(
    plot.background = ggplot2::element_rect(fill = BackGroundColor),
    panel.background = ggplot2::element_rect(fill = ChartColor, colour = BorderColor, size = 0.25, color = BorderColor),
    panel.grid.major = ggplot2::element_line(colour = BorderColor, size = 0.01, color = GridColor, linetype = 1),
    panel.grid.minor = ggplot2::element_line(colour = BorderColor, size = 0.01, color = GridColor, linetype = 1),
    legend.position = LegendPosition,
    legend.title = ggplot2::element_text(color = BorderColor, size = Size, face = 'bold'),
    plot.subtitle = ggplot2::element_text(color = SubTitleColor, size = max(1,floor(Size * 5 / 6)), face = 'bold'),
    legend.background = ggplot2::element_rect(fill = BackGroundColor, size = LegendBorderSize, linetype = LegendLineType, color = BorderColor),
    plot.title = ggplot2::element_text(color = TextColor, size = Size, face = 'bold'),
    axis.title = ggplot2::element_text(color = TextColor, size = Size, face = 'bold'),
    axis.text.x = ggplot2::element_text(colour = TextColor, face = "bold", angle = AngleX),
    axis.text.y = ggplot2::element_text(colour = TextColor, face = "bold", angle = AngleY),
    axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 20, r = 20, b = 20, l = 20)),
    axis.title.y = ggplot2::element_text(margin = ggplot2::margin(t = 20, r = 20, b = 20, l = 20)),
    panel.border = ggplot2::element_rect(colour = BorderColor, fill = NA, size = 1.5))
  chart_theme
}

# ----

# ----

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
# > Automated Plot Functions                                                  ----
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

#' @title Plot.StandardPlots
#'
#' @description Helper for standard plots
#'
#' @author Adrian Antico
#' @family Auto Plotting
#'
#' @param PlotType character
#' @param dt character
#' @param PreAgg = FALSE
#' @param AggMethod character
#' @param SampleSize character
#' @param YVar character
#' @param XVar character
#' @param ZVar character
#' @param GroupVar character
#' @param NumberBins character
#' @param Height valid css unit
#' @param Width valid css unit
#' @param PlotEngineType character
#' @param EchartsTheme character
#' @param TimeLine character
#' @param BackGroundColor character
#' @param ChartColor character
#' @param FillColor character
#' @param FillColorReverse character
#' @param GridColor character
#' @param TextColor character
#' @param Debug character
#'
#' @export
Plot.StandardPlots <- function(dt = NULL,
                               PreAgg = FALSE,
                               PlotType = 'Scatter',
                               SampleSize = 100000L,
                               YVar = NULL,
                               XVar = NULL,
                               ZVar = NULL,
                               GroupVar = NULL,
                               Height = "600px",
                               Width = "1135px",
                               PlotEngineType = "Plotly",
                               EchartsTheme = "dark-blue",
                               TimeLine = FALSE,
                               NumLevels_Y = 75,
                               NumLevels_X = 40,
                               AggMethod = 'mean',
                               NumberBins = 30,
                               BackGroundColor =  "#6a6969",
                               ChartColor =       "#001534",
                               FillColor =        "#0066ff",
                               FillColorReverse = "#97ff00",
                               GridColor =        "white",
                               TextColor =        "white",
                               Debug = FALSE) {

  # Debug
  if(Debug) print(paste0('Plot.StandardPlots() begin, PlotType = ', PlotType))

  # Pie Plot
  if(tolower(PlotType) == 'pieplot') {
    p1 <- AutoPlots:::Plot.Pie(
      dt = dt,
      PreAgg = PreAgg,
      AggMethod = AggMethod,
      XVar = if(length(XVar) == 0 && length(GroupVar) > 0L) GroupVar[1L] else XVar,
      YVar = YVar,
      GroupVar = NULL,
      Width = Width,
      Height = Height,
      Title = 'Pie Chart',
      Engine = PlotEngineType,
      EchartsTheme = EchartsTheme,
      TimeLine = TimeLine,
      X_Scroll = TRUE,
      Y_Scroll = TRUE,
      BackGroundColor = BackGroundColor,
      ChartColor = ChartColor,
      FillColor = FillColor,
      TextColor = TextColor,
      GridColor = GridColor,
      ZeroLineColor = GridColor,
      Debug = Debug)
    return(p1)
  }

  # Box Plot
  if(tolower(PlotType) == 'boxplot') {
    p1 <- AutoPlots:::Plot.Box(
      dt = dt,
      SampleSize = SampleSize,
      XVar = XVar,
      YVar = YVar,
      GroupVar = GroupVar,
      Width = Width,
      Height = Height,
      Title = 'Box Plot',
      Engine = PlotEngineType,
      EchartsTheme = EchartsTheme,
      TimeLine = TimeLine,
      X_Scroll = TRUE,
      Y_Scroll = TRUE,
      BackGroundColor = BackGroundColor,
      ChartColor = ChartColor,
      FillColor = FillColor,
      FillColorReverse = FillColorReverse,
      TextColor = TextColor,
      GridColor = GridColor,
      ZeroLineColor = GridColor,
      ZeroLineWidth = 1.25,
      Debug = Debug)
    return(p1)
  }

  # Violin Plot (Plotly Only)
  if(tolower(PlotType) == 'violinplot') {
    p1 <- AutoPlots:::Plot.Violin(
      dt = dt,
      SampleSize = SampleSize,
      XVar = XVar,
      YVar = YVar,
      GroupVar = GroupVar,
      Width = Width,
      Height = Height,
      Title = 'Violin Plot',
      FillColor = FillColor,
      ChartColor = ChartColor,
      BackGroundColor = BackGroundColor,
      TextColor = TextColor,
      GridColor = GridColor,
      Debug = Debug)
    return(p1)
  }

  # Histogram Plot
  if(tolower(PlotType) == 'histogramplot') {
    if(Debug) print('histogram 1')
    p1 <- AutoPlots:::Plot.Histogram(
      dt = dt,
      SampleSize = SampleSize,
      XVar = XVar,
      YVar = YVar,
      GroupVar = GroupVar,
      NumberBins = NumberBins,
      Width = Width,
      Height = Height,
      Title = "Histogram",
      X_Scroll = TRUE,
      Y_Scroll = TRUE,
      Engine = PlotEngineType,
      EchartsTheme = EchartsTheme,
      TimeLine = TimeLine,
      FillColor = FillColor,
      BackGroundColor = BackGroundColor,
      ChartColor = ChartColor,
      TextColor = TextColor,
      GridColor = GridColor,
      ZeroLineWidth = 1.25,
      ZeroLineColor = GridColor,
      Debug = Debug)
    return(p1)
  }

  # Density Plot
  if(tolower(PlotType) == 'densityplot') {
    p1 <- AutoPlots:::Plot.Density(
      dt = dt,
      SampleSize = SampleSize,
      GroupVar=GroupVar,
      XVar = NULL,
      YVar = if(length(YVar) > 0L) YVar else XVar,
      Width = Width,
      Height = Height,
      X_Scroll = TRUE,
      Y_Scroll = TRUE,
      Engine = PlotEngineType,
      EchartsTheme = EchartsTheme,
      TimeLine = TimeLine,
      ChartColor = ChartColor,
      TextColor = TextColor,
      GridColor = GridColor,
      BackGroundColor = BackGroundColor,
      Debug = Debug)
    return(p1)
  }

  # Line Plot
  if(tolower(PlotType) == 'lineplot') {
    p1 <- AutoPlots::Plot.Line(
      dt = dt,
      PreAgg = PreAgg,
      AggMethod = AggMethod,
      XVar = XVar,
      YVar = YVar,
      GroupVar = GroupVar,
      Width = Width,
      Height = Height,
      Title = 'Line Plot',
      Engine = PlotEngineType,
      EchartsTheme = EchartsTheme,
      TimeLine = TimeLine,
      X_Scroll = TRUE,
      Y_Scroll = TRUE,
      BackGroundColor = BackGroundColor,
      ChartColor = ChartColor,
      FillColor = FillColor,
      GridColor = GridColor,
      TextColor = TextColor,
      ZeroLineColor = GridColor,
      ZeroLineWidth = 1.25,
      Debug = Debug)
    return(p1)
  }

  # Area Plot
  if(tolower(PlotType) == 'areaplot') {
    p1 <- AutoPlots::Plot.Area(
      dt = dt,
      PreAgg = PreAgg,
      AggMethod = AggMethod,
      XVar = XVar,
      YVar = YVar,
      GroupVar = GroupVar,
      Width = Width,
      Height = Height,
      Title = 'Area Plot',
      Engine = PlotEngineType,
      EchartsTheme = EchartsTheme,
      TimeLine = TimeLine,
      X_Scroll = TRUE,
      Y_Scroll = TRUE,
      BackGroundColor = BackGroundColor,
      ChartColor = ChartColor,
      FillColor = FillColor,
      GridColor = GridColor,
      TextColor = TextColor,
      ZeroLineColor = GridColor,
      ZeroLineWidth = 1.25,
      Debug = Debug)
    return(p1)
  }

  # Step Plot
  if(tolower(PlotType) == 'stepplot') {
    p1 <- AutoPlots::Plot.Step(
      dt = dt,
      PreAgg = PreAgg,
      AggMethod = AggMethod,
      XVar = XVar,
      YVar = YVar,
      GroupVar = GroupVar,
      Width = Width,
      Height = Height,
      Title = 'Step Plot',
      Engine = PlotEngineType,
      EchartsTheme = EchartsTheme,
      TimeLine = TimeLine,
      X_Scroll = TRUE,
      Y_Scroll = TRUE,
      BackGroundColor = BackGroundColor,
      ChartColor = ChartColor,
      FillColor = FillColor,
      GridColor = GridColor,
      TextColor = TextColor,
      ZeroLineColor = GridColor,
      ZeroLineWidth = 1.25,
      Debug = Debug)
    return(p1)
  }

  # Step Plot
  if(tolower(PlotType) == 'riverplot') {
    p1 <- AutoPlots::Plot.River(
      dt = dt,
      PreAgg = PreAgg,
      AggMethod = AggMethod,
      XVar = XVar,
      YVar = YVar,
      GroupVar = GroupVar,
      Width = Width,
      Height = Height,
      Title = 'River Plot',
      Engine = PlotEngineType,
      EchartsTheme = EchartsTheme,
      TimeLine = TimeLine,
      X_Scroll = TRUE,
      Y_Scroll = TRUE,
      ShowSymbol = FALSE,
      BackGroundColor = BackGroundColor,
      ChartColor = ChartColor,
      FillColor = FillColor,
      FillColorReverse = FillColorReverse,
      GridColor = GridColor,
      TextColor = TextColor,
      ZeroLineColor = GridColor,
      ZeroLineWidth = 1.25,
      Debug = Debug)
    return(p1)
  }

  # Bar Plot
  if(tolower(PlotType) == 'barplot') {
    p1 <- AutoPlots:::Plot.Bar(
      dt = dt,
      PreAgg = PreAgg,
      AggMethod = AggMethod,
      XVar = XVar,
      YVar = YVar,
      GroupVar = if(all(XVar == GroupVar)) NULL else GroupVar,
      Width = Width,
      Height = Height,
      Title = 'Bar Plot',
      Engine = PlotEngineType,
      EchartsTheme = EchartsTheme,
      TimeLine = TimeLine,
      X_Scroll = TRUE,
      Y_Scroll = TRUE,
      BackGroundColor = BackGroundColor,
      ChartColor = ChartColor,
      FillColor = FillColor,
      TextColor = TextColor,
      GridColor = GridColor,
      ZeroLineColor = GridColor,
      Debug = Debug)
    return(p1)
  }

  # Bar Plot
  if(tolower(PlotType) == 'stackedbarplot') {
    print("Plot.StandardPlots --> AutoPlots:::Plot.StackedBar")
    p1 <- AutoPlots:::Plot.StackedBar(
      dt = dt,
      PreAgg = PreAgg,
      AggMethod = AggMethod,
      XVar = XVar,
      YVar = YVar,
      GroupVar = if(all(XVar == GroupVar)) NULL else GroupVar,
      Width = Width,
      Height = Height,
      Title = 'Stacked Bar',
      Engine = PlotEngineType,
      EchartsTheme = EchartsTheme,
      TimeLine = TimeLine,
      X_Scroll = TRUE,
      Y_Scroll = TRUE,
      BackGroundColor = BackGroundColor,
      ChartColor = ChartColor,
      FillColor = FillColor,
      TextColor = TextColor,
      GridColor = GridColor,
      ZeroLineColor = GridColor,
      Debug = Debug)
    print("AutoPlots:::Plot.StackedBar")
    print(paste0("length(p1) == ", length(p1)))
    return(p1)
  }

  # 3D Bar Plot
  if(tolower(PlotType) %in% c('barplot3d')) {
    p1 <- AutoPlots::Plot.BarPlot3D(
      PreAgg = PreAgg,
      dt = dt,
      YVar = YVar,
      XVar = XVar,
      ZVar = ZVar,
      AggMethod = AggMethod,
      NumberBins = 21,
      NumLevels_X = NumLevels_Y,
      NumLevels_Y = NumLevels_X,
      Width = Width,
      Height = Height,
      Engine = PlotEngineType,
      EchartsTheme = EchartsTheme,
      X_Scroll = X_Scroll,
      Y_Scroll = Y_Scroll,
      Title = "Heatmap",
      BackGroundColor = BackGroundColor,
      ChartColor = ChartColor,
      FillColor = FillColor,
      GridColor = GridColor,
      TextColor = TextColor,
      Debug = Debug)
    return(p1)
  }

  # Heat Map (Plotly & Echarts)
  if(tolower(PlotType) %in% c('heatmapplot')) {
    p1 <- AutoPlots::Plot.HeatMap(
      PreAgg = PreAgg,
      dt = dt,
      YVar = YVar,
      XVar = XVar,
      ZVar = ZVar,
      AggMethod = AggMethod,
      NumberBins = 21,
      NumLevels_X = NumLevels_Y,
      NumLevels_Y = NumLevels_X,
      Width = Width,
      Height = Height,
      Engine = PlotEngineType,
      EchartsTheme = EchartsTheme,
      Title = "Heatmap",
      BackGroundColor = BackGroundColor,
      ChartColor = ChartColor,
      FillColor = FillColor,
      GridColor = GridColor)
    print(paste0("Plot.HeatMap has length = ", length(p1)))
    print("Plot.StandardPlots return")
    return(p1)
  }

  # Correlation Matrix Plot
  if(tolower(PlotType) == 'correlogramplot') {
    p1 <- AutoPlots:::Plot.CorrMatrix(
      dt = dt,
      PreAgg = PreAgg,
      CorrVars = YVar,
      Width = Width,
      Height = Height,
      Engine = PlotEngineType,
      EchartsTheme = EchartsTheme,
      X_Scroll = TRUE,
      Y_Scroll = TRUE,
      Method = tolower("spearman"),
      BackGroundColor = BackGroundColor,
      ChartColor = ChartColor,
      FillColor = FillColor,
      GridColor = GridColor,
      TextColor = TextColor)
    return(p1)
  }

  # Scatter Plot
  if(tolower(PlotType) %in% c('scatterplot')) {
    if(SampleSize > 50000) SampleSize <- 50000
    p1 <- AutoPlots:::Plot.Scatter(
      dt = dt,
      SampleSize = SampleSize,
      XVar = XVar,
      YVar = YVar,
      GroupVar = if(all(XVar == GroupVar)) NULL else GroupVar,
      Width = Width,
      Height = Height,
      Title = 'Scatter Plot',
      Engine = PlotEngineType,
      EchartsTheme = EchartsTheme,
      TimeLine = TimeLine,
      X_Scroll = TRUE,
      Y_Scroll = TRUE,
      BackGroundColor = BackGroundColor,
      ChartColor = ChartColor,
      FillColor = FillColor,
      GridColor = GridColor,
      TextColor = TextColor,
      Debug = Debug)
    return(p1)
  }

  # Copula Plot
  if(tolower(PlotType) %in% c('copulaplot')) {
    if(SampleSize > 50000) SampleSize <- 50000
    p1 <- AutoPlots:::Plot.Copula(
      dt = dt,
      SampleSize = SampleSize,
      XVar = XVar,
      YVar = YVar,
      GroupVar = if(all(XVar == GroupVar)) NULL else GroupVar,
      Width = Width,
      Height = Height,
      Engine = PlotEngineType,
      EchartsTheme = EchartsTheme,
      TimeLine = TimeLine,
      X_Scroll = TRUE,
      Y_Scroll = TRUE,
      Title = 'Copula Plot',
      FillColor = FillColor,
      ChartColor = ChartColor,
      BackGroundColor = BackGroundColor,
      TextColor = TextColor,
      GridColor = GridColor,
      Debug = Debug)
    return(p1)
  }

  # Scatter3D Plot
  if(tolower(PlotType) %in% c('scatterplot3d')) {
    p1 <- AutoPlots:::Plot.Scatter3D(
      dt = dt,
      SampleSize = SampleSize,
      XVar = XVar,
      YVar = YVar,
      ZVar = ZVar,
      GroupVar = if(all(XVar == GroupVar)) NULL else GroupVar,
      Width = Width,
      Height = Height,
      Title = '3D Scatter Plot',
      Engine = PlotEngineType,
      EchartsTheme = EchartsTheme,
      TimeLine = TimeLine,
      FillColor = FillColor,
      ChartColor = ChartColor,
      BackGroundColor = BackGroundColor,
      TextColor = TextColor,
      GridColor = GridColor,
      Debug = Debug)
    return(p1)
  }

  # Copula Plot
  if(tolower(PlotType) %in% c('copulaplot3d')) {
    p1 <- AutoPlots:::Plot.Copula3D(
      dt = dt,
      SampleSize = SampleSize,
      XVar = XVar,
      YVar = YVar,
      ZVar = ZVar,
      GroupVar = if(all(XVar == GroupVar)) NULL else GroupVar,
      Width = Width,
      Height = Height,
      Title = '3D Copula Plot',
      Engine = PlotEngineType,
      EchartsTheme = EchartsTheme,
      TimeLine = TimeLine,
      FillColor = FillColor,
      ChartColor = ChartColor,
      BackGroundColor = BackGroundColor,
      TextColor = TextColor,
      GridColor = GridColor,
      Debug = Debug)
    return(p1)
  }
}

#' @title Plots.ModelEvaluation
#'
#' @description Plot helper for model evaluation plot types
#'
#' @author Adrian Antico
#' @family Auto Plotting
#'
#' @param dt data.table
#' @param AggMethod character
#' @param SampleSize 100000L
#' @param PlotEngineType character
#' @param EchartsTheme character
#' @param TimeLine logical
#' @param PlotType character
#' @param YVar character
#' @param TargetLevel character
#' @param ZVar character
#' @param XVar character
#' @param GroupVar character
#' @param NumLevels_Y = 75
#' @param NumLevels_X = 40
#' @param BackGroundColor hex
#' @param ChartColor hex
#' @param FillColor hex
#' @param FillColorReverse hex
#' @param GridColor hex
#' @param TextColor hex
#' @param NumberBins numeric
#' @param ShapAgg character. A string for aggregating shapely values for importances. Choices include, 'mean', 'absmean', 'meanabs', 'geomean', 'harmmean', 'sd', 'median', 'absmedian', 'medianabs'
#' @param Debug logical
#' @examples
#' \dontrun{
#' dt = NULL,
#' SampleSize = 100000L,
#' PlotType = NULL,
#' YVar = NULL,
#' TargetLevel = NULL,
#' ZVar = NULL,
#' XVar = NULL,
#' Title = NULL,
#' PlotEngineType = "Plotly",
#' EchartsTheme = "dark-blue",
#' TimeLine = FALSE,
#' BackGroundColor = "#6a6969",
#' ChartColor = '#001534',
#' GridColor = 'white',
#' FillColor = "#0066ff",
#' TextColor = 'white',
#' NumberBins = 20,
#' Debug = FALSE
#' }
#'
#' @export
Plots.ModelEvaluation <- function(dt = NULL,
                                  AggMethod = "mean",
                                  SampleSize = 100000L,
                                  PlotType = NULL,
                                  YVar = NULL,
                                  TargetLevel = NULL,
                                  ZVar = NULL,
                                  XVar = NULL,
                                  GroupVar = NULL,
                                  NumLevels_Y = 75,
                                  NumLevels_X = 40,
                                  Title = NULL,
                                  PlotEngineType = "Echarts",
                                  EchartsTheme = "dark-blue",
                                  TimeLine = FALSE,
                                  BackGroundColor =  "#6a6969",
                                  ChartColor =       "#001534",
                                  FillColor =        "#0066ff",
                                  FillColorReverse = "#97ff00",
                                  GridColor =        "white",
                                  TextColor =        "white",
                                  NumberBins = 20,
                                  ShapAgg = 'mean',
                                  Debug = FALSE) {

  # Debugging
  if(Debug) {print('Running Plots.ModelEvaluation')}
  if(length(SampleSize) == 0L) SampleSize <- 50000L

  # ----

  # Residuals_1 Histogram Plot ----
  if(any(PlotType %chin% "Residuals_1")) {
    if(Debug) print('Residuals_1')
    p1 <- AutoPlots::Plot.Residuals.Histogram(
      dt = dt,
      SampleSize = 50000L,
      XVar = ZVar,
      YVar = YVar,
      GroupVar = GroupVar,
      Title = 'ROC Plot',
      Engine = PlotEngineType,
      EchartsTheme = EchartsTheme,
      TimeLine = TimeLine,
      BackGroundColor = BackGroundColor,
      ChartColor = ChartColor,
      FillColor = FillColor,
      GridColor = GridColor,
      TextColor = TextColor,
      ZeroLineColor = GridColor,
      ZeroLineWidth = 1.25,
      Debug = Debug)
    return(eval(p1))
  }

  # ----

  # Residuals_2 Scatter Plot ----
  if(any(PlotType %chin% "Residuals_2")) {
    if(Debug) print('Residuals_2')
    p1 <- AutoPlots::Plot.Residuals.Scatter(
      dt = dt,
      SampleSize = min(SampleSize, 30000L),
      XVar = ZVar,
      YVar = YVar,
      GroupVar = GroupVar,
      Title = 'ROC Plot',
      Engine = PlotEngineType,
      EchartsTheme = EchartsTheme,
      TimeLine = TimeLine,
      BackGroundColor = BackGroundColor,
      ChartColor = ChartColor,
      FillColor = FillColor,
      GridColor = GridColor,
      TextColor = TextColor,
      ZeroLineColor = GridColor,
      ZeroLineWidth = 1.25,
      Debug = Debug)
    return(eval(p1))
  }

  # ----

  # Evaluation Plot ----
  if(any(PlotType %chin% "CalibrationPlot")) {
    if(Debug) print("AutoModelInsights: AutoPlots::Plot.Calibration.Line")
    p1 <- AutoPlots::Plot.Calibration.Line(
      dt = dt,
      Engine = PlotEngineType,
      EchartsTheme = EchartsTheme,
      TimeLine = TimeLine,
      XVar = ZVar,
      YVar = YVar,
      GroupVar = GroupVar,
      AggMethod = 'mean',
      NumberBins = 21,
      Title = 'Calibration Line Plot',
      BackGroundColor = BackGroundColor,
      ChartColor = ChartColor,
      FillColor = FillColor,
      GridColor = GridColor,
      TextColor = TextColor,
      ZeroLineColor = GridColor,
      ZeroLineWidth = 1.25,
      Debug = Debug)
    return(eval(p1))
  }

  # ----

  # Evaluation BoxPlot ----
  if(any(PlotType %chin% "CalibrationBoxPlot")) {
    if(Debug) print('CalibrationBoxPlot')
    p1 <- AutoPlots::Plot.Calibration.Box(
      dt = dt,
      Engine = PlotEngineType,
      EchartsTheme = EchartsTheme,
      TimeLine = TimeLine,
      XVar = ZVar,
      YVar = YVar,
      GroupVar = GroupVar,
      AggMethod = 'mean',
      NumberBins = 21,
      Title = 'Calibration Box Plot',
      SampleSize = SampleSize,
      BackGroundColor = BackGroundColor,
      ChartColor = ChartColor,
      GridColor = GridColor,
      TextColor = TextColor,
      FillColor = FillColor,
      ZeroLineColor = GridColor,
      ZeroLineWidth = 1.25,
      Debug = FALSE)
    return(eval(p1))
  }

  # ----

  # ROC Plot ----
  if(any(PlotType %chin% "ROCPlot")) {
    if(Debug) print('ROCPlot')
    p1 <- AutoPlots::Plot.ROC(
      dt = dt,
      SampleSize = SampleSize,
      XVar = ZVar,
      YVar = YVar,
      GroupVar = GroupVar,
      Title = 'ROC Plot',
      Engine = PlotEngineType,
      EchartsTheme = EchartsTheme,
      TimeLine = TimeLine,
      BackGroundColor = BackGroundColor,
      ChartColor = ChartColor,
      GridColor = GridColor,
      TextColor = TextColor,
      FillColor = FillColor,
      ZeroLineColor = GridColor,
      ZeroLineWidth = 1.25,
      Debug = Debug)
    return(eval(p1))
  }

  # ----

  # Gains Plot ----
  if(any(PlotType %chin% "GainsPlot")) {
    if(Debug) print('GainsPlot')
    p1 <- AutoPlots::Plot.Gains(
      dt = dt,
      PreAgg = FALSE,
      XVar = ZVar,
      YVar = YVar,
      ZVar = NULL,
      GroupVar = NULL,
      NumberBins = 20,
      NumLevels_X = 50,
      NumLevels_Y = 50,
      Title = "Gains Plot",
      Engine = PlotEngineType,
      EchartsTheme = EchartsTheme,
      TimeLine = TimeLine,
      BackGroundColor = BackGroundColor,
      ChartColor = ChartColor,
      FillColor = FillColor,
      GridColor = GridColor,
      TextColor = TextColor,
      ZeroLineColor = GridColor,
      ZeroLineWidth = 1.25,
      Debug = FALSE)
    return(p1)
  }

  # ----

  # Lift Plot ----
  if(any(PlotType %chin% "LiftPlot")) {
    if(Debug) print('LiftPlot')
    p1 <- AutoPlots::Plot.Lift(
      dt = dt,
      PreAgg = FALSE,
      XVar = ZVar,
      YVar = YVar,
      ZVar = NULL,
      GroupVar = NULL,
      NumberBins = 20,
      NumLevels_X = 50,
      NumLevels_Y = 50,
      Title = "Lift Plot",
      Engine = PlotEngineType,
      EchartsTheme = EchartsTheme,
      TimeLine = TimeLine,
      BackGroundColor = BackGroundColor,
      ChartColor = ChartColor,
      FillColor = FillColor,
      GridColor = GridColor,
      TextColor = TextColor,
      ZeroLineColor = GridColor,
      ZeroLineWidth = 1.25,
      Debug = FALSE)
    return(p1)
  }

  # ----

  # Variable Importance Plot ----
  if(any(PlotType %chin% "VariableImportance")) {
    p1 <- AutoPlots::Plot.VariableImportance(
      Algo = "CatBoost",
      dt = dt,
      AggMethod = 'mean',
      XVar = "Variable",
      YVar = "Importance",
      GroupVar = NULL,
      Title = 'Variable Importance Plot',
      Engine = PlotEngineType,
      EchartsTheme = EchartsTheme,
      TimeLine = TimeLine,
      BackGroundColor = BackGroundColor,
      ChartColor = ChartColor,
      FillColor = FillColor,
      GridColor = GridColor,
      TextColor = TextColor,
      ZeroLineColor = GridColor,
      ZeroLineWidth = 1.25,
      Debug = FALSE)
    if(Engine == "Echarts") {
      p1 <- echarts4r::e_flip_coords(e = p1)
    }
    return(p1)
  }

  # ----

  # Shap VI ----
  if(PlotType == 'ShapelyImportance') {
    print("Plot.ModelEvaluation --> ShapImportance")
    print(AggMethod)
    print("here fucker 2")
    p1 <- AutoPlots::Plot.ShapImportance(
      PreAgg = FALSE,
      dt = dt,
      YVar = NULL,
      GroupVar = GroupVar,
      AggMethod = AggMethod,
      NumberBins = 21,
      NumLevels_X = NumLevels_Y,
      NumLevels_Y = NumLevels_X,
      Engine = PlotEngineType,
      EchartsTheme = EchartsTheme,
      Title = "Shap Importance",
      BackGroundColor = BackGroundColor,
      ChartColor = ChartColor,
      FillColor = FillColor,
      GridColor = GridColor)
    return(p1)
  }

  # ----

  # Confusion Matrix Heatmap ----
  if(any(PlotType %chin% "ConfusionMatrixHeatmap")) {

    # Prepare Data
    if('Predict' %in% names(dt)) {
      pred <- 'Predict'
    } else if('p1' %in% names(dt)) {
      pred <- 'p1'
    } else {
      return(NULL)
    }

    # Build
    p1 <- AutoPlots::Plot.ConfusionMatrix(
      dt = dt,
      Engine = PlotEngineType,
      EchartsTheme = EchartsTheme,
      TimeLine = TimeLine,
      XVar = "Predict",
      YVar = YVar,
      ZVar = NULL,
      PreAgg = FALSE,
      NumberBins = 21,
      NumLevels_X = 50,
      NumLevels_Y = 50,
      Title = "Confusion Matrix",
      BackGroundColor = BackGroundColor,
      ChartColor = ChartColor,
      FillColor = FillColor,
      GridColor = GridColor,
      TextColor = TextColor,
      ZeroLineColor = GridColor,
      ZeroLineWidth = 1.25)
    return(p1)
  }

  # ----

  # Partial Dependence Plot ----
  if(any(PlotType %chin% 'PartialDependenceLine') && !is.null(XVar)) {

    # MultiClass Mgt
    nam <- names(dt)
    if(Debug) print(TargetLevel)
    if(ZVar %in% nam) {
      if(class(dt[[ZVar]])[1L] %in% c('character','factor')) {
        dt[, paste0('Temp_', TargetLevel) := data.table::fifelse(get(YVar) == eval(TargetLevel), 1.0, 0.0)]
        YVar <- paste0('Temp_', TargetLevel)
        ZVar <- TargetLevel
      }
    }

    # Build
    p1 <- AutoPlots::Plot.PartialDependence.Line(
      dt = dt,
      XVar = XVar,
      YVar = YVar,
      ZVar = ZVar,
      GroupVar = GroupVar,
      AggMethod = 'mean',
      NumberBins = 21,
      Title = 'Partial Dependence Line Plot',
      Engine = PlotEngineType,
      EchartsTheme = EchartsTheme,
      TimeLine = TimeLine,
      X_Scroll = TRUE,
      Y_Scroll = TRUE,
      BackGroundColor = BackGroundColor,
      ChartColor = ChartColor,
      FillColor = FillColor,
      GridColor = GridColor,
      TextColor = TextColor,
      ZeroLineColor = GridColor,
      ZeroLineWidth = 1.25,
      Debug = Debug)
    return(p1)
  }

  # ----

  # Partial Dependence Box Plot ----
  if(any(PlotType %chin% 'PartialDependenceBox') && !is.null(XVar)) {

    # MultiClass Mgt
    nam <- names(dt)
    if(Debug) print(TargetLevel)
    if(ZVar %in% nam) {
      if(class(dt[[ZVar]])[1L] %in% c('character','factor')) {
        dt[, paste0('Temp_', TargetLevel) := data.table::fifelse(get(YVar) == eval(TargetLevel), 1.0, 0.0)]
        YVar <- paste0('Temp_', TargetLevel)
        ZVar <- TargetLevel
      }
    }

    # Build
    p1 <- AutoPlots::Plot.PartialDependence.Box(
      dt = dt,
      PreAgg = FALSE,
      AggMethod = 'mean',
      SampleSize = SampleSize,
      XVar = XVar,
      YVar = YVar,
      ZVar = ZVar,
      GroupVar = GroupVar,
      NumberBins = 21,
      Title = 'Partial Dependence Line Plot',
      Engine = PlotEngineType,
      EchartsTheme = EchartsTheme,
      TimeLine = TimeLine,
      X_Scroll = TRUE,
      Y_Scroll = TRUE,
      BackGroundColor = BackGroundColor,
      ChartColor = ChartColor,
      FillColor = FillColor,
      FillColorReverse = FillColorReverse,
      GridColor = GridColor,
      TextColor = TextColor,
      ZeroLineColor = GridColor,
      ZeroLineWidth = 1.25,
      Debug = Debug)
    return(eval(p1))
  }

  # ----

  if(!exists('p1')) p1 <- NULL
  return(p1)
}

# ----

# ----

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
# > Distribution Plot Functions                                               ----
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

#' @title Plot.Pie
#'
#' @description Build a pie chart by simply passing arguments to a single function
#'
#' @family Standard Plots
#'
#' @author Adrian Antico
#'
#' @param dt Source data.table
#' @param PreAgg logical
#' @param XVar Column name of X-Axis variable. If NULL then ignored
#' @param YVar Column name of Y-Axis variable. If NULL then ignored
#' @param GroupVar Column name of Group Variable for distinct colored histograms by group levels
#' @param AggMethod Choose from 'mean', 'sum', 'sd', and 'median'
#' @param Height = NULL,
#' @param Width = NULL,
#' @param Title title
#' @param Engine 'Plotly' or "Echarts"
#' @param EchartsTheme "macaron"
#' @param TimeLine logical
#' @param X_Scroll logical
#' @param Y_Scroll logical
#' @param ZeroLineColor = '#ffff'
#' @param FillColor 'gray'
#' @param FillColorReverse character
#' @param ChartColor 'lightsteelblue'
#' @param TextColor 'darkblue'
#' @param GridColor 'white'
#' @param BackGroundColor 'gray95'
#' @param Debug FALSE
#'
#' @examples
#' \dontrun{
#' # Load packages
#' library(AutoPlots)
#' library(data.table)
#'
#' # Load data
#' data <- data.table::fread("./CatBoost_ML1_ScoringData.csv")
#'
#' # Run function
#' AutoPlots:::Plot.Pie(
#'   Engine = 'Plotly', # "Echarts"
#'   EchartsTheme = "macaron",
#'   TimeLine = TRUE,
#'   data = data,
#'   XVar = "Predict",
#'   YVar = "CHILLED_Margin_PerDay",
#'   GroupVar = "BRAND",
#'   AggMethod = 'mean',
#'   FillColor = "#0066ff",
#'   ChartColor = 'lightsteelblue1',
#'   TextColor = 'darkblue',
#'   GridColor = 'white',
#'   BackGroundColor = 'gray95',
#'   Debug = FALSE)
#'
#' # # Step through function
#' # dt <- data
#' # Engine = 'Plotly' # "Echarts"
#' # EchartsTheme = "macaron"
#' # XVar = "Predict"
#' # YVar = "CHILLED_Margin_PerDay"
#' # GroupVar = "BRAND"
#' # AggMethod = 'mean'
#' # GroupVar = NULL
#' # ZeroLineColor = '#ffff'
#' # ZeroLineWidth = 1.25
#' # Title = 'Bar Plot'
#' # FillColor = "#0066ff"
#' # BackGroundColor = "#6a6969" #, #"#1b1959", #'#00060b',
#' # ChartColor = '#001534'
#' # TextColor = 'darkblue'
#' # GridColor = 'white'
#' # Debug = FALSE
#' }
#'
#' @export
Plot.Pie <- function(Engine = 'Plotly',
                     dt = NULL,
                     PreAgg = FALSE,
                     XVar = NULL,
                     YVar = NULL,
                     GroupVar = NULL,
                     AggMethod = 'mean',
                     Height = "600px",
                     Width = "1135px",
                     Title = 'Bar Plot',
                     EchartsTheme = "macaron",
                     TimeLine = TRUE,
                     X_Scroll = TRUE,
                     Y_Scroll = TRUE,
                     BackGroundColor =  "#6a6969",
                     ChartColor =       "#001534",
                     FillColor =        "#0066ff",
                     FillColorReverse = "#97ff00",
                     GridColor =        "white",
                     TextColor =        "white",
                     ZeroLineColor = '#ffff',
                     ZeroLineWidth = 1.25,
                     title.fontSize = 22,
                     title.fontWeight = "bold", # normal
                     title.textShadowColor = '#63aeff',
                     title.textShadowBlur = 3,
                     title.textShadowOffsetY = 1,
                     title.textShadowOffsetX = -1,
                     xaxis.fontSize = 14,
                     yaxis.fontSize = 14,
                     Debug = FALSE) {

  if(length(YVar) > 0L) YVar <- YVar[1L]
  if(length(XVar) > 0L) XVar <- XVar[1L]

  X.HoverFormat <- "%{xaxis.title.text}: %{x:,.2f}<br>"
  Y.HoverFormat <- "%{yaxis.title.text}: %{y:,.2f}<br>"

  if(data.table::is.data.table(dt)) data.table::setDT(dt)

  # Used multiple times
  check1 <- length(XVar) != 0 && length(YVar) != 0

  if(!PreAgg) {
    aggFunc <- AutoPlots:::SummaryFunction(AggMethod)
  }

  # Create base plot object
  numvars <- c()
  byvars <- c()
  if(check1) {
    if(Debug) print("BarPlot 2.b")
    if(!PreAgg) {
      if(tryCatch({class(dt[[eval(YVar)]])[1L]}, error = function(x) "bla") %in% c('numeric','integer')) {
        numvars <- unique(c(numvars, YVar))
      } else {
        byvars <- unique(c(byvars, YVar))
      }
      if(tryCatch({class(dt[[eval(XVar)]])[1L]}, error = function(x) "bla") %in% c('numeric','integer')) {
        if(length(numvars) > 0) {
          x <- length(unique(dt[[XVar]]))
          y <- length(unique(dt[[YVar]]))
          if(x > y) {
            byvars <- unique(c(byvars, YVar))
            numvars[1L] <- XVar
          } else {
            byvars <- unique(c(byvars, XVar))
          }
        } else {
          numvars <- unique(c(numvars, XVar))
        }
      } else {
        byvars <- unique(c(byvars, XVar))
      }
      if(!is.null(byvars)) {
        temp <- dt[, lapply(.SD, noquote(aggFunc)), .SDcols = c(numvars), by = c(byvars)]
        for(i in byvars) {
          if(class(temp[[i]])[1L] %in% c('numeric','integer')) {
            temp[, eval(i) := as.character(get(i))]
          }
        }
      } else {
        temp <- dt[, lapply(.SD, noquote(aggFunc)), .SDcols = c(numvars)]
      }
    } else {
      temp <- data.table::copy(dt)
    }

    yvar <- temp[[YVar]]
    xvar <- temp[[XVar]]

    # Plotly
    if(Engine == "Plotly") {

      Width <- as.integer(gsub("[^\\d]+", "", Width, perl=TRUE))
      Heigth <- as.integer(gsub("[^\\d]+", "", Height, perl=TRUE))

      p1 <- plotly::plot_ly(
        data = temp,
        type = 'pie',
        labels = xvar,
        values = yvar,
        textinfo='label+percent',
        insidetextorientation='radial',
        color = I(FillColor[1]),
        showlegend = TRUE,
        width = Width,
        height = Height)
      p1 <- plotly::layout(
        p = p1,
        font = AutoPlots:::font_(),
        title = AutoPlots:::bold_(Title),
        plot_bgcolor = ChartColor,
        paper_bgcolor = BackGroundColor,
        legend = list(title=list(text = paste0('<b> ', XVar[1L], ' </b>'))),
        yaxis = list(
          title = AutoPlots:::bold_(YVar),
          zerolinecolor = ZeroLineColor,
          zerolinewidth = ZeroLineWidth,
          gridcolor = GridColor),
        xaxis = list(
          title = AutoPlots:::bold_(XVar),
          categoryorder = "total descending",
          zerolinecolor = ZeroLineColor,
          zerolinewidth = ZeroLineWidth,
          gridcolor = GridColor),
        barmode = 'group')

    } else {
      p1 <- echarts4r::e_charts_(temp, x = XVar, dispose = TRUE, width = Width, height = Height)
      p1 <- echarts4r::e_pie_(e = p1, YVar, stack = XVar)
      if(X_Scroll) p1 <- echarts4r::e_datazoom(e = p1, x_index = c(0,1))
      if(Y_Scroll) p1 <- echarts4r::e_datazoom(e = p1, y_Index = c(0,1))
      p1 <- echarts4r::e_theme(e = p1, name = EchartsTheme)
      p1 <- echarts4r::e_aria(e = p1, enabled = TRUE)
      p1 <- echarts4r::e_tooltip(e = p1)
      p1 <- echarts4r::e_toolbox_feature(e = p1, feature = c("saveAsImage","dataZoom"))
      p1 <- echarts4r::e_show_loading(e = p1, hide_overlay = TRUE, text = "Calculating...", color = "#000", text_color = TextColor, mask_color = "#000")
      p1 <- echarts4r::e_legend(e = p1, type = "scroll", orient = "vertical", right = 50, top = 40, height = "240px", textStyle = list(color = TextColor, fontWeight = "bold"))
      p1 <- echarts4r::e_brush(e = p1)
      p1 <- echarts4r::e_title(
        p1, Title,
        textStyle = list(
          color = TextColor,
          fontWeight = title.fontWeight,
          overflow = "truncate", # "none", "truncate", "break",
          ellipsis = '...',
          fontSize = title.fontSize,
          textShadowColor = title.textShadowColor,
          textShadowBlur = title.textShadowBlur,
          textShadowOffsetY = title.textShadowOffsetY,
          textShadowOffsetX = title.textShadowOffsetX))

    }
    return(p1)
  }
}

#' @title Plot.Box
#'
#' @description Build a box plot by simply passing arguments to a single function. It will sample your data using SampleSize number of rows. Sampled data is randomized.
#'
#' @family Standard Plots
#'
#' @author Adrian Antico
#'
#' @param dt Source data.table
#' @param SampleSize numeric
#' @param XVar character
#' @param YVar character
#' @param GroupVar character
#' @param Height = NULL,
#' @param Width = NULL,
#' @param Title character
#' @param Engine character
#' @param EchartsTheme character
#' @param TimeLine Logical
#' @param X_Scroll logical
#' @param Y_Scroll logical
#' @param BackGroundColor character hex
#' @param ChartColor character hex
#' @param FillColor character hex
#' @param FillColorReverse character hex
#' @param GridColor character hex
#' @param TextColor character hex
#' @param ZeroLineColor character hex
#' @param ZeroLineWidth numeric
#' @param Debug logical
#'
#' @examples
#' \dontrun{
#' # Load packages
#' library(AutoPlots)
#' library(data.table)
#'
#' # Load data
#' data <- data.table::fread("./CatBoost_ML1_ScoringData.csv")
#'
#' # Run function
#' AutoPlots:::Plot.Box(
#'   Engine = "Plotly",
#'   EchartsTheme = "macaron",
#'   TimeLine = TimeLine,
#'   dt = data,
#'   XVar = 'Predict',
#'   YVar = 'CHILLED_Margin_PerDay',
#'   GroupVar = 'BRAND',
#'   Title = 'Box Plot',
#'   ZeroLineColor = '#ffff',
#'   ZeroLineWidth = 1.25,
#'   SampleSize = 100000,
#'   FillColor = "#0066ff",
#'   FillColorReverse = "#ff9900",
#'   BackGroundColor = "#6a6969",
#'   ChartColor = '#001534',
#'   GridColor = '#ffff',
#'   TextColor = 'white',
#'   Debug = FALSE)
#'
#' # # Step through function
#' # Engine = "Plotly"
#' # EchartsTheme = "macaron"
#' # TimeLine = TimeLine
#' # data = data
#' # XVar = 'Predict'
#' # YVar = 'CHILLED_Margin_PerDay'
#' # GroupVar = 'BRAND'
#' # Title = 'Box Plot'
#' # ZeroLineColor = '#ffff'
#' # ZeroLineWidth = 1.25
#' # SampleSize = 100000
#' # FillColor = "#0066ff"
#' # FillColorReverse = "#ff9900"
#' # BackGroundColor = "#6a6969"
#' # ChartColor = '#001534'
#' # GridColor = '#ffff'
#' # TextColor = 'white'
#' # Debug = FALSE
#' }
#' @export
Plot.Box <- function(dt = NULL,
                     SampleSize = 100000L,
                     XVar = NULL,
                     YVar = NULL,
                     GroupVar = NULL,
                     Height = "600px",
                     Width = "1135px",
                     Title = 'Box Plot',
                     Engine = "Plotly",
                     EchartsTheme = "macaron",
                     TimeLine = TimeLine,
                     X_Scroll = TRUE,
                     Y_Scroll = TRUE,
                     BackGroundColor =  "#6a6969",
                     ChartColor =       "#001534",
                     FillColor =        "#0066ff",
                     FillColorReverse = "#97ff00",
                     GridColor =        "white",
                     TextColor =        "white",
                     ZeroLineColor = '#ffff',
                     ZeroLineWidth = 1.25,
                     title.fontSize = 22,
                     title.fontWeight = "bold", # normal
                     title.textShadowColor = '#63aeff',
                     title.textShadowBlur = 3,
                     title.textShadowOffsetY = 1,
                     title.textShadowOffsetX = -1,
                     xaxis.fontSize = 14,
                     yaxis.fontSize = 14,
                     Debug = FALSE) {

  # Ensure data.table
  if(!data.table::is.data.table(dt)) data.table::setDT(dt)
  if(Debug) print("Plot.BoxPlot 1")

  X.HoverFormat <- "%{xaxis.title.text}: %{x:,.2f}<br>"
  Y.HoverFormat <- "%{yaxis.title.text}: %{y:,.2f}<br>"

  # Cap number of records
  if(dt[,.N] > SampleSize) {
    dt1 <- dt[order(runif(.N))][seq_len(SampleSize)]
  } else {
    dt1 <- data.table::copy(dt)
  }

  if(length(YVar) > 0L && length(XVar) == 0L && length(GroupVar) > 0L) {
    XVar <- GroupVar; GroupVar <- NULL
    CoordFlip <- FALSE
  } else if(length(XVar) > 0L && length(YVar) == 0L && length(GroupVar) > 0L) {
    YVar <- XVar; XVar <- GroupVar; GroupVar <- NULL
    CoordFlip <- TRUE
  } else {
    CoordFlip <- FALSE
  }

  # Build Plot Based on Available Variables
  # Create logic checks to determine each case distinctly
  if(Debug) print("Plot.BoxPlot 2")
  X_and_Y_and_GroupVars <- length(XVar) > 0L && length(YVar) > 0L && length(GroupVar) > 0L
  X_and_Y <- length(XVar) > 0L && length(YVar) > 0L

  # X,Y,GroupVar
  if(X_and_Y_and_GroupVars) {

    # Build plot
    if(Engine == "Plotly") {

      Width <- as.integer(gsub("[^\\d]+", "", Width, perl=TRUE))
      Heigth <- as.integer(gsub("[^\\d]+", "", Height, perl=TRUE))

      if(Debug) print('Plot.Box X_and_Y_and_GroupVars')
      if(Debug) print('Plot.Box plotly::plot_ly')
      p1 <- plotly::plot_ly(
        data = dt1,
        x = ~get(XVar),
        y = ~get(YVar),
        color = ~get(GroupVar[1L]),
        type = "box",
        text = ~get(GroupVar[1L]),
        hovertemplate = paste(
          Y.HoverFormat,
          X.HoverFormat,
          "<extra></extra>"
        ),
        width = Width,
        height = Height)

      # Layout
      if(Debug) print('Plot.Box plotly::layout')
      p1 <- plotly::layout(
        p = p1,
        font = AutoPlots:::font_(),
        title = AutoPlots:::bold_(Title),
        plot_bgcolor = ChartColor,
        paper_bgcolor = BackGroundColor,
        legend = list(title=list(text = paste0('<b> ', GroupVar[1L], ' </b>'))),
        yaxis = list(
          title = AutoPlots:::bold_(YVar),
          zerolinecolor = ZeroLineColor,
          zerolinewidth = ZeroLineWidth,
          gridcolor = GridColor),
        xaxis = list(
          title = AutoPlots:::bold_(XVar),
          zerolinecolor = ZeroLineColor,
          zerolinewidth = ZeroLineWidth,
          gridcolor = GridColor),
        boxmode = 'group')
      return(p1)
    } else {
      if(Debug) print("Plot.Box Echarts")
      p1 <- echarts4r::e_charts_(dt1 |> dplyr::group_by(get(XVar),get(GroupVar)), x = YVar, dispose = TRUE, color = GroupVar, width = Width, height = Height)
      p1 <- echarts4r::e_boxplot_(e = p1, YVar)
      p1 <- echarts4r::e_visual_map_(e = p1, YVar, show = FALSE)
      if(X_Scroll) p1 <- echarts4r::e_datazoom(e = p1, x_index = c(0,1))
      if(Y_Scroll) p1 <- echarts4r::e_datazoom(e = p1, y_Index = c(0,1))
      p1 <- echarts4r::e_theme(e = p1, name = EchartsTheme)
      p1 <- echarts4r::e_aria(e = p1, enabled = TRUE)
      p1 <- echarts4r::e_tooltip(e = p1)
      p1 <- echarts4r::e_toolbox_feature(e = p1, feature = c("saveAsImage","dataZoom"))
      p1 <- echarts4r::e_show_loading(e = p1, hide_overlay = TRUE, text = "Calculating...", color = "#000", text_color = TextColor, mask_color = "#000")
      p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "x", name = XVar, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = xaxis.fontSize))
      p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "y", name = YVar, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = yaxis.fontSize))
      if(CoordFlip) p1 <- echarts4r::e_flip_coords(e = p1)
      p1 <- echarts4r::e_brush(e = p1)
      p1 <- echarts4r::e_title(
        p1, Title,
        textStyle = list(
          color = TextColor,
          fontWeight = title.fontWeight,
          overflow = "truncate", # "none", "truncate", "break",
          ellipsis = '...',
          fontSize = title.fontSize,
          textShadowColor = title.textShadowColor,
          textShadowBlur = title.textShadowBlur,
          textShadowOffsetY = title.textShadowOffsetY,
          textShadowOffsetX = title.textShadowOffsetX))
      return(p1)
    }
  }

  # X,Y
  if(X_and_Y) {

    if(Debug) print("Plot.Box X_and_Y")
    if(Engine == "Plotly") {

      Width <- as.integer(gsub("[^\\d]+", "", Width, perl=TRUE))
      Heigth <- as.integer(gsub("[^\\d]+", "", Height, perl=TRUE))

      # Build
      if(Debug) print('Plot.Box plotly::plot_ly')
      p1 <- plotly::plot_ly(
        data = dt1,
        type = "box",
        width = Width,
        height = Height)

      # Add lines
      if(Debug) print('Plot.Box # Add Lines')
      p1 <- plotly::add_boxplot(
        p = p1,
        x = if(CoordFlip) ~get(YVar) else ~get(XVar),
        y = if(CoordFlip) ~get(XVar) else ~get(YVar),
        color = I(FillColor),
        marker = list(color = FillColorReverse),
        showlegend = FALSE,
        text = NULL,
        hovertemplate = paste(
          Y.HoverFormat,
          X.HoverFormat,
          "<extra></extra>"
        ))

      # Format
      if(Debug) print('Plot.Box plotly::layout')
      p1 <- plotly::layout(
        p = p1,
        font = AutoPlots:::font_(),
        title = AutoPlots:::bold_(Title),
        plot_bgcolor = ChartColor,
        paper_bgcolor = BackGroundColor,
        xaxis = list(
          title = AutoPlots:::bold_(if(CoordFlip) YVar else XVar),
          zerolinewidth = ZeroLineWidth,
          gridcolor = GridColor),
        yaxis = list(
          title = AutoPlots:::bold_(if(CoordFlip) XVar else YVar),
          zerolinewidth = ZeroLineWidth,
          gridcolor = GridColor))#,showlegend = FALSE)
    } else {
      if(Debug) print("Plot.Box Echarts")
      p1 <- echarts4r::e_charts_(dt1 |> dplyr::group_by(get(XVar)), x = YVar, dispose = TRUE, color = GroupVar, width = Width, height = Height)
      p1 <- echarts4r::e_boxplot_(e = p1, YVar)
      p1 <- echarts4r::e_visual_map_(e = p1, YVar, show = FALSE)
      if(X_Scroll) p1 <- echarts4r::e_datazoom(e = p1, x_index = c(0,1))
      if(Y_Scroll) p1 <- echarts4r::e_datazoom(e = p1, y_Index = c(0,1))
      p1 <- echarts4r::e_theme(e = p1, name = EchartsTheme)
      p1 <- echarts4r::e_aria(e = p1, enabled = TRUE)
      p1 <- echarts4r::e_tooltip(e = p1)
      p1 <- echarts4r::e_toolbox_feature(e = p1, feature = c("saveAsImage","dataZoom"))
      p1 <- echarts4r::e_show_loading(e = p1, hide_overlay = TRUE, text = "Calculating...", color = "#000", text_color = TextColor, mask_color = "#000")
      p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "x", name = XVar, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = xaxis.fontSize))
      p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "y", name = YVar, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = yaxis.fontSize))
      if(CoordFlip) p1 <- echarts4r::e_flip_coords(e = p1)
      p1 <- echarts4r::e_brush(e = p1)
      p1 <- echarts4r::e_title(
        p1, Title,
        textStyle = list(
          color = TextColor,
          fontWeight = title.fontWeight,
          overflow = "truncate", # "none", "truncate", "break",
          ellipsis = '...',
          fontSize = title.fontSize,
          textShadowColor = title.textShadowColor,
          textShadowBlur = title.textShadowBlur,
          textShadowOffsetY = title.textShadowOffsetY,
          textShadowOffsetX = title.textShadowOffsetX))

    }

    # Return
    return(p1)
  }

  # Y Only
  if(length(YVar) > 0L) {

    if(Debug) print("Plot.Box Y Only")

    if(Engine == "Plotly") {

      Width <- as.integer(gsub("[^\\d]+", "", Width, perl=TRUE))
      Heigth <- as.integer(gsub("[^\\d]+", "", Height, perl=TRUE))

      # Build
      if(Debug) print('Plot.Box plotly::plot_ly')
      p1 <- plotly::plot_ly(
        data = dt1,
        type = "box",
        width = Width,
        height = Height)

      # Add lines
      p1 <- plotly::add_boxplot(
        p = p1,
        x = "",
        y = ~get(YVar),
        color = I(FillColor),
        marker = list(color = FillColorReverse),
        showlegend = FALSE,
        text = NULL,
        hovertemplate = paste(
          Y.HoverFormat,
          "<extra></extra>"
        ))

      # Format
      if(Debug) print('Plot.Box plotly::layout')
      p1 <- plotly::layout(
        p = p1,
        font = AutoPlots:::font_(),
        title = AutoPlots:::bold_(Title),
        plot_bgcolor = ChartColor,
        paper_bgcolor = BackGroundColor,
        yaxis = list(
          title = AutoPlots:::bold_(YVar),
          zerolinewidth = ZeroLineWidth,
          gridcolor = GridColor))
    } else {
      if(Debug) print("Plot.Box Echarts")
      p1 <- echarts4r::e_charts_(dt1, dispose = TRUE, width = Width, height = Height)
      p1 <- echarts4r::e_boxplot_(e = p1, YVar)
      p1 <- echarts4r::e_visual_map_(e = p1, YVar, show = FALSE)
      if(X_Scroll) p1 <- echarts4r::e_datazoom(e = p1, x_index = c(0,1))
      if(Y_Scroll) p1 <- echarts4r::e_datazoom(e = p1, y_Index = c(0,1))
      p1 <- echarts4r::e_theme(e = p1, name = EchartsTheme)
      p1 <- echarts4r::e_aria(e = p1, enabled = TRUE)
      p1 <- echarts4r::e_tooltip(e = p1)
      p1 <- echarts4r::e_toolbox_feature(e = p1, feature = c("saveAsImage","dataZoom"))
      p1 <- echarts4r::e_show_loading(e = p1, hide_overlay = TRUE, text = "Calculating...", color = "#000", text_color = TextColor, mask_color = "#000")
      p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "x", name = XVar, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = xaxis.fontSize))
      p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "y", name = YVar, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = yaxis.fontSize))
      p1 <- echarts4r::e_brush(e = p1)
      p1 <- echarts4r::e_title(
        p1, Title,
        textStyle = list(
          color = TextColor,
          fontWeight = title.fontWeight,
          overflow = "truncate", # "none", "truncate", "break",
          ellipsis = '...',
          fontSize = title.fontSize,
          textShadowColor = title.textShadowColor,
          textShadowBlur = title.textShadowBlur,
          textShadowOffsetY = title.textShadowOffsetY,
          textShadowOffsetX = title.textShadowOffsetX))

    }
    return(p1)
  }

  # X Only
  if(length(XVar) > 0L) {

    if(Debug) print("Plot.Box X Only")
    if(Engine == "Plotly") {

      Width <- as.integer(gsub("[^\\d]+", "", Width, perl=TRUE))
      Heigth <- as.integer(gsub("[^\\d]+", "", Height, perl=TRUE))

      # Build
      if(Debug) print("Plot.Box plotly::plot_ly")
      p1 <- plotly::plot_ly(
        data = dt1,
        type = "box",
        width = Width,
        height = Height)

      # Add lines
      if(Debug) print("Plot.Box # Add Lines")
      p1 <- plotly::add_boxplot(
        p = p1,
        x = ~get(XVar),
        y = "",
        color = I(FillColor),
        marker = list(color = FillColorReverse),
        showlegend = FALSE,
        text = NULL,
        hovertemplate = paste(
          X.HoverFormat,
          "<extra></extra>"
        ))

      # Format
      if(Debug) print('Plot.Box plotly::layout')
      p1 <- plotly::layout(
        p = p1,
        font = AutoPlots:::font_(),
        title = AutoPlots:::bold_(Title),
        plot_bgcolor = ChartColor,
        paper_bgcolor = BackGroundColor,
        xaxis = list(
          title = AutoPlots:::bold_(XVar),
          zerolinewidth = ZeroLineWidth,
          gridcolor = GridColor))

    } else {

      if(Debug) print("Plot.Box Echarts")
      p1 <- echarts4r::e_charts_(dt1, dispose = TRUE, width = Width, height = Height)
      p1 <- echarts4r::e_boxplot_(e = p1, XVar)
      p1 <- echarts4r::e_visual_map_(e = p1, XVar, show = FALSE)
      if(X_Scroll) p1 <- echarts4r::e_datazoom(e = p1, x_index = c(0,1))
      if(Y_Scroll) p1 <- echarts4r::e_datazoom(e = p1, y_Index = c(0,1))
      p1 <- echarts4r::e_theme(e = p1, name = EchartsTheme)
      p1 <- echarts4r::e_aria(e = p1, enabled = TRUE)
      p1 <- echarts4r::e_tooltip(e = p1)
      p1 <- echarts4r::e_toolbox_feature(e = p1, feature = c("saveAsImage","dataZoom"))
      p1 <- echarts4r::e_show_loading(e = p1, hide_overlay = TRUE, text = "Calculating...", color = "#000", text_color = TextColor, mask_color = "#000")
      p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "x", name = XVar, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = xaxis.fontSize))
      p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "y", name = YVar, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = yaxis.fontSize))
      p1 <- echarts4r::e_brush(e = p1)
      p1 <- echarts4r::e_title(
        p1, Title,
        textStyle = list(
          color = TextColor,
          fontWeight = title.fontWeight,
          overflow = "truncate", # "none", "truncate", "break",
          ellipsis = '...',
          fontSize = title.fontSize,
          textShadowColor = title.textShadowColor,
          textShadowBlur = title.textShadowBlur,
          textShadowOffsetY = title.textShadowOffsetY,
          textShadowOffsetX = title.textShadowOffsetX))

    }

    # Return
    return(p1)
  }
  return(NULL)
}

#' @title Plot.Violin
#'
#' @description Build a violin plot by simply passing arguments to a single function. It will sample your data using SampleSize number of rows. Sampled data is randomized.
#'
#' @family Standard Plots
#'
#' @author Adrian Antico
#'
#' @param dt Source data.table
#' @param XVar Column name of X-Axis variable. If NULL then ignored
#' @param YVar Column name of Y-Axis variable. If NULL then ignored
#' @param GroupVar Requires an XVar and YVar already be defined
#' @param Height = NULL,
#' @param Width = NULL,
#' @param Title 'Violin Plot'
#' @param SampleSize An integer for the number of rows to use. Sampled data is randomized. If NULL then ignored
#' @param FillColor 'gray'
#' @param FillColorReverse character
#' @param ZeroLineColor = '#ffff',
#' @param ZeroLineWidth = 2,
#' @param ChartColor 'lightsteelblue'
#' @param TextColor 'darkblue'
#' @param GridColor 'white'
#' @param BackGroundColor 'gray95'
#' @param Debug FALSE
#'
#' @examples
#' \dontrun{
#' # Step Through Function
#' library(Rappture)
#' library(data.table)
#' dt <- data.table::fread("C:/Users/Bizon/Documents/GitHub/rappwd/CatBoost_ML_Regression_ScoringData.csv")
#' PreAgg = TRUE
#' DataReady = FALSE
#' PlotEngineType = Engine =  "Echarts" # "Plotly"
#' TimeLine = FALSE
#' EchartsTheme = "purple-passion"
#' GroupVar = NULL# "ARTICLE" # "BRAND" #c("BRAND",'ARTICLE')
#' GroupVars = NULL# c("BRAND",'ARTICLE')
#' CorrVars = c("CHILLED_Net_Revenue_PerDay","CHILLED_Margin_PerDay","CHILLED_Liters_PerDay","CHILLED_Units_PerDay")
#' Method = 'spearman'
#' NumLevelsDisplay = 50L
#' AggMethod = 'mean'
#' NumberBins = 20
#' ZeroLineColor = '#ffff'
#' ZeroLineWidth = 1.25
#' Title = 'Bar Plot'
#' SampleSize = 100000
#' FillColor = "#0066ff"
#' FillColorReverse = "#97ff00"
#' BackGroundColor = "#6a6969"
#' ChartColor = '#001534'
#' GridColor = 'white'
#' TextColor = 'white'
#' X_Scroll = TRUE
#' Y_Scroll = TRUE
#' NumLevels_X = 15
#' NumLevels_Y = 15
#' Debug = FALSE
#' Alpha = 0.8
#'
#' # BoxPlot
#' XVar <- NULL
#' GroupVar <- "BRAND"
#' YVar <- "CHILLED_Margin_PerDay"
#' }
#'
#' @export
Plot.Violin <- function(dt = NULL,
                        XVar = NULL,
                        YVar = NULL,
                        GroupVar = NULL,
                        Height = "600px",
                        Width = "1135px",
                        Title = 'Violin Plot',
                        SampleSize = 100000,
                        BackGroundColor =  "#6a6969",
                        ChartColor =       "#001534",
                        FillColor =        "#0066ff",
                        FillColorReverse = "#97ff00",
                        GridColor =        "white",
                        TextColor =        "white",
                        ZeroLineColor = '#ffff',
                        ZeroLineWidth = 1.25,
                        Debug = FALSE) {

  # Ensure data.table
  if(!data.table::is.data.table(dt)) data.table::setDT(dt)

  # XVar > 0 & YVar > 0 --> remove GroupVar?
  if(length(XVar) > 0 && length(YVar) == 0) {
    YVar <- XVar
    if(length(GroupVar) > 0L) {
      XVar <- GroupVar
      GroupVar <- NULL
    }
  } else if(length(XVar) == 0 && length(YVar) > 0) {
    if(length(GroupVar) > 0L) {
      XVar <- GroupVar
      GroupVar <- NULL
    }
  }

  X.HoverFormat <- "%{xaxis.title.text}: %{x:,.2f}<br>"
  Y.HoverFormat <- "%{yaxis.title.text}: %{y:,.2f}<br>"

  # Cap number of records
  if(dt[,.N] > SampleSize) {
    dt1 <- dt[order(runif(.N))][seq_len(SampleSize)]
  } else {
    dt1 <- data.table::copy(dt)
  }

  # Build Plot Based on Available Variables
  # Create logic checks to determine each case distinctly
  X_and_Y_and_GroupVar <- length(XVar) > 0L && length(YVar) > 0L && length(GroupVar) > 0L
  X_and_Y <- length(XVar) > 0L && length(YVar) > 0L

  # Create base plot object
  if(Debug) print('Create Plot with only data')

  # X,Y,GroupVar
  if(X_and_Y_and_GroupVar) {

    # Initalize vars
    vals <- Rappture:::ColorPallete()
    levels <- sort(dt1[, unique(get(GroupVar[1L]))])
    ColorInc <- 1

    # If length(levels) == 2, then do an overlay with the left side being the one case and the right side being the other
    # both sides get different colors
    if(length(levels) != 2L) {

      # Build plot
      p1 <- plotly::plot_ly(
        data = dt1,
        x = ~get(XVar),
        y = ~get(YVar),
        color = ~as.factor(get(GroupVar[1L])),
        type = "violin",
        name = eval(GroupVar[1L]),
        text = ~get(GroupVar[1L]),
        hovertemplate = paste(
          "<b>%{text}</b><br><br>", # Group Var
          Y.HoverFormat,
          X.HoverFormat,
          "<extra></extra>"
        ),
        width = Width,
        height = Height)
      p1 <- plotly::add_trace(
        p = p1,
        x = ~get(XVar),
        y = ~get(YVar),
        color = ~get(GroupVar[1L]),
        box = list(visible = TRUE),
        meanline = list(visible = TRUE))

      # Format
      p1 <- plotly::layout(
        p = p1,
        font = AutoPlots:::font_(),
        title = AutoPlots:::bold_(Title),
        plot_bgcolor = ChartColor,
        paper_bgcolor = BackGroundColor,
        legend = list(title=list(text = paste0('<b> ', GroupVar[1L], ' </b>'))),
        yaxis = list(
          title = AutoPlots:::bold_(YVar),
          zerolinecolor = ZeroLineColor,
          zerolinewidth = ZeroLineWidth,
          gridcolor = GridColor),
        xaxis = list(
          title = AutoPlots:::bold_(XVar),
          zerolinecolor = ZeroLineColor,
          zerolinewidth = ZeroLineWidth,
          gridcolor = GridColor),
        violinmode = 'group')

    } else {

      p1 <- plotly::plot_ly(data = dt1, type = 'violin', width = Width, height = Height)

      # Loop through levels, supplying subsets of the data for x and y each iteration
      for(g in levels) { # g = levels[2]
        p1 <- plotly::add_trace(
          p = p1,
          x = ~dt1[get(GroupVar) == eval(g)][[eval(XVar)]],
          y = ~dt1[get(GroupVar) == eval(g)][[eval(YVar)]],
          legendgroup = g,
          scalegroup = g,
          name = g,
          side = if(ColorInc == 1) 'negative' else 'positive',
          box = list(visible = TRUE),
          meanline = list(visible = TRUE),
          color = if(ColorInc == 1) I(Rappture:::transp_('#0000FF', 0.62)) else I(Rappture:::transp_('#7CFC00', 0.62)))
        ColorInc <- ColorInc + 1L
      }

      # Format
      p1 <- plotly::layout(
        p = p1,
        font = AutoPlots:::font_(),
        title = AutoPlots:::bold_(Title),
        plot_bgcolor = ChartColor,
        paper_bgcolor = BackGroundColor,
        yaxis = list(
          title = AutoPlots:::bold_(YVar),
          zerolinewidth = ZeroLineWidth,
          zerolinecolor = ZeroLineColor,
          gridcolor = GridColor),
        xaxis = list(
          title = AutoPlots:::bold_(XVar),
          zerolinewidth = ZeroLineWidth,
          zerolinecolor = ZeroLineColor,
          gridcolor = GridColor),
        violingap = 0,
        violingroupgap = -2,
        violinmode = 'overlay')
    }

    # Return
    return(p1)
  }

  # X,Y
  if(X_and_Y) {

    p1 <- plotly::plot_ly(
      data = dt1,
      type = 'violin',
      width = Width,
      height = Height)

    # Add lines
    p1 <- plotly::add_trace(
      p = p1,
      x = ~get(XVar),
      y = ~get(YVar),
      color = I(FillColor),
      showlegend = FALSE,
      text = NULL,
      hovertemplate = paste(
        Y.HoverFormat,
        X.HoverFormat,
        "<extra></extra>"
      ))

    # Format
    p1 <- plotly::layout(
      p = p1,
      font = AutoPlots:::font_(),
      title = AutoPlots:::bold_(Title),
      plot_bgcolor = ChartColor,
      paper_bgcolor = BackGroundColor,
      xaxis = list(
        title = AutoPlots:::bold_(XVar),
        zerolinewidth = ZeroLineWidth,
        gridcolor = GridColor),
      yaxis = list(
        title = AutoPlots:::bold_(YVar),
        zerolinewidth = ZeroLineWidth,
        gridcolor = GridColor))

    # Return
    return(p1)
  }

  # Y Only
  if(length(YVar) > 0L) {

    if(Debug) print('YVar > 0L')
    p1 <- plotly::plot_ly(
      data = dt1,
      type = 'violin',
      width = Width,
      height = Height)
    p1 <- plotly::add_trace(
      p = p1,
      x = "",
      y = ~get(YVar),
      color = I(FillColor),
      showlegend = FALSE,
      text = NULL,
      hovertemplate = paste(
        Y.HoverFormat,
        "<extra></extra>"
      ))
    p1 <- plotly::layout(
      p = p1,
      font = AutoPlots:::font_(),
      title = AutoPlots:::bold_(Title),
      plot_bgcolor = ChartColor,
      paper_bgcolor = BackGroundColor,
      yaxis = list(
        title = AutoPlots:::bold_(YVar),
        zerolinewidth = ZeroLineWidth,
        gridcolor = GridColor))

    # Return
    return(p1)
  }

  # X Only
  if(length(XVar) > 0L) {
    p1 <- plotly::plot_ly(
      data = dt1,
      type = 'violin',
      width = Width,
      height = Height)
    p1 <- plotly::add_trace(
      p = p1,
      x = ~get(XVar),
      y = "",
      color = I(FillColor),
      showlegend = FALSE,
      text = NULL,
      hovertemplate = paste(
        X.HoverFormat,
        "<extra></extra>"
      ))

    # Format
    p1 <- plotly::layout(
      p = p1,
      font = AutoPlots:::font_(),
      title = AutoPlots:::bold_(Title),
      plot_bgcolor = ChartColor,
      paper_bgcolor = BackGroundColor,
      xaxis = list(
        title = AutoPlots:::bold_(XVar),
        zerolinewidth = ZeroLineWidth,
        gridcolor = GridColor))

    # Return
    return(p1)
  }
  return(NULL)
}

#' @title Plot.Histogram
#'
#' @description Build a histogram plot by simply passing arguments to a single function. It will sample your data using SampleSize number of rows. Sampled data is randomized.
#'
#' @family Standard Plots
#'
#' @author Adrian Antico
#'
#' @param dt Source data.table
#' @param SampleSize An integer for the number of rows to use. Sampled data is randomized. If NULL then ignored
#' @param XVar Column name of X-Axis variable. If NULL then ignored
#' @param YVar Column name of Y-Axis variable. If NULL then ignored
#' @param GroupVar Column name of Group Variable for distinct colored histograms by group levels
#' @param NumberBins = 30
#' @param Height = NULL,
#' @param Width = NULL,
#' @param Engine = PlotEngineType,
#' @param EchartsTheme = EchartsTheme,
#' @param TimeLine logical
#' @param X_Scroll logical
#' @param Y_Scroll logical
#' @param FillColor 'gray'
#' @param FillColorReverse character
#' @param ChartColor 'lightsteelblue'
#' @param TextColor 'darkblue'
#' @param GridColor 'white'
#' @param BackGroundColor 'gray95'
#' @param ZeroLineWidth = 1.25,
#' @param ZeroLineColor = "white",
#' @param Debug FALSE
#'
#' @examples
#' \dontrun{
#' # Load packages
#' library(AutoPlots)
#' library(data.table)
#'
#' # Load data
#' data <- data.table::fread("./CatBoost_ML1_ScoringData.csv")
#'
#' # Run function
#' p1 <- AutoPlots:::Plot.Histogram(
#'   dt = data,
#'   TimeLine = FALSE,
#'   X_Scroll = TRUE,
#'   Y_Scroll = TRUE,
#'   XVar = "Predict",
#'   YVar = "CHILLED_Margin_PerDay",
#'   GroupVar = "BRAND",
#'   SampleSize = 100000,
#'   NumberBins = 20,
#'   FillColor = "#0066ff",
#'   ChartColor = 'lightsteelblue1',
#'   TextColor = 'darkblue',
#'   GridColor = 'white',
#'   BackGroundColor = 'gray95',
#'   Debug = FALSE)
#'
#' # # Step through function
#' # XVar = "Predict"
#' # YVar = "CHILLED_Margin_PerDay"
#' # GroupVar = "BRAND"
#' # TimeLine = FALSE
#' # AggMethod = 'mean'
#' # ZeroLineWidth = 1.25
#' # Title = 'Histogram'
#' # NumberBins = 20
#' # SampleSize = 100000
#' # FillColor = "#0066ff"
#' # ChartColor = '#001534'
#' # GridColor = 'white'
#' # TextColor = 'darkblue'
#' # Debug = FALSE
#' }
#'
#' @export
Plot.Histogram <- function(dt = NULL,
                           SampleSize = 30000L,
                           XVar = NULL,
                           YVar = NULL,
                           GroupVar = NULL,
                           NumberBins = 30,
                           Height = "600px",
                           Width = "1135px",
                           Title = 'Histogram',
                           Engine = "Plotly",
                           EchartsTheme = "macaron",
                           TimeLine = FALSE,
                           X_Scroll = TRUE,
                           Y_Scroll = TRUE,
                           BackGroundColor =  "#6a6969",
                           ChartColor =       "#001534",
                           FillColor =        "#0066ff",
                           FillColorReverse = "#97ff00",
                           GridColor =        "white",
                           TextColor =        "white",
                           ZeroLineWidth = 1.25,
                           ZeroLineColor = "white",
                           title.fontSize = 22,
                           title.fontWeight = "bold", # normal
                           title.textShadowColor = '#63aeff',
                           title.textShadowBlur = 3,
                           title.textShadowOffsetY = 1,
                           title.textShadowOffsetX = -1,
                           xaxis.fontSize = 14,
                           yaxis.fontSize = 14,
                           Debug = FALSE) {

  TimeLine <- FALSE
  Y.HoverFormat <- "%{yaxis.title.text}: %{y:,.2f}<br>"

  # Cap number of records
  if(length(SampleSize) == 0L) SampleSize <- 30000
  if(!data.table::is.data.table(dt)) data.table::setDT(dt)
  if(dt[, .N] > SampleSize) {
    dt1 <- dt[order(runif(.N))][seq_len(SampleSize)]
  } else {
    dt1 <- data.table::copy(dt)
  }

  # Define Plotting Variable
  if(length(YVar) == 0L && length(XVar) == 0) return(NULL)
  if(length(YVar) == 0L) YVar <- XVar
  if(length(XVar) > 0L && length(GroupVar) == 0L) {
    GroupVar <- XVar
    XVar <- NULL
  }

  GroupVar <- tryCatch({GroupVar[1L]}, error = function(x) NULL)
  YVar <- tryCatch({YVar[1L]}, error = function(x) NULL)

  # Create base plot object
  if(Debug) print('Create Plot with only data')

  # Format
  if(Engine == "Plotly") {

    Width <- as.integer(gsub("[^\\d]+", "", Width, perl=TRUE))
    Heigth <- as.integer(gsub("[^\\d]+", "", Height, perl=TRUE))

    print("Plotly Histogram 1")
    p1 <- plotly::plot_ly(
      data = dt1,
      alpha = 0.6,
      nbinsx = NumberBins,
      width = Width,
      height = Height)
    if(length(GroupVar) > 0L) {
      if(is.numeric(dt1[[GroupVar]])) {
        p1 <- plotly::add_histogram(
          p = p1,
          x = ~get(YVar),
          color = ~get(GroupVar[1L]),
          text = ~get(GroupVar[1L]),
          hovertemplate = paste(
            "<b>%{text}</b><br><br>", # Group Var
            Y.HoverFormat,
            "<extra></extra>"
          )
        )
      } else {
        p1 <- plotly::add_histogram(
          p = p1,
          x = ~get(YVar),
          color = ~get(GroupVar[1L]),
          legendgroup = GroupVar[1L],
          text = ~get(GroupVar[1L]),
          hovertemplate = paste(
            "<b>%{text}</b><br><br>", # Group Var
            Y.HoverFormat,
            "<extra></extra>"
          ))
      }
    } else {
      p1 <- plotly::add_histogram(
        p = p1,
        x = ~get(YVar),
        color = I(FillColor),
        showlegend = FALSE,
        text = NULL,
        hovertemplate = paste(
          "<b>Histogram:</b><br><br>", # Group Var
          Y.HoverFormat,
          "<extra></extra>"
        ))
    }
    p1 <- plotly::layout(
      p = p1,
      font = AutoPlots:::font_(),
      title = AutoPlots:::bold_(Title),
      plot_bgcolor = ChartColor,
      paper_bgcolor = BackGroundColor,
      xaxis = list(
        title = AutoPlots:::bold_(YVar),
        zerolinewidth = ZeroLineWidth,
        gridcolor = GridColor),
      barmode = 'stack')

  } else {

    if(Debug) print("Echarts Histogram 1")
    if(length(GroupVar) > 0L) {
      p1 <- echarts4r::e_charts_(dt1 |> dplyr::group_by(get(GroupVar)), timeline = TimeLine, dispose = TRUE, width = Width, height = Height)
    } else {
      p1 <- echarts4r::e_charts_(
        dt1,
        x = NULL,
        dispose = TRUE,
        width = Width,
        height = Height)
    }
    p1 <- echarts4r::e_histogram_(e = p1, YVar, breaks = NumberBins)
    if(X_Scroll) p1 <- echarts4r::e_datazoom(e = p1, x_index = c(0,1))
    if(Y_Scroll) p1 <- echarts4r::e_datazoom(e = p1, y_Index = c(0,1))
    p1 <- echarts4r::e_theme(e = p1, name = EchartsTheme)
    p1 <- echarts4r::e_aria(e = p1, enabled = TRUE)
    p1 <- echarts4r::e_tooltip(e = p1)
    p1 <- echarts4r::e_toolbox_feature(e = p1, feature = c("saveAsImage","dataZoom"))
    p1 <- echarts4r::e_show_loading(e = p1, hide_overlay = TRUE, text = "Calculating...", color = "#000", text_color = TextColor, mask_color = "#000")
    p1 <- echarts4r::e_legend(e = p1, type = "scroll", orient = "vertical", right = 50, top = 40, height = "240px", textStyle = list(color = TextColor, fontWeight = "bold"))
    p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "x", name = YVar, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = xaxis.fontSize))
    p1 <- echarts4r::e_brush(e = p1)
    p1 <- echarts4r::e_title(
      p1, Title,
      textStyle = list(
        color = TextColor,
        fontWeight = title.fontWeight,
        overflow = "truncate", # "none", "truncate", "break",
        ellipsis = '...',
        fontSize = title.fontSize,
        textShadowColor = title.textShadowColor,
        textShadowBlur = title.textShadowBlur,
        textShadowOffsetY = title.textShadowOffsetY,
        textShadowOffsetX = title.textShadowOffsetX))

  }
  return(p1)
}

#' @title Plot.Density
#'
#' @description Density plots, by groups, with transparent continuous plots
#'
#' @family Standard Plots
#'
#' @param dt From App
#' @param SampleSize = 100000L
#' @param GroupVar From App
#' @param YVar From App
#' @param XVar character
#' @param GroupVar character
#' @param Height = NULL,
#' @param Width = NULL,
#' @param Title = "Density Plot"
#' @param Engine character
#' @param EchartsTheme character
#' @param TimeLine logical
#' @param X_Scroll logical
#' @param Y_Scroll logical
#' @param BackGroundColor "#6a6969",
#' @param ChartColor "#001534",
#' @param FillColor "#0066ff",
#' @param FillColorReverse "#97ff00",
#' @param GridColor "white",
#' @param TextColor "white",
#' @param Debug From App
#'
#' @examples
#' \dontrun{
#' # Load packages
#' library(AutoPlots)
#' library(data.table)
#'
#' # Load data
#' data <- data.table::fread("./CatBoost_ML1_ScoringData.csv")
#'
#' # Run function
#' p1 <- AutoPlots:::Plot.Density(
#'   TimeLine = FALSE,
#'   data = data,
#'   YVar = NULL,
#'   GroupVar = 'Weekly_Sales',
#'   ChartColor = 'lightsteelblue1',
#'   TextColor = 'darkblue',
#'   GridColor = 'white',
#'   BackGroundColor = 'gray95',
#'   Debug = FALSE)
#'
#' # Step through function
#' # # plotly::ggplotly(p1)
#' # TimeLine = FALSE
#' # Engine = "Plotly"
#' # EchartsTheme = "macarons"
#' # data = data
#' # YVar = 'Weekly_Sales'
#' # GroupVar = c('Store','Dept')
#' # ChartColor = 'lightsteelblue1'
#' # TextColor = 'darkblue'
#' # GridColor = 'white'
#' # BackGroundColor = 'gray95'
#' # Debug = FALSE
#' # NumberBins
#' }
#'
#' @export
Plot.Density <- function(dt = NULL,
                         SampleSize = 100000L,
                         YVar = NULL,
                         XVar = NULL,
                         GroupVar = NULL,
                         Height = "600px",
                         Width = "1135px",
                         Title = "Density Plot",
                         Engine = "Plotly",
                         EchartsTheme = "macarons",
                         TimeLine = FALSE,
                         X_Scroll = TRUE,
                         Y_Scroll = TRUE,
                         BackGroundColor =  "#6a6969",
                         ChartColor =       "#001534",
                         FillColor =        "#0066ff",
                         FillColorReverse = "#97ff00",
                         GridColor =        "white",
                         TextColor =        "white",
                         title.fontSize = 22,
                         title.fontWeight = "bold", # normal
                         title.textShadowColor = '#63aeff',
                         title.textShadowBlur = 3,
                         title.textShadowOffsetY = 1,
                         title.textShadowOffsetX = -1,
                         xaxis.fontSize = 14,
                         yaxis.fontSize = 14,
                         Debug = FALSE) {

  TimeLine <- FALSE

  # Cap number of records
  if(length(SampleSize) == 0L) SampleSize <- 30000
  if(!data.table::is.data.table(dt)) data.table::setDT(dt)
  if(dt[, .N] > SampleSize) {
    dt1 <- dt[order(runif(.N))][seq_len(SampleSize)]
  } else {
    dt1 <- data.table::copy(dt)
  }

  # Define Plotting Variable
  if(length(YVar) == 0L && length(XVar) == 0) return(NULL)
  if(length(YVar) == 0L) YVar <- XVar
  if(length(XVar) > 0L && length(GroupVar) == 0L) {
    GroupVar <- XVar
    XVar <- NULL
  }

  GroupVar <- tryCatch({GroupVar[1L]}, error = function(x) NULL)
  YVar <- tryCatch({YVar[1L]}, error = function(x) NULL)

  # Create base plot object
  if(Debug) print('Create Plot with only data')

  if(length(GroupVar) == 0L) {

    # Build plot
    if(Engine == "Plotly") {

      Width <- as.integer(gsub("[^\\d]+", "", Width, perl=TRUE))
      Heigth <- as.integer(gsub("[^\\d]+", "", Height, perl=TRUE))

      p1 <- ggplot2::ggplot(dt1, ggplot2::aes(x = get(YVar)))
      p1 <- p1 + ggplot2::geom_density(alpha = 0.3, color = GridColor)
      p1 <- p1 + ggplot2::xlab(eval(YVar))
      p1 <- p1 + AutoPlots:::ChartTheme(
        ChartColor = ChartColor,
        TextColor = TextColor,
        GridColor = GridColor,
        BackGroundColor = BackGroundColor)
      p1 <- plotly::ggplotly(p1)
    } else {
      p1 <- echarts4r::e_charts_(dt1, x = NULL, dispose = TRUE, width = Width, height = Height)
      p1 <- echarts4r::e_density_(e = p1, YVar, areaStyle = list(opacity = .4), smooth = TRUE, y_index = 1)
      if(X_Scroll) p1 <- echarts4r::e_datazoom(e = p1, x_index = c(0,1))
      if(Y_Scroll) p1 <- echarts4r::e_datazoom(e = p1, y_Index = c(0,1))
      p1 <- echarts4r::e_theme(e = p1, name = EchartsTheme)
      p1 <- echarts4r::e_aria(e = p1, enabled = TRUE)
      p1 <- echarts4r::e_tooltip(e = p1)
      p1 <- echarts4r::e_toolbox_feature(e = p1, feature = c("saveAsImage","dataZoom"))
      p1 <- echarts4r::e_show_loading(e = p1, hide_overlay = TRUE, text = "Calculating...", color = "#000", text_color = TextColor, mask_color = "#000")
      p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "x", name = XVar, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = xaxis.fontSize))
      p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "y", name = YVar, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = yaxis.fontSize))
      p1 <- echarts4r::e_brush(e = p1)
      p1 <- echarts4r::e_title(
        p1, Title,
        textStyle = list(
          color = TextColor,
          fontWeight = title.fontWeight,
          overflow = "truncate", # "none", "truncate", "break",
          ellipsis = '...',
          fontSize = title.fontSize,
          textShadowColor = title.textShadowColor,
          textShadowBlur = title.textShadowBlur,
          textShadowOffsetY = title.textShadowOffsetY,
          textShadowOffsetX = title.textShadowOffsetX))

    }
    return(p1)

  } else {

    # Prepare data
    if(Engine == "Plotly") {

      Width <- as.integer(gsub("[^\\d]+", "", Width, perl=TRUE))
      Heigth <- as.integer(gsub("[^\\d]+", "", Height, perl=TRUE))

      if(length(YVar) > 1L) {
        xx <- data.table::melt.data.table(
          data = dt1,
          id.vars = c(GroupVar), measure.vars= c(YVar), variable.name='Method', value.name='Value')
        p1 <- ggplot2::ggplot(xx, ggplot2::aes(x = Value, fill = Method)) +
          ggplot2::geom_density(alpha = 0.3)
      } else {
        p1 <- ggplot2::ggplot(dt1, ggplot2::aes(x = get(YVar), group = get(GroupVar[1L]), fill = get(GroupVar[1L]))) + ggplot2::geom_density(alpha = 0.3)
      }

      # Add ChartTheme
      if(Debug) print('ChartTheme')
      p1 <- p1 + AutoPlots:::ChartTheme(
        ChartColor = ChartColor,
        TextColor = TextColor,
        GridColor = GridColor,
        BackGroundColor = BackGroundColor)
      p1 <- tryCatch({p1 + ggplot2::guides(fill = ggplot2::guide_legend(title = NULL))}, error = function(x) p1)
      p1 <- p1 + ggplot2::xlab(GroupVar[1L])
      p1 <- plotly::ggplotly(p1)

    } else {

      data.table::setorderv(x = dt1, cols = GroupVar[1L], 1)
      p1 <- echarts4r::e_charts_(dt1 |> dplyr::group_by(get(GroupVar[1L])), timeline = TimeLine, dispose = TRUE, width = Width, height = Height)
      p1 <- echarts4r::e_density_(e = p1, YVar, areaStyle = list(opacity = .4), smooth = TRUE, y_index = 1)
      if(X_Scroll) p1 <- echarts4r::e_datazoom(e = p1, x_index = c(0,1))
      if(Y_Scroll) p1 <- echarts4r::e_datazoom(e = p1, y_Index = c(0,1))
      p1 <- echarts4r::e_theme(e = p1, name = EchartsTheme)
      p1 <- echarts4r::e_aria(e = p1, enabled = TRUE)
      p1 <- echarts4r::e_tooltip(e = p1)
      p1 <- echarts4r::e_toolbox_feature(e = p1, feature = c("saveAsImage","dataZoom"))
      p1 <- echarts4r::e_show_loading(e = p1, hide_overlay = TRUE, text = "Calculating...", color = "#000", text_color = TextColor, mask_color = "#000")
      p1 <- echarts4r::e_legend(e = p1, type = "scroll", orient = "vertical", right = 50, top = 40, height = "240px", textStyle = list(color = TextColor, fontWeight = "bold"))
      p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "x", name = YVar, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = xaxis.fontSize))
      p1 <- echarts4r::e_brush(e = p1)
      p1 <- echarts4r::e_title(
        p1, Title,
        textStyle = list(
          color = TextColor,
          fontWeight = title.fontWeight,
          overflow = "truncate", # "none", "truncate", "break",
          ellipsis = '...',
          fontSize = title.fontSize,
          textShadowColor = title.textShadowColor,
          textShadowBlur = title.textShadowBlur,
          textShadowOffsetY = title.textShadowOffsetY,
          textShadowOffsetX = title.textShadowOffsetX))

    }
    return(p1)
  }
}

# ----

# ----

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
# > Aggreagated Plot Functions                                                ----
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

#' @title Plot.Line
#'
#' @description This function automatically builds calibration plots and calibration boxplots for model evaluation using regression, quantile regression, and binary and multinomial classification
#'
#' @author Adrian Antico
#' @family Standard Plots
#'
#' @param dt data.table
#' @param PreAgg logical
#' @param AggMethod character
#' @param XVar Column name of the predicted values from your model
#' @param YVar Column name of the target variable from your model
#' @param GroupVar One Grouping Variable
#' @param Height = NULL,
#' @param Width = NULL,
#' @param Title "Title"
#' @param Engine "Echarts" or "Plotly"
#' @param EchartsTheme Provide an "Echarts" theme
#' @param TimeLine Logical
#' @param X_Scroll logical
#' @param Y_Scroll logical
#' @param Area logical
#' @param Alpha 0 to 1 for setting transparency
#' @param Smooth = TRUE
#' @param ShowSymbol = FALSE
#' @param ZeroLineColor color
#' @param ZeroLineWidth 1
#' @param BackGroundColor color
#' @param ChartColor color
#' @param FillColor color
#' @param FillColorReverse character
#' @param GridColor color
#' @param TextColor "Not Implemented"
#' @param Debug FALSE
#'
#' @return Calibration plot or boxplot
#' @examples
#' \dontrun{
# Create fake data
#' data <- data.table::fread("./CatBoost_ML1_ScoringData.csv")
#'
#' # Build Plot
#' AutoPlots::Plot.Line(
#'   dt = data,
#'   PreAgg = TRUE,
#'   Engine = "Echarts", # "Plotly"
#'   EchartsTheme = "macaron",
#'   TimeLine = TRUE,
#'   XVar = "Predict",
#'   YVar = "CHILLED_Margin_PerDay",
#'   GroupVar = "BRAND",
#'   ZeroLineColor = '#ffff',
#'   ZeroLineWidth = 1.25,
#'   Title = 'Bar Plot',
#'   FillColor = "#0066ff",
#'   BackGroundColor = "#6a6969", #"#1b1959", #'#00060b',
#'   ChartColor = '#001534',
#'   GridColor = 'white',
#'   TextColor = 'white',
#'   Debug = FALSE)
#'
#' # # Step Through Function
#' # library(AutoPlots)
#' # library(data.table)
#' # dt = data
#' # Y_Scroll = FALSE
#' # X_Scroll = FALSE
#' # Engine = "Echarts" # "Plotly"
#' # TimeLine = TRUE
#' # EchartsTheme = "macaron"
#' # Area = TRUE
#' # Alpha = 0.50
#' # XVar = "Predict"
#' # YVar = "CHILLED_Margin_PerDay"
#' # GroupVar = "BRAND"
#' # ZeroLineColor = '#ffff'
#' # ZeroLineWidth = 1.25
#' # Title = 'Bar Plot'
#' # FillColor = "#0066ff"
#' # BackGroundColor = "#6a6969"
#' # ChartColor = '#001534'
#' # GridColor = 'white'
#' # TextColor = 'white'
#' # PreAgg = TRUE
#' # Debug = FALSE
#' }
#' @export
Plot.Line <- function(dt = NULL,
                      AggMethod = "mean",
                      PreAgg = TRUE,
                      Engine = 'Echarts',
                      XVar = NULL,
                      YVar = NULL,
                      GroupVar = NULL,
                      Height = "600px",
                      Width = "1135px",
                      Title = 'Line Plot',
                      EchartsTheme = "macaron",
                      X_Scroll = FALSE,
                      Y_Scroll = FALSE,
                      TimeLine = TRUE,
                      Area = FALSE,
                      Alpha = 0.50,
                      Smooth = TRUE,
                      ShowSymbol = FALSE,
                      BackGroundColor =  "#6a6969",
                      ChartColor =       "#001534",
                      FillColor =        "#0066ff",
                      FillColorReverse = "#97ff00",
                      GridColor =        "white",
                      TextColor =        "white",
                      ZeroLineColor = '#ffff',
                      ZeroLineWidth = 1.25,
                      title.fontSize = 22,
                      title.fontWeight = "bold", # normal
                      title.textShadowColor = '#63aeff',
                      title.textShadowBlur = 3,
                      title.textShadowOffsetY = 1,
                      title.textShadowOffsetX = -1,
                      xaxis.fontSize = 14,
                      yaxis.fontSize = 14,
                      Debug = FALSE) {

  if(TimeLine) X_Scroll <- FALSE
  Y.HoverFormat <- "%{yaxis.title.text}: %{y:,.2f}<br>"

  # Correct args
  if(length(GroupVar) > 0L && length(XVar) == 0L) {
    XVar <- GroupVar
    GroupVar <- NULL
  }

  if(!data.table::is.data.table(dt)) data.table::setDT(dt)
  Ncols <- ncol(dt)
  if(Ncols > 2L && length(GroupVar) == 0L) {
    if(Debug) print("Plot.Line() Ncols > 2L && length(GroupVar) == 0L")
    dt1 <- data.table::copy(dt[, .SD, .SDcols = c(YVar, XVar)])
  } else if(Ncols > 3L && length(GroupVar) > 0L) {
    if(Debug) print("Plot.Line() Ncols > 3L && length(GroupVar) > 0L")
    dt1 <- data.table::copy(dt[, .SD, .SDcols = c(YVar, XVar, GroupVar[1L])])
  } else {
    if(Debug) print("Plot.Line() make copy of data")
    dt1 <- data.table::copy(dt)
  }

  # Minimize data before moving on
  if(!PreAgg) {

    # Define Aggregation function
    if(Debug) print("Plot.Calibration.Line # Define Aggregation function")
    aggFunc <- AutoPlots:::SummaryFunction(AggMethod)

    # Aggregate data
    if(length(GroupVar) > 0L) {
      dt1 <- dt1[, lapply(.SD, noquote(aggFunc)), by = c(XVar,GroupVar[1L])]
      data.table::setorderv(x = dt1, cols = c(GroupVar[1L], XVar), c(1L,1L))
    } else {
      dt1 <- dt1[, lapply(.SD, noquote(aggFunc)), by = c(XVar)]
      data.table::setorderv(x = dt1, cols = XVar, 1L)
    }
  }

  # Group Variable Case
  if(length(GroupVar) > 0L) {

    # Prepare Data
    if(Debug) print("Plot.Line() Build 1")
    gv <- GroupVar[1L]
    if(PreAgg) data.table::setorderv(x = dt1, cols = c(GroupVar[1L], XVar), c(1L,1L))

    cxv <- class(dt1[[XVar]])[1L]
    if(cxv %in% "IDate") {
      dt1[, eval(XVar) := as.Date(get(XVar))]
    } else if(cxv %in% "IDateTime") {
      dt1[, eval(XVar) := as.POSIXct(get(XVar))]
    }

    # Plot
    if(Engine == "Echarts") {
      if(Debug) print("Plot.Line() Build Echarts 1")

      # Build base plot depending on GroupVar availability
      if(Debug) print(paste0("Plot.Line TimeLine = ", TimeLine))
      p1 <- echarts4r::e_charts_(
        data = dt1 |> dplyr::group_by(get(gv)),
        x = XVar,
        timeline = TimeLine, dispose = TRUE, width = Width, height = Height)

      # Finalize Plot Build
      if(Debug) print("Plot.Line() Build Echarts 4")
      p1 <- echarts4r::e_line_(e = p1, serie = YVar, smooth = Smooth, showSymbol = ShowSymbol)
      if(X_Scroll) p1 <- echarts4r::e_datazoom(e = p1, x_index = c(0,1))
      if(Y_Scroll) p1 <- echarts4r::e_datazoom(e = p1, y_Index = c(0,1))
      p1 <- echarts4r::e_theme(e = p1, name = EchartsTheme)
      p1 <- echarts4r::e_aria(e = p1, enabled = TRUE)
      p1 <- echarts4r::e_tooltip(e = p1)
      p1 <- echarts4r::e_toolbox_feature(e = p1, feature = c("saveAsImage","dataZoom"))
      p1 <- echarts4r::e_show_loading(e = p1, hide_overlay = TRUE, text = "Calculating...", color = "#000", text_color = TextColor, mask_color = "#000")
      p1 <- echarts4r::e_legend(e = p1, type = "scroll", orient = "vertical", right = 50, top = 40, height = "240px", textStyle = list(color = TextColor, fontWeight = "bold"))
      p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "x", name = XVar, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = xaxis.fontSize))
      p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "y", name = YVar, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = yaxis.fontSize))
      p1 <- echarts4r::e_brush(e = p1)
      p1 <- echarts4r::e_title(
        p1, Title,
        textStyle = list(
          color = TextColor,
          fontWeight = title.fontWeight,
          overflow = "truncate", # "none", "truncate", "break",
          ellipsis = '...',
          fontSize = title.fontSize,
          textShadowColor = title.textShadowColor,
          textShadowBlur = title.textShadowBlur,
          textShadowOffsetY = title.textShadowOffsetY,
          textShadowOffsetX = title.textShadowOffsetX))

    } else {

      Width <- as.integer(gsub("[^\\d]+", "", Width, perl=TRUE))
      Heigth <- as.integer(gsub("[^\\d]+", "", Height, perl=TRUE))

      # Build base plot depending on GroupVar availability
      if(Debug) print("Plot.Line group plotly::plot_ly")
      p1 <- plotly::plot_ly(
        dt1,
        x = ~get(XVar),
        y = ~get(YVar),
        color = ~get(gv[1L]),
        type = "scatter",
        mode = "lines",
        text = ~get(gv[1L]),
        hovertemplate = paste(
          "<b>%{text}</b><br><br>", # Group Var
          Y.HoverFormat,
          "<extra></extra>"
        ),
        width = Width,
        height = 700)

      # Finalize Plot Build
      if(Debug) print("Plot.Line group plotly::layout")
      p1 <- plotly::layout(
        p = p1,
        font = AutoPlots:::font_(),
        title = AutoPlots:::bold_(Title),
        plot_bgcolor = ChartColor,
        paper_bgcolor = BackGroundColor,
        xaxis = list(
          title = AutoPlots:::bold_(XVar),
          zerolinewidth = ZeroLineWidth,
          gridcolor = GridColor),
        yaxis = list(
          title = AutoPlots:::bold_(YVar),
          zerolinewidth = ZeroLineWidth,
          gridcolor = GridColor))
    }

  } else {

    # Plot
    data.table::setorderv(x = dt1, cols = XVar, 1L)
    cxv <- class(dt1[[XVar]])[1L]
    if(cxv %in% "IDate") {
      dt1[, eval(XVar) := as.Date(get(XVar))]
    } else if(cxv %in% "IDateTime") {
      dt1[, eval(XVar) := as.POSIXct(get(XVar))]
    }
    if(Engine == "Echarts") {

      # Build base plot depending on GroupVar availability
      if(Debug) print("Plot.Line no group Echarts")
      p1 <- echarts4r::e_charts_(data = dt1, x = XVar, dispose = TRUE, width = Width, height = Height)
      p1 <- echarts4r::e_line_(e = p1, serie = YVar, smooth = Smooth, showSymbol = ShowSymbol)

      # Finalize Plot Build
      if(X_Scroll) p1 <- echarts4r::e_datazoom(e = p1, x_index = c(0,1))
      if(Y_Scroll) p1 <- echarts4r::e_datazoom(e = p1, y_Index = c(0,1))
      p1 <- echarts4r::e_theme(e = p1, name = EchartsTheme)
      p1 <- echarts4r::e_aria(e = p1, enabled = TRUE)
      p1 <- echarts4r::e_tooltip(e = p1)
      p1 <- echarts4r::e_toolbox_feature(e = p1, feature = c("saveAsImage","dataZoom"))
      p1 <- echarts4r::e_show_loading(e = p1, hide_overlay = TRUE, text = "Calculating...", color = "#000", text_color = TextColor, mask_color = "#000")
      p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "x", name = XVar, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = xaxis.fontSize))
      p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "y", name = YVar, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = yaxis.fontSize))
      p1 <- echarts4r::e_brush(e = p1)
      p1 <- echarts4r::e_title(
        p1, Title,
        textStyle = list(
          color = TextColor,
          fontWeight = title.fontWeight,
          overflow = "truncate", # "none", "truncate", "break",
          ellipsis = '...',
          fontSize = title.fontSize,
          textShadowColor = title.textShadowColor,
          textShadowBlur = title.textShadowBlur,
          textShadowOffsetY = title.textShadowOffsetY,
          textShadowOffsetX = title.textShadowOffsetX))

    } else {

      # Build base plot depending on GroupVar availability
      if(Debug) print("Plot.Line no group plotly::plot_ly")
      if(Area) {
        Fill <- "tozeroy"
      } else {
        Fill <- "Tozeroyr"
      }
      p1 <- plotly::plot_ly(
        dt1,
        x = ~get(XVar),
        y = ~get(YVar),
        type = "scatter",
        mode = "lines",
        fill = Fill,
        fillcolor = FillColor,
        alpha = Alpha,
        text = NULL,
        hovertemplate = paste(
          Y.HoverFormat,
          "<extra></extra>"
        ),
        width = Width,
        height = Height)

      # Finalize Plot Build
      if(Debug) print("Plot.Line no group plotly::layout")
      p1 <- plotly::layout(
        p = p1,
        font = AutoPlots:::font_(),
        title = AutoPlots:::bold_(Title),
        plot_bgcolor = ChartColor,
        paper_bgcolor = BackGroundColor,
        xaxis = list(
          title = AutoPlots:::bold_(XVar),
          zerolinewidth = ZeroLineWidth,
          gridcolor = GridColor),
        yaxis = list(
          title = AutoPlots:::bold_(YVar),
          zerolinewidth = ZeroLineWidth,
          gridcolor = GridColor))
    }
  }
  return(p1)
}

#' @title Plot.Area
#'
#' @description This function automatically builds calibration plots and calibration boxplots for model evaluation using regression, quantile regression, and binary and multinomial classification
#'
#' @author Adrian Antico
#' @family Standard Plots
#'
#' @param dt data.table
#' @param PreAgg logical
#' @param AggMethod character
#' @param XVar Column name of the predicted values from your model
#' @param YVar Column name of the target variable from your model
#' @param GroupVar One Grouping Variable
#' @param Height = NULL,
#' @param Width = NULL,
#' @param Title "Title"
#' @param Engine "Echarts" or "Plotly"
#' @param EchartsTheme Provide an "Echarts" theme
#' @param TimeLine Logical
#' @param X_Scroll logical
#' @param Y_Scroll logical
#' @param Area logical
#' @param Alpha 0 to 1 for setting transparency
#' @param Smooth = TRUE
#' @param ShowSymbol = FALSE
#' @param ZeroLineColor color
#' @param ZeroLineWidth 1
#' @param BackGroundColor color
#' @param ChartColor color
#' @param FillColor color
#' @param FillColorReverse character
#' @param GridColor color
#' @param TextColor "Not Implemented"
#' @param Debug FALSE
#'
#' @return Calibration plot or boxplot
#' @examples
#' \dontrun{
# Create fake data
#' data <- data.table::fread("./CatBoost_ML1_ScoringData.csv")
#'
#' # Build Plot
#' AutoPlots::Plot.Area(
#'   dt = data,
#'   PreAgg = TRUE,
#'   Engine = "Echarts", # "Plotly"
#'   EchartsTheme = "macaron",
#'   TimeLine = TRUE,
#'   XVar = "Predict",
#'   YVar = "CHILLED_Margin_PerDay",
#'   GroupVar = "BRAND",
#'   ZeroLineColor = '#ffff',
#'   ZeroLineWidth = 1.25,
#'   Title = 'Bar Plot',
#'   FillColor = "#0066ff",
#'   BackGroundColor = "#6a6969", #"#1b1959", #'#00060b',
#'   ChartColor = '#001534',
#'   GridColor = 'white',
#'   TextColor = 'white',
#'   Debug = FALSE)
#'
#' # # Step Through Function
#' # library(AutoPlots)
#' # library(data.table)
#' # dt = data
#' # Y_Scroll = FALSE
#' # X_Scroll = FALSE
#' # Engine = "Echarts" # "Plotly"
#' # TimeLine = TRUE
#' # EchartsTheme = "macaron"
#' # Area = TRUE
#' # Alpha = 0.50
#' # XVar = "Predict"
#' # YVar = "CHILLED_Margin_PerDay"
#' # GroupVar = "BRAND"
#' # ZeroLineColor = '#ffff'
#' # ZeroLineWidth = 1.25
#' # Title = 'Bar Plot'
#' # FillColor = "#0066ff"
#' # BackGroundColor = "#6a6969"
#' # ChartColor = '#001534'
#' # GridColor = 'white'
#' # TextColor = 'white'
#' # PreAgg = TRUE
#' # Debug = FALSE
#' }
#' @export
Plot.Area <- function(dt = NULL,
                      AggMethod = "mean",
                      PreAgg = TRUE,
                      Engine = 'Echarts',
                      XVar = NULL,
                      YVar = NULL,
                      GroupVar = NULL,
                      Height = "600px",
                      Width = "1135px",
                      Title = 'Line Plot',
                      EchartsTheme = "macaron",
                      X_Scroll = FALSE,
                      Y_Scroll = FALSE,
                      TimeLine = TRUE,
                      Alpha = 0.50,
                      Smooth = TRUE,
                      ShowSymbol = FALSE,
                      BackGroundColor =  "#6a6969",
                      ChartColor =       "#001534",
                      FillColor =        "#0066ff",
                      FillColorReverse = "#97ff00",
                      GridColor =        "white",
                      TextColor =        "white",
                      ZeroLineColor = '#ffff',
                      ZeroLineWidth = 1.25,
                      title.fontSize = 22,
                      title.fontWeight = "bold", # normal
                      title.textShadowColor = '#63aeff',
                      title.textShadowBlur = 3,
                      title.textShadowOffsetY = 1,
                      title.textShadowOffsetX = -1,
                      xaxis.fontSize = 14,
                      yaxis.fontSize = 14,
                      Debug = FALSE) {

  if(TimeLine) X_Scroll <- FALSE

  Y.HoverFormat <- "%{yaxis.title.text}: %{y:,.2f}<br>"

  # Correct args
  if(length(GroupVar) > 0L && length(XVar) == 0L) {
    XVar <- GroupVar
    GroupVar <- NULL
  }

  if(!data.table::is.data.table(dt)) data.table::setDT(dt)
  Ncols <- ncol(dt)
  if(Ncols > 2L && length(GroupVar) == 0L) {
    if(Debug) print("Plot.Line() Ncols > 2L && length(GroupVar) == 0L")
    dt1 <- data.table::copy(dt[, .SD, .SDcols = c(YVar, XVar)])
  } else if(Ncols > 3L && length(GroupVar) > 0L) {
    if(Debug) print("Plot.Line() Ncols > 3L && length(GroupVar) > 0L")
    dt1 <- data.table::copy(dt[, .SD, .SDcols = c(YVar, XVar, GroupVar[1L])])
  } else {
    if(Debug) print("Plot.Line() make copy of data")
    dt1 <- data.table::copy(dt)
  }

  # Minimize data before moving on
  if(!PreAgg) {

    # Define Aggregation function
    if(Debug) print("Plot.Calibration.Line # Define Aggregation function")
    aggFunc <- AutoPlots:::SummaryFunction(AggMethod)

    # Aggregate data
    if(length(GroupVar) > 0L) {
      dt1 <- dt1[, lapply(.SD, noquote(aggFunc)), by = c(XVar,GroupVar[1L])]
      data.table::setorderv(x = dt1, cols = c(GroupVar[1L], XVar), c(1L,1L))
    } else {
      dt1 <- dt1[, lapply(.SD, noquote(aggFunc)), by = c(XVar)]
      data.table::setorderv(x = dt1, cols = XVar, 1L)
    }
  }

  # Group Variable Case
  if(length(GroupVar) > 0L) {

    # Prepare Data
    if(Debug) print("Plot.Line() Build 1")
    gv <- GroupVar[1L]
    if(PreAgg) data.table::setorderv(x = dt1, cols = c(GroupVar[1L], XVar), c(1L,1L))

    cxv <- class(dt1[[XVar]])[1L]
    if(cxv %in% "IDate") {
      dt1[, eval(XVar) := as.Date(get(XVar))]
    } else if(cxv %in% "IDateTime") {
      dt1[, eval(XVar) := as.POSIXct(get(XVar))]
    }

    # Plot
    if(Engine == "Echarts") {
      if(Debug) print("Plot.Line() Build Echarts 1")

      # Build base plot depending on GroupVar availability
      if(Debug) print(paste0("Plot.Line TimeLine = ", TimeLine))
      p1 <- echarts4r::e_charts_(
        data = dt1 |> dplyr::group_by(get(gv)),
        x = XVar,
        timeline = TimeLine, dispose = TRUE, width = Width, height = Height)

      # Finalize Plot Build
      if(Debug) print("Plot.Line() Build Echarts 4")
      p1 <- echarts4r::e_area_(e = p1, serie = YVar, smooth = Smooth, showSymbol = ShowSymbol)
      if(X_Scroll) p1 <- echarts4r::e_datazoom(e = p1, x_index = c(0,1))
      if(Y_Scroll) p1 <- echarts4r::e_datazoom(e = p1, y_Index = c(0,1))
      p1 <- echarts4r::e_theme(e = p1, name = EchartsTheme)
      p1 <- echarts4r::e_aria(e = p1, enabled = TRUE)
      p1 <- echarts4r::e_tooltip(e = p1)
      p1 <- echarts4r::e_toolbox_feature(e = p1, feature = c("saveAsImage","dataZoom"))
      p1 <- echarts4r::e_show_loading(e = p1, hide_overlay = TRUE, text = "Calculating...", color = "#000", text_color = TextColor, mask_color = "#000")
      p1 <- echarts4r::e_legend(e = p1, type = "scroll", orient = "vertical", right = 50, top = 40, height = "240px", textStyle = list(color = TextColor, fontWeight = "bold"))
      p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "x", name = XVar, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = xaxis.fontSize))
      p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "y", name = YVar, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = yaxis.fontSize))
      p1 <- echarts4r::e_brush(e = p1)
      p1 <- echarts4r::e_title(
        p1, Title,
        textStyle = list(
          color = TextColor,
          fontWeight = title.fontWeight,
          overflow = "truncate", # "none", "truncate", "break",
          ellipsis = '...',
          fontSize = title.fontSize,
          textShadowColor = title.textShadowColor,
          textShadowBlur = title.textShadowBlur,
          textShadowOffsetY = title.textShadowOffsetY,
          textShadowOffsetX = title.textShadowOffsetX))

    } else {

      # Build base plot depending on GroupVar availability
      if(Debug) print("Plot.Line group plotly::plot_ly")
      p1 <- plotly::plot_ly(
        dt1,
        x = ~get(XVar),
        y = ~get(YVar),
        color = ~get(gv[1L]),
        type = "scatter",
        fill = "tozeroy",
        mode = "lines",
        text = ~get(gv[1L]),
        hovertemplate = paste(
          "<b>%{text}</b><br><br>", # Group Var
          Y.HoverFormat,
          "<extra></extra>"
        ),
        width = Width,
        height = Height)

      # Finalize Plot Build
      if(Debug) print("Plot.Line group plotly::layout")
      p1 <- plotly::layout(
        p = p1,
        font = AutoPlots:::font_(),
        title = AutoPlots:::bold_(Title),
        plot_bgcolor = ChartColor,
        paper_bgcolor = BackGroundColor,
        xaxis = list(
          title = AutoPlots:::bold_(XVar),
          zerolinewidth = ZeroLineWidth,
          gridcolor = GridColor),
        yaxis = list(
          title = AutoPlots:::bold_(YVar),
          zerolinewidth = ZeroLineWidth,
          gridcolor = GridColor))
    }

  } else {

    # Plot
    data.table::setorderv(x = dt1, cols = XVar, 1L)
    cxv <- class(dt1[[XVar]])[1L]
    if(cxv %in% "IDate") {
      dt1[, eval(XVar) := as.Date(get(XVar))]
    } else if(cxv %in% "IDateTime") {
      dt1[, eval(XVar) := as.POSIXct(get(XVar))]
    }
    if(Engine == "Echarts") {

      # Build base plot depending on GroupVar availability
      if(Debug) print("Plot.Line no group Echarts")
      p1 <- echarts4r::e_charts_(data = dt1, x = XVar, dispose = TRUE, width = Width, height = Height)
      p1 <- echarts4r::e_area_(e = p1, serie = YVar, smooth = Smooth, showSymbol = ShowSymbol)

      # Finalize Plot Build
      if(X_Scroll) p1 <- echarts4r::e_datazoom(e = p1, x_index = c(0,1))
      if(Y_Scroll) p1 <- echarts4r::e_datazoom(e = p1, y_Index = c(0,1))
      p1 <- echarts4r::e_theme(e = p1, name = EchartsTheme)
      p1 <- echarts4r::e_aria(e = p1, enabled = TRUE)
      p1 <- echarts4r::e_tooltip(e = p1)
      p1 <- echarts4r::e_toolbox_feature(e = p1, feature = c("saveAsImage","dataZoom"))
      p1 <- echarts4r::e_show_loading(e = p1, hide_overlay = TRUE, text = "Calculating...", color = "#000", text_color = TextColor, mask_color = "#000")
      p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "x", name = XVar, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = xaxis.fontSize))
      p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "y", name = YVar, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = yaxis.fontSize))
      p1 <- echarts4r::e_brush(e = p1)
      p1 <- echarts4r::e_title(
        p1, Title,
        textStyle = list(
          color = TextColor,
          fontWeight = title.fontWeight,
          overflow = "truncate", # "none", "truncate", "break",
          ellipsis = '...',
          fontSize = title.fontSize,
          textShadowColor = title.textShadowColor,
          textShadowBlur = title.textShadowBlur,
          textShadowOffsetY = title.textShadowOffsetY,
          textShadowOffsetX = title.textShadowOffsetX))

    } else {

      # Build base plot depending on GroupVar availability
      if(Debug) print("Plot.Line no group plotly::plot_ly")
      p1 <- plotly::plot_ly(
        dt1,
        x = ~get(XVar),
        y = ~get(YVar),
        type = "scatter",
        mode = "lines",
        fill = "tozeroy",
        fillcolor = FillColor,
        alpha = Alpha,
        text = NULL,
        hovertemplate = paste(
          Y.HoverFormat,
          "<extra></extra>"
        ),
        width = Width,
        height = Height)

      # Finalize Plot Build
      if(Debug) print("Plot.Line no group plotly::layout")
      p1 <- plotly::layout(
        p = p1,
        font = AutoPlots:::font_(),
        title = AutoPlots:::bold_(Title),
        plot_bgcolor = ChartColor,
        paper_bgcolor = BackGroundColor,
        xaxis = list(
          title = AutoPlots:::bold_(XVar),
          zerolinewidth = ZeroLineWidth,
          gridcolor = GridColor),
        yaxis = list(
          title = AutoPlots:::bold_(YVar),
          zerolinewidth = ZeroLineWidth,
          gridcolor = GridColor))
    }
  }
  return(p1)
}

#' @title Plot.Step
#'
#' @description This function automatically builds calibration plots and calibration boxplots for model evaluation using regression, quantile regression, and binary and multinomial classification
#'
#' @author Adrian Antico
#' @family Standard Plots
#'
#' @param dt data.table
#' @param PreAgg logical
#' @param AggMethod character
#' @param XVar Column name of the predicted values from your model
#' @param YVar Column name of the target variable from your model
#' @param GroupVar One Grouping Variable
#' @param Height = NULL,
#' @param Width = NULL,
#' @param Title "Title"
#' @param Engine "Echarts" or "Plotly"
#' @param EchartsTheme Provide an "Echarts" theme
#' @param TimeLine Logical
#' @param X_Scroll logical
#' @param Y_Scroll logical
#' @param ShowSymbol = FALSE
#' @param ZeroLineColor color
#' @param ZeroLineWidth 1
#' @param BackGroundColor color
#' @param ChartColor color
#' @param FillColor color
#' @param FillColorReverse character
#' @param GridColor color
#' @param TextColor "Not Implemented"
#' @param Debug FALSE
#'
#' @return Calibration plot or boxplot
#' @examples
#' \dontrun{
# Create fake data
#' data <- data.table::fread("./CatBoost_ML1_ScoringData.csv")
#'
#' # Build Plot
#' AutoPlots::Plot.Step(
#'   dt = data,
#'   PreAgg = TRUE,
#'   Engine = "Echarts", # "Plotly"
#'   EchartsTheme = "macaron",
#'   TimeLine = TRUE,
#'   XVar = "Predict",
#'   YVar = "CHILLED_Margin_PerDay",
#'   GroupVar = "BRAND",
#'   ZeroLineColor = '#ffff',
#'   ZeroLineWidth = 1.25,
#'   Title = 'Bar Plot',
#'   FillColor = "#0066ff",
#'   BackGroundColor = "#6a6969", #"#1b1959", #'#00060b',
#'   ChartColor = '#001534',
#'   GridColor = 'white',
#'   TextColor = 'white',
#'   Debug = FALSE)
#'
#' # # Step Through Function
#' # library(AutoPlots)
#' # library(data.table)
#' # dt = data
#' # Y_Scroll = FALSE
#' # X_Scroll = FALSE
#' # Engine = "Echarts" # "Plotly"
#' # TimeLine = TRUE
#' # EchartsTheme = "macaron"
#' # Area = TRUE
#' # Alpha = 0.50
#' # XVar = "Predict"
#' # YVar = "CHILLED_Margin_PerDay"
#' # GroupVar = "BRAND"
#' # ZeroLineColor = '#ffff'
#' # ZeroLineWidth = 1.25
#' # Title = 'Bar Plot'
#' # FillColor = "#0066ff"
#' # BackGroundColor = "#6a6969"
#' # ChartColor = '#001534'
#' # GridColor = 'white'
#' # TextColor = 'white'
#' # PreAgg = TRUE
#' # Debug = FALSE
#' }
#' @export
Plot.Step <- function(dt = NULL,
                      AggMethod = "mean",
                      PreAgg = TRUE,
                      Engine = 'Echarts',
                      XVar = NULL,
                      YVar = NULL,
                      GroupVar = NULL,
                      Height = "600px",
                      Width = "1135px",
                      Title = 'Line Plot',
                      EchartsTheme = "macaron",
                      X_Scroll = FALSE,
                      Y_Scroll = FALSE,
                      TimeLine = TRUE,
                      ShowSymbol = FALSE,
                      BackGroundColor =  "#6a6969",
                      ChartColor =       "#001534",
                      FillColor =        "#0066ff",
                      FillColorReverse = "#97ff00",
                      GridColor =        "white",
                      TextColor =        "white",
                      ZeroLineColor = '#ffff',
                      ZeroLineWidth = 1.25,
                      title.fontSize = 22,
                      title.fontWeight = "bold", # normal
                      title.textShadowColor = '#63aeff',
                      title.textShadowBlur = 3,
                      title.textShadowOffsetY = 1,
                      title.textShadowOffsetX = -1,
                      xaxis.fontSize = 14,
                      yaxis.fontSize = 14,
                      Debug = FALSE) {

  if(TimeLine) X_Scroll <- FALSE

  # Correct args
  if(length(GroupVar) > 0L && length(XVar) == 0L) {
    XVar <- GroupVar
    GroupVar <- NULL
  }

  if(!data.table::is.data.table(dt)) data.table::setDT(dt)
  Ncols <- ncol(dt)
  if(Ncols > 2L && length(GroupVar) == 0L) {
    if(Debug) print("Plot.Line() Ncols > 2L && length(GroupVar) == 0L")
    dt1 <- data.table::copy(dt[, .SD, .SDcols = c(YVar, XVar)])
  } else if(Ncols > 3L && length(GroupVar) > 0L) {
    if(Debug) print("Plot.Line() Ncols > 3L && length(GroupVar) > 0L")
    dt1 <- data.table::copy(dt[, .SD, .SDcols = c(YVar, XVar, GroupVar[1L])])
  } else {
    if(Debug) print("Plot.Line() make copy of data")
    dt1 <- data.table::copy(dt)
  }

  # Minimize data before moving on
  if(!PreAgg) {

    # Define Aggregation function
    if(Debug) print("Plot.Calibration.Line # Define Aggregation function")
    aggFunc <- AutoPlots:::SummaryFunction(AggMethod)

    # Aggregate data
    if(length(GroupVar) > 0L) {
      dt1 <- dt1[, lapply(.SD, noquote(aggFunc)), by = c(XVar,GroupVar[1L])]
      data.table::setorderv(x = dt1, cols = c(GroupVar[1L], XVar), c(1L,1L))
    } else {
      dt1 <- dt1[, lapply(.SD, noquote(aggFunc)), by = c(XVar)]
      data.table::setorderv(x = dt1, cols = XVar, 1L)
    }
  }

  # Group Variable Case
  if(length(GroupVar) > 0L) {

    # Prepare Data
    if(Debug) print("Plot.Line() Build 1")
    gv <- GroupVar[1L]
    if(PreAgg) data.table::setorderv(x = dt1, cols = c(GroupVar[1L], XVar), c(1L,1L))

    cxv <- class(dt1[[XVar]])[1L]
    if(cxv %in% "IDate") {
      dt1[, eval(XVar) := as.Date(get(XVar))]
    } else if(cxv %in% "IDateTime") {
      dt1[, eval(XVar) := as.POSIXct(get(XVar))]
    }

    # Plot
    if(Debug) print("Plot.Line() Build Echarts 1")

    # Build base plot depending on GroupVar availability
    if(Debug) print(paste0("Plot.Line TimeLine = ", TimeLine))
    p1 <- echarts4r::e_charts_(
      data = dt1 |> dplyr::group_by(get(gv)),
      x = XVar,
      timeline = TimeLine, dispose = TRUE, width = Width, height = Height)

    # Finalize Plot Build
    if(Debug) print("Plot.Line() Build Echarts 4")
    p1 <- echarts4r::e_step_(e = p1, serie = YVar, showSymbol = ShowSymbol)
    if(X_Scroll) p1 <- echarts4r::e_datazoom(e = p1, x_index = c(0,1))
    if(Y_Scroll) p1 <- echarts4r::e_datazoom(e = p1, y_Index = c(0,1))
    p1 <- echarts4r::e_theme(e = p1, name = EchartsTheme)
    p1 <- echarts4r::e_aria(e = p1, enabled = TRUE)
    p1 <- echarts4r::e_tooltip(e = p1)
    p1 <- echarts4r::e_toolbox_feature(e = p1, feature = c("saveAsImage","dataZoom"))
    p1 <- echarts4r::e_show_loading(e = p1, hide_overlay = TRUE, text = "Calculating...", color = "#000", text_color = TextColor, mask_color = "#000")
    p1 <- echarts4r::e_legend(e = p1, type = "scroll", orient = "vertical", right = 50, top = 40, height = "240px", textStyle = list(color = TextColor, fontWeight = "bold"))
    p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "x", name = XVar, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = xaxis.fontSize))
    p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "y", name = YVar, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = yaxis.fontSize))
    p1 <- echarts4r::e_brush(e = p1)
    p1 <- echarts4r::e_title(
      p1, Title,
      textStyle = list(
        color = TextColor,
        fontWeight = title.fontWeight,
        overflow = "truncate", # "none", "truncate", "break",
        ellipsis = '...',
        fontSize = title.fontSize,
        textShadowColor = title.textShadowColor,
        textShadowBlur = title.textShadowBlur,
        textShadowOffsetY = title.textShadowOffsetY,
        textShadowOffsetX = title.textShadowOffsetX))

  } else {

  # Plot
  data.table::setorderv(x = dt1, cols = XVar, 1L)
  cxv <- class(dt1[[XVar]])[1L]
  if(cxv %in% "IDate") {
    dt1[, eval(XVar) := as.Date(get(XVar))]
  } else if(cxv %in% "IDateTime") {
    dt1[, eval(XVar) := as.POSIXct(get(XVar))]
  }

    # Build base plot depending on GroupVar availability
    if(Debug) print("Plot.Line no group Echarts")
    p1 <- echarts4r::e_charts_(data = dt1, x = XVar, dispose = TRUE, width = Width, height = Height)
    p1 <- echarts4r::e_step_(e = p1, serie = YVar, showSymbol = ShowSymbol)

    # Finalize Plot Build
    if(X_Scroll) p1 <- echarts4r::e_datazoom(e = p1, x_index = c(0,1))
    if(Y_Scroll) p1 <- echarts4r::e_datazoom(e = p1, y_Index = c(0,1))
    p1 <- echarts4r::e_theme(e = p1, name = EchartsTheme)
    p1 <- echarts4r::e_aria(e = p1, enabled = TRUE)
    p1 <- echarts4r::e_tooltip(e = p1)
    p1 <- echarts4r::e_toolbox_feature(e = p1, feature = c("saveAsImage","dataZoom"))
    p1 <- echarts4r::e_show_loading(e = p1, hide_overlay = TRUE, text = "Calculating...", color = "#000", text_color = TextColor, mask_color = "#000")
    p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "x", name = XVar, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = xaxis.fontSize))
    p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "y", name = YVar, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = yaxis.fontSize))
    p1 <- echarts4r::e_brush(e = p1)
    p1 <- echarts4r::e_title(
      p1, Title,
      textStyle = list(
        color = TextColor,
        fontWeight = title.fontWeight,
        overflow = "truncate", # "none", "truncate", "break",
        ellipsis = '...',
        fontSize = title.fontSize,
        textShadowColor = title.textShadowColor,
        textShadowBlur = title.textShadowBlur,
        textShadowOffsetY = title.textShadowOffsetY,
        textShadowOffsetX = title.textShadowOffsetX))

  }
  return(p1)
}

#' @title Plot.River
#'
#' @description This function automatically builds calibration plots and calibration boxplots for model evaluation using regression, quantile regression, and binary and multinomial classification
#'
#' @author Adrian Antico
#' @family Standard Plots
#'
#' @param dt data.table
#' @param PreAgg logical
#' @param AggMethod character
#' @param XVar Column name of the predicted values from your model
#' @param YVar Column name of the target variable from your model
#' @param GroupVar One Grouping Variable
#' @param Height = NULL,
#' @param Width = NULL,
#' @param Title "Title"
#' @param Engine "Echarts" or "Plotly"
#' @param EchartsTheme Provide an "Echarts" theme
#' @param TimeLine Logical
#' @param X_Scroll logical
#' @param Y_Scroll logical
#' @param ShowSymbol = FALSE
#' @param ZeroLineColor color
#' @param ZeroLineWidth 1
#' @param BackGroundColor color
#' @param ChartColor color
#' @param FillColor color
#' @param FillColorReverse character
#' @param GridColor color
#' @param TextColor "Not Implemented"
#' @param Debug FALSE
#'
#' @return Calibration plot or boxplot
#' @examples
#' \dontrun{
# Create fake data
#' data <- data.table::fread("./CatBoost_ML1_ScoringData.csv")
#'
#' # Build Plot
#' AutoPlots::Plot.River(
#'   dt = data,
#'   PreAgg = TRUE,
#'   Engine = "Echarts", # "Plotly"
#'   EchartsTheme = "macaron",
#'   TimeLine = TRUE,
#'   XVar = "Predict",
#'   YVar = "CHILLED_Margin_PerDay",
#'   GroupVar = "BRAND",
#'   ZeroLineColor = '#ffff',
#'   ZeroLineWidth = 1.25,
#'   Title = 'Bar Plot',
#'   FillColor = "#0066ff",
#'   BackGroundColor = "#6a6969", #"#1b1959", #'#00060b',
#'   ChartColor = '#001534',
#'   GridColor = 'white',
#'   TextColor = 'white',
#'   Debug = FALSE)
#'
#' # # Step Through Function
#' # library(AutoPlots)
#' # library(data.table)
#' # dt = data
#' # Y_Scroll = FALSE
#' # X_Scroll = FALSE
#' # Engine = "Echarts" # "Plotly"
#' # TimeLine = TRUE
#' # EchartsTheme = "macaron"
#' # Area = TRUE
#' # Alpha = 0.50
#' # XVar = "Predict"
#' # YVar = "CHILLED_Margin_PerDay"
#' # GroupVar = "BRAND"
#' # ZeroLineColor = '#ffff'
#' # ZeroLineWidth = 1.25
#' # Title = 'Bar Plot'
#' # FillColor = "#0066ff"
#' # BackGroundColor = "#6a6969"
#' # ChartColor = '#001534'
#' # GridColor = 'white'
#' # TextColor = 'white'
#' # PreAgg = TRUE
#' # Debug = FALSE
#' }
#' @export
Plot.River <- function(dt = NULL,
                      AggMethod = "mean",
                      PreAgg = TRUE,
                      Engine = 'Echarts',
                      XVar = NULL,
                      YVar = NULL,
                      GroupVar = NULL,
                      Height = "600px",
                      Width = "1135px",
                      Title = 'River Plot',
                      EchartsTheme = "macaron",
                      X_Scroll = FALSE,
                      Y_Scroll = FALSE,
                      TimeLine = TRUE,
                      ShowSymbol = FALSE,
                      BackGroundColor =  "#6a6969",
                      ChartColor =       "#001534",
                      FillColor =        "#0066ff",
                      FillColorReverse = "#97ff00",
                      GridColor =        "white",
                      TextColor =        "white",
                      ZeroLineColor = '#ffff',
                      ZeroLineWidth = 1.25,
                      title.fontSize = 22,
                      title.fontWeight = "bold", # normal
                      title.textShadowColor = '#63aeff',
                      title.textShadowBlur = 3,
                      title.textShadowOffsetY = 1,
                      title.textShadowOffsetX = -1,
                      xaxis.fontSize = 14,
                      yaxis.fontSize = 14,
                      Debug = FALSE) {

  if(Debug) print("Plot.River 1")

  if(length(GroupVar) == 0L && length(YVar) <= 1L) {
    print("if(length(GroupVar) == 0L && length(YVar) <= 1L) return(NULL)")
    return(NULL)
  }

  if(Debug) print("Plot.River 2")

  if(!data.table::is.data.table(dt)) data.table::setDT(dt)
  Ncols <- ncol(dt)
  dt1 <- data.table::copy(dt[, .SD, .SDcols = c(YVar, XVar, GroupVar)])

  if(Debug) print("Plot.River 3")

  # Minimize data before moving on
  if(!PreAgg) {

    if(Debug) print("Plot.River 4")

    # Define Aggregation function
    if(Debug) print("Plot.Calibration.Line # Define Aggregation function")
    aggFunc <- AutoPlots:::SummaryFunction(AggMethod)

    # Aggregate data
    if(length(GroupVar) > 0L) {
      dt1 <- dt1[, lapply(.SD, noquote(aggFunc)), by = c(XVar,GroupVar[1L])]
      data.table::setorderv(x = dt1, cols = c(GroupVar[1L], XVar), rep(1L, length(c(GroupVar[1L], XVar))))
    } else {
      dt1 <- dt1[, lapply(.SD, noquote(aggFunc)), by = c(XVar)]
      data.table::setorderv(x = dt1, cols = XVar, 1L)
    }
  }

  if(Debug) print("Plot.River 6b")

  # Plot
  data.table::setorderv(x = dt1, cols = XVar, 1L)
  cxv <- class(dt1[[XVar]])[1L]
  if(cxv %in% "IDate") {
    dt1[, eval(XVar) := as.Date(get(XVar))]
  } else if(cxv %in% "IDateTime") {
    dt1[, eval(XVar) := as.POSIXct(get(XVar))]
  }

  if(Debug) print("Plot.River 7b")

  # Build base plot depending on GroupVar availability
  if(Debug) print("Plot.Line no group Echarts")
  p1 <- echarts4r::e_charts_(data = dt1, x = XVar, dispose = TRUE, width = Width, height = Height)
  for(i in YVar) p1 <- echarts4r::e_river_(e = p1, serie = i)

  if(Debug) print("Plot.River 8b")

  # Finalize Plot Build
  if(X_Scroll) p1 <- echarts4r::e_datazoom(e = p1, x_index = c(0,1))
  if(Y_Scroll) p1 <- echarts4r::e_datazoom(e = p1, y_Index = c(0,1))
  p1 <- echarts4r::e_theme(e = p1, name = EchartsTheme)
  p1 <- echarts4r::e_aria(e = p1, enabled = TRUE)
  p1 <- echarts4r::e_tooltip(e = p1)
  p1 <- echarts4r::e_toolbox_feature(e = p1, feature = c("saveAsImage","dataZoom"))
  p1 <- echarts4r::e_show_loading(e = p1, hide_overlay = TRUE, text = "Calculating...", color = "#000", text_color = TextColor, mask_color = "#000")
  p1 <- echarts4r::e_legend(e = p1, type = "scroll", orient = "vertical", right = 50, top = 40, height = "240px", textStyle = list(color = TextColor, fontWeight = "bold"))
  # p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "x", name = XVar, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = xaxis.fontSize))
  # Sp1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "y", name = YVar, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = yaxis.fontSize))
  p1 <- echarts4r::e_brush(e = p1)
  p1 <- echarts4r::e_title(
    p1, Title,
    textStyle = list(
      color = TextColor,
      fontWeight = title.fontWeight,
      overflow = "truncate", # "none", "truncate", "break",
      ellipsis = '...',
      fontSize = title.fontSize,
      textShadowColor = title.textShadowColor,
      textShadowBlur = title.textShadowBlur,
      textShadowOffsetY = title.textShadowOffsetY,
      textShadowOffsetX = title.textShadowOffsetX))



  if(Debug) print("Plot.River return")

  return(p1)
}

#' @title Plot.Bar
#'
#' @description Build a bar plot by simply passing arguments to a single function
#'
#' @family Standard Plots
#'
#' @author Adrian Antico
#'
#' @param dt Source data.table
#' @param PreAgg logical
#' @param XVar Column name of X-Axis variable. If NULL then ignored
#' @param YVar Column name of Y-Axis variable. If NULL then ignored
#' @param GroupVar Column name of Group Variable for distinct colored histograms by group levels
#' @param AggMethod Choose from 'mean', 'sum', 'sd', and 'median'
#' @param Height = NULL,
#' @param Width = NULL,
#' @param Title title
#' @param Engine 'Plotly' or "Echarts"
#' @param EchartsTheme "macaron"
#' @param TimeLine logical
#' @param X_Scroll logical
#' @param Y_Scroll logical
#' @param ZeroLineColor = '#ffff'
#' @param FillColor 'gray'
#' @param FillColorReverse character
#' @param ChartColor 'lightsteelblue'
#' @param TextColor 'darkblue'
#' @param GridColor 'white'
#' @param BackGroundColor 'gray95'
#' @param Debug FALSE
#'
#' @examples
#' \dontrun{
#' # Load packages
#' library(AutoPlots)
#' library(data.table)
#'
#' # Load data
#' data <- data.table::fread("./CatBoost_ML1_ScoringData.csv")
#'
#' # Run function
#' AutoPlots:::Plot.Bar(
#'   Engine = 'Plotly', # "Echarts"
#'   EchartsTheme = "macaron",
#'   TimeLine = TRUE,
#'   data = data,
#'   XVar = "Predict",
#'   YVar = "CHILLED_Margin_PerDay",
#'   GroupVar = "BRAND",
#'   AggMethod = 'mean',
#'   FillColor = "#0066ff",
#'   ChartColor = 'lightsteelblue1',
#'   TextColor = 'darkblue',
#'   GridColor = 'white',
#'   BackGroundColor = 'gray95',
#'   Debug = FALSE)
#'
#' # # Step through function
#' # dt <- data
#' # Engine = 'Plotly' # "Echarts"
#' # EchartsTheme = "macaron"
#' # XVar = "Predict"
#' # YVar = "CHILLED_Margin_PerDay"
#' # GroupVar = "BRAND"
#' # AggMethod = 'mean'
#' # GroupVar = NULL
#' # ZeroLineColor = '#ffff'
#' # ZeroLineWidth = 1.25
#' # Title = 'Bar Plot'
#' # FillColor = "#0066ff"
#' # BackGroundColor = "#6a6969" #, #"#1b1959", #'#00060b',
#' # ChartColor = '#001534'
#' # TextColor = 'darkblue'
#' # GridColor = 'white'
#' # Debug = FALSE
#' }
#'
#' @export
Plot.Bar <- function(dt = NULL,
                     PreAgg = FALSE,
                     XVar = NULL,
                     YVar = NULL,
                     GroupVar = NULL,
                     AggMethod = 'mean',
                     Height = "600px",
                     Width = "1135px",
                     Title = 'Bar Plot',
                     Engine = 'Echarts',
                     EchartsTheme = "macaron",
                     TimeLine = TRUE,
                     X_Scroll = TRUE,
                     Y_Scroll = TRUE,
                     BackGroundColor =  "#6a6969",
                     ChartColor =       "#001534",
                     FillColor =        "#0066ff",
                     FillColorReverse = "#97ff00",
                     GridColor =        "white",
                     TextColor =        "white",
                     ZeroLineColor = '#ffff',
                     ZeroLineWidth = 1.25,
                     title.fontSize = 22,
                     title.fontWeight = "bold", # normal
                     title.textShadowColor = '#63aeff',
                     title.textShadowBlur = 3,
                     title.textShadowOffsetY = 1,
                     title.textShadowOffsetX = -1,
                     xaxis.fontSize = 14,
                     yaxis.fontSize = 14,
                     Debug = FALSE) {

  if(data.table::is.data.table(dt)) data.table::setDT(dt)

  X.HoverFormat <- "%{xaxis.title.text}: %{x:,.2f}<br>"
  Y.HoverFormat <- "%{yaxis.title.text}: %{y:,.2f}<br>"

  # Used multiple times
  check1 <- length(XVar) != 0 && length(YVar) != 0
  check2 <- length(XVar) == 0 && length(YVar) != 0
  check3 <- length(XVar) != 0 && length(YVar) == 0

  # Define Aggregation function
  if(Debug) print("Plot.Calibration.Line # Define Aggregation function")
  if(!PreAgg) {
    aggFunc <- AutoPlots:::SummaryFunction(AggMethod)
  }

  # Create base plot object
  numvars <- c()
  byvars <- c()
  if(check1) {
    if(length(GroupVar) != 0L) {
      if(!PreAgg) {
        if(any(tryCatch({class(dt[[eval(YVar)]])}, error = function(x) "bla") %in% c('numeric','integer'))) {
          numvars <- unique(c(numvars, YVar))
        } else {
          byvars <- unique(c(byvars, YVar))
        }
        if(any(tryCatch({class(dt[[eval(XVar)]])}, error = function(x) "bla") %in% c('numeric','integer'))) {
          if(length(numvars) > 0) {
            x <- length(unique(dt[[XVar]]))
            y <- length(unique(dt[[YVar]]))
            if(x > y) {
              byvars <- unique(c(byvars, YVar))
              numvars[1L] <- XVar
            } else {
              byvars <- unique(c(byvars, XVar))
            }
          } else {
            numvars <- unique(c(numvars, XVar))
          }
        } else {
          byvars <- unique(c(byvars, XVar))
        }
        if(any(tryCatch({class(dt[[eval(GroupVar)]])}, error = function(x) "bla") %in% c('numeric','integer'))) {
          dt[, eval(GroupVar) := as.character(get(GroupVar))]
          byvars <- unique(c(byvars, GroupVar))
        } else {
          byvars <- unique(c(byvars, GroupVar))
        }
        if(!is.null(byvars)) {
          temp <- dt[, lapply(.SD, noquote(aggFunc)), .SDcols = c(numvars), by = c(byvars)]
          for(i in byvars) {
            if(class(temp[[i]]) %in% c('numeric','integer')) {
              temp[, eval(i) := as.character(get(i))]
            }
          }
        } else {
          temp <- dt[, lapply(.SD, noquote(aggFunc)), .SDcols = c(numvars)]
        }
      } else {
        temp <- data.table::copy(dt)
      }

      # Plotly
      if(Engine == "Plotly") {

        Width <- as.integer(gsub("[^\\d]+", "", Width, perl=TRUE))
        Heigth <- as.integer(gsub("[^\\d]+", "", Height, perl=TRUE))

        p1 <- plotly::plot_ly(
          data = temp,
          x = ~get(XVar),
          y = ~get(YVar),
          color = ~as.factor(get(GroupVar[1L])),
          type = "bar",
          name = eval(GroupVar[1L]),
          text = ~get(GroupVar[1L]),
          hovertemplate = paste(
            "<b>%{text}</b><br><br>",
            Y.HoverFormat,
            X.HoverFormat,
            "<extra></extra>"
          ),
          width = Width,
          height = Height)
        p1 <- plotly::layout(
          p = p1,
          font = AutoPlots:::font_(),
          title = AutoPlots:::bold_(Title),
          plot_bgcolor = ChartColor,
          paper_bgcolor = BackGroundColor,
          legend = list(title=list(text = paste0('<b> ', GroupVar[1L], ' </b>'))),
          yaxis = list(
            title = AutoPlots:::bold_(YVar),
            zerolinecolor = ZeroLineColor,
            zerolinewidth = ZeroLineWidth,
            gridcolor = GridColor),
          xaxis = list(
            title = AutoPlots:::bold_(XVar),
            categoryorder = "total descending",
            zerolinecolor = ZeroLineColor,
            zerolinewidth = ZeroLineWidth,
            gridcolor = GridColor),
          barmode = 'group')
      } else {
        p1 <- echarts4r::e_charts_(temp, x = XVar, dispose = TRUE, width = Width, height = Height)
        p1 <- echarts4r::e_bar_(e = p1, YVar)
        if(X_Scroll) p1 <- echarts4r::e_datazoom(e = p1, x_index = c(0,1))
        if(Y_Scroll) p1 <- echarts4r::e_datazoom(e = p1, y_Index = c(0,1))
        p1 <- echarts4r::e_theme(e = p1, name = EchartsTheme)
        p1 <- echarts4r::e_aria(e = p1, enabled = TRUE)
        p1 <- echarts4r::e_tooltip(e = p1)
        p1 <- echarts4r::e_toolbox_feature(e = p1, feature = c("saveAsImage","dataZoom"))
        p1 <- echarts4r::e_show_loading(e = p1, hide_overlay = TRUE, text = "Calculating...", color = "#000", text_color = TextColor, mask_color = "#000")
        p1 <- echarts4r::e_brush(e = p1)
        p1 <- echarts4r::e_title(
          p1, Title,
          textStyle = list(
            color = TextColor,
            fontWeight = title.fontWeight,
            overflow = "truncate", # "none", "truncate", "break",
            ellipsis = '...',
            fontSize = title.fontSize,
            textShadowColor = title.textShadowColor,
            textShadowBlur = title.textShadowBlur,
            textShadowOffsetY = title.textShadowOffsetY,
            textShadowOffsetX = title.textShadowOffsetX))

      }
      return(p1)

    } else {

      if(Debug) print("BarPlot 2.b")
      if(!PreAgg) {
        if(tryCatch({class(dt[[eval(YVar)]])[1L]}, error = function(x) "bla") %in% c('numeric','integer')) {
          numvars <- unique(c(numvars, YVar))
        } else {
          byvars <- unique(c(byvars, YVar))
        }
        if(tryCatch({class(dt[[eval(XVar)]])[1L]}, error = function(x) "bla") %in% c('numeric','integer')) {
          if(length(numvars) > 0) {
            x <- length(unique(dt[[XVar]]))
            y <- length(unique(dt[[YVar]]))
            if(x > y) {
              byvars <- unique(c(byvars, YVar))
              numvars[1L] <- XVar
            } else {
              byvars <- unique(c(byvars, XVar))
            }
          } else {
            numvars <- unique(c(numvars, XVar))
          }
        } else {
          byvars <- unique(c(byvars, XVar))
        }
        if(!is.null(byvars)) {
          temp <- dt[, lapply(.SD, noquote(aggFunc)), .SDcols = c(numvars), by = c(byvars)]
          for(i in byvars) {
            if(class(temp[[i]])[1L] %in% c('numeric','integer')) {
              temp[, eval(i) := as.character(get(i))]
            }
          }
        } else {
          temp <- dt[, lapply(.SD, noquote(aggFunc)), .SDcols = c(numvars)]
        }
      } else {
        temp <- data.table::copy(dt)
      }

      yvar <- temp[[YVar]]
      xvar <- temp[[XVar]]

      # Plotly
      if(Engine == "Plotly") {

        Width <- as.integer(gsub("[^\\d]+", "", Width, perl=TRUE))
        Heigth <- as.integer(gsub("[^\\d]+", "", Height, perl=TRUE))

        p1 <- plotly::plot_ly(
          data = temp,
          x = ~get(XVar),
          y = ~get(YVar),
          type = 'bar',
          color = I(FillColor[1]),
          orientation = "vert",
          showlegend = FALSE,
          text = NULL,
          hovertemplate = paste(
            Y.HoverFormat,
            X.HoverFormat,
            "<extra></extra>"
          ),
          width = Width,
          height = Height)
        p1 <- plotly::layout(
          p = p1,
          font = AutoPlots:::font_(),
          title = AutoPlots:::bold_(Title),
          plot_bgcolor = ChartColor,
          paper_bgcolor = BackGroundColor,
          legend = list(title=list(text = paste0('<b> ', GroupVar[1L], ' </b>'))),
          yaxis = list(
            title = AutoPlots:::bold_(YVar),
            zerolinecolor = ZeroLineColor,
            zerolinewidth = ZeroLineWidth,
            gridcolor = GridColor),
          xaxis = list(
            title = AutoPlots:::bold_(XVar),
            categoryorder = "total descending",
            zerolinecolor = ZeroLineColor,
            zerolinewidth = ZeroLineWidth,
            gridcolor = GridColor),
          barmode = 'group')

      } else {
        if(XVar == "Importance" && YVar == "Variable") {
          XVar <- "Variable"
          YVar <- "Importance"
        }
        p1 <- echarts4r::e_charts_(temp, x = XVar, dispose = TRUE, width = Width, height = Height)
        p1 <- echarts4r::e_bar_(e = p1, YVar)
        if(X_Scroll) p1 <- echarts4r::e_datazoom(e = p1, x_index = c(0,1))
        if(Y_Scroll) p1 <- echarts4r::e_datazoom(e = p1, y_Index = c(0,1))
        p1 <- echarts4r::e_theme(e = p1, name = EchartsTheme)
        p1 <- echarts4r::e_aria(e = p1, enabled = TRUE)
        p1 <- echarts4r::e_tooltip(e = p1)
        p1 <- echarts4r::e_toolbox_feature(e = p1, feature = c("saveAsImage","dataZoom"))
        p1 <- echarts4r::e_show_loading(e = p1, hide_overlay = TRUE, text = "Calculating...", color = "#000", text_color = TextColor, mask_color = "#000")
        p1 <- echarts4r::e_legend(e = p1, type = "scroll", orient = "vertical", right = 50, top = 40, height = "240px", textStyle = list(color = TextColor, fontWeight = "bold"))
        p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "x", name = XVar, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = xaxis.fontSize))
        p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "y", name = YVar, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = yaxis.fontSize))
        p1 <- echarts4r::e_brush(e = p1)
        p1 <- echarts4r::e_title(
          p1, Title,
          textStyle = list(
            color = TextColor,
            fontWeight = title.fontWeight,
            overflow = "truncate", # "none", "truncate", "break",
            ellipsis = '...',
            fontSize = title.fontSize,
            textShadowColor = title.textShadowColor,
            textShadowBlur = title.textShadowBlur,
            textShadowOffsetY = title.textShadowOffsetY,
            textShadowOffsetX = title.textShadowOffsetX))

      }
      return(p1)
    }

  } else if(check2) {

    if(length(GroupVar) != 0) {
      if(!PreAgg) {
        if(any(tryCatch({class(dt[[eval(YVar)]])}, error = function(x) "bla") %in% c('numeric','integer'))) {
          numvars <- unique(c(numvars, YVar))
        } else {
          byvars <- unique(c(byvars, YVar))
        }
        if(any(tryCatch({class(dt[[eval(GroupVar)]])}, error = function(x) "bla") %in% c('numeric','integer'))) {
          numvars <- unique(c(numvars, GroupVar))
        } else {
          byvars <- unique(c(byvars, GroupVar))
        }
        if(!is.null(byvars)) {
          temp <- dt[, lapply(.SD, noquote(aggFunc)), .SDcols = c(numvars), by = c(byvars)]
        } else {
          temp <- dt[, lapply(.SD, noquote(aggFunc)), .SDcols = c(numvars)]
        }
      } else {
        temp <- data.table::copy(dt)
      }

      if(Engine == "Plotly") {

        Width <- as.integer(gsub("[^\\d]+", "", Width, perl=TRUE))
        Heigth <- as.integer(gsub("[^\\d]+", "", Height, perl=TRUE))

        p1 <- plotly::plot_ly(
          data = temp,
          x = ~get(GroupVar[1L]),
          y = ~get(YVar),
          type = 'bar',
          name = eval(YVar),
          color = I(FillColor),
          showlegend = FALSE,
          text = ~get(GroupVar[1L]),
          hovertemplate = paste(
            "<b>%{text}</b><br><br>", # Group Var
            Y.HoverFormat,
            X.HoverFormat,
            "<extra></extra>"
          ),
          width = Width,
          height = Height)
        p1 <- plotly::layout(
          p = p1,
          font = AutoPlots:::font_(),
          title = AutoPlots:::bold_(Title),
          plot_bgcolor = ChartColor,
          paper_bgcolor = BackGroundColor,
          legend = list(title=list(text = paste0('<b> ', GroupVar[1L], ' </b>'))),
          yaxis = list(
            title = AutoPlots:::bold_(YVar),
            zerolinecolor = ZeroLineColor,
            zerolinewidth = ZeroLineWidth,
            gridcolor = GridColor),
          xaxis = list(
            title = AutoPlots:::bold_(GroupVar[1L]),
            categoryorder = "total descending",
            zerolinecolor = ZeroLineColor,
            zerolinewidth = ZeroLineWidth,
            gridcolor = GridColor),
          barmode = 'group')
      } else {
        p1 <- echarts4r::e_charts_(temp, x = GroupVar[1L], dispose = TRUE, width = Width, height = Height)
        p1 <- echarts4r::e_bar_(e = p1, YVar)
        if(X_Scroll) p1 <- echarts4r::e_datazoom(e = p1, x_index = c(0,1))
        if(Y_Scroll) p1 <- echarts4r::e_datazoom(e = p1, y_Index = c(0,1))
        p1 <- echarts4r::e_theme(e = p1, name = EchartsTheme)
        p1 <- echarts4r::e_aria(e = p1, enabled = TRUE)
        p1 <- echarts4r::e_tooltip(e = p1)
        p1 <- echarts4r::e_toolbox_feature(e = p1, feature = c("saveAsImage","dataZoom"))
        p1 <- echarts4r::e_show_loading(e = p1, hide_overlay = TRUE, text = "Calculating...", color = "#000", text_color = TextColor, mask_color = "#000")
        p1 <- echarts4r::e_legend(e = p1, type = "scroll", orient = "vertical", right = 50, top = 40, height = "240px", textStyle = list(color = TextColor, fontWeight = "bold"))
        p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "x", name = XVar, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = xaxis.fontSize))
        p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "y", name = YVar, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = yaxis.fontSize))
        p1 <- echarts4r::e_brush(e = p1)
        p1 <- echarts4r::e_title(
          p1, Title,
          textStyle = list(
            color = TextColor,
            fontWeight = title.fontWeight,
            overflow = "truncate", # "none", "truncate", "break",
            ellipsis = '...',
            fontSize = title.fontSize,
            textShadowColor = title.textShadowColor,
            textShadowBlur = title.textShadowBlur,
            textShadowOffsetY = title.textShadowOffsetY,
            textShadowOffsetX = title.textShadowOffsetX))

      }
      return(p1)
    } else {
      return(NULL)
    }

  } else if(check3) {

    if(length(GroupVar) != 0) {
      if(!PreAgg) {
        if(any(tryCatch({class(dt[[eval(XVar)]])}, error = function(x) "bla") %in% c('numeric','integer'))) {
          numvars <- unique(c(numvars, XVar))
        } else {
          byvars <- unique(c(byvars, XVar))
        }
        if(any(tryCatch({class(dt[[eval(GroupVar)]])}, error = function(x) "bla") %in% c('numeric','integer'))) {
          numvars <- unique(c(numvars, GroupVar))
        } else {
          byvars <- unique(c(byvars, GroupVar))
        }
        if(!is.null(byvars)) {
          temp <- dt[, lapply(.SD, noquote(aggFunc)), .SDcols = c(numvars), by = c(byvars)]
        } else {
          temp <- dt[, lapply(.SD, noquote(aggFunc)), .SDcols = c(numvars)]
        }
      } else {
        temp <- data.table::copy(dt)
      }

      # Plot
      if(Engine == "Plotly") {

        Width <- as.integer(gsub("[^\\d]+", "", Width, perl=TRUE))
        Heigth <- as.integer(gsub("[^\\d]+", "", Height, perl=TRUE))

        p1 <- plotly::plot_ly(
          data = temp,
          x = ~get(GroupVar[1L]),
          y = ~get(XVar),
          type = 'bar',
          name = eval(XVar),
          color = I(FillColor),
          showlegend = FALSE,
          text = ~get(GroupVar[1L]),
          hovertemplate = paste(
            Y.HoverFormat,
            X.HoverFormat,
            "<extra></extra>"
          ),
          width = Width,
          height = Height)
        p1 <- plotly::layout(
          p = p1,
          font = AutoPlots:::font_(),
          title = AutoPlots:::bold_(Title),
          plot_bgcolor = ChartColor,
          paper_bgcolor = BackGroundColor,
          legend = list(title=list(text = paste0('<b> ', GroupVar[1L], ' </b>'))),
          yaxis = list(
            title = AutoPlots:::bold_(XVar),
            zerolinecolor = ZeroLineColor,
            zerolinewidth = ZeroLineWidth,
            gridcolor = GridColor),
          xaxis = list(
            title = AutoPlots:::bold_(GroupVar[1L]),
            categoryorder = "total descending",
            zerolinecolor = ZeroLineColor,
            zerolinewidth = ZeroLineWidth,
            gridcolor = GridColor),
          barmode = 'group')
      } else {
        p1 <- echarts4r::e_charts_(temp, x = GroupVar[1L], dispose = TRUE, width = Width, height = Height)
        p1 <- echarts4r::e_bar_(e = p1, XVar)
        if(X_Scroll) p1 <- echarts4r::e_datazoom(e = p1, x_index = c(0,1))
        if(Y_Scroll) p1 <- echarts4r::e_datazoom(e = p1, y_Index = c(0,1))
        p1 <- echarts4r::e_theme(e = p1, name = EchartsTheme)
        p1 <- echarts4r::e_aria(e = p1, enabled = TRUE)
        p1 <- echarts4r::e_tooltip(e = p1)
        p1 <- echarts4r::e_toolbox_feature(e = p1, feature = c("saveAsImage","dataZoom"))
        p1 <- echarts4r::e_show_loading(e = p1, hide_overlay = TRUE, text = "Calculating...", color = "#000", text_color = TextColor, mask_color = "#000")
        p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "x", name = XVar, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = xaxis.fontSize))
        p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "y", name = YVar, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = yaxis.fontSize))
        p1 <- echarts4r::e_brush(e = p1)
        p1 <- echarts4r::e_title(
          p1, Title,
          textStyle = list(
            color = TextColor,
            fontWeight = title.fontWeight,
            overflow = "truncate", # "none", "truncate", "break",
            ellipsis = '...',
            fontSize = title.fontSize,
            textShadowColor = title.textShadowColor,
            textShadowBlur = title.textShadowBlur,
            textShadowOffsetY = title.textShadowOffsetY,
            textShadowOffsetX = title.textShadowOffsetX))

      }
      return(p1)
    } else {
      return(NULL)
    }
  } else {
    return(NULL)
  }

  # Return plot
  return(eval(p1))
}

#' @title Plot.StackedBar
#'
#' @description Build a stacked bar plot vs a grouped bar plot
#'
#' @family Standard Plots
#'
#' @author Adrian Antico
#'
#' @param dt Source data.table
#' @param PreAgg logical
#' @param XVar Column name of X-Axis variable. If NULL then ignored
#' @param YVar Column name of Y-Axis variable. If NULL then ignored
#' @param GroupVar Column name of Group Variable for distinct colored histograms by group levels
#' @param AggMethod Choose from 'mean', 'sum', 'sd', and 'median'
#' @param Height = NULL,
#' @param Width = NULL,
#' @param Title title
#' @param Engine 'Plotly' or "Echarts"
#' @param EchartsTheme "macaron"
#' @param TimeLine logical
#' @param X_Scroll logical
#' @param Y_Scroll logical
#' @param ZeroLineColor = '#ffff'
#' @param FillColor 'gray'
#' @param FillColorReverse character
#' @param ChartColor 'lightsteelblue'
#' @param TextColor 'darkblue'
#' @param GridColor 'white'
#' @param BackGroundColor 'gray95'
#' @param Debug FALSE
#'
#' @examples
#' \dontrun{
#' # Load packages
#' library(AutoPlots)
#' library(data.table)
#'
#' # Load data
#' data <- data.table::fread("./CatBoost_ML1_ScoringData.csv")
#'
#' # Run function
#' AutoPlots:::Plot.Bar(
#'   Engine = 'Plotly', # "Echarts"
#'   EchartsTheme = "macaron",
#'   TimeLine = TRUE,
#'   data = data,
#'   XVar = "Predict",
#'   YVar = "CHILLED_Margin_PerDay",
#'   GroupVar = "BRAND",
#'   AggMethod = 'mean',
#'   FillColor = "#0066ff",
#'   ChartColor = 'lightsteelblue1',
#'   TextColor = 'darkblue',
#'   GridColor = 'white',
#'   BackGroundColor = 'gray95',
#'   Debug = FALSE)
#'
#' # # Step through function
#' # dt <- data
#' # Engine = 'Plotly' # "Echarts"
#' # EchartsTheme = "macaron"
#' # XVar = "Predict"
#' # YVar = "CHILLED_Margin_PerDay"
#' # GroupVar = "BRAND"
#' # AggMethod = 'mean'
#' # GroupVar = NULL
#' # ZeroLineColor = '#ffff'
#' # ZeroLineWidth = 1.25
#' # Title = 'Bar Plot'
#' # FillColor = "#0066ff"
#' # BackGroundColor = "#6a6969" #, #"#1b1959", #'#00060b',
#' # ChartColor = '#001534'
#' # TextColor = 'darkblue'
#' # GridColor = 'white'
#' # Debug = FALSE
#' }
#'
#' @export
Plot.StackedBar <- function(dt = NULL,
                            PreAgg = FALSE,
                            XVar = NULL,
                            YVar = NULL,
                            GroupVar = NULL,
                            AggMethod = 'mean',
                            Height = "600px",
                            Width = "1135px",
                            Title = "~Stacked Bar~",
                            Engine = 'Echarts',
                            EchartsTheme = "macaron",
                            TimeLine = TRUE,
                            X_Scroll = TRUE,
                            Y_Scroll = TRUE,
                            BackGroundColor =  "#6a6969",
                            ChartColor =       "#001534",
                            FillColor =        "#0066ff",
                            FillColorReverse = "#97ff00",
                            GridColor =        "white",
                            TextColor =        "white",
                            ZeroLineColor = '#ffff',
                            ZeroLineWidth = 1.25,
                            title.fontSize = 22,
                            title.fontWeight = "bold", # normal
                            title.textShadowColor = '#63aeff',
                            title.textShadowBlur = 3,
                            title.textShadowOffsetY = 1,
                            title.textShadowOffsetX = -1,
                            yaxis.fontSize = 14,
                            xaxis.fontSize = 14,
                            Debug = FALSE) {

  print("StackedBarPlot step 1")

  if(data.table::is.data.table(dt)) data.table::setDT(dt)
  X.HoverFormat <- "%{xaxis.title.text}: %{x:,.2f}<br>"
  Y.HoverFormat <- "%{yaxis.title.text}: %{y:,.2f}<br>"

  # Used multiple times
  check1 <- length(XVar) != 0 && length(YVar) != 0 && length(GroupVar) > 0L

  if(!PreAgg) {
    aggFunc <- AutoPlots:::SummaryFunction(AggMethod)
  }

  # Create base plot object
  numvars <- c()
  byvars <- c()
  if(check1) {
    if(length(GroupVar) != 0L) {
      if(!PreAgg) {
        if(any(tryCatch({class(dt[[eval(YVar)]])}, error = function(x) "bla") %in% c('numeric','integer'))) {
          numvars <- unique(c(numvars, YVar))
        } else {
          byvars <- unique(c(byvars, YVar))
        }
        if(any(tryCatch({class(dt[[eval(XVar)]])}, error = function(x) "bla") %in% c('numeric','integer'))) {
          if(length(numvars) > 0) {
            x <- length(unique(dt[[XVar]]))
            y <- length(unique(dt[[YVar]]))
            if(x > y) {
              byvars <- unique(c(byvars, YVar))
              numvars[1L] <- XVar
            } else {
              byvars <- unique(c(byvars, XVar))
            }
          } else {
            numvars <- unique(c(numvars, XVar))
          }
        } else {
          byvars <- unique(c(byvars, XVar))
        }
        if(any(tryCatch({class(dt[[eval(GroupVar)]])}, error = function(x) "bla") %in% c('numeric','integer'))) {
          dt[, eval(GroupVar) := as.character(get(GroupVar))]
          byvars <- unique(c(byvars, GroupVar))
        } else {
          byvars <- unique(c(byvars, GroupVar))
        }
        if(!is.null(byvars)) {
          temp <- dt[, lapply(.SD, noquote(aggFunc)), .SDcols = c(numvars), by = c(byvars)]
          for(i in byvars) {
            if(class(temp[[i]]) %in% c('numeric','integer')) {
              temp[, eval(i) := as.character(get(i))]
            }
          }
        } else {
          temp <- dt[, lapply(.SD, noquote(aggFunc)), .SDcols = c(numvars)]
        }
      } else {
        temp <- data.table::copy(dt)
      }

      p1 <- echarts4r::e_charts_(data = temp |> dplyr::group_by(get(GroupVar[1L])), x = XVar, dispose = TRUE, width = Width, height = Height)
      p1 <- echarts4r::e_bar_(e = p1, YVar, stack = XVar)
      if(X_Scroll) p1 <- echarts4r::e_datazoom(e = p1, x_index = c(0,1))
      if(Y_Scroll) p1 <- echarts4r::e_datazoom(e = p1, y_Index = c(0,1))
      p1 <- echarts4r::e_theme(e = p1, name = EchartsTheme)
      p1 <- echarts4r::e_aria(e = p1, enabled = TRUE)
      p1 <- echarts4r::e_tooltip(e = p1)
      p1 <- echarts4r::e_toolbox_feature(e = p1, feature = c("saveAsImage","dataZoom"))
      p1 <- echarts4r::e_show_loading(e = p1, hide_overlay = TRUE, text = "Calculating...", color = "#000", text_color = TextColor, mask_color = "#000")
      p1 <- echarts4r::e_legend(e = p1, type = "scroll", orient = "vertical", right = 50, top = 40, height = "240px", textStyle = list(color = TextColor, fontWeight = "bold"))
      p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "x", name = XVar, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = xaxis.fontSize))
      p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "y", name = YVar, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = yaxis.fontSize))
      p1 <- echarts4r::e_brush(e = p1)
      p1 <- echarts4r::e_title(
        p1, Title,
        textStyle = list(
          color = TextColor,
          fontWeight = title.fontWeight,
          overflow = "truncate", # "none", "truncate", "break",
          ellipsis = '...',
          fontSize = title.fontSize,
          textShadowColor = title.textShadowColor,
          textShadowBlur = title.textShadowBlur,
          textShadowOffsetY = title.textShadowOffsetY,
          textShadowOffsetX = title.textShadowOffsetX))
      print("StackedBarPlot FINISHED BUILDING PLOT")
      return(p1)

    } else {

      if(Debug) print("BarPlot 2.b")
      if(!PreAgg) {
        if(tryCatch({class(dt[[eval(YVar)]])[1L]}, error = function(x) "bla") %in% c('numeric','integer')) {
          numvars <- unique(c(numvars, YVar))
        } else {
          byvars <- unique(c(byvars, YVar))
        }
        if(tryCatch({class(dt[[eval(XVar)]])[1L]}, error = function(x) "bla") %in% c('numeric','integer')) {
          if(length(numvars) > 0) {
            x <- length(unique(dt[[XVar]]))
            y <- length(unique(dt[[YVar]]))
            if(x > y) {
              byvars <- unique(c(byvars, YVar))
              numvars[1L] <- XVar
            } else {
              byvars <- unique(c(byvars, XVar))
            }
          } else {
            numvars <- unique(c(numvars, XVar))
          }
        } else {
          byvars <- unique(c(byvars, XVar))
        }
        if(!is.null(byvars)) {
          temp <- dt[, lapply(.SD, noquote(aggFunc)), .SDcols = c(numvars), by = c(byvars)]
          for(i in byvars) {
            if(class(temp[[i]]) %in% c('numeric','integer')) {
              temp[, eval(i) := as.character(get(i))]
            }
          }
        } else {
          temp <- dt[, lapply(.SD, noquote(aggFunc)), .SDcols = c(numvars)]
        }
      } else {
        temp <- data.table::copy(dt)
      }

      yvar <- temp[[YVar]]
      xvar <- temp[[XVar]]

      if(XVar == "Importance" && YVar == "Variable") {
        XVar <- "Variable"
        YVar <- "Importance"
      }
      p1 <- echarts4r::e_charts_(temp, x = XVar, dispose = TRUE, width = Width, height = Height)
      p1 <- echarts4r::e_bar_(e = p1, YVar, stack = XVar)
      if(X_Scroll) p1 <- echarts4r::e_datazoom(e = p1, x_index = c(0,1))
      if(Y_Scroll) p1 <- echarts4r::e_datazoom(e = p1, y_Index = c(0,1))
      p1 <- echarts4r::e_theme(e = p1, name = EchartsTheme)
      p1 <- echarts4r::e_aria(e = p1, enabled = TRUE)
      p1 <- echarts4r::e_tooltip(e = p1)
      p1 <- echarts4r::e_toolbox_feature(e = p1, feature = c("saveAsImage","dataZoom"))
      p1 <- echarts4r::e_show_loading(e = p1, hide_overlay = TRUE, text = "Calculating...", color = "#000", text_color = TextColor, mask_color = "#000")
      p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "x", name = XVar, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = xaxis.fontSize))
      p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "y", name = YVar, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = yaxis.fontSize))
      p1 <- echarts4r::e_brush(e = p1)
      p1 <- echarts4r::e_title(
        p1, Title,
        textStyle = list(
          color = TextColor,
          fontWeight = title.fontWeight,
          overflow = "truncate", # "none", "truncate", "break",
          ellipsis = '...',
          fontSize = title.fontSize,
          textShadowColor = title.textShadowColor,
          textShadowBlur = title.textShadowBlur,
          textShadowOffsetY = title.textShadowOffsetY,
          textShadowOffsetX = title.textShadowOffsetX))
      return(p1)

    }

  } else {
    print("XVar, YVar, and GroupVar need to have length > 0")
  }
}

#' @title Plot.BarPlot3D
#'
#' @description Build a 3D Bar Plot
#'
#' @family Standard Plots
#' @author Adrian Antico
#'
#' @param dt Source data.table
#' @param XVar = NULL,
#' @param YVar = NULL,
#' @param ZVar = NULL,
#' @param Height = NULL,
#' @param Width = NULL,
#' @param Engine "plotly", "echarts4r"
#' @param EchartsTheme "dark-blue"
#' @param AggMethod 'mean', 'median', 'sum', 'sd', 'coeffvar', 'count'
#' @param NumberBins = 21
#' @param NumLevels_Y = 20
#' @param NumLevels_X = 20
#' @param Title "Heatmap"
#' @param BackGroundColor = "#6a6969"
#' @param ChartColor = '#001534'
#' @param FillColor = "#0066ff"
#' @param FillColorReverse character
#' @param GridColor = '#ffffff'
#' @param Debug logical
#'
#' @examples
#' \dontrun{
#' data <- data.table::fread("./CatBoost_ML1_ScoringData.csv")
#' Columns <- c(
#'   "DATE_ISO",
#'   "ARTICLE",
#'   "BRAND",
#'   "CUSTOMER_COD_char",
#'   "CHILLED_Margin_PerDay",
#'   "CHILLED_Liters_PerDay",
#'   "CHILLED_Units_PerDay")
#' data <- AutoPlots::DM.pgQuery(
#'   Query = NULL,
#'   DataBase = "KompsProcessed",
#'   SELECT = Columns,
#'   AggStat = "AVG",
#'   FROM = "POS_Processed_Long_Daily_backward",
#'   GroupBy = NULL,
#'   SamplePercent = 1,
#'   Host = 'localhost',
#'   User = 'postgres',
#'   Port = 5432,
#'   Password = "Aa...")
#'
#' AutoPlots:::Plot.BarPlot3D(
#'   data,
#'   PreAgg = FALSE,
#'   Engine = 'echarts4r',
#'   XVar = 'ARTICLE',
#'   YVar = 'BRAND',
#'   ZVar = 'CHILLED_Margin_PerDay',
#'   AggMethod = 'mean',
#'   NumberBins = 21,
#'   NumLevels_Y = 33,
#'   NumLevels_X = 33)
#'
#' # QA
#' data
#' PreAgg = FALSE
#' Engine = 'echarts4r'
#' XVar = 'ARTICLE'
#' YVar = 'BRAND'
#' ZVar = 'CHILLED_Margin_PerDay'
#' AggMethod = 'mean'
#' NumberBins = 21
#' NumLevels_Y = 33
#' NumLevels_X = 33
#' gridcolor = GridColor
#' plot_bgcolor = ChartColor
#' paper_bgcolor = BackGroundColor
#'
#' }
#'
#' @export
Plot.BarPlot3D <- function(dt,
                           PreAgg = FALSE,
                           AggMethod = 'mean',
                           XVar = NULL,
                           YVar = NULL,
                           ZVar = NULL,
                           NumberBins = 21,
                           NumLevels_Y = 33,
                           NumLevels_X = 33,
                           Height = "600px",
                           Width = "1135px",
                           Title = "Heatmap",
                           Engine = "Plotly",
                           EchartsTheme = "dark",
                           X_Scroll = TRUE,
                           Y_Scroll = TRUE,
                           BackGroundColor =  "#6a6969",
                           ChartColor =       "#001534",
                           FillColor =        "#0066ff",
                           FillColorReverse = "#97ff00",
                           GridColor =        "white",
                           TextColor =        "white",
                           title.fontSize = 22,
                           title.fontWeight = "bold", # normal
                           title.textShadowColor = '#63aeff',
                           title.textShadowBlur = 3,
                           title.textShadowOffsetY = 1,
                           title.textShadowOffsetX = -1,
                           yaxis.fontSize = 14,
                           xaxis.fontSize = 14,
                           zaxis.fontSize = 14,
                           Debug     =        FALSE) {

  # Subset cols
  dt1 <- dt[, .SD, .SDcols = c(XVar,YVar,ZVar)]
  x_check <- class(dt1[[XVar]])[1L] %in% c('numeric','integer')
  y_check <- class(dt1[[YVar]])[1L] %in% c('numeric','integer')
  x_y_num <- x_check && y_check
  x_num <- x_check && !y_check
  x_char <- !x_check && y_check
  all_char <- !x_check && !y_check
  X.HoverFormat <- "%{xaxis.title.text}: %{x:,.2f}<br>"
  Y.HoverFormat <- "%{yaxis.title.text}: %{y:,.2f}<br>"
  Z.HoverFormat <- "%{zaxis.title.text}: %{y:,.2f}<br>"

  # XVar == numeric or integer && YVar == numeric or integer
  if(x_y_num) {

    # rank XVar and YVar
    if(!PreAgg) {
      dt1[, eval(XVar) := round(data.table::frank(dt1[[XVar]]) * NumberBins /.N) / NumberBins]
      dt1[, eval(YVar) := round(data.table::frank(dt1[[YVar]]) * NumberBins /.N) / NumberBins]
      data.table::setnames(dt1, eval(ZVar), 'Measure_Variable')
    }

    # Formatting
    vals <- unique(scales::rescale(c(dt1[['Measure_Variable']])))
    o <- order(vals, decreasing = FALSE)
    cols <- scales::col_numeric("Purples", domain = NULL)(vals)
    colz <- setNames(data.frame(vals[o], cols[o]), NULL)

    # Create final data for plot
    if(Engine == "Plotly") {

      Width <- as.integer(gsub("[^\\d]+", "", Width, perl=TRUE))
      Heigth <- as.integer(gsub("[^\\d]+", "", Height, perl=TRUE))

      p1 <- plotly::plot_ly(
        dt1,
        x = ~get(XVar),
        y = ~get(YVar),
        z = ~Measure_Variable,
        colorscale = colz,
        type = "heatmap",
        text = NULL,
        hovertemplate = paste(
          Y.HoverFormat,
          X.HoverFormat,
          Z.HoverFormat,
          "<extra></extra>"
        ),
        width = Width,
        height = Height)
      p1 <- plotly::layout(
        p = p1,
        title = AutoPlots:::bold_(Title),
        font = AutoPlots:::font_(),
        xaxis = list(title = ''),
        yaxis = list(title = ''),
        gridcolor = GridColor,
        plot_bgcolor = ChartColor,
        paper_bgcolor = BackGroundColor)

    } else if(Engine == "Echarts") {
      g <- "Measure_Variable"
      p1 <- echarts4r::e_charts_(data = dt1, x = XVar, dispose = TRUE, width = Width, height = Height)
      p1 <- echarts4r::e_heatmap_(e = p1, YVar, g, itemStyle = list(emphasis = list(shadowBlur = 10)))
      p1 <- echarts4r::e_visual_map_(e = p1, g, show = FALSE)
      p1 <- echarts4r::e_tooltip(e = p1)
      p1 <- echarts4r::e_theme(e = p1, name = EchartsTheme)
      if(X_Scroll) p1 <- echarts4r::e_datazoom(e = p1, x_index = c(0,1))
      if(Y_Scroll) p1 <- echarts4r::e_datazoom(e = p1, y_index = c(0,1))
      p1 <- echarts4r::e_aria(e = p1, enabled = TRUE)
      p1 <- echarts4r::e_toolbox_feature(e = p1, feature = c("saveAsImage","dataZoom"))
      p1 <- echarts4r::e_show_loading(e = p1, hide_overlay = TRUE, text = "Calculating...", color = "#000", text_color = TextColor, mask_color = "#000")
      p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "x", name = XVar, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = xaxis.fontSize))
      p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "y", name = YVar, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = yaxis.fontSize))
      p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "z", name = ZVar, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = yaxis.fontSize))
      p1 <- echarts4r::e_brush(e = p1)
      p1 <- echarts4r::e_title(
        p1, Title,
        textStyle = list(
          color = TextColor,
          fontWeight = title.fontWeight,
          overflow = "truncate", # "none", "truncate", "break",
          ellipsis = '...',
          fontSize = title.fontSize,
          textShadowColor = title.textShadowColor,
          textShadowBlur = title.textShadowBlur,
          textShadowOffsetY = title.textShadowOffsetY,
          textShadowOffsetX = title.textShadowOffsetX))

    }
    return(p1)
  }

  # XVar == character && YVar == numeric or integer
  if(x_char) {

    # rank YVar
    if(!PreAgg) {
      dt1[, eval(YVar) := round(data.table::frank(dt1[[YVar]]) * NumberBins /.N) / NumberBins]
      data.table::setnames(dt1, eval(ZVar), 'Measure_Variable')

      # Top YVar Levels
      temp <- dt1[, lapply(.SD, mean, na.rm = TRUE), .SDcols = c('Measure_Variable'), by = c(YVar)][order(-Measure_Variable)]
      temp <- temp[seq_len(min(NumLevels_X, temp[, .N]))][[1L]]
      dt1 <- dt1[get(YVar) %in% eval(temp)]

      # Formatting
      vals <- unique(scales::rescale(c(dt1[['Measure_Variable']])))
      o <- order(vals, decreasing = FALSE)
      cols <- scales::col_numeric("Purples", domain = NULL)(vals)
      colz <- setNames(data.frame(vals[o], cols[o]), NULL)
    }

    # Create final data for plot
    if(Engine == "Plotly") {

      Width <- as.integer(gsub("[^\\d]+", "", Width, perl=TRUE))
      Heigth <- as.integer(gsub("[^\\d]+", "", Height, perl=TRUE))

      p1 <- plotly::plot_ly(
        dt1,
        x = ~get(XVar),
        y = ~get(YVar),
        z = ~Measure_Variable,
        colorscale = colz,
        type = "heatmap",
        text = NULL,
        hovertemplate = paste(
          Y.HoverFormat,
          X.HoverFormat,
          Z.HoverFormat,
          "<extra></extra>"
        ),
        width = Width,
        height = Height)
      p1 <- plotly::layout(
        p = p1,
        title = AutoPlots:::bold_(Title),
        xaxis = list(title = ''),
        yaxis = list(title = ''),
        font = AutoPlots:::font_(),
        gridcolor = GridColor,
        plot_bgcolor = ChartColor,
        paper_bgcolor = BackGroundColor)

    } else if(Engine == "echarts4r") {
      g <- "Measure_Variable"
      p1 <- echarts4r::e_charts_(data = dt1, x = XVar, dispose = TRUE, width = Width, height = Height)
      p1 <- echarts4r::e_heatmap_(e = p1, YVar, g, itemStyle = list(emphasis = list(shadowBlur = 10)))
      p1 <- echarts4r::e_visual_map_(e = p1, g, show = FALSE)

      if(X_Scroll) p1 <- echarts4r::e_datazoom(e = p1, x_index = c(0,1))
      if(Y_Scroll) p1 <- echarts4r::e_datazoom(e = p1, y_index = c(0,1))
      p1 <- echarts4r::e_theme(e = p1, name = EchartsTheme)
      p1 <- echarts4r::e_aria(e = p1, enabled = TRUE)
      p1 <- echarts4r::e_show_loading(e = p1, hide_overlay = TRUE, text = "Calculating...", color = "#000", text_color = TextColor, mask_color = "#000")
      p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "x", name = XVar, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = xaxis.fontSize))
      p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "y", name = YVar, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = yaxis.fontSize))
      p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "z", name = ZVar, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = yaxis.fontSize))
      p1 <- echarts4r::e_brush(e = p1)
      p1 <- echarts4r::e_title(
        p1, Title,
        textStyle = list(
          color = TextColor,
          fontWeight = title.fontWeight,
          overflow = "truncate", # "none", "truncate", "break",
          ellipsis = '...',
          fontSize = title.fontSize,
          textShadowColor = title.textShadowColor,
          textShadowBlur = title.textShadowBlur,
          textShadowOffsetY = title.textShadowOffsetY,
          textShadowOffsetX = title.textShadowOffsetX))

    }
    return(p1)
  }

  # XVar == numeric or integer && YVar == character
  if(x_num) {

    # rank XVar
    if(!PreAgg) {
      dt1[, eval(XVar) := round(data.table::frank(dt1[[XVar]]) * NumberBins /.N) / NumberBins]
      data.table::setnames(dt1, eval(ZVar), 'Measure_Variable')

      # Top YVar Levels
      temp <- dt1[, lapply(.SD, mean, na.rm = TRUE), .SDcols = c('Measure_Variable'), by = c(YVar)][order(-Measure_Variable)]
      temp <- temp[seq_len(min(NumLevels_Y, temp[, .N]))][[1L]]

      # Formatting
      dt1 <- dt1[get(YVar) %in% eval(temp)]
      vals <- unique(scales::rescale(c(dt1[['Measure_Variable']])))
      o <- order(vals, decreasing = FALSE)
      cols <- scales::col_numeric("Purples", domain = NULL)(vals)
      colz <- setNames(data.frame(vals[o], cols[o]), NULL)
    }

    # Create final dt1 for plot
    if(Engine == "Plotly") {

      Width <- as.integer(gsub("[^\\d]+", "", Width, perl=TRUE))
      Heigth <- as.integer(gsub("[^\\d]+", "", Height, perl=TRUE))

      p1 <- plotly::plot_ly(
        dt1,
        x = ~get(XVar),
        y = ~get(YVar),
        z = ~Measure_Variable,
        colorscale = colz,
        type = "heatmap",
        text = NULL,
        hovertemplate = paste(
          Y.HoverFormat,
          X.HoverFormat,
          Z.HoverFormat,
          "<extra></extra>"
        ),
        width = Width,
        height = Height)
      p1 <- plotly::layout(
        p = p1,
        title = AutoPlots:::bold_(Title),
        xaxis = list(title = ''),
        yaxis = list(title = ''),
        font = AutoPlots:::font_(),
        gridcolor = GridColor,
        plot_bgcolor = ChartColor,
        paper_bgcolor = BackGroundColor)
    } else if(Engine == "echarts4r") {
      g <- "Measure_Variable"
      p1 <- echarts4r::e_charts_(data = dt1, x = XVar, dispose = TRUE, width = Width, height = Height)
      p1 <- echarts4r::e_heatmap_(e = p1, YVar, g, itemStyle = list(emphasis = list(shadowBlur = 10)))
      p1 <- echarts4r::e_visual_map_(e = p1, g, show = FALSE)
      p1 <- echarts4r::e_theme(e = p1, name = EchartsTheme)
      p1 <- echarts4r::e_datazoom(e = p1, x_index = c(0,1))
      p1 <- echarts4r::e_datazoom(e = p1, y_index = c(0,1))
      p1 <- echarts4r::e_aria(e = p1, enabled = TRUE)
      p1 <- echarts4r::e_show_loading(e = p1, hide_overlay = TRUE, text = "Calculating...", color = "#000", text_color = TextColor, mask_color = "#000")
      p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "x", name = XVar, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = xaxis.fontSize))
      p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "y", name = YVar, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = yaxis.fontSize))
      p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "z", name = ZVar, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = yaxis.fontSize))
      p1 <- echarts4r::e_brush(e = p1)
      p1 <- echarts4r::e_title(
        p1, Title,
        textStyle = list(
          color = TextColor,
          fontWeight = title.fontWeight,
          overflow = "truncate", # "none", "truncate", "break",
          ellipsis = '...',
          fontSize = title.fontSize,
          textShadowColor = title.textShadowColor,
          textShadowBlur = title.textShadowBlur,
          textShadowOffsetY = title.textShadowOffsetY,
          textShadowOffsetX = title.textShadowOffsetX))

    }
    return(p1)
  }

  # XVar == character or integer && YVar == character
  if(all_char) {

    # Starter pack
    if(!PreAgg) {
      if(AggMethod == 'mean') {
        temp_y <- dt1[, lapply(.SD, mean, na.rm = TRUE), .SDcols = c(ZVar), by = c(YVar)][order(-get(ZVar))]
        temp_x <- dt1[, lapply(.SD, mean, na.rm = TRUE), .SDcols = c(ZVar), by = c(XVar)][order(-get(ZVar))]
        temp_yy <- temp_y[seq_len(min(NumLevels_X, temp_y[, .N]))][[1L]]
        temp_xx <- temp_x[seq_len(min(NumLevels_Y, temp_x[, .N]))][[1L]]
        dt1 <- dt1[get(YVar) %in% eval(temp_yy) & get(XVar) %in% eval(temp_xx)]
        dt1 <- dt1[, lapply(.SD, mean, na.rm = TRUE), .SDcols = c(ZVar), by = c(XVar,YVar)]
      } else if(AggMethod == 'median') {
        temp_y <- dt1[, lapply(.SD, median, na.rm = TRUE), .SDcols = c(ZVar), by = c(YVar)][order(-get(ZVar))]
        temp_x <- dt1[, lapply(.SD, median, na.rm = TRUE), .SDcols = c(ZVar), by = c(XVar)][order(-get(ZVar))]
        temp_y <- temp_y[seq_len(min(NumLevels_X, temp_y[, .N]))][[1L]]
        temp_x <- temp_x[seq_len(min(NumLevels_Y, temp_x[, .N]))][[1L]]
        dt1 <- dt1[get(YVar) %in% eval(temp_y) & get(XVar) %in% eval(temp_x)]
        dt1 <- dt1[, lapply(.SD, median, na.rm = TRUE), .SDcols = c(ZVar), by = c(XVar,YVar)]
      } else if(AggMethod == 'sum') {
        temp_y <- dt1[, lapply(.SD, sum, na.rm = TRUE), .SDcols = c(ZVar), by = c(YVar)][order(-get(ZVar))]
        temp_x <- dt1[, lapply(.SD, sum, na.rm = TRUE), .SDcols = c(ZVar), by = c(XVar)][order(-get(ZVar))]
        temp_y <- temp_y[seq_len(min(NumLevels_X, temp_y[, .N]))][[1L]]
        temp_x <- temp_x[seq_len(min(NumLevels_Y, temp_x[, .N]))][[1L]]
        dt1 <- dt1[get(YVar) %in% eval(temp_y) & get(XVar) %in% eval(temp_x)]
        dt1 <- dt1[, lapply(.SD, sum, na.rm = TRUE), .SDcols = c(ZVar), by = c(XVar,YVar)]
      } else if(AggMethod == 'sd') {
        temp_y <- dt1[, lapply(.SD, sd, na.rm = TRUE), .SDcols = c(ZVar), by = c(YVar)][order(-get(ZVar))]
        temp_x <- dt1[, lapply(.SD, sd, na.rm = TRUE), .SDcols = c(ZVar), by = c(XVar)][order(-get(ZVar))]
        temp_y <- temp_y[seq_len(min(NumLevels_X, temp_y[, .N]))][[1L]]
        temp_x <- temp_x[seq_len(min(NumLevels_Y, temp_x[, .N]))][[1L]]
        dt1 <- dt1[get(YVar) %in% eval(temp_y) & get(XVar) %in% eval(temp_x)]
        dt1 <- dt1[, lapply(.SD, sd, na.rm = TRUE), .SDcols = c(ZVar), by = c(XVar,YVar)]
      } else if(AggMethod == 'coeffvar') {
        temp_y <- dt1[, lapply(.SD, .N, na.rm = TRUE), .SDcols = c(ZVar), by = c(YVar)][order(-get(ZVar))]
        temp_x <- dt1[, lapply(.SD, .N, na.rm = TRUE), .SDcols = c(ZVar), by = c(XVar)][order(-get(ZVar))]
        temp_y <- temp_y[seq_len(min(NumLevels_X, temp_y[, .N]))][[1L]]
        temp_x <- temp_x[seq_len(min(NumLevels_Y, temp_x[, .N]))][[1L]]
        dt1 <- dt1[get(YVar) %in% eval(temp_y) & get(XVar) %in% eval(temp_x)]
        dt1 <- dt1[, lapply(.SD, .N, na.rm = TRUE), .SDcols = c(ZVar), by = c(XVar,YVar)]
      } else if(AggMethod == 'count') {
        temp_y <- dt1[, lapply(.SD, .N, na.rm = TRUE), .SDcols = c(ZVar), by = c(YVar)][order(-get(ZVar))]
        temp_x <- dt1[, lapply(.SD, .N, na.rm = TRUE), .SDcols = c(ZVar), by = c(XVar)][order(-get(ZVar))]
        temp_y <- temp_y[seq_len(min(NumLevels_X, temp_y[, .N]))][[1L]]
        temp_x <- temp_x[seq_len(min(NumLevels_Y, temp_x[, .N]))][[1L]]
        dt1 <- dt1[get(YVar) %in% eval(temp_y) & get(XVar) %in% eval(temp_x)]
        dt1 <- dt1[, lapply(.SD, .N, na.rm = TRUE), .SDcols = c(ZVar), by = c(XVar,YVar)]
      }
    }

    if(XVar %in% c("Predict","p1")) data.table::setorderv(x = dt1, "Predict")
    p1 <- echarts4r::e_charts_(data = dt1, x = XVar, dispose = TRUE, width = Width, height = Height)
    p1 <- echarts4r::e_bar_3d_(e = p1, YVar, ZVar, coord_system = "cartesian3D", itemStyle = list(emphasis = list(shadowBlur = 10)))
    p1 <- echarts4r::e_visual_map_(e = p1, ZVar, show = FALSE)

    p1 <- echarts4r::e_theme(e = p1, name = EchartsTheme)
    p1 <- echarts4r::e_datazoom(e = p1, x_index = c(0,1))
    p1 <- echarts4r::e_datazoom(e = p1, y_index = c(0,1))
    p1 <- echarts4r::e_aria(e = p1, enabled = TRUE)
    p1 <- echarts4r::e_show_loading(e = p1, hide_overlay = TRUE, text = "Calculating...", color = "#000", text_color = TextColor, mask_color = "#000")
    p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "x", name = XVar, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = xaxis.fontSize))
    p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "y", name = YVar, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = yaxis.fontSize))
    p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "z", name = ZVar, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = yaxis.fontSize))
    p1 <- echarts4r::e_brush(e = p1)
    p1 <- echarts4r::e_title(
      p1, Title,
      textStyle = list(
        color = TextColor,
        fontWeight = title.fontWeight,
        overflow = "truncate", # "none", "truncate", "break",
        ellipsis = '...',
        fontSize = title.fontSize,
        textShadowColor = title.textShadowColor,
        textShadowBlur = title.textShadowBlur,
        textShadowOffsetY = title.textShadowOffsetY,
        textShadowOffsetX = title.textShadowOffsetX))
    return(p1)
  }
}

#' @title Plot.HeatMap
#'
#' @description Create heat maps with numeric or categorical dt
#'
#' @family Standard Plots
#' @author Adrian Antico
#'
#' @param dt Source data.table
#' @param XVar = NULL,
#' @param YVar = NULL,
#' @param ZVar = NULL,
#' @param Height = NULL,
#' @param Width = NULL,
#' @param Engine "plotly", "echarts4r"
#' @param EchartsTheme "dark-blue"
#' @param AggMethod 'mean', 'median', 'sum', 'sd', 'coeffvar', 'count'
#' @param NumberBins = 21
#' @param NumLevels_Y = 20
#' @param NumLevels_X = 20
#' @param Title "Heatmap"
#' @param BackGroundColor = "#6a6969"
#' @param ChartColor = '#001534'
#' @param FillColor = "#0066ff"
#' @param FillColorReverse character
#' @param GridColor = '#ffffff'
#'
#' @examples
#' \dontrun{
#' data <- data.table::fread("./CatBoost_ML1_ScoringData.csv")
#' Columns <- c(
#'   "DATE_ISO",
#'   "ARTICLE",
#'   "BRAND",
#'   "CUSTOMER_COD_char",
#'   "CHILLED_Margin_PerDay",
#'   "CHILLED_Liters_PerDay",
#'   "CHILLED_Units_PerDay")
#' data <- AutoPlots::DM.pgQuery(
#'   Query = NULL,
#'   DataBase = "KompsProcessed",
#'   SELECT = Columns,
#'   AggStat = "AVG",
#'   FROM = "POS_Processed_Long_Daily_backward",
#'   GroupBy = NULL,
#'   SamplePercent = 1,
#'   Host = 'localhost',
#'   User = 'postgres',
#'   Port = 5432,
#'   Password = "Aa...")
#'
#' AutoPlots:::Plot.HeatMap(
#'   data,
#'   PreAgg = FALSE,
#'   Engine = 'echarts4r',
#'   XVar = 'ARTICLE',
#'   YVar = 'BRAND',
#'   ZVar = 'CHILLED_Margin_PerDay',
#'   AggMethod = 'mean',
#'   NumberBins = 21,
#'   NumLevels_Y = 33,
#'   NumLevels_X = 33)
#'
#' # QA
#' data
#' PreAgg = FALSE
#' Engine = 'echarts4r'
#' XVar = 'ARTICLE'
#' YVar = 'BRAND'
#' ZVar = 'CHILLED_Margin_PerDay'
#' AggMethod = 'mean'
#' NumberBins = 21
#' NumLevels_Y = 33
#' NumLevels_X = 33
#' gridcolor = GridColor
#' plot_bgcolor = ChartColor
#' paper_bgcolor = BackGroundColor
#'
#' }
#'
#' @export
Plot.HeatMap <- function(dt,
                         PreAgg = FALSE,
                         AggMethod = 'mean',
                         XVar = NULL,
                         YVar = NULL,
                         ZVar = NULL,
                         NumberBins = 21,
                         NumLevels_Y = 33,
                         NumLevels_X = 33,
                         Height = "600px",
                         Width = "1135px",
                         Title = "Heatmap",
                         Engine = "Plotly",
                         EchartsTheme = "dark",
                         X_Scroll = TRUE,
                         Y_Scroll = TRUE,
                         BackGroundColor =  "#6a6969",
                         ChartColor =       "#001534",
                         FillColor =        "#0066ff",
                         FillColorReverse = "#97ff00",
                         GridColor =        "white",
                         TextColor =        "white",
                         title.fontSize = 22,
                         title.fontWeight = "bold", # normal
                         title.textShadowColor = '#63aeff',
                         title.textShadowBlur = 3,
                         title.textShadowOffsetY = 1,
                         title.textShadowOffsetX = -1,
                         yaxis.fontSize = 14,
                         xaxis.fontSize = 14,
                         Debug     =        FALSE) {

  # Subset cols
  dt1 <- dt[, .SD, .SDcols = c(XVar,YVar,ZVar)]
  x_check <- class(dt1[[XVar]])[1L] %in% c('numeric','integer')
  y_check <- class(dt1[[YVar]])[1L] %in% c('numeric','integer')
  x_y_num <- x_check && y_check
  x_num <- x_check && !y_check
  x_char <- !x_check && y_check
  all_char <- !x_check && !y_check
  X.HoverFormat <- "%{xaxis.title.text}: %{x:,.2f}<br>"
  Y.HoverFormat <- "%{yaxis.title.text}: %{y:,.2f}<br>"
  Z.HoverFormat <- "%{zaxis.title.text}: %{y:,.2f}<br>"

  # XVar == numeric or integer && YVar == numeric or integer
  if(x_y_num) {

    # rank XVar and YVar
    if(!PreAgg) {
      dt1[, eval(XVar) := round(data.table::frank(dt1[[XVar]]) * NumberBins /.N) / NumberBins]
      dt1[, eval(YVar) := round(data.table::frank(dt1[[YVar]]) * NumberBins /.N) / NumberBins]
    }

    # Formatting
    vals <- unique(scales::rescale(c(dt1[[ZVar]])))
    o <- order(vals, decreasing = FALSE)
    cols <- scales::col_numeric("Purples", domain = NULL)(vals)
    colz <- setNames(data.frame(vals[o], cols[o]), NULL)
    data.table::setnames(dt1, ZVar, "Measure")
    data.table::setorderv(x = dt1, cols = c(XVar,YVar),c(1L,1L))

    # Create final data for plot
    if(Engine == "Plotly") {

      Width <- as.integer(gsub("[^\\d]+", "", Width, perl=TRUE))
      Heigth <- as.integer(gsub("[^\\d]+", "", Height, perl=TRUE))

      p1 <- plotly::plot_ly(
        dt1,
        x = ~get(XVar),
        y = ~get(YVar),
        z = ~Measure,
        colorscale = colz,
        type = "heatmap",
        width = Width,
        height = Height)
      p1 <- plotly::layout(
        p = p1,
        title = AutoPlots:::bold_(Title),
        font = AutoPlots:::font_(),
        xaxis = list(title = XVar),
        yaxis = list(title = YVar),
        legend = list(title = list(text = "yo")),
        gridcolor = GridColor,
        plot_bgcolor = ChartColor,
        paper_bgcolor = BackGroundColor)

    } else if(Engine == "Echarts") {
      g <- "Measure"
      p1 <- echarts4r::e_charts_(data = dt1, x = XVar, width = Width, height = Height)#, dispose = TRUE)
      p1 <- echarts4r::e_heatmap_(e = p1, YVar, g, itemStyle = list(emphasis = list(shadowBlur = 10)))
      p1 <- echarts4r::e_visual_map_(e = p1, g, show = FALSE)
      p1 <- echarts4r::e_tooltip(e = p1)
      p1 <- echarts4r::e_theme(e = p1, name = EchartsTheme)
      #if(X_Scroll) p1 <- echarts4r::e_datazoom(e = p1, x_index = c(0,1))
      #if(Y_Scroll) p1 <- echarts4r::e_datazoom(e = p1, y_index = c(0,1))
      p1 <- echarts4r::e_aria(e = p1, enabled = TRUE)
      p1 <- echarts4r::e_toolbox_feature(e = p1, feature = c("saveAsImage","dataZoom"))
      p1 <- echarts4r::e_show_loading(e = p1, hide_overlay = TRUE, text = "Calculating...", color = "#000", text_color = TextColor, mask_color = "#000")
      p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "x", name = XVar, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = xaxis.fontSize))
      p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "y", name = YVar, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = yaxis.fontSize))
      p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "z", name = ZVar, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = yaxis.fontSize))
      p1 <- echarts4r::e_brush(e = p1)
      p1 <- echarts4r::e_title(
        p1, Title,
        textStyle = list(
          color = TextColor,
          fontWeight = title.fontWeight,
          overflow = "truncate", # "none", "truncate", "break",
          ellipsis = '...',
          fontSize = title.fontSize,
          textShadowColor = title.textShadowColor,
          textShadowBlur = title.textShadowBlur,
          textShadowOffsetY = title.textShadowOffsetY,
          textShadowOffsetX = title.textShadowOffsetX))

    }
    return(p1)
  }

  # XVar == character && YVar == numeric or integer
  if(x_char) {

    # rank YVar
    if(!PreAgg) {
      dt1[, eval(YVar) := round(data.table::frank(dt1[[YVar]]) * NumberBins /.N) / NumberBins]
      data.table::setnames(dt1, eval(ZVar), 'Measure_Variable')

      # Top YVar Levels
      temp <- dt1[, lapply(.SD, mean, na.rm = TRUE), .SDcols = c('Measure_Variable'), by = c(YVar)][order(-Measure_Variable)]
      temp <- temp[seq_len(min(NumLevels_X, temp[, .N]))][[1L]]
      dt1 <- dt1[get(YVar) %in% eval(temp)]

      # Formatting
      vals <- unique(scales::rescale(c(dt1[['Measure_Variable']])))
      o <- order(vals, decreasing = FALSE)
      cols <- scales::col_numeric("Purples", domain = NULL)(vals)
      colz <- setNames(data.frame(vals[o], cols[o]), NULL)
    }

    # Create final data for plot
    if(Engine == "Plotly") {

      Width <- as.integer(gsub("[^\\d]+", "", Width, perl=TRUE))
      Heigth <- as.integer(gsub("[^\\d]+", "", Height, perl=TRUE))

      p1 <- plotly::plot_ly(
        dt1,
        x = ~get(XVar),
        y = ~get(YVar),
        z = ~Measure_Variable,
        colorscale = colz,
        type = "heatmap",
        text = NULL,
        hovertemplate = paste(
          Y.HoverFormat,
          X.HoverFormat,
          Z.HoverFormat,
          "<extra></extra>"
        ),
        width = Width,
        height = Height)
      p1 <- plotly::layout(
        p = p1,
        title = AutoPlots:::bold_(Title),
        xaxis = list(title = ''),
        yaxis = list(title = ''),
        font = AutoPlots:::font_(),
        gridcolor = GridColor,
        plot_bgcolor = ChartColor,
        paper_bgcolor = BackGroundColor)

    } else if(Engine == "echarts4r") {
      g <- "Measure_Variable"
      p1 <- echarts4r::e_charts_(data = dt1, x = XVar, dispose = TRUE, width = Width, height = Height)
      p1 <- echarts4r::e_heatmap_(e = p1, YVar, g, itemStyle = list(emphasis = list(shadowBlur = 10)))
      p1 <- echarts4r::e_visual_map_(e = p1, g, show = FALSE)
      if(X_Scroll) p1 <- echarts4r::e_datazoom(e = p1, x_index = c(0,1))
      if(Y_Scroll) p1 <- echarts4r::e_datazoom(e = p1, y_index = c(0,1))
      p1 <- echarts4r::e_theme(e = p1, name = EchartsTheme)
      p1 <- echarts4r::e_aria(e = p1, enabled = TRUE)
      p1 <- echarts4r::e_show_loading(e = p1, hide_overlay = TRUE, text = "Calculating...", color = "#000", text_color = TextColor, mask_color = "#000")
      p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "x", name = XVar, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = xaxis.fontSize))
      p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "y", name = YVar, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = yaxis.fontSize))
      p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "z", name = ZVar, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = yaxis.fontSize))
      p1 <- echarts4r::e_brush(e = p1)
      p1 <- echarts4r::e_title(
        p1, Title,
        textStyle = list(
          color = TextColor,
          fontWeight = title.fontWeight,
          overflow = "truncate", # "none", "truncate", "break",
          ellipsis = '...',
          fontSize = title.fontSize,
          textShadowColor = title.textShadowColor,
          textShadowBlur = title.textShadowBlur,
          textShadowOffsetY = title.textShadowOffsetY,
          textShadowOffsetX = title.textShadowOffsetX))

    }
    return(p1)
  }

  # XVar == numeric or integer && YVar == character
  if(x_num) {

    # rank XVar
    if(!PreAgg) {
      dt1[, eval(XVar) := round(data.table::frank(dt1[[XVar]]) * NumberBins /.N) / NumberBins]
      data.table::setnames(dt1, eval(ZVar), 'Measure_Variable')

      # Top YVar Levels
      temp <- dt1[, lapply(.SD, mean, na.rm = TRUE), .SDcols = c('Measure_Variable'), by = c(YVar)][order(-Measure_Variable)]
      temp <- temp[seq_len(min(NumLevels_Y, temp[, .N]))][[1L]]

      # Formatting
      dt1 <- dt1[get(YVar) %in% eval(temp)]
      vals <- unique(scales::rescale(c(dt1[['Measure_Variable']])))
      o <- order(vals, decreasing = FALSE)
      cols <- scales::col_numeric("Purples", domain = NULL)(vals)
      colz <- setNames(data.frame(vals[o], cols[o]), NULL)
    }

    # Create final dt1 for plot
    if(Engine == "Plotly") {

      Width <- as.integer(gsub("[^\\d]+", "", Width, perl=TRUE))
      Heigth <- as.integer(gsub("[^\\d]+", "", Height, perl=TRUE))

      p1 <- plotly::plot_ly(
        dt1,
        x = ~get(XVar),
        y = ~get(YVar),
        z = ~Measure_Variable,
        colorscale = colz,
        type = "heatmap",
        text = NULL,
        hovertemplate = paste(
          Y.HoverFormat,
          X.HoverFormat,
          Z.HoverFormat,
          "<extra></extra>"
        ),
        width = Width,
        height = Height)
      p1 <- plotly::layout(
        p = p1,
        title = AutoPlots:::bold_(Title),
        xaxis = list(title = ''),
        yaxis = list(title = ''),
        font = AutoPlots:::font_(),
        gridcolor = GridColor,
        plot_bgcolor = ChartColor,
        paper_bgcolor = BackGroundColor)
    } else if(Engine == "echarts4r") {
      g <- "Measure_Variable"
      p1 <- echarts4r::e_charts_(data = dt1, x = XVar, dispose = TRUE, width = Width, height = Height)
      p1 <- echarts4r::e_heatmap_(e = p1, YVar, g, itemStyle = list(emphasis = list(shadowBlur = 10)))
      p1 <- echarts4r::e_visual_map_(e = p1, g, show = FALSE)
      p1 <- echarts4r::e_theme(e = p1, name = EchartsTheme)
      p1 <- echarts4r::e_datazoom(e = p1, x_index = c(0,1))
      p1 <- echarts4r::e_datazoom(e = p1, y_index = c(0,1))
      p1 <- echarts4r::e_aria(e = p1, enabled = TRUE)
      p1 <- echarts4r::e_show_loading(e = p1, hide_overlay = TRUE, text = "Calculating...", color = "#000", text_color = TextColor, mask_color = "#000")
      p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "x", name = XVar, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = xaxis.fontSize))
      p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "y", name = YVar, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = yaxis.fontSize))
      p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "z", name = ZVar, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = yaxis.fontSize))
      p1 <- echarts4r::e_brush(e = p1)
      p1 <- echarts4r::e_title(
        p1, Title,
        textStyle = list(
          color = TextColor,
          fontWeight = title.fontWeight,
          overflow = "truncate", # "none", "truncate", "break",
          ellipsis = '...',
          fontSize = title.fontSize,
          textShadowColor = title.textShadowColor,
          textShadowBlur = title.textShadowBlur,
          textShadowOffsetY = title.textShadowOffsetY,
          textShadowOffsetX = title.textShadowOffsetX))

    }
    return(p1)
  }

  # XVar == character or integer && YVar == character
  if(all_char) {

    # Starter pack
    if(!PreAgg) {
      print("Echarts PreAgg 1")
      if(AggMethod == 'mean') {
        print("Echarts PreAgg 2")
        temp_y <- dt1[, lapply(.SD, mean, na.rm = TRUE), .SDcols = c(ZVar), by = c(YVar)][order(-get(ZVar))]
        print("Echarts PreAgg 3")
        temp_x <- dt1[, lapply(.SD, mean, na.rm = TRUE), .SDcols = c(ZVar), by = c(XVar)][order(-get(ZVar))]
        print("Echarts PreAgg 4")
        temp_yy <- temp_y[seq_len(min(NumLevels_X, temp_y[, .N]))][[1L]]
        print("Echarts PreAgg 5")
        temp_xx <- temp_x[seq_len(min(NumLevels_Y, temp_x[, .N]))][[1L]]
        print("Echarts PreAgg 6")
        dt1 <- dt1[get(YVar) %in% eval(temp_yy) & get(XVar) %in% eval(temp_xx)]
        print("Echarts PreAgg 7")
        dt1 <- dt1[, lapply(.SD, mean, na.rm = TRUE), .SDcols = c(ZVar), by = c(XVar,YVar)]
        print("Echarts PreAgg 8")
      } else if(AggMethod == 'median') {
        temp_y <- dt1[, lapply(.SD, median, na.rm = TRUE), .SDcols = c(ZVar), by = c(YVar)][order(-get(ZVar))]
        temp_x <- dt1[, lapply(.SD, median, na.rm = TRUE), .SDcols = c(ZVar), by = c(XVar)][order(-get(ZVar))]
        temp_y <- temp_y[seq_len(min(NumLevels_X, temp_y[, .N]))][[1L]]
        temp_x <- temp_x[seq_len(min(NumLevels_Y, temp_x[, .N]))][[1L]]
        dt1 <- dt1[get(YVar) %in% eval(temp_y) & get(XVar) %in% eval(temp_x)]
        dt1 <- dt1[, lapply(.SD, median, na.rm = TRUE), .SDcols = c(ZVar), by = c(XVar,YVar)]
      } else if(AggMethod == 'sum') {
        temp_y <- dt1[, lapply(.SD, sum, na.rm = TRUE), .SDcols = c(ZVar), by = c(YVar)][order(-get(ZVar))]
        temp_x <- dt1[, lapply(.SD, sum, na.rm = TRUE), .SDcols = c(ZVar), by = c(XVar)][order(-get(ZVar))]
        temp_y <- temp_y[seq_len(min(NumLevels_X, temp_y[, .N]))][[1L]]
        temp_x <- temp_x[seq_len(min(NumLevels_Y, temp_x[, .N]))][[1L]]
        dt1 <- dt1[get(YVar) %in% eval(temp_y) & get(XVar) %in% eval(temp_x)]
        dt1 <- dt1[, lapply(.SD, sum, na.rm = TRUE), .SDcols = c(ZVar), by = c(XVar,YVar)]
      } else if(AggMethod == 'sd') {
        temp_y <- dt1[, lapply(.SD, sd, na.rm = TRUE), .SDcols = c(ZVar), by = c(YVar)][order(-get(ZVar))]
        temp_x <- dt1[, lapply(.SD, sd, na.rm = TRUE), .SDcols = c(ZVar), by = c(XVar)][order(-get(ZVar))]
        temp_y <- temp_y[seq_len(min(NumLevels_X, temp_y[, .N]))][[1L]]
        temp_x <- temp_x[seq_len(min(NumLevels_Y, temp_x[, .N]))][[1L]]
        dt1 <- dt1[get(YVar) %in% eval(temp_y) & get(XVar) %in% eval(temp_x)]
        dt1 <- dt1[, lapply(.SD, sd, na.rm = TRUE), .SDcols = c(ZVar), by = c(XVar,YVar)]
      } else if(AggMethod == 'coeffvar') {
        temp_y <- dt1[, lapply(.SD, .N, na.rm = TRUE), .SDcols = c(ZVar), by = c(YVar)][order(-get(ZVar))]
        temp_x <- dt1[, lapply(.SD, .N, na.rm = TRUE), .SDcols = c(ZVar), by = c(XVar)][order(-get(ZVar))]
        temp_y <- temp_y[seq_len(min(NumLevels_X, temp_y[, .N]))][[1L]]
        temp_x <- temp_x[seq_len(min(NumLevels_Y, temp_x[, .N]))][[1L]]
        dt1 <- dt1[get(YVar) %in% eval(temp_y) & get(XVar) %in% eval(temp_x)]
        dt1 <- dt1[, lapply(.SD, .N, na.rm = TRUE), .SDcols = c(ZVar), by = c(XVar,YVar)]
      } else if(AggMethod == 'count') {
        temp_y <- dt1[, lapply(.SD, .N, na.rm = TRUE), .SDcols = c(ZVar), by = c(YVar)][order(-get(ZVar))]
        temp_x <- dt1[, lapply(.SD, .N, na.rm = TRUE), .SDcols = c(ZVar), by = c(XVar)][order(-get(ZVar))]
        temp_y <- temp_y[seq_len(min(NumLevels_X, temp_y[, .N]))][[1L]]
        temp_x <- temp_x[seq_len(min(NumLevels_Y, temp_x[, .N]))][[1L]]
        dt1 <- dt1[get(YVar) %in% eval(temp_y) & get(XVar) %in% eval(temp_x)]
        dt1 <- dt1[, lapply(.SD, .N, na.rm = TRUE), .SDcols = c(ZVar), by = c(XVar,YVar)]
      }
    }

    # Create final dt1 for plot
    if(Engine == "Plotly") {

      Width <- as.integer(gsub("[^\\d]+", "", Width, perl=TRUE))
      Heigth <- as.integer(gsub("[^\\d]+", "", Height, perl=TRUE))

      data.table::setnames(dt1, eval(ZVar), 'Measure_Variable')
      p1 <- plotly::plot_ly(
        dt1,
        x = ~get(XVar),
        y = ~get(YVar),
        z = ~Measure_Variable,
        colors = grDevices::colorRamp(c(FillColorReverse,"black",FillColor)),
        type = "heatmap",
        width = Width,
        height = Height)
      p1 <- plotly::layout(
        p = p1,
        title = AutoPlots:::bold_(Title),
        xaxis = list(title = ''),
        yaxis = list(title = ''),
        font = AutoPlots:::font_(),
        gridcolor = GridColor,
        plot_bgcolor = ChartColor,
        paper_bgcolor = BackGroundColor)
    } else if(Engine == "Echarts") {
      print("Echarts 1")
      if(XVar %in% c("Predict","p1")) data.table::setorderv(x = dt1, "Predict")
      print("Echarts 2")
      p1 <- echarts4r::e_charts_(data = dt1, x = XVar, dispose = TRUE, width = Width, height = Height)
      print("Echarts 3")
      p1 <- echarts4r::e_heatmap_(e = p1, YVar, ZVar, itemStyle = list(emphasis = list(shadowBlur = 10)))
      print("Echarts 4")
      p1 <- echarts4r::e_visual_map_(e = p1, ZVar, show = FALSE)
      print("Echarts 5")
      p1 <- echarts4r::e_theme(e = p1, name = EchartsTheme)
      print("Echarts 6")
      p1 <- echarts4r::e_datazoom(e = p1, x_index = c(0,1))
      print("Echarts 7")
      p1 <- echarts4r::e_datazoom(e = p1, y_index = c(0,1))
      print("Echarts 8")
      p1 <- echarts4r::e_aria(e = p1, enabled = TRUE)
      print("Echarts 9")
      p1 <- echarts4r::e_show_loading(e = p1, hide_overlay = TRUE, text = "Calculating...", color = "#000", text_color = TextColor, mask_color = "#000")
      print("Echarts 10")
      p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "x", name = XVar, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = xaxis.fontSize))
      print("Echarts 11")
      p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "y", name = YVar, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = yaxis.fontSize))
      print("Echarts 12")
      p1 <- echarts4r::e_brush(e = p1)
      p1 <- echarts4r::e_title(
        p1, Title,
        textStyle = list(
          color = TextColor,
          fontWeight = title.fontWeight,
          overflow = "truncate", # "none", "truncate", "break",
          ellipsis = '...',
          fontSize = title.fontSize,
          textShadowColor = title.textShadowColor,
          textShadowBlur = title.textShadowBlur,
          textShadowOffsetY = title.textShadowOffsetY,
          textShadowOffsetX = title.textShadowOffsetX))
      print("Echarts return")

    }
    return(p1)
  }
}

# ----

# ----

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
# > Relationships Plot Functions                                              ----
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

# Correlation Matrix Updates: https://okanbulut.github.io/bigdata/visualizing-big-data.html

#' @title Plot.CorrMatrix
#'
#' @description Build a violin plot by simply passing arguments to a single function. It will sample your data using SampleSize number of rows. Sampled data is randomized.
#'
#' @family Standard Plots
#'
#' @author Adrian Antico
#'
#' @param dt Source data.table
#' @param CorrVars character
#' @param Method character
#' @param Height = NULL,
#' @param Width = NULL,
#' @param Title character
#' @param Engine character
#' @param EchartsTheme = "macaron"
#' @param X_Scroll logical
#' @param Y_Scroll logical
#' @param PreAgg logical
#' @param BackGroundColor character hex
#' @param ChartColor character hex
#' @param FillColor character hex
#' @param FillColorReverse character
#' @param GridColor character hex
#' @param TextColor character hex
#' @param Debug logical
#' @export
Plot.CorrMatrix <- function(dt = NULL,
                            CorrVars = NULL,
                            Method = 'spearman',
                            PreAgg = FALSE,
                            Height = "600px",
                            Width = "1135px",
                            Title = "Correlation Matrix",
                            Engine = "Plotly",
                            EchartsTheme = "macaron",
                            X_Scroll = TRUE,
                            Y_Scroll = TRUE,
                            BackGroundColor =  "#6a6969",
                            ChartColor =       "#001534",
                            FillColor =        "#0066ff",
                            FillColorReverse = "#97ff00",
                            GridColor =        "white",
                            TextColor =        "white",
                            title.fontSize = 22,
                            title.fontWeight = "bold", # normal
                            title.textShadowColor = '#63aeff',
                            title.textShadowBlur = 3,
                            title.textShadowOffsetY = 1,
                            title.textShadowOffsetX = -1,
                            yaxis.fontSize = 14,
                            xaxis.fontSize = 14,
                            Debug = FALSE) {

  # Plot
  if(!PreAgg) {
    dt1 <- na.omit(dt[, .SD, .SDcols = c(CorrVars)])
    for(i in seq_along(names(dt1))) {
      yy <- names(dt1)[i]
      zz <- nchar(yy)
      data.table::setnames(dt1, yy, substr(x = yy, start = max(0L, zz - 40L), stop = nchar(yy)))
    }
    corr_mat <- cor(method = tolower(Method), x = dt1)
  } else {
    corr_mat <- dt
  }

  if(Engine == "Plotly") {

    Width <- as.integer(gsub("[^\\d]+", "", Width, perl=TRUE))
    Heigth <- as.integer(gsub("[^\\d]+", "", Height, perl=TRUE))

    dt2 <- data.table::melt.data.table(
      data = data.table::as.data.table(corr_mat)[, Vars := rownames(corr_mat)],
      id.vars = "Vars", measure.vars = rownames(corr_mat), variable.name = "Variables", value.name = "Spearman")

    # Build
    p1 <- plotly::plot_ly(
      dt2,
      x = ~Vars,
      y = ~Variables,
      z = ~Spearman,
      colors = grDevices::colorRamp(c(FillColorReverse,"black",FillColor)),
      type = "heatmap",
      width = Width,
      height = Height)
    p1 <- plotly::layout(
      p = p1,
      title = AutoPlots:::bold_(Title),
      font = AutoPlots:::font_(),
      xaxis = list(title = ''),
      yaxis = list(title = ''),
      zaxis = list(title = "Spearman rank correlation: "),
      gridcolor = GridColor,
      plot_bgcolor = FillColor,
      paper_bgcolor = BackGroundColor)

  } else {
    if(Debug) print("Plot.CorrMatrix Echarts")
    p1 <- echarts4r::e_charts(data = corr_mat, width = Width, height = Height)
    p1 <- echarts4r::e_correlations(e = p1, order = "hclust")
    p1 <- echarts4r::e_tooltip(e = p1)
    if(X_Scroll) p1 <- echarts4r::e_datazoom(e = p1, x_index = c(0,1))
    if(Y_Scroll) p1 <- echarts4r::e_datazoom(e = p1, y_Index = c(0,1))
    p1 <- echarts4r::e_theme(e = p1, name = EchartsTheme)
    p1 <- echarts4r::e_aria(e = p1, enabled = TRUE)
    p1 <- echarts4r::e_toolbox_feature(e = p1, feature = c("saveAsImage","dataZoom"))
    p1 <- echarts4r::e_show_loading(e = p1, hide_overlay = TRUE, text = "Calculating...", color = "#000", text_color = TextColor, mask_color = "#000")
    p1 <- echarts4r::e_brush(e = p1)
    p1 <- echarts4r::e_title(
      p1, Title,
      textStyle = list(
        color = TextColor,
        fontWeight = title.fontWeight,
        overflow = "truncate", # "none", "truncate", "break",
        ellipsis = '...',
        fontSize = title.fontSize,
        textShadowColor = title.textShadowColor,
        textShadowBlur = title.textShadowBlur,
        textShadowOffsetY = title.textShadowOffsetY,
        textShadowOffsetX = title.textShadowOffsetX))

  }

  # Return plot
  return(p1)
}

#' @title Plot.Copula
#'
#' @description Build a copula plot by simply passing arguments to a single function. It will sample your data using SampleSize number of rows. Sampled data is randomized.
#'
#' @family Standard Plots
#'
#' @author Adrian Antico
#' @param dt Source data.table
#' @param SampleSize An integer for the number of rows to use. Sampled data is randomized. If NULL then ignored
#' @param XVar Column name of X-Axis variable. If NULL then ignored
#' @param YVar Column name of Y-Axis variable. If NULL then ignored
#' @param GroupVar Requires an XVar and YVar already be defined
#' @param Height = NULL,
#' @param Width = NULL,
#' @param Title 'Violin Plot'
#' @param Engine = "Plotly",
#' @param EchartsTheme = "dark-blue",
#' @param TimeLine Logical
#' @param X_Scroll logical
#' @param Y_Scroll logical
#' @param ZeroLineColor = '#ffff',
#' @param ZeroLineWidth = 2,
#' @param BackGroundColor 'gray95'
#' @param ChartColor 'lightsteelblue'
#' @param FillColor 'gray'
#' @param GridColor 'white'
#' @param FillColorReverse character
#' @param TextColor 'darkblue'
#' @param Debug FALSE
#'
#' @examples
#' \dontrun{
#' # Load packages
#' library(AutoQuant)
#' library(data.table)
#' library(echarts4r)
#'
#' # Load data
#' data <- data.table::fread("./CatBoost_ML1_ScoringData.csv")
#'
#' # Run function
#' AutoPlots:::Plot.Scatter(
#'   dt = data,
#'   SampleSize = 100000,
#'   XVar = 'CHILLED_Units_PerDay',
#'   YVar = 'CHILLED_Margin_PerDay',
#'   GroupVar = 'BRAND',
#'   TimeLine = FALSE,
#'   X_Scroll = TRUE,
#'   Y_Scroll = TRUE,
#'   BackGroundColor = 'gray95',
#'   ChartColor = 'lightsteelblue1',
#'   FillColor = "#0066ff",
#'   GridColor = 'white',
#'   TextColor = 'darkblue',
#'   Debug = FALSE)
#'
#' # Step through function
#' data# = data |> dplyr::group_by(Region)
#' Engine = "Echarts"
#' EchartsTheme = "macaron"
#' TimeLine = FALSE
#' XVar = 'CHILLED_Units_PerDay'
#' YVar = 'CHILLED_Margin_PerDay'
#' GroupVar = 'Store'
#' SampleSize = 100000
#' BackGroundColor = 'gray95'
#' ChartColor = 'lightsteelblue1'
#' FillColor = "#0066ff"
#' TextColor = 'darkblue'
#' GridColor = 'white'
#' Debug = FALSE
#' }
#'
#' @export
Plot.Copula <- function(dt = NULL,
                        SampleSize = 30000L,
                        XVar = NULL,
                        YVar = NULL,
                        GroupVar = NULL,
                        Height = "600px",
                        Width = "1135px",
                        Title = 'Copula Plot',
                        Engine = "Plotly",
                        EchartsTheme = "dark-blue",
                        TimeLine = FALSE,
                        X_Scroll = TRUE,
                        Y_Scroll = TRUE,
                        BackGroundColor =  "#6a6969",
                        ChartColor =       "#001534",
                        FillColor =        "#0066ff",
                        FillColorReverse = "#97ff00",
                        GridColor =        "white",
                        TextColor =        "white",
                        ZeroLineColor = '#ffff',
                        ZeroLineWidth = 1.25,
                        yaxis.fontSize = 14,
                        xaxis.fontSize = 14,
                        title.fontSize = 22,
                        title.fontWeight = "bold", # normal
                        title.textShadowColor = '#63aeff',
                        title.textShadowBlur = 3,
                        title.textShadowOffsetY = 1,
                        title.textShadowOffsetX = -1,
                        Debug = FALSE) {

  X.HoverFormat <- "%{xaxis.title.text}: %{x:,.2f}<br>"
  Y.HoverFormat <- "%{yaxis.title.text}: %{y:,.2f}<br>"

  # Cap number of records
  if(Debug) print('Plot.Copula # Cap number of records')
  if(dt[,.N] > SampleSize) {
    dt1 <- dt[order(runif(.N))][seq_len(SampleSize)]
  } else {
    dt1 <- data.table::copy(dt)
  }
  dt1[, eval(YVar) := data.table::frank(get(YVar)) * (1 / 0.001) / .N * 0.001]
  dt1[, eval(XVar) := data.table::frank(get(XVar)) * (1 / 0.001) / .N * 0.001]
  if(length(GroupVar) == 0L) {
    if(Debug) print('Plot.Copula length(GroupVar) == 0L')
    if(Engine == "Plotly") {

      Width <- as.integer(gsub("[^\\d]+", "", Width, perl=TRUE))
      Heigth <- as.integer(gsub("[^\\d]+", "", Height, perl=TRUE))

      # Build
      if(Debug) print('Plot.Copula plotly::plot_ly')
      p1 <- plotly::plot_ly(
        data = dt1,
        x = ~get(XVar),
        y = ~get(YVar),
        type = 'scatter',
        mode = 'markers',
        color = I(FillColor),
        text = NULL,
        hovertemplate = paste(
          Y.HoverFormat,
          X.HoverFormat,
          "<extra></extra>"
        ),
        width = Width,
        height = Height)

      # Add line
      if(Debug) print('Plot.Copula # Add Line')
      if(Debug) print('copulaplot 3')
      p1 <- plotly::add_trace(
        p = p1,
        data = dt1,
        x = ~get(XVar),
        y = ~get(YVar),
        type = "scatter",
        mode = "markers",
        color = I(FillColor))

      # Layout
      if(Debug) print('Plot.Copula plotly::layout')
      p1 <- plotly::layout(
        p = p1,
        title = AutoPlots:::bold_(Title),
        font = AutoPlots:::font_(),
        plot_bgcolor = ChartColor,
        paper_bgcolor = BackGroundColor,
        yaxis = list(
          title = AutoPlots:::bold_(YVar),
          gridColor = GridColor),
        xaxis = list(
          title = AutoPlots:::bold_(XVar),
          gridColor = GridColor))
      p1 <- plotly::hide_legend(p = p1)

    } else {
      if(Debug) print('Plot.Copula Echarts')
      dt1[, size_vals := seq_len(.N)/1000]
      sv <- "size_vals"
      p1 <- echarts4r::e_charts_(dt1, x = XVar, dispose = TRUE, width = Width, height = Height)
      p1 <- echarts4r::e_scatter_(e = p1, YVar, color = YVar)
      p1 <- echarts4r::e_glm(e = p1, smooth = TRUE, formula = get(YVar) ~ get(XVar))
      p1 <- echarts4r::e_visual_map_(e = p1, scale = echarts4r::e_scale, show = FALSE)
      if(X_Scroll) p1 <- echarts4r::e_datazoom(e = p1, x_index = c(0,1))
      if(Y_Scroll) p1 <- echarts4r::e_datazoom(e = p1, y_Index = c(0,1))
      p1 <- echarts4r::e_theme(e = p1, name = EchartsTheme)
      p1 <- echarts4r::e_aria(e = p1, enabled = TRUE)
      p1 <- echarts4r::e_tooltip(e = p1)
      p1 <- echarts4r::e_toolbox_feature(e = p1, feature = c("saveAsImage","dataZoom"))
      p1 <- echarts4r::e_show_loading(e = p1, hide_overlay = TRUE, text = "Calculating...", color = "#000", text_color = TextColor, mask_color = "#000")
      p1 <- echarts4r::e_legend(e = p1, type = "scroll", orient = "vertical", right = 50, top = 40, height = "240px", textStyle = list(color = TextColor, fontWeight = "bold"))
      p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "x", name = XVar, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = xaxis.fontSize))
      p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "y", name = YVar, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = yaxis.fontSize))
      p1 <- echarts4r::e_brush(e = p1)
      p1 <- echarts4r::e_title(
        p1, Title,
        textStyle = list(
          color = TextColor,
          fontWeight = title.fontWeight,
          overflow = "truncate", # "none", "truncate", "break",
          ellipsis = '...',
          fontSize = title.fontSize,
          textShadowColor = title.textShadowColor,
          textShadowBlur = title.textShadowBlur,
          textShadowOffsetY = title.textShadowOffsetY,
          textShadowOffsetX = title.textShadowOffsetX))

    }

  } else {

    if(Debug) print('Plot.Copula length(GroupVar) > 0L')
    if(Engine == "Plotly") {

      Width <- as.integer(gsub("[^\\d]+", "", Width, perl=TRUE))
      Heigth <- as.integer(gsub("[^\\d]+", "", Height, perl=TRUE))

      # Build
      if(Debug) print('Plot.Copula plotly::plot_ly')
      p1 <- plotly::plot_ly(
        data = dt1,
        x = ~get(XVar),
        y = ~get(YVar),
        color = ~get(GroupVar),
        mode = 'markers',
        text = ~get(GroupVar[1L]),
        hovertemplate = paste(
          "<b>%{text}</b><br><br>", # Group Var
          Y.HoverFormat,
          X.HoverFormat,
          "<extra></extra>"
        ),
        width = Width,
        height = Height)

      # Layout
      if(Debug) print('Plot.Copula plotly::layout')
      p1 <- plotly::layout(
        p = p1,
        title = AutoPlots:::bold_(Title),
        font = AutoPlots:::font_(),
        plot_bgcolor = ChartColor,
        paper_bgcolor = BackGroundColor,
        yaxis = list(
          title = AutoPlots:::bold_(YVar),
          gridColor = GridColor),
        xaxis = list(
          title = AutoPlots:::bold_(XVar),
          gridColor = GridColor))

    } else {
      if(Debug) print('Plot.Copula Echarts')
      if(TimeLine) {
        p1 <- echarts4r::e_charts_(dt1 |> dplyr::group_by(get(GroupVar[1L])), x = XVar, colorBy = GroupVar[1L], timeline = TRUE, dispose = TRUE, width = Width, height = Height)
      } else {
        p1 <- echarts4r::e_charts_(dt1 |> dplyr::group_by(get(GroupVar[1L])), x = XVar, dispose = TRUE, width = Width, height = Height)
      }
      p1 <- echarts4r::e_scatter_(e = p1, YVar)
      p1 <- echarts4r::e_glm(e = p1, smooth = TRUE, formula = get(YVar) ~ get(XVar))
      p1 <- echarts4r::e_visual_map_(e = p1, scale = echarts4r::e_scale, show = FALSE)
      if(X_Scroll) p1 <- echarts4r::e_datazoom(e = p1, x_index = c(0,1))
      if(Y_Scroll) p1 <- echarts4r::e_datazoom(e = p1, y_Index = c(0,1))
      p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "x", name = XVar)
      p1 <- echarts4r::e_theme(e = p1, name = EchartsTheme)
      p1 <- echarts4r::e_aria(e = p1, enabled = TRUE)
      p1 <- echarts4r::e_legend(e = p1, type = "scroll", orient = "vertical", right = 50, top = 40, height = "240px", textStyle = list(color = GridColor))
      p1 <- echarts4r::e_tooltip(e = p1)
      p1 <- echarts4r::e_toolbox_feature(e = p1, feature = c("saveAsImage","dataZoom"))
      p1 <- echarts4r::e_show_loading(e = p1, hide_overlay = TRUE, text = "Calculating...", color = "#000", text_color = TextColor, mask_color = "#000")
      p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "x", name = XVar, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = xaxis.fontSize))
      p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "y", name = YVar, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = yaxis.fontSize))
      p1 <- echarts4r::e_brush(e = p1)
      p1 <- echarts4r::e_title(
        p1, Title,
        textStyle = list(
          color = TextColor,
          fontWeight = title.fontWeight,
          overflow = "truncate", # "none", "truncate", "break",
          ellipsis = '...',
          fontSize = title.fontSize,
          textShadowColor = title.textShadowColor,
          textShadowBlur = title.textShadowBlur,
          textShadowOffsetY = title.textShadowOffsetY,
          textShadowOffsetX = title.textShadowOffsetX))

    }
  }

  # Return plot
  return(eval(p1))
}

#' @title Plot.Copula3D
#'
#' @description Build a 3D-copula plot by simply passing arguments to a single function. It will sample your data using SampleSize number of rows. Sampled data is randomized.
#'
#' @family Standard Plots
#'
#' @author Adrian Antico
#'
#' @param dt Source data.table
#' @param SampleSize An integer for the number of rows to use. Sampled data is randomized. If NULL then ignored
#' @param XVar Column name of X-Axis variable. If NULL then ignored
#' @param YVar Column name of Y-Axis variable. If NULL then ignored
#' @param ZVar Column name of Z-Axis variable. If NULL then ignored
#' @param GroupVar Requires an XVar and YVar already be defined
#' @param Height = NULL,
#' @param Width = NULL,
#' @param Title 'Violin Plot'
#' @param Engine = "Plotly"
#' @param EchartsTheme = "dark-blue"
#' @param TimeLine Logical
#' @param BackGroundColor 'gray95'
#' @param ChartColor 'lightsteelblue'
#' @param FillColor 'gray'
#' @param FillColorReverse character
#' @param TextColor 'darkblue'
#' @param GridColor 'white'
#' @param ZeroLineColor = '#ffff',
#' @param ZeroLineWidth = 2,
#' @param Debug FALSE
#'
#' @examples
#' \dontrun{
#' # Load packages
#' library(AutoQuant)
#' library(data.table)
#' library(magrittr)
#'
#' # Load data
#' data <- data.table::fread("./CatBoost_ML1_ScoringData.csv")
#'
#' # Run function
#' AutoPlots:::Plot.Copula3D(
#'   dt = data,
#'   SampleSize = 100000,
#'   XVar = 'CHILLED_Units_PerDay',
#'   YVar = 'CHILLED_Margin_PerDay',
#'   ZVar = 'CHILLED_Liters_PerDay',
#'   GroupVar = NULL,
#'   Title = 'Copula 3D',
#'   Engine = "Plotly",
#'   EchartsTheme = "macaron",
#'   TimeLine = FALSE,
#'   BackGroundColor = "#bbbec7", #"#1b1959", #'#00060b',
#'   ChartColor = '#001534', # #5757576e
#'   FillColor = "#0066ff",
#'   GridColor = '#ffff',
#'   TextColor = '#FFFFFF',
#'   ZeroLineColor = '#ffff',
#'   ZeroLineWidth = 1.25,
#'   Debug = FALSE)
#'
#' dt = data
#' XVar = 'XREG1'
#' YVar = 'XREG2'
#' ZVar = 'XREG3'
#' GroupVar = NULL
#' Title = 'Copula 3D'
#' SampleSize = 100000
#' FillColor = "#0066ff"
#' BackGroundColor = "#bbbec7"
#' ChartColor = '#001534'
#' GridColor = '#ffff'
#' TextColor = '#FFFFFF'
#' ZeroLineColor = '#ffff'
#' ZeroLineWidth = 1.25
#' Debug = FALSE
#' }
#'
#' @export
Plot.Copula3D <- function(dt = NULL,
                          SampleSize = 100000,
                          XVar = NULL,
                          YVar = NULL,
                          ZVar = NULL,
                          GroupVar = NULL,
                          Height = "600px",
                          Width = "1135px",
                          Title = 'Copula 3D',
                          Engine = "Plotly",
                          EchartsTheme = "dark-blue",
                          TimeLine = FALSE,
                          BackGroundColor =  "#6a6969",
                          ChartColor =       "#001534",
                          FillColor =        "#0066ff",
                          FillColorReverse = "#97ff00",
                          GridColor =        "white",
                          TextColor =        "white",
                          ZeroLineColor = '#ffff',
                          ZeroLineWidth = 1.25,
                          title.fontSize = 22,
                          title.fontWeight = "bold", # normal
                          title.textShadowColor = '#63aeff',
                          title.textShadowBlur = 3,
                          title.textShadowOffsetY = 1,
                          title.textShadowOffsetX = -1,
                          yaxis.fontSize = 14,
                          xaxis.fontSize = 14,
                          zaxis.fontSize = 14,
                          Debug = FALSE) {

  # Cap number of records
  if(Debug) print('Plot.Copula3D # Cap number of records')
  N <- dt[,.N]
  if(SampleSize > 50000L) SampleSize <- 50000L
  if(N > SampleSize) dt <- dt[order(runif(.N))][seq_len(SampleSize)]
  dt1 <- data.table::copy(dt)
  dt1[, eval(YVar) := data.table::frank(get(YVar)) * (1 / 0.001) / .N * 0.001]
  dt1[, eval(XVar) := data.table::frank(get(XVar)) * (1 / 0.001) / .N * 0.001]
  dt1[, eval(ZVar) := data.table::frank(get(ZVar)) * (1 / 0.001) / .N * 0.001]
  if(length(GroupVar) > 0L) {
    if(Debug) print('Plot.Copula3D length(GroupVar) > 0L')
    if(Engine == "Plotly") {

      Width <- as.integer(gsub("[^\\d]+", "", Width, perl=TRUE))
      Heigth <- as.integer(gsub("[^\\d]+", "", Height, perl=TRUE))

      # Build
      if(Debug) print('Plot.Copula3D Build')
      p1 <- plotly::plot_ly(
        dt1,
        x = ~get(XVar),
        y = ~get(YVar),
        z = ~get(ZVar),
        color = ~get(GroupVar[[1L]]),
        size = ~get(ZVar),
        marker = list(
          symbol = 'circle',
          sizemode = 'diameter'),
        sizes = c(1.5, 12),
        text = ~paste(
          paste0(GroupVar[[1L]], ":"),
          get(GroupVar[[1L]])),
        width = Width,
        height = Height)

      # Layout
      if(Debug) print('Plot.Copula3D Layout')
      p1 <- p1 %>% plotly::layout(
        title = Title,
        font = list(color = TextColor),
        scene = list(
          xaxis = list(
            title = AutoPlots:::bold_(XVar),
            gridcolor = GridColor,
            zerolinewidth = 1,
            ticklen = 5,
            gridwidth = 2),
          yaxis = list(
            title = AutoPlots:::bold_(YVar),
            gridcolor = GridColor,
            zerolinewidth = 1,
            ticklen = 5,
            gridwidth = 2),
          zaxis = list(
            title = AutoPlots:::bold_(ZVar),
            gridcolor = GridColor,
            zerolinewidth = 1,
            ticklen = 5,
            gridwidth = 2)),
        backgroundcolor = BackGroundColor,
        paper_bgcolor = "rgb(0, 20, 51, 0.61)")

    } else {

      if(Debug) print('Plot.Copula3D Echarts')
      p1 <- echarts4r::e_charts_(
        dt1 |> dplyr::group_by(get(GroupVar[1L])),
        x = XVar,
        timeline = TimeLine,
        colorBy = GroupVar[1L], dispose = TRUE, width = Width, height = Height)
      p1 <- echarts4r::e_scatter_3d_(e = p1, YVar, ZVar, ZVar, GroupVar[[1L]])
      p1 <- echarts4r::e_theme(e = p1, name = EchartsTheme)
      p1 <- echarts4r::e_aria(e = p1, enabled = TRUE)
      p1 <- echarts4r::e_legend(e = p1, type = "scroll", orient = "vertical", right = 50, top = 40, height = "240px", textStyle = list(color = GridColor))
      p1 <- echarts4r::e_tooltip(e = p1)
      p1 <- echarts4r::e_toolbox_feature(e = p1, feature = c("saveAsImage","dataZoom"))
      p1 <- echarts4r::e_show_loading(e = p1, hide_overlay = TRUE, text = "Calculating...", color = "#000", text_color = TextColor, mask_color = "#000")
      p1 <- echarts4r::e_legend(e = p1, type = "scroll", orient = "vertical", right = 50, top = 40, height = "240px", textStyle = list(color = TextColor, fontWeight = "bold"))
      p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "x", name = XVar, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = xaxis.fontSize))
      p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "y", name = YVar, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = yaxis.fontSize))
      p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "z", name = ZVar, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = xaxis.fontSize))
      p1 <- echarts4r::e_brush(e = p1)
      p1 <- echarts4r::e_title(
        p1, Title,
        textStyle = list(
          color = TextColor,
          fontWeight = title.fontWeight,
          overflow = "truncate", # "none", "truncate", "break",
          ellipsis = '...',
          fontSize = title.fontSize,
          textShadowColor = title.textShadowColor,
          textShadowBlur = title.textShadowBlur,
          textShadowOffsetY = title.textShadowOffsetY,
          textShadowOffsetX = title.textShadowOffsetX))

    }

  } else {

    if(Debug) print('Plot.Copula3D length(GroupVar) == 0L')
    if(Engine == "Plotly") {

      Width <- as.integer(gsub("[^\\d]+", "", Width, perl=TRUE))
      Heigth <- as.integer(gsub("[^\\d]+", "", Height, perl=TRUE))

      # Build
      if(Debug) print('Plot.Copula3D plotly::plot_ly')
      p1 <- plotly::plot_ly(
        dt1,
        x = ~get(XVar),
        y = ~get(YVar),
        z = ~get(ZVar),
        size = ~get(ZVar),
        marker = list(
          symbol = 'circle',
          sizemode = 'diameter'),
        sizes = c(1.5, 12),
        width = Width,
        height = Height)

      # Layout
      if(Debug) print('Plot.Copula3D plotly::layout')
      p1 <- p1 %>% plotly::layout(
        title = Title,
        font = list(color = TextColor),
        scene = list(
          xaxis = list(
            title = AutoPlots:::bold_(XVar),
            gridcolor = GridColor,
            zerolinewidth = 1,
            ticklen = 5,
            gridwidth = 2),
          yaxis = list(
            title = AutoPlots:::bold_(YVar),
            gridcolor = GridColor,
            zerolinewidth = 1,
            ticklen = 5,
            gridwidth = 2),
          zaxis = list(
            title = AutoPlots:::bold_(ZVar),
            gridcolor = GridColor,
            zerolinewidth = 1,
            ticklen = 5,
            gridwidth = 2)),
        backgroundcolor = BackGroundColor,
        paper_bgcolor = "rgb(0, 20, 51, 0.61)")
    } else {
      if(Debug) print('Plot.Copula3D Echarts')
      p1 <- echarts4r::e_charts_(dt1, x = XVar, dispose = TRUE, width = Width, height = Height)
      p1 <- echarts4r::e_scatter_3d_(e = p1, YVar, ZVar, ZVar)
      p1 <- echarts4r::e_theme(e = p1, name = EchartsTheme)
      p1 <- echarts4r::e_aria(e = p1, enabled = TRUE)
      p1 <- echarts4r::e_tooltip(e = p1)
      p1 <- echarts4r::e_toolbox_feature(e = p1, feature = c("saveAsImage","dataZoom"))
      p1 <- echarts4r::e_show_loading(e = p1, hide_overlay = TRUE, text = "Calculating...", color = "#000", text_color = TextColor, mask_color = "#000")
      p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "x", name = XVar, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = xaxis.fontSize))
      p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "y", name = YVar, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = yaxis.fontSize))
      p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "z", name = ZVar, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = xaxis.fontSize))
      p1 <- echarts4r::e_brush(e = p1)
      p1 <- echarts4r::e_title(
        p1, Title,
        textStyle = list(
          color = TextColor,
          fontWeight = title.fontWeight,
          overflow = "truncate", # "none", "truncate", "break",
          ellipsis = '...',
          fontSize = title.fontSize,
          textShadowColor = title.textShadowColor,
          textShadowBlur = title.textShadowBlur,
          textShadowOffsetY = title.textShadowOffsetY,
          textShadowOffsetX = title.textShadowOffsetX))

    }
  }

  # Return plot
  return(eval(p1))
}

#' @title Plot.Scatter
#'
#' @description Build a copula plot by simply passing arguments to a single function. It will sample your data using SampleSize number of rows. Sampled data is randomized.
#'
#' @family Standard Plots
#'
#' @author Adrian Antico
#'
#' @param dt Source data.table
#' @param SampleSize numeric
#' @param XVar character
#' @param YVar character
#' @param GroupVar character
#' @param Height = NULL,
#' @param Width = NULL,
#' @param Title character
#' @param Engine character
#' @param EchartsTheme character
#' @param TimeLine logical
#' @param X_Scroll logical
#' @param Y_Scroll logical
#' @param FillColor character hex
#' @param FillColorReverse character
#' @param BackGroundColor character hexb
#' @param ChartColor character hex
#' @param TextColor character hex
#' @param GridColor character hex
#' @param ZeroLineColor character hex
#' @param ZeroLineWidth numeric
#' @param Debug logical
#'
#' @examples
#' \dontrun{
#' # Load packages
#' library(AutoQuant)
#' library(data.table)
#' library(echarts4r)
#'
#' # Load data
#' data <- data.table::fread("./CatBoost_ML1_ScoringData.csv")
#'
#' # Run function
#' AutoPlots:::Plot.Scatter(
#'   dt = data,
#'   SampleSize = 100000,
#'   XVar = 'CHILLED_Units_PerDay',
#'   YVar = 'CHILLED_Margin_PerDay',
#'   GroupVar = 'BRAND',
#'   Engine = "Echarts",
#'   EchartsTheme = "macaron",
#'   TimeLine = FALSE,
#'   X_Scroll = TRUE,
#'   Y_Scroll = TRUE,
#'   FillColor = "#0066ff",
#'   BackGroundColor = 'gray95',
#'   ChartColor = 'lightsteelblue1',
#'   TextColor = 'darkblue',
#'   GridColor = 'white',
#'   Debug = FALSE)
#'
#' # Step through function
#' # dt = data |> dplyr::group_by(Region)
#' # SampleSize = 100000
#' # XVar = 'XREG1'
#' # YVar = 'XREG2'
#' # GroupVar = 'Store'
#' # Engine = "Echarts"
#' # EchartsTheme = "macaron"
#' # TimeLine = FALSE
#' # X_Scroll = TRUE
#' # Y_Scroll = TRUE
#' # Title = "Bla"
#' # FillColor = "#0066ff"
#' # BackGroundColor = 'gray95'
#' # ChartColor = 'lightsteelblue1'
#' # TextColor = 'darkblue'
#' # GridColor = 'white'
#' # Debug = FALSE
#' }
#'
#' @export
Plot.Scatter <- function(dt = NULL,
                         SampleSize = 30000L,
                         XVar = NULL,
                         YVar = NULL,
                         GroupVar = NULL,
                         Height = "600px",
                         Width = "1135px",
                         Title = 'Scatter Plot',
                         Engine = "Plotly",
                         EchartsTheme = "macaron",
                         TimeLine = FALSE,
                         X_Scroll = TRUE,
                         Y_Scroll = TRUE,
                         BackGroundColor =  "#6a6969",
                         ChartColor =       "#001534",
                         FillColor =        "#0066ff",
                         FillColorReverse = "#97ff00",
                         GridColor =        "white",
                         TextColor =        "white",
                         ZeroLineColor = '#ffff',
                         ZeroLineWidth = 1.25,
                         title.fontSize = 22,
                         title.fontWeight = "bold", # normal
                         title.textShadowColor = '#63aeff',
                         title.textShadowBlur = 3,
                         title.textShadowOffsetY = 1,
                         title.textShadowOffsetX = -1,
                         yaxis.fontSize = 14,
                         xaxis.fontSize = 14,
                         Debug = FALSE) {

  X.HoverFormat <- "%{xaxis.title.text}: %{x:,.2f}<br>"
  Y.HoverFormat <- "%{yaxis.title.text}: %{y:,.2f}<br>"

  # Cap number of records
  if(Debug) print('Plot.Scatter # Cap number of records')
  if(length(SampleSize) == 0L) SampleSize <- 30000L
  if(!data.table::is.data.table(dt)) data.table::setDT(dt)
  if(dt[,.N] > SampleSize) {
    dt1 <- dt[order(runif(.N))][seq_len(SampleSize)]
  } else {
    dt1 <- data.table::copy(dt)
  }
  if(length(GroupVar) == 0L) {
    if(Debug) print('Plot.Scatter  length(GroupVar) == 0L')
    if(Engine == "Plotly") {

      Width <- as.integer(gsub("[^\\d]+", "", Width, perl=TRUE))
      Heigth <- as.integer(gsub("[^\\d]+", "", Height, perl=TRUE))

      # Build
      if(Debug) print('Plot.Scatter  plotly::plot_ly')
      p1 <- plotly::plot_ly(
        data = dt1,
        x = ~get(XVar),
        y = ~get(YVar),
        type = 'scatter',
        mode = 'markers',
        color = I(FillColor),
        text = NULL,
        hovertemplate = paste(
          Y.HoverFormat,
          X.HoverFormat,
          "<extra></extra>"
        ),
        width = Width,
        height = Height)

      # Add line
      if(Debug) print('Plot.Scatter  # Add line')
      dt1 <- tryCatch({dt1[, lm := lm(formula = get(YVar) ~ get(XVar), data = dt1)$fitted]}, error = function(x) data)
      p1 <- plotly::add_trace(p = p1, data = dt1, x = ~get(XVar), y = ~get(YVar), type = "scatter", mode = "markers")
      if('lm' %in% names(data)) {
        p1 <- plotly::add_trace(p = p1, data = dt1, x = ~get(XVar), y = ~lm, type = "scatter", mode = "line")
      }

      # Layout
      if(Debug) print('Plot.Scatter  plotly::layout')
      p1 <- plotly::layout(
        p = p1,
        font = AutoPlots:::font_(),
        title = AutoPlots:::bold_(Title),
        plot_bgcolor = ChartColor,
        paper_bgcolor = BackGroundColor,
        yaxis = list(
          title = AutoPlots:::bold_(YVar),
          gridColor = GridColor),
        xaxis = list(
          title = AutoPlots:::bold_(XVar),
          gridColor = GridColor))
      p1 <- plotly::hide_legend(p = p1)

    } else {
      if(Debug) print('Plot.Scatter  Echarts')
      p1 <- echarts4r::e_charts_(dt1, x = XVar, dispose = TRUE, width = Width, height = Height)
      p1 <- echarts4r::e_scatter_(e = p1, YVar)
      p1 <- echarts4r::e_glm(e = p1, smooth = TRUE, formula = get(YVar) ~ get(XVar))
      p1 <- echarts4r::e_visual_map_(e = p1, scale = echarts4r::e_scale, show = FALSE)
      if(X_Scroll) p1 <- echarts4r::e_datazoom(e = p1, x_index = c(0,1))
      if(Y_Scroll) p1 <- echarts4r::e_datazoom(e = p1, y_Index = c(0,1))
      p1 <- echarts4r::e_theme(e = p1, name = EchartsTheme)
      p1 <- echarts4r::e_aria(e = p1, enabled = TRUE)
      p1 <- echarts4r::e_tooltip(e = p1)
      p1 <- echarts4r::e_toolbox_feature(e = p1, feature = c("saveAsImage","dataZoom"))
      p1 <- echarts4r::e_show_loading(e = p1, hide_overlay = TRUE, text = "Calculating...", color = "#000", text_color = TextColor, mask_color = "#000")
      p1 <- echarts4r::e_legend(e = p1, type = "scroll", orient = "vertical", right = 50, top = 40, height = "240px", textStyle = list(color = TextColor, fontWeight = "bold"))
      p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "x", name = XVar, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = xaxis.fontSize))
      p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "y", name = YVar, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = yaxis.fontSize))
      p1 <- echarts4r::e_brush(e = p1)
      p1 <- echarts4r::e_title(
        p1, Title,
        textStyle = list(
          color = TextColor,
          fontWeight = title.fontWeight,
          overflow = "truncate", # "none", "truncate", "break",
          ellipsis = '...',
          fontSize = title.fontSize,
          textShadowColor = title.textShadowColor,
          textShadowBlur = title.textShadowBlur,
          textShadowOffsetY = title.textShadowOffsetY,
          textShadowOffsetX = title.textShadowOffsetX))

    }

  } else {
    if(Debug) print('Plot.Scatter  length(GroupVar) > 0L')
    if(Engine == "Plotly") {

      Width <- as.integer(gsub("[^\\d]+", "", Width, perl=TRUE))
      Heigth <- as.integer(gsub("[^\\d]+", "", Height, perl=TRUE))

      # Build
      if(Debug) print('Plot.Scatter  length(GroupVar) > 0L')
      p1 <- plotly::plot_ly(
        data = dt1,
        x = ~get(XVar),
        y = ~get(YVar),
        color = ~get(GroupVar),
        mode = 'markers',
        width = Width,
        height = Height)

      # Layout
      if(Debug) print('Plot.Scatter  plotly::layout')
      p1 <- plotly::layout(
        p = p1,
        font = AutoPlots:::font_(),
        title = AutoPlots:::bold_(Title),
        plot_bgcolor = ChartColor,
        paper_bgcolor = BackGroundColor,
        yaxis = list(
          title = AutoPlots:::bold_(YVar),
          gridColor = GridColor),
        xaxis = list(
          title = AutoPlots:::bold_(XVar),
          gridColor = GridColor))

    } else {

      if(Debug) print('Plot.Scatter  Echarts')
      p1 <- echarts4r::e_charts_(
        dt1 |> dplyr::group_by(get(GroupVar[1L])),
        x = XVar,
        timeline = TimeLine,
        colorBy = GroupVar[1L], dispose = TRUE, width = Width, height = Height)
      p1 <- echarts4r::e_scatter_(e = p1, YVar)
      p1 <- echarts4r::e_visual_map_(e = p1, scale = echarts4r::e_scale, show = FALSE)
      if(X_Scroll) p1 <- echarts4r::e_datazoom(e = p1, x_index = c(0,1))
      if(Y_Scroll) p1 <- echarts4r::e_datazoom(e = p1, y_Index = c(0,1))
      p1 <- echarts4r::e_theme(e = p1, name = EchartsTheme)
      p1 <- echarts4r::e_aria(e = p1, enabled = TRUE)
      p1 <- echarts4r::e_tooltip(e = p1)
      p1 <- echarts4r::e_legend(e = p1, type = "scroll", orient = "vertical", right = 50, top = 40, height = "240px", textStyle = list(color = GridColor))
      p1 <- echarts4r::e_toolbox_feature(e = p1, feature = c("saveAsImage","dataZoom"))
      p1 <- echarts4r::e_show_loading(e = p1, hide_overlay = TRUE, text = "Calculating...", color = "#000", text_color = TextColor, mask_color = "#000")
      p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "x", name = XVar, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = xaxis.fontSize))
      p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "y", name = YVar, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = yaxis.fontSize))
      p1 <- echarts4r::e_brush(e = p1)
      p1 <- echarts4r::e_title(
        p1, Title,
        textStyle = list(
          color = TextColor,
          fontWeight = title.fontWeight,
          overflow = "truncate", # "none", "truncate", "break",
          ellipsis = '...',
          fontSize = title.fontSize,
          textShadowColor = title.textShadowColor,
          textShadowBlur = title.textShadowBlur,
          textShadowOffsetY = title.textShadowOffsetY,
          textShadowOffsetX = title.textShadowOffsetX))

    }
  }

  # Return plot
  return(eval(p1))
}

#' @title Plot.Scatter3D
#'
#' @description Build a 3D-copula plot by simply passing arguments to a single function. It will sample your data using SampleSize number of rows. Sampled data is randomized.
#'
#' @family Standard Plots
#'
#' @author Adrian Antico
#'
#' @param dt Source data.table
#' @param SampleSize An integer for the number of rows to use. Sampled data is randomized. If NULL then ignored
#' @param XVar Column name of X-Axis variable. If NULL then ignored
#' @param YVar Column name of Y-Axis variable. If NULL then ignored
#' @param ZVar Column name of Z-Axis variable. If NULL then ignored
#' @param GroupVar Requires an XVar and YVar already be defined
#' @param Height = NULL,
#' @param Width = NULL,
#' @param Title 'Violin Plot'
#' @param Engine = "Plotly"
#' @param EchartsTheme = "macaron"
#' @param TimeLine Logical
#' @param FillColor 'gray'
#' @param FillColorReverse character
#' @param BackGroundColor 'gray95'
#' @param ChartColor 'lightsteelblue'
#' @param GridColor 'white'
#' @param TextColor 'darkblue'
#' @param ZeroLineColor = '#ffff',
#' @param ZeroLineWidth = 2,
#' @param Debug FALSE
#'
#' @examples
#' \dontrun{
#' # Load packages
#' library(AutoQuant)
#' library(data.table)
#'
#' # Load data
#' data <- data.table::fread("./CatBoost_ML1_ScoringData.csv")
#'
#' # Run function
#' AutoPlots:::Plot.Scatter3D(
#'   dt = data,
#'   SampleSize = 100000,
#'   XVar = 'ARTICLE',
#'   YVar = 'BRAND',
#'   ZVar = 'CHILLED_Units_PerDay',
#'   GroupVar = 'Store',
#'   Title = "Something",
#'   Engine = "Plotly",
#'   EchartsTheme = "macaron",
#'   TimeLine = FALSE,
#'   FillColor = "#0066ff",
#'   ChartColor = 'lightsteelblue1',
#'   TextColor = 'darkblue',
#'   GridColor = 'white',
#'   BackGroundColor = 'gray95',
#'   Debug = FALSE)
#'
#' # Step through function
#' # dt = data
#' # SampleSize = 100000
#' # XVar = "Predict",
#' # YVar = "CHILLED_Margin_PerDay",
#' # GroupVar = "BRAND",
#' # Title = 'Copula 3D'
#' # Engine = "Plotly"
#' # EchartsTheme = "macaron"
#' # TimeLine = FALSE
#' # FillColor = "#0066ff"
#' # BackGroundColor = "#bbbec7"
#' # ChartColor = '#001534'
#' # GridColor = '#ffff'
#' # TextColor = '#FFFFFF'
#' # ZeroLineColor = '#ffff'
#' # ZeroLineWidth = 1.25
#' # Debug = FALSE
#' }
#'
#' @export
Plot.Scatter3D <- function(dt = NULL,
                           SampleSize = 100000,
                           XVar = NULL,
                           YVar = NULL,
                           ZVar = NULL,
                           GroupVar = NULL,
                           Height = "600px",
                           Width = "1135px",
                           Title = '3D Scatter',
                           Engine = "Plotly",
                           EchartsTheme = "macaron",
                           TimeLine = FALSE,
                           BackGroundColor =  "#6a6969",
                           ChartColor =       "#001534",
                           FillColor =        "#0066ff",
                           FillColorReverse = "#97ff00",
                           GridColor =        "white",
                           TextColor =        "white",
                           ZeroLineColor = '#ffff',
                           ZeroLineWidth = 1.25,
                           title.fontSize = 22,
                           title.fontWeight = "bold", # normal
                           title.textShadowColor = '#63aeff',
                           title.textShadowBlur = 3,
                           title.textShadowOffsetY = 1,
                           title.textShadowOffsetX = -1,
                           yaxis.fontSize = 14,
                           xaxis.fontSize = 14,
                           zaxis.fontSize = 14,
                           Debug = FALSE) {

  # Cap number of records
  if(Debug) print('Plot.Scatter3D # Cap number of records')
  if(length(SampleSize) == 0L) SampleSize <- 30000L
  if(dt[,.N] > SampleSize) {
    dt1 <- dt[order(runif(.N))][seq_len(SampleSize)]
  } else {
    dt1 <- data.table::copy(dt)
  }
  if(length(GroupVar) > 0L) {
    if(Debug) print('Plot.Scatter3D length(GroupVar) > 0L')
    if(Engine == "Plotly") {

      Width <- as.integer(gsub("[^\\d]+", "", Width, perl=TRUE))
      Heigth <- as.integer(gsub("[^\\d]+", "", Height, perl=TRUE))

      # Build
      if(Debug) print('Plot.Scatter3D  plotly::plot_ly')
      p1 <- plotly::plot_ly(
        dt1,
        x = ~get(XVar),
        y = ~get(YVar),
        z = ~get(ZVar),
        color = ~get(GroupVar),
        size = ~get(ZVar),
        marker = list(
          symbol = 'circle',
          sizemode = 'diameter'),
        sizes = c(5, 25),
        text = ~paste(
          paste0(GroupVar, ":"),
          get(GroupVar)),
        width = Width,
        height = Height)

      # Layout
      if(Debug) print('Plot.Scatter3D  plotly::layout')
      p1 <- plotly::layout(
        p = p1,
        title = Title,
        font = list(color = '#FFFFFF'),
        scene = list(
          xaxis = list(
            title = AutoPlots:::bold_(XVar),
            gridcolor = GridColor,
            zerolinewidth = 1,
            ticklen = 5,
            gridwidth = 2),
          yaxis = list(
            title = AutoPlots:::bold_(YVar),
            gridcolor = GridColor,
            zerolinewidth = 1,
            ticklen = 5,
            gridwidth = 2),
          zaxis = list(
            title = AutoPlots:::bold_(ZVar),
            gridcolor = GridColor,
            zerolinewidth = 1,
            ticklen = 5,
            gridwidth = 2)),
        backgroundcolor = BackGroundColor,
        paper_bgcolor = "rgb(0, 20, 51, 0.61)")
    } else {
      if(Debug) print('Plot.Scatter3D  Echarts')
      p1 <- echarts4r::e_charts_(dt1 |> dplyr::group_by(get(GroupVar[1L])), x = XVar, timeline = TimeLine, colorBy = GroupVar[1L], dispose = TRUE, width = Width, height = Height)
      p1 <- echarts4r::e_scatter_3d_(e = p1, YVar, ZVar, ZVar, GroupVar[1L])
      p1 <- echarts4r::e_theme(e = p1, name = EchartsTheme)
      p1 <- echarts4r::e_aria(e = p1, enabled = TRUE)
      p1 <- echarts4r::e_tooltip(e = p1)
      p1 <- echarts4r::e_toolbox_feature(e = p1, feature = c("saveAsImage","dataZoom"))
      p1 <- echarts4r::e_show_loading(e = p1, hide_overlay = TRUE, text = "Calculating...", color = "#000", text_color = TextColor, mask_color = "#000")
      p1 <- echarts4r::e_legend(e = p1, type = "scroll", orient = "vertical", right = 50, top = 40, height = "240px", textStyle = list(color = TextColor, fontWeight = "bold"))
      p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "x", name = XVar, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = xaxis.fontSize))
      p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "y", name = YVar, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = yaxis.fontSize))
      p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "z", name = ZVar, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = xaxis.fontSize))
      p1 <- echarts4r::e_brush(e = p1)
      p1 <- echarts4r::e_title(
        p1, Title,
        textStyle = list(
          color = TextColor,
          fontWeight = title.fontWeight,
          overflow = "truncate", # "none", "truncate", "break",
          ellipsis = '...',
          fontSize = title.fontSize,
          textShadowColor = title.textShadowColor,
          textShadowBlur = title.textShadowBlur,
          textShadowOffsetY = title.textShadowOffsetY,
          textShadowOffsetX = title.textShadowOffsetX))

    }

  } else {

    if(Debug) print('Plot.Scatter3D length(GroupVar) == 0L')
    if(Engine == "Plotly") {

      Width <- as.integer(gsub("[^\\d]+", "", Width, perl=TRUE))
      Heigth <- as.integer(gsub("[^\\d]+", "", Height, perl=TRUE))

      # Build
      if(Debug) print('Plot.Scatter3D  plotly::plot_ly')
      p1 <- plotly::plot_ly(
        dt1,
        x = ~get(XVar),
        y = ~get(YVar),
        z = ~get(ZVar),
        size = ~get(ZVar),
        marker = list(
          symbol = 'circle',
          sizemode = 'diameter'),
        sizes = c(5, 25),
        width = Width,
        height = Height)

      # Layout
      if(Debug) print('Plot.Scatter3D  plotly::layout')
      p1 <- plotly::layout(
        p = p1,
        title = Title,
        font = list(color = '#FFFFFF'),
        scene = list(
          xaxis = list(
            title = AutoPlots:::bold_(XVar),
            gridcolor = GridColor,
            zerolinewidth = 1,
            ticklen = 5,
            gridwidth = 2),
          yaxis = list(
            title = AutoPlots:::bold_(YVar),
            gridcolor = GridColor,
            zerolinewidth = 1,
            ticklen = 5,
            gridwidth = 2),
          zaxis = list(
            title = AutoPlots:::bold_(ZVar),
            gridcolor = GridColor,
            zerolinewidth = 1,
            ticklen = 5,
            gridwidth = 2)),
        backgroundcolor = BackGroundColor,
        paper_bgcolor = "rgb(0, 20, 51, 0.61)")
    } else {
      if(Debug) print('Plot.Scatter3D  Echarts')
      p1 <- echarts4r::e_charts_(dt1 |> dplyr::group_by(GroupVar[[1L]]), x = XVar, timeline = TRUE, dispose = TRUE, width = Width, height = Height)
      p1 <- echarts4r::e_scatter_3d_(e = p1, YVar, ZVar, ZVar, GroupVar[[1L]])
      p1 <- echarts4r::e_theme(e = p1, name = EchartsTheme)
      p1 <- echarts4r::e_aria(e = p1, enabled = TRUE)
      p1 <- echarts4r::e_tooltip(e = p1)
      p1 <- echarts4r::e_toolbox_feature(e = p1, feature = c("saveAsImage","dataZoom"))
      p1 <- echarts4r::e_show_loading(e = p1, hide_overlay = TRUE, text = "Calculating...", color = "#000", text_color = TextColor, mask_color = "#000")
      p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "x", name = XVar, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = xaxis.fontSize))
      p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "y", name = YVar, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = yaxis.fontSize))
      p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "z", name = ZVar, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = xaxis.fontSize))
      p1 <- echarts4r::e_brush(e = p1)
      p1 <- echarts4r::e_title(
        p1, Title,
        textStyle = list(
          color = TextColor,
          fontWeight = title.fontWeight,
          overflow = "truncate", # "none", "truncate", "break",
          ellipsis = '...',
          fontSize = title.fontSize,
          textShadowColor = title.textShadowColor,
          textShadowBlur = title.textShadowBlur,
          textShadowOffsetY = title.textShadowOffsetY,
          textShadowOffsetX = title.textShadowOffsetX))

    }
  }

  # Return plot
  return(p1)
}

# ----

# ----

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
# > Stocks Plots Functions                                                    ----
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

#' @noRd
holidayNYSE <- function(year = getRmetricsOptions("currentYear")) {
  # A function implemented by Diethelm Wuertz
  # improved speed and handling of time zone by Yohan Chalabi

  # Description:
  #   Returns 'timeDate' object for full-day NYSE holidays

  # Arguments:
  #   year - an integer variable or vector for the year(s)
  #       ISO-8601 formatted as "CCYY" where easter or
  #       easter related feasts should be computed.

  # Value:
  #   Returns the holiday calendar for the NYSE formatted as
  #   'timeDate' object.

  # Details:
  #   The "New York Stock Exchange" calendar starts from year 1885.
  #   The rules are listed at the web site http://www.nyse.com.

  # Example:
  #   > holiday.NYSE(2004)
  #   [1] "America/New_York"
  #   [1] [2004-01-01] [2004-01-19] [2004-02-16] [2004-04-09]
  #   [5] [2004-05-31] [2004-07-05] [2004-09-06] [2004-11-25]

  # FUNCTION:
  library(timeDate)
  #  Settings:
  holidays <- NULL

  # Iterate years:
  for (y in year ) {
    if (y >= 1885)
      holidays <- c(holidays, as.character(USNewYearsDay(y)))
    if (y >= 1885)
      holidays <- c(holidays, as.character(USIndependenceDay(y)))
    if (y >= 1885)
      holidays <- c(holidays, as.character(USThanksgivingDay(y)))
    if (y >= 1885)
      holidays <- c(holidays, as.character(USChristmasDay(y)))
    if (y >= 1887)
      holidays <- c(holidays, as.character(USLaborDay(y)))
    if (y != 1898 & y != 1906 & y != 1907)
      holidays <- c(holidays, as.character(USGoodFriday(y)))
    if (y >= 1909 & y <= 1953)
      holidays <- c(holidays, as.character(USColumbusDay(y)))
    if (y >= 1998)
      holidays <- c(holidays, as.character(USMLKingsBirthday(y)))
    if (y >= 1896 & y <= 1953)
      holidays <- c(holidays, as.character(USLincolnsBirthday(y)))
    if (y <= 1970)
      holidays <- c(holidays, as.character(USWashingtonsBirthday(y)))
    if (y > 1970)
      holidays <- c(holidays, as.character(USPresidentsDay(y)))
    if (y == 1918 | y == 1921 | (y >= 1934 & y <= 1953))
      holidays <- c(holidays, as.character(USVeteransDay(y)))
    if (y <= 1968 | y == 1972 | y == 1976 | y == 1980)
      holidays <- c(holidays, as.character(USElectionDay(y)))
    if (y <= 1970)
      holidays <- c(holidays, as.character(USDecorationMemorialDay(y)))
    if (y >= 1971)
      holidays <- c(holidays, as.character(USMemorialDay(y)))
  }

  # Sort and Convert to 'timeDate':
  holidays <- sort(holidays)
  ans <- timeDate(format(holidays), zone = "NewYork", FinCenter = "NewYork")

  # Move Sunday Holidays to Monday:
  posix1 <- as.POSIXlt(ans, tz = "GMT")
  ans <- ans + as.integer(posix1$wday==0) * 24 * 3600

  # After July 3, 1959, move Saturday holidays to Friday
  # ... except if at the end of monthly/yearly accounting period
  # this is the last business day of a month.
  posix2 <- as.POSIXlt(as.POSIXct(ans, tz = "GMT") - 24 * 3600)
  y <- posix2$year + 1900
  m <- posix2$mon + 1
  calendar <- timeCalendar(y = y+(m+1)%/%13,
                           m = m+1-(m+1)%/%13*12, d = 1,
                           zone = "GMT", FinCenter = "GMT")
  lastday <- as.POSIXlt(calendar - 24*3600, tz = "GMT")$mday
  lon <- .last.of.nday(year = y, month = m, lastday = lastday, nday = 5)
  ExceptOnLastFriday <- timeDate(format(lon), zone = "NewYork",
                                 FinCenter = "NewYork")
  ans <- ans - as.integer(ans >= timeDate("1959-07-03",
                                          zone ="GMT", FinCenter = "GMT") &
                            as.POSIXlt(ans, tz = "GMT")$wday == 6  &
                            (ans - 24*3600) != ExceptOnLastFriday ) * 24 * 3600

  # Remove Remaining Weekend Dates:
  posix3 <- as.POSIXlt(ans, tz = "GMT")
  ans <- ans[ !(posix3$wday == 0 | posix3$wday == 6)]

  # Return Value:
  ans
}

#' @noRd
StockSymbols <- function() {
  x <- jsonlite::fromJSON("https://api.polygon.io/v3/reference/tickers?active=true&sort=ticker&order=asc&limit=1000&apiKey=hvyL7ZOsKK_5PNplOmv55tBTRd8rdA20")
  xx <- data.table::setDT(x$results)
  return(xx[, .SD, .SDcols = c(names(xx)[c(1,2,5,6,12)])])
}

#' @noRd
GetAllTickers <- function() {
  x <- jsonlite::fromJSON("https://api.polygon.io/v3/reference/tickers?active=true&sort=ticker&order=asc&limit=1000&apiKey=hvyL7ZOsKK_5PNplOmv55tBTRd8rdA20")
  xx <- data.table::setDT(x$results)
  counter <- 1000L
  while(is.list(x)) {
    print(paste0('Working on first ', counter, ' ticker symbols'))
    x <- tryCatch({jsonlite::fromJSON(paste0(x$next_url, "&apiKey=hvyL7ZOsKK_5PNplOmv55tBTRd8rdA20"))}, error = function(x) 1)
    xx <- data.table::rbindlist(list(xx, data.table::setDT(x$results)), fill = TRUE, use.names = TRUE)
    counter <- counter + 1000L
    Sys.sleep(12L)
  }
  xx <- xx[, .SD, .SDcols = c(names(xx)[c(1,2,5,6,12)])]
  data.table::fwrite(xx, file = file.path('C:/Users/Bizon/Documents/GitHub/AutoPlots/inst/shiny-apps/AutoInsights/ticker_data.csv'))
  AutoQuant::PostGRE_RemoveCreateAppend(
    data = xx,
    TableName = "ticker_data",
    CloseConnection = TRUE,
    CreateSchema = NULL,
    Host = "localhost",
    DBName = "AutoQuant",
    User = "postgres",
    Port = 5432,
    Password = "Aa",
    Temporary = FALSE,
    Connection = NULL,
    Append = TRUE)
  return(xx)
}

#' @noRd
OptionsSymbols <- function() {
  x <- jsonlite::fromJSON('https://api.polygon.io/v3/reference/tickers/types?asset_class=options&locale=us&apiKey=hvyL7ZOsKK_5PNplOmv55tBTRd8rdA20')
  xx <- data.table::setDT(x$results)
  return(xx[, .SD, .SDcols = c(names(xx)[c(1,2,5,6,12)])])
}

#' @noRd
CryptoSymbols <- function() {
  x <- jsonlite::fromJSON('https://api.polygon.io/v3/reference/tickers/types?asset_class=crypto&locale=us&apiKey=hvyL7ZOsKK_5PNplOmv55tBTRd8rdA20')
  xx <- data.table::setDT(x$results)
  return(xx[, .SD, .SDcols = c(names(xx)[c(1,2,5,6,12)])])
}

#' @noRd
Financials <- function() {
  x <- jsonlite::fromJSON("https://api.polygon.io/vX/reference/financials?apiKey=hvyL7ZOsKK_5PNplOmv55tBTRd8rdA20")
}

#' @title StockData
#'
#' @description  Create stock data for plotting using Plot.Stock()
#'
#' @family Standard Plots
#' @author Adrian Antico
#'
#' @param PolyOut NULL. If NULL, data is pulled. If supplied, data is not pulled.
#' @param Type 'candlestick', 'ohlc'
#' @param Metric Stock Price, Percent Returns (use symbol for percent), Percent Log Returns (use symbol for percent), Index, Quadratic Variation
#' @param TimeAgg = 'days', 'weeks', 'months'
#' @param Symbol ticker symbol string
#' @param CompanyName company name if you have it. ends up in title, that is all
#' @param StartDate Supply a start date. E.g. '2022-01-01'
#' @param EndDate Supply an end date. E.g. `Sys.Date()`
#' @param APIKey Supply your polygon API key
#'
#' @export
StockData <- function(PolyOut = NULL,
                      Symbol = 'TSLA',
                      CompanyName = 'Tesla Inc. Common Stock',
                      Metric = 'Stock Price',
                      TimeAgg = 'days',
                      StartDate = '2021-01-01',
                      EndDate = '2022-01-01',
                      APIKey = NULL) {
  print("StockData 1")
  StartDate <- as.Date(StartDate)
  EndDate <- as.Date(EndDate)
  PolyOut <- jsonlite::fromJSON(paste0("https://api.polygon.io/v2/aggs/ticker/",Symbol,"/range/1/day/",StartDate, "/", EndDate, "?adjusted=true&sort=asc&limit=10000&apiKey=", APIKey))
  print("StockData 2")
  data <- data.table::setDT(PolyOut$results)
  print("StockData 3")
  datas <- data.table::data.table(Date = seq(StartDate, EndDate, 'days'))
  print("StockData 4")
  datas <- AutoQuant::CreateCalendarVariables(data = datas, DateCols = 'Date', AsFactor = FALSE, TimeUnits = 'wday')
  print("StockData 5")
  datas <- datas[Date_wday %in% c(2L:6L)]
  print("StockData 6")
  datas <- datas[!Date %in% as.Date(holidayNYSE(year = c(data.table::year(StartDate),data.table::year(EndDate))))]
  print("StockData 7")
  if(nrow(datas) == nrow(data) + 1L) datas <- datas[seq_len(.N-1L)]
  print("StockData 8")
  data <- cbind(data, datas)
  print("StockData 9")
  if(TimeAgg == 'weeks') {
    data[, Date := lubridate::floor_date(Date, unit = 'weeks')]
    data <- data[, lapply(.SD, mean, na.rm = TRUE), .SD = c('v','vw','o','c','h','l','t','n'), by = 'Date']
  } else if(TimeAgg == 'months') {
    data[, Date := lubridate::floor_date(Date, unit = 'months')]
    data <- data[, lapply(.SD, mean, na.rm = TRUE), .SD = c('v','vw','o','c','h','l','t','n'), by = 'Date']
  } else if(TimeAgg == 'quarters') {
    data[, Date := lubridate::floor_date(Date, unit = 'quarters')]
    data <- data[, lapply(.SD, mean, na.rm = TRUE), .SD = c('v','vw','o','c','h','l','t','n'), by = 'Date']
  } else if(TimeAgg == 'years') {
    data[, Date := lubridate::floor_date(Date, unit = 'years')]
    data <- data[, lapply(.SD, mean, na.rm = TRUE), .SD = c('v','vw','o','c','h','l','t','n'), by = 'Date']
  }
  print("StockData 10")
  if(Metric == '% Returns') {
    for(i in c('o','c','h','l')) data[, paste0(i) := get(i) / data.table::shift(x = get(i)) - 1]
  } else if(Metric  == '% Log Returns') {
    for(i in c('o','c','h','l')) data[, paste0(i) := log(get(i)) - log(data.table::shift(x = get(i)))]
  } else if(Metric  == 'Index') {
    for(i in c('o','c','h','l')) data[, paste0(i) := get(i) / data.table::first(get(i))]
  } else if(Metric  == 'Quadratic Variation') {
    for(i in c('o','c','h','l')) data[, temp_temp := data.table::shift(x = get(i), n = 1L, fill = NA, type = 'lag')][, paste0(i) := (get(i) - temp_temp)^2][, temp_temp := NULL]
  }
  print("StockData 11")
  return(list(data = data, PolyOut = PolyOut, CompanyName = CompanyName, Symbol = Symbol, Metric = Metric, StartDate = StartDate, EndDate = EndDate, APIKey = APIKey))
}

#' @title Plot.Stock
#'
#' @description  Create a candlestick plot for stocks. See https://plotly.com/r/figure-labels/
#'
#' @family Standard Plots
#' @author Adrian Antico
#'
#' @param Type 'candlestick', 'ohlc'
#' @param StockDataOutput PolyOut returned from StockData()
#'
#' @export
Plot.Stock <- function(StockDataOutput,
                       Type = 'candlestick') {
  print('Plot.Stock 1')
  if(missing(StockDataOutput)) stop('StockDataOutput cannot be missing')
  if(Type == 'CandlestickPlot') Type <- 'candlestick'
  if(Type == 'OHLCPlot') Type <- 'ohlc'
  print(length(StockDataOutput))
  print(length(StockDataOutput$data))
  print('Plot.Stock 2')
  p1 <- plotly::plot_ly(
    data = StockDataOutput$data,
    x = ~Date,
    type = Type,
    open = ~o,
    close = ~c,
    high = ~h,
    low = ~l,
    decreasing = list(line = list(color = '#ff0055')),
    increasing = list(line = list(color = '#66ff00')),
    width = Width,
    height = Height)
  print('Plot.Stock 3')
  p1 <- plotly::layout(
    p = p1,
    font = AutoPlots:::font_(),
    title = if(length(StockDataOutput$CompanyName) == 0L) list(text = paste0(StockDataOutput$Symbol, ": ", StockDataOutput$StartDate, " to ", StockDataOutput$EndDate), font = 'Segoe UI') else list(text = paste0(StockDataOutput$CompanyName, " - ", StockDataOutput$Symbol, ": ", StockDataOutput$StartDate, " to ", StockDataOutput$EndDate), font = 'Segoe UI'),
    plot_bgcolor = '#001534',
    paper_bgcolor = "#6a6969",
    yaxis = list(title = AutoPlots:::bold_(StockDataOutput$Metric)),
    xaxis = list(title = AutoPlots:::bold_('Date')))
  print('Plot.Stock 3: done')
  return(p1)
}


# ----

# ----

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
# > Model Evaluation Plots                                                    ----
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

#' @title Plot.Calibration.Line
#'
#' @description This function automatically builds calibration plots and calibration boxplots for model evaluation using regression, quantile regression, and binary and multinomial classification
#'
#' @author Adrian Antico
#' @family Model Evaluation
#'
#' @param dt data.table
#' @param AggMethod character
#' @param SampleSize numeric
#' @param XVar character
#' @param YVar character
#' @param GroupVar character
#' @param NumberBins numeric
#' @param ZeroLineColor character hex
#' @param ZeroLineWidth numeric
#' @param Title character
#' @param Engine character
#' @param EchartsTheme character
#' @param TimeLine logical
#' @param X_Scroll logical
#' @param Y_Scroll logical
#' @param BackGroundColor character hex
#' @param ChartColor character hex
#' @param FillColor character hex
#' @param FillColorReverse character hex
#' @param GridColor character hex
#' @param TextColor "Not Implemented"
#' @param Debug logical
#'
#' @return Calibration plot
#' @examples
#' \dontrun{
#' # Create fake data
#' data <- data.table::fread("./CatBoost_ML1_ScoringData.csv")
#'
#' # Build Plot
#' AutoPlots::Plot.Calibration.Line(
#'   dt = data,
#'   Engine = "Echarts", # "Plotly"
#'   EchartsTheme = "macaron",
#'   TimeLine = TRUE,
#'   XVar = "Predict",
#'   YVar = "CHILLED_Margin_PerDay",
#'   GroupVar = "BRAND",
#'   AggMethod = 'mean',
#'   NumberBins = 20,
#'   ZeroLineColor = '#ffff',
#'   ZeroLineWidth = 1.25,
#'   Title = 'Bar Plot',
#'   FillColor = "#0066ff",
#'   BackGroundColor = "#6a6969", #"#1b1959", #'#00060b',
#'   ChartColor = '#001534',
#'   GridColor = 'white',
#'   TextColor = 'white',
#'   Debug = FALSE)
#'
#' # # Step Through Function
#' # library(AutoPlots)
#' # library(data.table)
#' # dt = data
#' # Engine = "Echarts" # "Plotly"
#' # TimeLine = FALSE
#' # EchartsTheme = "macaron"
#' # X_Scroll = TRUE
#' # Y_Scroll = TRUE
#' # XVar = "Predict"
#' # YVar = "Adrian"
#' # AggMethod = 'mean'
#' # GroupVar = "Factor_1"
#' # NumberBins = 20
#' # AggStat = "mean"
#' # Title = 'Bar Plot'
#' # FillColor = "#0066ff"
#' # BackGroundColor = "#6a6969"
#' # ChartColor = '#001534'
#' # GridColor = 'white'
#' # TextColor = 'white'
#' # ZeroLineColor = '#ffff'
#' # ZeroLineWidth = 1.25
#' # Debug = FALSE
#' }
#' @export
Plot.Calibration.Line <- function(dt = NULL,
                                  AggMethod = 'mean',
                                  XVar = NULL,
                                  YVar = NULL,
                                  GroupVar = NULL,
                                  NumberBins = 21,
                                  Title = 'Calibration Plot',
                                  Engine = 'Echarts',
                                  EchartsTheme = "macaron",
                                  TimeLine = FALSE,
                                  X_Scroll = TRUE,
                                  Y_Scroll = TRUE,
                                  BackGroundColor =  "#6a6969",
                                  ChartColor =       "#001534",
                                  FillColor =        "#0066ff",
                                  FillColorReverse = "#97ff00",
                                  GridColor =        "white",
                                  TextColor =        "white",
                                  ZeroLineColor = '#ffff',
                                  ZeroLineWidth = 1.25,
                                  Debug = FALSE) {

  # Minimize data before moving on
  if(Debug) print("Plot.Calibration.Line # Minimize data before moving on")
  Ncols <- ncol(dt)
  if(Ncols > 2L && length(GroupVar) == 0L) {
    dt1 <- data.table::copy(dt[, .SD, .SDcols = c(YVar, XVar)])
  } else if(Ncols > 3L && length(GroupVar) > 0L) {
    dt1 <- data.table::copy(dt[, .SD, .SDcols = c(YVar, XVar, GroupVar[[1L]])])
  } else {
    dt1 <- data.table::copy(dt)
  }

  # Define Aggregation function
  if(Debug) print("Plot.Calibration.Line # Define Aggregation function")
  aggFunc <- AutoPlots:::SummaryFunction(AggMethod)

  # If actual is in factor form, convert to numeric
  if(Debug) print("Plot.Calibration.Line # If actual is in factor form, convert to numeric")
  if(!is.numeric(dt1[[YVar]])) {
    data.table::set(dt1, j = YVar, value = as.numeric(as.character(dt1[[YVar]])))
  }

  # Add a column that ranks predicted values
  if(length(GroupVar) > 0L) {
    if(Debug) print("Plot.Calibration.Line # if(length(GroupVar) > 0L)")
    dt1[, Percentile := round(data.table::frank(get(XVar)) * NumberBins / .N) / NumberBins, by = c(GroupVar[1L])]
    dt1 <- dt1[, lapply(.SD, noquote(aggFunc)), by = c("Percentile",GroupVar[1L])]
    dt1[, `Target - Predicted` := get(YVar) - get(XVar)]
    data.table::setorderv(x = dt1, cols = c("Percentile",GroupVar[1L]), c(1L,1L))
  } else {
    if(Debug) print("Plot.Calibration.Line # if(length(GroupVar) == 0L)")
    dt1[, rank := round(data.table::frank(get(XVar)) * NumberBins / .N) / NumberBins]
    dt1 <- dt1[, lapply(.SD, noquote(aggFunc)), by = "rank"]
    dt1 <- data.table::melt.data.table(data = dt1, id.vars = "rank", measure.vars = c(YVar,XVar))
    data.table::setnames(dt1, names(dt1), c("Percentile", "Variable", YVar))
    data.table::setorderv(x = dt1, cols = c("Percentile","Variable"), c(1L,1L))
  }

  # Build Plot
  if(Debug) print("Plot.Calibration.Line # AutoPlots::Plot.Line()")
  yvar <- if(length(GroupVar) > 0L) "Target - Predicted" else YVar
  gv <- if(length(GroupVar) == 0L) "Variable" else GroupVar
  tl <- if(length(GroupVar) == 0L) FALSE else TimeLine
  p1 <- AutoPlots::Plot.Line(
    dt = dt1,
    PreAgg = TRUE,
    YVar = yvar,
    XVar = "Percentile",
    GroupVar = gv,
    Title = 'Calibration Line Plot',
    Engine = Engine,
    EchartsTheme = EchartsTheme,
    TimeLine = tl,
    X_Scroll = X_Scroll,
    Y_Scroll = Y_Scroll,
    BackGroundColor = BackGroundColor,
    ChartColor = ChartColor,
    FillColor = FillColor,
    GridColor = GridColor,
    TextColor = GridColor,
    ZeroLineColor = ZeroLineColor,
    ZeroLineWidth = ZeroLineWidth,
    Debug = Debug)

  return(p1)
}

#' @title Plot.Calibration.Box
#'
#' @description This function automatically builds calibration plots and calibration boxplots for model evaluation using regression, quantile regression, and binary and multinomial classification
#'
#' @author Adrian Antico
#' @family Model Evaluation
#'
#' @param dt data.table
#' @param SampleSize numeric
#' @param XVar character
#' @param YVar character
#' @param AggMethod character
#' @param GroupVar character
#' @param NumberBins numeric
#' @param Title character
#' @param Engine character
#' @param EchartsTheme character
#' @param TimeLine logical
#' @param X_Scroll logical
#' @param Y_Scroll logical
#' @param BackGroundColor character hex
#' @param ChartColor character hex
#' @param FillColor character hex
#' @param FillColorReverse character hex
#' @param GridColor character hex
#' @param TextColor "Not Implemented"
#' @param ZeroLineColor character hex
#' @param ZeroLineWidth numeric
#' @param Debug logical
#'
#' @return Calibration plot or boxplot
#' @examples
#' \dontrun{
#' # Create fake data
#' data <- data.table::fread("./CatBoost_ML1_ScoringData.csv")
#'
#' # Build Plot
#' AutoPlots::Plot.Calibration.Box(
#'   dt = data,
#'   SampleSize = 100000,
#'   XVar = "Predict",
#'   YVar = "Adrian",
#'   AggMethod = 'mean',
#'   GroupVar = "Factor_1",
#'   NumberBins = 20,
#'   AggStat = "mean",
#'   ZeroLineColor = '#ffff',
#'   ZeroLineWidth = 1.25,
#'   Title = 'Bar Plot',
#'   Engine = "Echarts", # "Plotly"
#'   EchartsTheme = "macaron",
#'   TimeLine = TRUE,
#'   X_Scroll = TRUE,
#'   Y_Scroll = TRUE,
#'   FillColor = "#0066ff",
#'   FillColorReverse = "#ff9900",
#'   BackGroundColor = "#6a6969",
#'   ChartColor = '#001534',
#'   GridColor = 'white',
#'   TextColor = 'white',
#'   Debug = FALSE)
#'
#' # # Step Through Function
#' # library(AutoPlots)
#' # library(data.table)
#' # dt = data
#' # Engine = "Echarts" # "Plotly"
#' # TimeLine = FALSE
#' # EchartsTheme = "macaron"
#' # X_Scroll = TRUE
#' # Y_Scroll = TRUE
#' # XVar = "Predict"
#' # YVar = "Adrian"
#' # AggMethod = 'mean'
#' # GroupVar = "Factor_1"
#' # NumberBins = 20
#' # AggStat = "mean"
#' # ZeroLineColor = '#ffff'
#' # ZeroLineWidth = 1.25
#' # Title = 'Bar Plot'
#' # SampleSize = 100000
#' # FillColor = "#0066ff"
#' # FillColorReverse = "#ff9900"
#' # BackGroundColor = "#6a6969"
#' # ChartColor = '#001534'
#' # GridColor = 'white'
#' # TextColor = 'white'
#' # Debug = FALSE
#' }
#' @export
Plot.Calibration.Box <- function(dt = NULL,
                                 SampleSize = 100000L,
                                 AggMethod = 'mean',
                                 XVar = NULL,
                                 YVar = NULL,
                                 GroupVar = NULL,
                                 NumberBins = 21,
                                 Title = 'Calibration Plot',
                                 Engine = 'Echarts',
                                 EchartsTheme = "macaron",
                                 TimeLine = FALSE,
                                 X_Scroll = TRUE,
                                 Y_Scroll = TRUE,
                                 BackGroundColor =  "#6a6969",
                                 ChartColor =       "#001534",
                                 FillColor =        "#0066ff",
                                 FillColorReverse = "#97ff00",
                                 GridColor =        "white",
                                 TextColor =        "white",
                                 ZeroLineColor = '#ffff',
                                 ZeroLineWidth = 1.25,
                                 Debug = FALSE) {

  # Minimize data before moving on
  if(dt[, .N] > SampleSize) dt <- dt[order(runif(.N))][seq_len(SampleSize)]
  Ncols <- ncol(dt)
  if(Ncols > 2L && length(GroupVar) == 0L) {
    dt1 <- data.table::copy(dt[, .SD, .SDcols = c(YVar, XVar)])
  } else if(Ncols > 3L && length(GroupVar) > 0L) {
    dt1 <- data.table::copy(dt[, .SD, .SDcols = c(YVar, XVar, GroupVar[[1L]])])
  } else {
    dt1 <- data.table::copy(dt)
  }

  # If actual is in factor form, convert to numeric
  if(!data.table::is.data.table(dt1)) data.table::setDT(dt1)
  if(!is.numeric(dt1[[YVar]])) {
    data.table::set(dt1, j = YVar, value = as.numeric(as.character(dt1[[YVar]])))
  }

  # Add a column that ranks predicted values
  if(Debug) print(paste0("NumberBins = ", NumberBins))
  if(length(GroupVar) > 0L) {
    dt1[, rank := round(data.table::frank(get(XVar)) * NumberBins / .N) / NumberBins, by = c(GroupVar[1L])]
  } else {
    dt1[, rank := round(data.table::frank(get(XVar)) * NumberBins / .N) / NumberBins]
  }
  dt1[, `Target - Predicted` := get(YVar) - get(XVar)]
  data.table::setnames(dt1, "rank", "Percentile")
  if(length(GroupVar) > 0L) {
    data.table::setorderv(x = dt1, cols = c("Percentile", GroupVar[1L]), c(1L,1L))
  } else {
    data.table::setorderv(x = dt1, cols = "Percentile", 1L)
  }

  # Plot
  if(Debug) print(paste0("TimeLine for AutoPlots:::Plot.Box=", TimeLine))
  p1 <- AutoPlots:::Plot.Box(
    dt = dt1,
    SampleSize = SampleSize,
    XVar = "Percentile",
    YVar = "Target - Predicted",
    GroupVar = GroupVar,
    Title = 'Calibration Box Plot',
    Engine = Engine,
    EchartsTheme = EchartsTheme,
    TimeLine = TimeLine,
    X_Scroll = X_Scroll,
    Y_Scroll = Y_Scroll,
    BackGroundColor = BackGroundColor,
    ChartColor = ChartColor,
    FillColor = FillColor,
    FillColorReverse = FillColorReverse,
    TextColor = GridColor,
    GridColor = GridColor,
    ZeroLineColor = '#ffff',
    ZeroLineWidth = 1.25,
    Debug = Debug)
  return(p1)
}

#' @title Plot.ROC
#'
#' @description ROC Plot
#'
#' @author Adrian Antico
#' @family Model Evaluation
#'
#' @param dt data.table
#' @param XVar character
#' @param YVar character
#' @param AggMethod character
#' @param GroupVar character
#' @param NumberBins numeric
#' @param Title character
#' @param Engine character
#' @param EchartsTheme character
#' @param TimeLine logical
#' @param X_Scroll logical
#' @param Y_Scroll logical
#' @param SampleSize numeric
#' @param BackGroundColor character hex
#' @param ChartColor character hex
#' @param FillColor character hex
#' @param FillColorReverse character hex
#' @param GridColor character hex
#' @param TextColor character hex
#' @param ZeroLineColor character hex
#' @param ZeroLineWidth numeric
#' @param Debug logical
#'
#' @return Calibration plot or boxplot
#' @examples
#' \dontrun{
#' # Create fake data
#' data <- data.table::fread("./CatBoost_ML1_ScoringData.csv")
#'
#' # Build Plot
#' AutoPlots::Plot.ROC(
#'   dt = data,
#'   Engine = "Echarts", # "Plotly"
#'   EchartsTheme = "macaron",
#'   TimeLine = TRUE,
#'   X_Scroll = TRUE,
#'   Y_Scroll = TRUE,
#'   XVar = "Predict",
#'   YVar = "CHILLED_Margin_PerDay",
#'   AggMethod = 'mean',
#'   GroupVar = "BRAND",
#'   NumberBins = 20,
#'   AggStat = "mean",
#'   ZeroLineColor = '#ffff',
#'   ZeroLineWidth = 1.25,
#'   Title = 'Bar Plot',
#'   SampleSize = 100000,
#'   FillColor = "#0066ff",
#'   BackGroundColor = "#6a6969", #"#1b1959", #'#00060b',
#'   ChartColor = '#001534',
#'   GridColor = 'white',
#'   TextColor = 'white',
#'   Debug = FALSE)
#'
#' # # Step Through Function
#' # library(AutoPlots)
#' # library(data.table)
#' # dt = data
#' # Engine = "Echarts" # "Plotly"
#' # TimeLine = FALSE
#' # X_Scroll = TRUE,
#' # Y_Scroll = TRUE,
#' # EchartsTheme = "macaron"
#' # XVar = "Predict"
#' # YVar = "CHILLED_Margin_PerDay",
#' # AggMethod = 'mean',
#' # GroupVar = "BRAND",
#' # NumberBins = 20
#' # AggStat = "mean"
#' # ZeroLineColor = '#ffff'
#' # ZeroLineWidth = 1.25
#' # Title = 'Bar Plot'
#' # SampleSize = 100000
#' # FillColor = "#0066ff"
#' # BackGroundColor = "#6a6969"
#' # ChartColor = '#001534'
#' # GridColor = 'white'
#' # TextColor = 'white'
#' # Debug = FALSE
#' }
#' @export
Plot.ROC <- function(dt = NULL,
                     SampleSize = 100000,
                     XVar = NULL,
                     YVar = NULL,
                     GroupVar = NULL,
                     AggMethod = 'mean',
                     Title = 'Calibration Plot',
                     Engine = 'Echarts',
                     EchartsTheme = "macaron",
                     TimeLine = FALSE,
                     X_Scroll = TRUE,
                     Y_Scroll = TRUE,
                     BackGroundColor =  "#6a6969",
                     ChartColor =       "#001534",
                     FillColor =        "#0066ff",
                     FillColorReverse = "#97ff00",
                     GridColor =        "white",
                     TextColor =        "white",
                     ZeroLineColor = '#ffff',
                     ZeroLineWidth = 1.25,
                     Debug = FALSE) {

  # Data Prep1
  if(dt[, .N] > SampleSize) dt <- dt[order(runif(.N))][seq_len(SampleSize)]
  if(length(GroupVar) > 0L) {
    vals <- sort(unique(dt[[GroupVar[1L]]]))
    for(i in seq_along(vals)) {
      temp <- dt[get(GroupVar[1L]) %in% eval(vals[i])]
      AUC_Metrics <- pROC::roc(
        response = temp[[YVar]],
        predictor = temp[[XVar]],
        na.rm = TRUE,
        algorithm = 3L,
        auc = TRUE,
        ci = TRUE)
      if(i == 1L) {
        data <- data.table::data.table(
          GroupLevels = vals[i],
          Sensitivity = AUC_Metrics$sensitivities,
          Specificity = AUC_Metrics$specificities)
      } else {
        data <- data.table::rbindlist(list(
          data,
          data.table::data.table(
            GroupLevels = vals[i],
            Sensitivity = AUC_Metrics$sensitivities,
            Specificity = AUC_Metrics$specificities)
        ))
      }
    }

    # For Title: auc = AUC_Metrics$auc
    AUC_Metrics <- pROC::roc(
      response = dt[[YVar]],
      predictor = dt[[XVar]],
      na.rm = TRUE,
      algorithm = 3L,
      auc = TRUE,
      ci = TRUE)

  } else {
    AUC_Metrics <- pROC::roc(
      response = dt[[YVar]],
      predictor = dt[[XVar]],
      na.rm = TRUE,
      algorithm = 3L,
      auc = TRUE,
      ci = TRUE)
    data <- data.table::data.table(
      GroupLevels = 0L,
      Sensitivity = AUC_Metrics$sensitivities,
      Specificity = AUC_Metrics$specificities)
  }

  # Data Prep2
  if(Debug) print("Plot.Calibration.Line # AutoPlots::Plot.Line()")
  data[, `1 - Specificity` := 1 - Specificity]
  data.table::set(data, j = "Specificity", value = NULL)
  YVar <- "Sensitivity"
  XVar <- "1 - Specificity"
  tl <- if(length(GroupVar) == 0L) FALSE else TimeLine
  title <- paste0(Title, ": auc = ", 100 * round(AUC_Metrics$auc, 3), "%")
  gv <- if(length(GroupVar) > 0L) "GroupLevels" else NULL
  data.table::setorderv(x = data, cols = c(gv, "Sensitivity"))

  # Build Plot
  p1 <- AutoPlots::Plot.Line(
    AggCheck = FALSE,
    dt = data,
    Area = TRUE,
    Alpha = 0.50,
    Engine = Engine,
    EchartsTheme = EchartsTheme,
    TimeLine = tl,
    YVar = YVar,
    XVar = XVar,
    GroupVar = gv,
    ZeroLineColor = ZeroLineColor,
    ZeroLineWidth = ZeroLineWidth,
    Title = title,
    BackGroundColor = BackGroundColor,
    ChartColor = ChartColor,
    FillColor = FillColor,
    GridColor = GridColor,
    TextColor = GridColor,
    X_Scroll = X_Scroll,
    Y_Scroll = Y_Scroll,
    Debug = Debug)

  # Y == X dashed line
  if(class(p1)[1L] == "plotly") p1 <- plotly::add_segments(p = p1, x = 0, xend = 1, y = 0, yend = 1, line = list(dash = "dash", color = TextColor),inherit = FALSE, showlegend = FALSE)

  # Return
  return(p1)
}

#' @title Plot.Residuals.Histogram
#'
#' @description Residuals Plot
#'
#' @author Adrian Antico
#' @family Model Evaluation
#'
#' @param dt data.table
#' @param AggMethod character
#' @param SampleSize numeric
#' @param XVar character
#' @param YVar character
#' @param GroupVar character
#' @param NumberBins numeric
#' @param ZeroLineColor character hex
#' @param ZeroLineWidth numeric
#' @param Title character
#' @param Engine character
#' @param EchartsTheme character
#' @param TimeLine logical
#' @param X_Scroll logical
#' @param Y_Scroll logical
#' @param BackGroundColor character hex
#' @param ChartColor character hex
#' @param FillColor character hex
#' @param FillColorReverse character hex
#' @param GridColor character hex
#' @param TextColor Not Implemented
#' @param Debug logical
#'
#' @examples
#' \dontrun{
#' # Create fake data
#' data <- data.table::fread("./CatBoost_ML1_ScoringData.csv")
#'
#' # Build Plot
#' AutoPlots::Plot.Residuals.Histogram(
#'   dt = data,
#'   AggMethod = 'mean',
#'   SampleSize = 100000,
#'   XVar = "Predict",
#'   YVar = "CHILLED_Margin_PerDay",
#'   GroupVar = "BRAND",
#'   NumberBins = 20,
#'   Title = 'Bar Plot',
#'   Engine = "Echarts", # "Plotly"
#'   EchartsTheme = "macaron",
#'   TimeLine = TRUE,
#'   BackGroundColor = "#6a6969", #"#1b1959", #'#00060b',
#'   ChartColor = '#001534',
#'   FillColor = "#0066ff",
#'   GridColor = 'white',
#'   TextColor = 'white',
#'   ZeroLineColor = '#ffff',
#'   ZeroLineWidth = 1.25,
#'   Debug = FALSE)
#'
#' # # Step Through Function
#' # library(AutoPlots)
#' # library(data.table)
#' # dt = data
#' # AggMethod = 'mean',
#' # SampleSize = 100000
#' # XVar = "Predict"
#' # YVar = "CHILLED_Margin_PerDay",
#' # GroupVar = "BRAND",
#' # NumberBins = 20
#' # Title = 'Bar Plot'
#' # Engine = "Echarts" # "Plotly"
#' # EchartsTheme = "macaron"
#' # TimeLine = FALSE
#' # BackGroundColor = "#6a6969"
#' # ChartColor = '#001534'
#' # FillColor = "#0066ff"
#' # GridColor = 'white'
#' # TextColor = 'white'
#' # ZeroLineColor = '#ffff'
#' # ZeroLineWidth = 1.25
#' # Debug = FALSE
#' }
#' @export
Plot.Residuals.Histogram <- function(dt = NULL,
                                     AggMethod = 'mean',
                                     SampleSize = 100000,
                                     XVar = NULL,
                                     YVar = NULL,
                                     GroupVar = NULL,
                                     NumberBins = 20,
                                     Title = 'Calibration Plot',
                                     Engine = 'Echarts',
                                     EchartsTheme = "macaron",
                                     TimeLine = FALSE,
                                     X_Scroll = TRUE,
                                     Y_Scroll = TRUE,
                                     BackGroundColor =  "#6a6969",
                                     ChartColor =       "#001534",
                                     FillColor =        "#0066ff",
                                     FillColorReverse = "#97ff00",
                                     GridColor =        "white",
                                     TextColor =        "white",
                                     ZeroLineColor = '#ffff',
                                     ZeroLineWidth = 1.25,
                                     Debug = FALSE) {

  # Data Prep1
  if(length(SampleSize) == 0L) SampleSize <- 30000L
  if(!data.table::is.data.table(dt)) data.table::setDT(dt)
  dt1 <- dt1[, .SD, .SDcols = c(XVar,YVar,GroupVar)]
  if(dt1[, .N] > SampleSize) {
    dt1 <- dt[order(runif(.N))][seq_len(SampleSize)]
  } else {
    dt1 <- data.table::copy(dt)
  }
  dt1[, `Target - Predicted` := get(YVar) - get(XVar)]
  YVar <- "Target - Predicted"
  if(length(GroupVar) > 0L) GroupVar <- GroupVar[1L]

  # Data Prep2
  if(Debug) print("Plot.Residuals.Histogram")
  tl <- if(length(GroupVar) == 0L) FALSE else TimeLine
  data.table::setorderv(x = dt1, cols = c(GroupVar, XVar))

  # Build Plot
  p1 <- AutoPlots::Plot.Histogram(
    dt = dt1,
    SampleSize = SampleSize,
    YVar = "Target - Predicted",
    XVar = XVar,
    GroupVar = GroupVar,
    NumberBins = NumberBins,
    Title = Title,
    Engine = Engine,
    EchartsTheme = EchartsTheme,
    TimeLine = tl,
    X_Scroll = X_Scroll,
    Y_Scroll = Y_Scroll,
    BackGroundColor = BackGroundColor,
    ChartColor = ChartColor,
    FillColor = FillColor,
    GridColor = GridColor,
    TextColor = GridColor,
    ZeroLineColor = ZeroLineColor,
    ZeroLineWidth = ZeroLineWidth,
    Debug = Debug)
  return(p1)
}

#' @title Plot.Residuals.Scatter
#'
#' @description Residuals_2 Plot
#'
#' @author Adrian Antico
#' @family Model Evaluation
#'
#' @param dt data.table
#' @param AggMethod character
#' @param SampleSize numeric
#' @param XVar character
#' @param YVar character
#' @param GroupVar character
#' @param NumberBins numeric
#' @param Title character
#' @param Engine character
#' @param EchartsTheme character
#' @param TimeLine logical
#' @param X_Scroll logical
#' @param Y_Scroll logical
#' @param BackGroundColor character hex
#' @param ChartColor character hex
#' @param FillColor character hex
#' @param FillColorReverse character hex
#' @param GridColor character hex
#' @param TextColor "Not Implemented"
#' @param ZeroLineColor character hex
#' @param ZeroLineWidth numeric
#' @param Debug logical
#'
#' @examples
#' \dontrun{
#' # Create fake data
#' data <- data.table::fread("./CatBoost_ML1_ScoringData.csv")
#'
#' # Build Plot
#' AutoPlots::Plot.Residuals.Scatter(
#'   dt = data,
#'   AggMethod = 'mean',
#'   SampleSize = 100000,
#'   XVar = "Predict",
#'   YVar = "CHILLED_Margin_PerDay",
#'   GroupVar = "BRAND",
#'   NumberBins = 20,
#'   Title = 'Bar Plot',
#'   Engine = "Echarts", # "Plotly"
#'   EchartsTheme = "macaron",
#'   TimeLine = TRUE,
#'   BackGroundColor = "#6a6969",
#'   ChartColor = '#001534',
#'   FillColor = "#0066ff",
#'   GridColor = 'white',
#'   TextColor = 'white',
#'   ZeroLineColor = '#ffff',
#'   ZeroLineWidth = 1.25,
#'   Debug = FALSE)
#'
#' # # Step Through Function
#' # library(AutoPlots)
#' # library(data.table)
#' # dt = data
#' # AggMethod = 'mean',
#' # SampleSize = 100000
#' # XVar = "Predict"
#' # YVar = "CHILLED_Margin_PerDay",
#' # GroupVar = "BRAND",
#' # NumberBins = 20
#' # Title = 'Bar Plot'
#' # Engine = "Echarts" # "Plotly"
#' # TimeLine = FALSE
#' # EchartsTheme = "macaron"
#' # FillColor = "#0066ff"
#' # BackGroundColor = "#6a6969"
#' # ChartColor = '#001534'
#' # GridColor = 'white'
#' # TextColor = 'white'
#' # ZeroLineColor = '#ffff'
#' # ZeroLineWidth = 1.25
#' # Debug = FALSE
#' }
#' @export
Plot.Residuals.Scatter <- function(dt = NULL,
                                   AggMethod = 'mean',
                                   SampleSize = 100000,
                                   XVar = NULL,
                                   YVar = NULL,
                                   GroupVar = NULL,
                                   Title = 'Calibration Plot',
                                   Engine = 'Echarts',
                                   EchartsTheme = "macaron",
                                   TimeLine = FALSE,
                                   X_Scroll = TRUE,
                                   Y_Scroll = TRUE,
                                   BackGroundColor =  "#6a6969",
                                   ChartColor =       "#001534",
                                   FillColor =        "#0066ff",
                                   FillColorReverse = "#97ff00",
                                   GridColor =        "white",
                                   TextColor =        "white",
                                   ZeroLineColor = '#ffff',
                                   ZeroLineWidth = 1.25,
                                   Debug = FALSE) {

  # Data Prep1
  if(length(SampleSize) == 0L) SampleSize <- 30000L
  if(!data.table::is.data.table(dt)) data.table::setDT(dt)
  dt1 <- dt[, .SD, .SDcols = c(XVar,YVar,GroupVar)]
  if(dt1[, .N] > SampleSize) dt1 <- dt1[order(runif(.N))][seq_len(SampleSize)]
  dt1[, `Target - Predicted` := get(YVar) - get(XVar)]
  if(length(GroupVar) >0L) GroupVar <- GroupVar[1L]
  if(length(GroupVar) > 0L) {
    dt1[, eval(XVar) := round(data.table::frank(get(XVar)) * 20 / .N) / 20, by = c(GroupVar[1L])]
  } else {
    dt1[, eval(XVar) := round(data.table::frank(get(XVar)) * 20 / .N) / 20]
  }
  YVar <- "Target - Predicted"

  # Data Prep2
  tl <- if(length(GroupVar) == 0L) FALSE else TimeLine
  data.table::setorderv(x = dt1, cols = c(GroupVar[1L], XVar))

  # Build Plot
  p1 <- AutoPlots::Plot.Scatter(
    dt = dt1,
    SampleSize = SampleSize,
    YVar = "Target - Predicted",
    XVar = XVar,
    GroupVar = GroupVar[1L],
    Title = Title,
    Engine = Engine,
    EchartsTheme = EchartsTheme,
    TimeLine = tl,
    X_Scroll = X_Scroll,
    Y_Scroll = Y_Scroll,
    FillColor = FillColor,
    BackGroundColor = BackGroundColor,
    ChartColor = ChartColor,
    GridColor = GridColor,
    TextColor = GridColor,
    ZeroLineColor = ZeroLineColor,
    ZeroLineWidth = ZeroLineWidth,
    Debug = Debug)
  return(p1)
}

#' @title Plot.VariableImportance
#'
#' @description Generate variable importance plots
#'
#' @author Adrian Antico
#' @family Model Evaluation
#'
#' @param Algo 'catboost', 'xgboost', 'h2o'
#' @param dt Source data.table
#' @param AggMethod Choose from 'mean', 'sum', 'sd', and 'median'
#' @param XVar Column name of X-Axis variable. If NULL then ignored
#' @param YVar Column name of Y-Axis variable. If NULL then ignored
#' @param GroupVar Column name of Group Variable for distinct colored histograms by group levels
#' @param Title title
#' @param Engine 'Plotly' or "Echarts"
#' @param EchartsTheme "macaron"
#' @param TimeLine logical
#' @param X_Scroll logical
#' @param Y_Scroll logical
#' @param BackGroundColor 'gray95'
#' @param ChartColor 'lightsteelblue'
#' @param FillColor 'gray'
#' @param FillColorReverse character hex
#' @param GridColor 'white'
#' @param TextColor 'darkblue'
#' @param ZeroLineColor = '#ffff'
#' @param Debug FALSE
#'
#' @examples
#' \dontrun{
#' # Create fake data
#' data <- data.table::fread("./CatBoost_ML1_ScoringData.csv")
#' data <- data.table::fread("./CatBoost_ML2_ScoringData.csv")
#' data <- data.table::fread("./CatBoost_ML1_Test_VI_Data.csv")
#'
#' # Step Through Function
#' library(AutoPlots)
#' library(data.table)
#' dt = data
#' Engine =  "Plotly"
#' TimeLine = FALSE
#' X_Scroll = TRUE,
#' Y_Scroll = TRUE,
#' EchartsTheme = "purple-passion"
#' XVar = "Importance" # "Predict"
#' YVar = "Variable" # "CHILLED_Margin_PerDay"
#' AggMethod = 'mean'
#' GroupVar = NULL # "BRAND"
#' NumberBins = 20
#' AggStat = "mean"
#' ZeroLineColor = '#ffff'
#' ZeroLineWidth = 1.25
#' Title = 'Bar Plot'
#' FillColor = "#0066ff"
#' BackGroundColor = "#6a6969"
#' ChartColor = '#001534'
#' GridColor = 'white'
#' TextColor = 'white'
#' Debug = FALSE
#'
#' Rappture::Plot.VariableImportance(
#' # Algo = "catboost",
#' # dt = data,
#' # Engine = 'Plotly',
#' # EchartsTheme = "macaron",
#' # TimeLine = TRUE,
#' # X_Scroll = TRUE,
#' # Y_Scroll = TRUE,
#' # XVar = "Importance",
#' # YVar = "Variable",
#' # GroupVar = NULL,
#' # AggMethod = 'mean',
#' # Title = 'Variable Importance Plot',
#' # BackGroundColor = "#6a6969",
#' # ChartColor = '#001534',
#' # FillColor = "#0066ff",
#' # GridColor = 'white',
#' # TextColor = 'white',
#' # ZeroLineColor = '#ffff',
#' # ZeroLineWidth = 1.25,
#' # Debug = FALSE)
#' }
#'
#' @return ROC Plot for classification models
#' @export
Plot.VariableImportance <- function(Algo = "CatBoost",
                                    dt = NULL,
                                    XVar = NULL,
                                    YVar = NULL,
                                    GroupVar = NULL,
                                    AggMethod = 'mean',
                                    Title = 'Variable Importance Plot',
                                    Engine = 'Plotly',
                                    EchartsTheme = "macaron",
                                    TimeLine = TRUE,
                                    X_Scroll = TRUE,
                                    Y_Scroll = TRUE,
                                    BackGroundColor =  "#6a6969",
                                    ChartColor =       "#001534",
                                    FillColor =        "#0066ff",
                                    FillColorReverse = "#97ff00",
                                    GridColor =        "white",
                                    TextColor =        "white",
                                    ZeroLineColor = '#ffff',
                                    ZeroLineWidth = 1.25,
                                    Debug = FALSE) {

  # Bar Plot
  p1 <- AutoPlots::Plot.Bar(
    dt = dt,
    PreAgg = TRUE,
    XVar = if(Algo != "h2o") "Importance" else "ScaledImportance",
    YVar = "Variable",
    GroupVar = GroupVar,
    AggMethod = AggMethod,
    Title = paste0(Algo, ": ", Title),
    Engine = Engine,
    EchartsTheme = EchartsTheme,
    TimeLine = TimeLine,
    X_Scroll = X_Scroll,
    Y_Scroll = Y_Scroll,
    BackGroundColor = BackGroundColor,
    ChartColor = ChartColor,
    FillColor = FillColor,
    GridColor = GridColor,
    TextColor = GridColor,
    Debug = FALSE)
  if(class(p1)[1L] == "plotly") {
    p1 <- plotly::layout(p1, yaxis = list(autorange = "reversed"))
  } else {
    p1 <- echarts4r::e_flip_coords(e = p1)
  }
  return(p1)
}

#' @title Plot.ConfusionMatrix
#'
#' @description Generate variable importance plots
#'
#' @author Adrian Antico
#' @family Model Evaluation
#'
#' @param Algo 'catboost', 'xgboost', 'h2o'
#' @param Engine 'Plotly' or "Echarts"
#' @param EchartsTheme "macaron"
#' @param TimeLine logical
#' @param dt Source data.table
#' @param PreAgg FALSE
#' @param XVar Column name of X-Axis variable. If NULL then ignored
#' @param YVar Column name of Y-Axis variable. If NULL then ignored
#' @param ZVar = "N"
#' @param NumberBins = 21,
#' @param NumLevels_X = NumLevels_Y,
#' @param NumLevels_Y = NumLevels_X,
#' @param GroupVar Column name of Group Variable for distinct colored histograms by group levels
#' @param Title title
#' @param GroupVar = NULL
#' @param ZeroLineColor = '#ffff'
#' @param AggMethod Choose from 'mean', 'sum', 'sd', and 'median'
#' @param FillColor 'gray'
#' @param FillColorReverse character hex
#' @param ChartColor 'lightsteelblue'
#' @param TextColor 'darkblue'
#' @param GridColor 'white'
#' @param BackGroundColor 'gray95'
#' @param Debug FALSE
#'
#' @examples
#' \dontrun{
#' # Create fake data
#' data <- data.table::fread("./CatBoost_ML1_ScoringData.csv")
#' data <- data.table::fread("./CatBoost_ML2_ScoringData.csv")
#' data <- data.table::fread("./CatBoost_ML1_Test_VI_Data.csv")
#'
#' # Step Through Function
#' library(AutoPlots)
#' library(data.table)
#' dt = data
#' Engine =  "Plotly"
#' TimeLine = FALSE
#' PreAgg = FALSE
#' EchartsTheme = "purple-passion"
#' XVar = "Importance" # "Predict"
#' YVar = "Variable" # "CHILLED_Margin_PerDay"
#' AggMethod = 'mean'
#' GroupVar = NULL # "BRAND"
#' NumberBins = 20
#' AggStat = "mean"
#' ZeroLineColor = '#ffff'
#' ZeroLineWidth = 1.25
#' Title = 'Bar Plot'
#' FillColor = "#0066ff"
#' BackGroundColor = "#6a6969"
#' ChartColor = '#001534'
#' GridColor = 'white'
#' TextColor = 'white'
#' Debug = FALSE
#'
#'
#' # dt = data,
#' # PreAgg = FALSE
#' # Engine = 'Plotly',
#' # EchartsTheme = "macaron",
#' # TimeLine = TRUE,
#' # XVar = "Importance",
#' # YVar = "Variable",
#' # ZVar = "N"
#' # NumberBins = 21,
#' # NumLevels_X = NumLevels_Y,
#' # NumLevels_Y = NumLevels_X,
#' # GroupVar = NULL,
#' # AggMethod = 'mean',
#' # Title = 'Variable Importance Plot',
#' # BackGroundColor = "#6a6969",
#' # ChartColor = '#001534',
#' # FillColor = "#0066ff",
#' # GridColor = 'white',
#' # TextColor = 'white',
#' # ZeroLineColor = '#ffff',
#' # ZeroLineWidth = 1.25,
#' # Debug = FALSE)
#' }
#'
#' @return ROC Plot for classification models
#' @export
Plot.ConfusionMatrix <- function(dt = NULL,
                                 PreAgg = FALSE,
                                 XVar = NULL,
                                 YVar = NULL,
                                 ZVar = "N",
                                 NumberBins = 21,
                                 NumLevels_X = 50,
                                 NumLevels_Y = 50,
                                 Title = "Confusion Matrix",
                                 Engine = 'Plotly',
                                 EchartsTheme = "macaron",
                                 TimeLine = TRUE,
                                 X_Scroll = TRUE,
                                 Y_Scroll = TRUE,
                                 BackGroundColor =  "#6a6969",
                                 ChartColor =       "#001534",
                                 FillColor =        "#0066ff",
                                 FillColorReverse = "#97ff00",
                                 GridColor =        "white",
                                 TextColor =        "white",
                                 ZeroLineColor = '#ffff',
                                 ZeroLineWidth = 1.25,
                                 AggMethod = "count",
                                 GroupVar = NULL,
                                 Debug = FALSE) {

  # Data prep
  if(!PreAgg) {
    if(Debug) print("Confusion Matrix 1")
    dt2 <- data.table::CJ(unique(dt[[YVar]]), unique(dt[[XVar]]))
    data.table::setnames(dt2, c("V1","V2"), c(YVar, XVar))
    dt1 <- dt[, list(Metric = .N), by = c(YVar, XVar)]
    data.table::setkeyv(x = dt1, cols = c(YVar, XVar))
    data.table::setkeyv(x = dt2, cols = c(YVar, XVar))
    dt2[dt1, Metric := i.Metric]
    data.table::set(dt2, i = which(is.na(dt2[["Metric"]])), j = "Metric", value = 0)
    if(Debug) print("Confusion Matrix Plot.Heatmap")
    ZVar = "Metric"
  } else {
    dt2 <- data.table::copy(dt)
  }

  # Corr Matrix for the automatic ordering
  data.table::setorderv(dt2, c(XVar,YVar), c(1L,1L))
  p1 <- AutoPlots:::Plot.HeatMap(
    PreAgg = TRUE,
    Engine = Engine,
    EchartsTheme = EchartsTheme,
    Title = Title,
    dt = dt2,
    YVar = YVar,
    XVar = XVar,
    ZVar = ZVar,
    AggMethod = if(!PreAgg) "centroidial" else AggMethod,
    NumberBins = NumberBins,
    NumLevels_X = NumLevels_X,
    NumLevels_Y = NumLevels_Y,
    X_Scroll = X_Scroll,
    Y_Scroll = Y_Scroll,
    BackGroundColor = BackGroundColor,
    ChartColor = ChartColor,
    FillColor = FillColor,
    GridColor = GridColor)
  p1
}

#' @title Plot.Lift
#'
#' @description Create a cumulative gains chart
#'
#' @family Model Evaluation
#'
#' @author Adrian Antico
#'
#' @param dt data.table
#' @param XVar character
#' @param YVar character
#' @param ZVar character
#' @param GroupVar character
#' @param NumberBins numeric
#' @param PreAgg logical
#' @param NumberBins numeric
#' @param Title character
#' @param Engine character
#' @param EchartsTheme character
#' @param TimeLine logical
#' @param X_Scroll logical
#' @param Y_Scroll logical
#' @param BackGroundColor character hex
#' @param ChartColor character hex
#' @param FillColor character hex
#' @param FillColorReverse character hex
#' @param GridColor character hex
#' @param TextColor character hex
#' @param ZeroLineColor character hex
#' @param ZeroLineWidth numeric
#' @param Debug logical
#'
#' @export
Plot.Lift <- function(dt = NULL,
                      PreAgg = FALSE,
                      XVar = NULL,
                      YVar = NULL,
                      ZVar = "N",
                      GroupVar = NULL,
                      NumberBins = 20,
                      Title = "Confusion Matrix",
                      Engine = 'Plotly',
                      EchartsTheme = "macaron",
                      TimeLine = TRUE,
                      X_Scroll = TRUE,
                      Y_Scroll = TRUE,
                      BackGroundColor =  "#6a6969",
                      ChartColor =       "#001534",
                      FillColor =        "#0066ff",
                      FillColorReverse = "#97ff00",
                      GridColor =        "white",
                      TextColor =        "white",
                      ZeroLineColor = '#ffff',
                      ZeroLineWidth = 1.25,
                      Debug = FALSE) {

  # Data Prep
  dt1 <- dt[, .SD, .SDcols = c(XVar,YVar)]
  dt1[, NegScore := -get(XVar)]
  NumberBins <- c(seq(1/NumberBins, 1 - 1/NumberBins, 1/NumberBins), 1)
  Cuts <- quantile(x = dt1[["NegScore"]], probs = NumberBins)
  dt1[, eval(YVar) := as.character(get(YVar))]
  grp <- dt1[, .N, by = eval(YVar)][order(N)]
  smaller_class <- grp[1L, 1L][[1L]]
  dt2 <- round(100 * sapply(Cuts, function(x) {
    dt1[NegScore <= x & get(YVar) == eval(smaller_class), .N] / dt1[get(YVar) == eval(smaller_class), .N]
  }), 2)
  dt3 <- rbind(dt2, -Cuts)
  rownames(dt3) <- c("Gain", "Score.Point")
  dt4 <- grp[1,2] / (grp[2,2] + grp[1,2])
  dt5 <- data.table::as.data.table(t(dt3))
  dt5[, Population := as.numeric(100 * eval(NumberBins))]
  dt5[, Lift := round(Gain / 100 / NumberBins, 2)]
  dt6 <- data.table::rbindlist(list(
    data.table::data.table(Gain = 0, Score.Point = 0, Population = 0, Lift = 0),
    dt5
  ))

  # Build
  p1 <- AutoPlots::Plot.Line(
    dt = dt6[2L:nrow(dt6)],
    PreAgg = TRUE,
    XVar = "Population",
    YVar = "Lift",
    GroupVar = NULL,
    Title = Title,
    Area = TRUE,
    Engine = Engine,
    EchartsTheme = EchartsTheme,
    TimeLine = TRUE,
    X_Scroll = X_Scroll,
    Y_Scroll = Y_Scroll,
    BackGroundColor = BackGroundColor,
    ChartColor = ChartColor,
    FillColor = FillColor,
    GridColor = GridColor,
    TextColor = TextColor,
    ZeroLineColor = GridColor,
    ZeroLineWidth = 1.25,
    Debug = FALSE)
  g <- class(p1)[1L]
  if(g == "plotly") {
    p1 <- plotly::layout(p = p1, uniformtext = list(minsize=8, mode='hide', color="white"))
  } else if(g == "echarts4r") {
    p1 <- echarts4r::e_labels(e = p1, show = TRUE)
  }

  # Return
  return(p1)
}

#' @title Plot.Gains
#'
#' @description Create a cumulative gains chart
#'
#' @family Model Evaluation
#'
#' @author Adrian Antico
#'
#' @param dt data.table
#' @param XVar character
#' @param YVar character
#' @param ZVar character
#' @param GroupVar character
#' @param NumberBins numeric
#' @param PreAgg logical
#' @param NumberBins numeric
#' @param Title character
#' @param Engine character
#' @param EchartsTheme character
#' @param TimeLine logical
#' @param X_Scroll logical
#' @param Y_Scroll logical
#' @param BackGroundColor character hex
#' @param ChartColor character hex
#' @param FillColor character hex
#' @param FillColorReverse character hex
#' @param GridColor character hex
#' @param TextColor character hex
#' @param ZeroLineColor character hex
#' @param ZeroLineWidth numeric
#' @param Debug logical
#'
#' @export
Plot.Gains <- function(dt = NULL,
                       PreAgg = FALSE,
                       XVar = NULL,
                       YVar = NULL,
                       ZVar = "N",
                       GroupVar = NULL,
                       NumberBins = 20,
                       Title = "Gains Plot",
                       Engine = 'Plotly',
                       EchartsTheme = "macaron",
                       TimeLine = TRUE,
                       X_Scroll = TRUE,
                       Y_Scroll = TRUE,
                       BackGroundColor =  "#6a6969",
                       ChartColor =       "#001534",
                       FillColor =        "#0066ff",
                       FillColorReverse = "#97ff00",
                       GridColor =        "white",
                       TextColor =        "white",
                       ZeroLineColor = '#ffff',
                       ZeroLineWidth = 1.25,
                       Debug = FALSE) {

  # Data prep
  dt1 <- dt[, .SD, .SDcols = c(XVar,YVar)]
  dt1[, NegScore := -get(XVar)]
  NumberBins <- c(seq(1/NumberBins, 1 - 1/NumberBins, 1/NumberBins), 1)
  Cuts <- quantile(x = dt1[["NegScore"]], probs = NumberBins)
  dt1[, eval(YVar) := as.character(get(YVar))]
  grp <- dt1[, .N, by = eval(YVar)][order(N)]
  smaller_class <- grp[1L, 1L][[1L]]
  dt2 <- round(100 * sapply(Cuts, function(x) {
    dt1[NegScore <= x & get(YVar) == eval(smaller_class), .N] / dt1[get(YVar) == eval(smaller_class), .N]
  }), 2)
  dt3 <- rbind(dt2, -Cuts)
  rownames(dt3) <- c("Gain", "Score.Point")
  dt4 <- grp[1L,2L] / (grp[2L,2L] + grp[1L,2L])
  dt5 <- data.table::as.data.table(t(dt3))
  dt5[, Population := as.numeric(100 * eval(NumberBins))]
  dt5[, Lift := round(Gain / 100 / NumberBins, 2)]
  dt6 <- data.table::rbindlist(list(
    data.table::data.table(Gain = 0, Score.Point = 0, Population = 0, Lift = 0),
    dt5
  ))

  # Build
  p1 <- AutoPlots::Plot.Line(
    dt = dt6[2L:nrow(dt6)],
    PreAgg = TRUE,
    XVar = "Population",
    YVar = "Gain",
    GroupVar = NULL,
    Title = Title,
    Area = TRUE,
    Engine = Engine,
    EchartsTheme = EchartsTheme,
    TimeLine = TimeLine,
    X_Scroll = X_Scroll,
    Y_Scroll = Y_Scroll,
    BackGroundColor = BackGroundColor,
    ChartColor = ChartColor,
    FillColor = FillColor,
    GridColor = GridColor,
    TextColor = TextColor,
    ZeroLineColor = '#ffff',
    ZeroLineWidth = 1.25,
    Debug = FALSE)
  g <- class(p1)[1L]
  if(g == "plotly") {
    p1 <- plotly::layout(p = p1, uniformtext = list(minsize=8, mode='hide', color="white"))
  } else if(g == "echarts4r") {
    p1 <- echarts4r::e_labels(e = p1, show = TRUE)
  }

  # Return
  return(p1)
}

#' @title Plot.PartialDependence.Line
#'
#' @description This function automatically builds partial dependence calibration plots
#'
#' @author Adrian Antico
#' @family Model Evaluation
#'
#' @param dt data.table
#' @param XVar character
#' @param YVar character
#' @param ZVar character
#' @param NumberBins numeric
#' @param Title character
#' @param Engine character
#' @param EchartsTheme character
#' @param EchartsLabels character
#' @param TimeLine logical
#' @param X_Scroll = TRUE,
#' @param Y_Scroll = TRUE,
#' @param BackGroundColor hex character
#' @param ChartColor hex character
#' @param FillColor hex character
#' @param FillColorReverse hex character
#' @param GridColor hex character
#' @param TextColor hex character
#' @param ZeroLineColor hex character
#' @param ZeroLineWidth numeric
#' @param AggMethod character
#' @param GroupVar character
#' @param Debug logical
#'
#' @return Partial dependence calibration plot
#' @examples
#' \dontrun{
#' # Query postgres
#' data <- AutoPlots::DM.pgQuery(
#'   Host = 'localhost',
#'   DataBase = 'KompsProcessed',
#'   SELECT = c('ARTICLE','BRAND','CHILLED_Liters_PerDay','CHILLED_Margin_PerDay','CHILLED_Net_Revenue_PerDay','CHILLED_Units_PerDay','CUSTOMER_COD_char','DATE_ISO'),
#'   AggStat = 'AVG',
#'   FROM = 'POS_Processed_Long_Daily_backward',
#'   GroupBy = NULL,
#'   SamplePercent = 1,
#'   User = 'postgres',
#'   Port = 5432,
#'   Password = 'Aa')
#'
#' # # Step Through Function
#' # library(AutoPlots)
#' # library(data.table)
#' # dt = data
#' # SampleSize = 100000L
#' # PlotEngineType = Engine =  "Echarts" # "Plotly"
#' # TimeLine = FALSE
#' # EchartsLabels = FALSE
#' # EchartsTheme = "purple-passion"
#' # X_Scroll = TRUE,
#' # Y_Scroll = TRUE,
#' # XVar = "CHILLED_Liters_PerDay" # "Predict"
#' # YVar = "CHILLED_Margin_PerDay"
#' # ZVar = "CHILLED_Units_PerDay"
#' # GroupVar = NULL
#' # AggMethod = 'mean'
#' # GroupVar = NULL # "BRAND"
#' # NumberBins = 20
#' # AggStat = "mean"
#' # ZeroLineColor = '#ffff'
#' # ZeroLineWidth = 1.25
#' # Title = 'Bar Plot'
#' # FillColor = "#0066ff"
#' # BackGroundColor = "#6a6969"
#' # ChartColor = '#001534'
#' # GridColor = 'white'
#' # TextColor = 'white'
#' # Debug = FALSE
#' }
#' @export
Plot.PartialDependence.Line <- function(dt = NULL,
                                        XVar = NULL,
                                        YVar = NULL,
                                        ZVar = NULL,
                                        GroupVar = NULL,
                                        NumberBins = 20,
                                        AggMethod = "mean",
                                        Title = "Gains Plot",
                                        Engine = 'Plotly',
                                        EchartsTheme = "macaron",
                                        EchartsLabels = FALSE,
                                        TimeLine = TRUE,
                                        X_Scroll = TRUE,
                                        Y_Scroll = TRUE,
                                        BackGroundColor =  "#6a6969",
                                        ChartColor =       "#001534",
                                        FillColor =        "#0066ff",
                                        FillColorReverse = "#97ff00",
                                        GridColor =        "white",
                                        TextColor =        "white",
                                        ZeroLineColor = '#ffff',
                                        ZeroLineWidth = 1.25,
                                        Debug = FALSE) {

  # Minimize data before moving on
  if(Debug) print("Plot.PartialDependence.Line # Minimize data before moving on")
  Ncols <- ncol(dt)
  if(Ncols > 2L && length(GroupVar) == 0L) {
    dt1 <- data.table::copy(dt[, .SD, .SDcols = c(YVar, XVar, ZVar)])
  } else if(Ncols > 3L && length(GroupVar) > 0L) {
    dt1 <- data.table::copy(dt[, .SD, .SDcols = c(YVar, XVar, ZVar, GroupVar[1L])])
  } else {
    dt1 <- data.table::copy(dt)
  }

  # Define Aggregation function
  if(Debug) print("Plot.PartialDependence.Line # Define Aggregation function")
  aggFunc <- AutoPlots:::SummaryFunction(AggMethod)

  # If actual is in factor form, convert to numeric
  if(Debug) print("Plot.PartialDependence.Line # If actual is in factor form, convert to numeric")
  if(!is.numeric(dt1[[YVar]])) {
    data.table::set(dt1, j = YVar, value = as.numeric(as.character(dt1[[YVar]])))
  }

  # Data Mgt
  if(length(GroupVar) > 0L) {
    if(Debug) print("Plot.PartialDependence.Line # if(length(GroupVar) > 0L)")
    dt1[, eval(XVar) := round(data.table::frank(get(XVar)) * NumberBins / .N) / NumberBins, by = c(GroupVar[1L])]
    dt1 <- dt1[, lapply(.SD, noquote(aggFunc)), by = c(XVar,GroupVar[1L])]
    dt1[, `Target - Predicted` := get(YVar) - get(ZVar)]
    data.table::setorderv(x = dt1, cols = c(XVar,GroupVar[1L]), c(1L,1L))
    yvar <- "Target - Predicted"
    gv <- GroupVar
    tl <- TimeLine
  } else {
    if(Debug) print("Plot.PartialDependence.Line # if(length(GroupVar) == 0L)")
    dt1[, eval(XVar) := round(data.table::frank(get(XVar)) * NumberBins / .N) / NumberBins]
    dt1 <- dt1[, lapply(.SD, noquote(aggFunc)), by = eval(XVar)]
    dt1 <- data.table::melt.data.table(data = dt1, id.vars = eval(XVar), measure.vars = c(YVar,ZVar))
    data.table::setnames(dt1, names(dt1), c(XVar, "Variable", YVar))
    data.table::setorderv(x = dt1, cols = c(XVar,"Variable"), c(1L,1L))
    yvar <- YVar
    gv <- "Variable"
    tl <- FALSE
  }

  # Build
  if(Debug) print("Plot.PartialDependence.Line --> AutoPlots::Plot.Line()")
  p1 <- AutoPlots::Plot.Line(
    Area = FALSE,
    dt = dt1,
    PreAgg = TRUE,
    AggMethod = "mean",
    Engine = Engine,
    EchartsTheme = EchartsTheme,
    TimeLine = tl,
    XVar = XVar,
    YVar = yvar,
    GroupVar = gv,
    Title = paste0("X-Axis: ", XVar, " - every 5th percentile"),
    BackGroundColor = BackGroundColor,
    ChartColor = ChartColor,
    FillColor = FillColor,
    GridColor = GridColor,
    TextColor = TextColor,
    ZeroLineColor = GridColor,
    ZeroLineWidth = 1.25,
    X_Scroll = X_Scroll,
    Y_Scroll = Y_Scroll,
    Debug = Debug)
  return(p1)
}

#' @title Plot.PartialDependence.Box
#'
#' @description This function automatically builds partial dependence calibration plots
#'
#' @author Adrian Antico
#' @family Model Evaluation
#'
#' @param dt data.table
#' @param SampleSize numeric
#' @param XVar character
#' @param YVar character
#' @param ZVar character
#' @param NumberBins numeric
#' @param PreAgg logical
#' @param Title character
#' @param Engine character
#' @param EchartsTheme character
#' @param EchartsLabels character
#' @param TimeLine logical
#' @param X_Scroll logical
#' @param Y_Scroll logical
#' @param BackGroundColor hex character
#' @param ChartColor hex character
#' @param FillColor hex character
#' @param FillColorReverse hex character
#' @param GridColor hex character
#' @param TextColor hex character
#' @param ZeroLineColor hex character
#' @param ZeroLineWidth numeric
#' @param AggMethod character
#' @param GroupVar character
#' @param Debug logical
#'
#' @return Partial dependence calibration plot
#' @examples
#' \dontrun{
#' # Query postgres
#' data <- AutoPlots::DM.pgQuery(
#'   Host = 'localhost',
#'   DataBase = 'KompsProcessed',
#'   SELECT = c('ARTICLE','BRAND','CHILLED_Liters_PerDay','CHILLED_Margin_PerDay','CHILLED_Net_Revenue_PerDay','CHILLED_Units_PerDay','CUSTOMER_COD_char','DATE_ISO'),
#'   AggStat = 'AVG',
#'   FROM = 'POS_Processed_Long_Daily_backward',
#'   GroupBy = NULL,
#'   SamplePercent = 1,
#'   User = 'postgres',
#'   Port = 5432,
#'   Password = 'Aa')
#'
#' # # Step Through Function
#' # library(AutoPlots)
#' # library(data.table)
#' # PreAgg = FALSE
#' # dt = data
#' # SampleSize = 100000L
#' # PlotEngineType = Engine =  "Echarts" # "Plotly"
#' # TimeLine = FALSE
#' # EchartsLabels = FALSE
#' # EchartsTheme = "purple-passion"
#' # XVar = "CHILLED_Liters_PerDay" # "Predict"
#' # YVar = "CHILLED_Margin_PerDay"
#' # ZVar = "CHILLED_Units_PerDay"
#' # GroupVar = NULL
#' # AggMethod = 'mean'
#' # GroupVar = NULL # "BRAND"
#' # NumberBins = 20
#' # AggStat = "mean"
#' # ZeroLineColor = '#ffff'
#' # ZeroLineWidth = 1.25
#' # Title = 'Bar Plot'
#' # X_Scroll = TRUE,
#' # Y_Scroll = FALSE,
#' # FillColor = "#0066ff"
#' # BackGroundColor = "#6a6969"
#' # ChartColor = '#001534'
#' # GridColor = 'white'
#' # TextColor = 'white'
#' # Debug = FALSE
#' }
#' @export
Plot.PartialDependence.Box <- function(dt = NULL,
                                       PreAgg = FALSE,
                                       SampleSize = 100000L,
                                       XVar = NULL,
                                       YVar = NULL,
                                       ZVar = NULL,
                                       GroupVar = NULL,
                                       NumberBins = 20,
                                       AggMethod = "mean",
                                       Title = "Gains Plot",
                                       Engine = 'Plotly',
                                       EchartsTheme = "macaron",
                                       EchartsLabels = FALSE,
                                       TimeLine = TRUE,
                                       X_Scroll = TRUE,
                                       Y_Scroll = FALSE,
                                       BackGroundColor =  "#6a6969",
                                       ChartColor =       "#001534",
                                       FillColor =        "#0066ff",
                                       FillColorReverse = "#97ff00",
                                       GridColor =        "white",
                                       TextColor =        "white",
                                       ZeroLineColor = '#ffff',
                                       ZeroLineWidth = 1.25,
                                       Debug = FALSE) {

  # Minimize data before moving on
  if(Debug) print("Plot.PartialDependence.Box # Minimize data before moving on")
  Ncols <- ncol(dt)
  if(Ncols > 2L && length(GroupVar) == 0L) {
    dt1 <- data.table::copy(dt[, .SD, .SDcols = c(YVar, XVar, ZVar)])
  } else if(Ncols > 3L && length(GroupVar) > 0L) {
    dt1 <- data.table::copy(dt[, .SD, .SDcols = c(YVar, XVar, ZVar, GroupVar[1L])])
  } else {
    dt1 <- data.table::copy(dt)
  }

  # If actual is in factor form, convert to numeric
  if(Debug) print("Plot.PartialDependence.Box # If actual is in factor form, convert to numeric")
  if(!is.numeric(dt1[[YVar]])) {
    data.table::set(dt1, j = YVar, value = as.numeric(as.character(dt1[[YVar]])))
  }

  # Add a column that ranks predicted values
  if(length(GroupVar) > 0L) {
    if(Debug) print("Plot.PartialDependence.Box # if(length(GroupVar) > 0L)")
    dt1[, eval(XVar) := round(data.table::frank(get(XVar)) * (NumberBins) / .N) / NumberBins, by = c(GroupVar[1L])]
    dt1[, `Target - Predicted` := get(YVar) - get(ZVar)]
    data.table::setorderv(x = dt1, cols = c(XVar,GroupVar[1L]), c(1L,1L))
  } else {
    dt1[, eval(XVar) := round(data.table::frank(get(XVar)) * (NumberBins) / .N) / NumberBins]
    dt1[, `Target - Predicted` := get(YVar) - get(ZVar)]
    data.table::setorderv(x = dt1, cols = XVar, 1L)
  }

  # Build Plot
  tl <- if(length(GroupVar) == 0L) FALSE else TimeLine

  # Build
  if(Debug) print("Plot.PartialDependence.Box --> AutoPlots::Plot.Box()")
  p1 <- AutoPlots::Plot.Box(
    dt = dt1,
    SampleSize = SampleSize,
    XVar = XVar,
    YVar = "Target - Predicted",
    GroupVar = GroupVar,
    Title = paste0("X-Axis: ", XVar, " - every 5th percentile"),
    Engine = Engine,
    EchartsTheme = EchartsTheme,
    TimeLine = tl,
    BackGroundColor = BackGroundColor,
    ChartColor = ChartColor,
    FillColor = FillColor,
    FillColorReverse = FillColorReverse,
    GridColor = GridColor,
    TextColor = TextColor,
    X_Scroll = X_Scroll,
    Y_Scroll = Y_Scroll,
    ZeroLineColor = GridColor,
    ZeroLineWidth = 1.25,
    Debug = Debug)
  return(p1)
}

#' @title Plot.PartialDependence.Line
#'
#' @description This function automatically builds partial dependence calibration plots
#'
#' @author Adrian Antico
#' @family Model Evaluation
#'
#' @param dt data.table
#' @param XVar character
#' @param YVar character
#' @param ZVar character
#' @param NumberBins numeric
#' @param Title character
#' @param Engine character
#' @param EchartsTheme character
#' @param EchartsLabels character
#' @param TimeLine logical
#' @param X_Scroll = TRUE,
#' @param Y_Scroll = TRUE,
#' @param BackGroundColor hex character
#' @param ChartColor hex character
#' @param FillColor hex character
#' @param FillColorReverse hex character
#' @param GridColor hex character
#' @param TextColor hex character
#' @param ZeroLineColor hex character
#' @param ZeroLineWidth numeric
#' @param AggMethod character
#' @param GroupVar character
#' @param Debug logical
#'
#' @return Partial dependence calibration plot
#' @examples
#' \dontrun{
#' # Query postgres
#' data <- Rappture::DM.pgQuery(
#'   Host = 'localhost',
#'   DataBase = 'KompsProcessed',
#'   SELECT = c('ARTICLE','BRAND','CHILLED_Liters_PerDay','CHILLED_Margin_PerDay','CHILLED_Net_Revenue_PerDay','CHILLED_Units_PerDay','CUSTOMER_COD_char','DATE_ISO'),
#'   AggStat = 'AVG',
#'   FROM = 'POS_Processed_Long_Daily_backward',
#'   GroupBy = NULL,
#'   SamplePercent = 1,
#'   User = 'postgres',
#'   Port = 5432,
#'   Password = 'Aa')
#'
#' # # Step Through Function
#' # library(AutoPlots)
#' # library(data.table)
#' # dt = data
#' # SampleSize = 100000L
#' # PlotEngineType = Engine =  "Echarts" # "Plotly"
#' # TimeLine = FALSE
#' # EchartsLabels = FALSE
#' # EchartsTheme = "purple-passion"
#' # X_Scroll = TRUE,
#' # Y_Scroll = TRUE,
#' # XVar = "CHILLED_Liters_PerDay" # "Predict"
#' # YVar = "CHILLED_Margin_PerDay"
#' # ZVar = "CHILLED_Units_PerDay"
#' # GroupVar = NULL
#' # AggMethod = 'mean'
#' # GroupVar = NULL # "BRAND"
#' # NumberBins = 20
#' # AggStat = "mean"
#' # ZeroLineColor = '#ffff'
#' # ZeroLineWidth = 1.25
#' # Title = 'Bar Plot'
#' # FillColor = "#0066ff"
#' # BackGroundColor = "#6a6969"
#' # ChartColor = '#001534'
#' # GridColor = 'white'
#' # TextColor = 'white'
#' # Debug = FALSE
#' }
#' @export
Plot.PartialDependence.Heatmap <- function(dt = NULL,
                                           XVar = NULL,
                                           YVar = NULL,
                                           ZVar = NULL,
                                           GroupVar = NULL,
                                           NumberBins = 20,
                                           AggMethod = "mean",
                                           Title = "Gains Plot",
                                           Engine = 'Plotly',
                                           EchartsTheme = "macaron",
                                           EchartsLabels = FALSE,
                                           TimeLine = TRUE,
                                           X_Scroll = TRUE,
                                           Y_Scroll = TRUE,
                                           BackGroundColor =  "#6a6969",
                                           ChartColor =       "#001534",
                                           FillColor =        "#0066ff",
                                           FillColorReverse = "#97ff00",
                                           GridColor =        "white",
                                           TextColor =        "white",
                                           ZeroLineColor = '#ffff',
                                           ZeroLineWidth = 1.25,
                                           Debug = FALSE) {

  # Minimize data before moving on
  if(Debug) print("Plot.PartialDependence.HeatMap # Minimize data before moving on")
  Ncols <- ncol(dt)
  dt1 <- data.table::copy(dt[, .SD, .SDcols = c(YVar, XVar, ZVar)])

  if(Debug) print("Plot.PartialDependence.Line # Define Aggregation function")
  aggFunc <- AutoPlots:::SummaryFunction(AggMethod)
  if(Debug) print("Plot.PartialDependence.Line # if(length(GroupVar) == 0L)")
  for(i in seq_along(XVar)) dt1[, eval(XVar[i]) := round(data.table::frank(get(XVar[i])) * NumberBins / .N) / NumberBins]
  dt1 <- dt1[, lapply(.SD, noquote(aggFunc)), by = eval(XVar)]
  dt1[, `Target - Predicted` := get(YVar) - Predict]
  ZVar <- "Target - Predicted"
  YVar <- XVar[2L]
  XVar <- XVar[1L]

  data.table::setorderv(x = dt1, cols = c(XVar,YVar),c(1L,1L))
  for(i in c(XVar,YVar)) dt1[, eval(i) := as.character(get(i))]

  # Build
  if(Debug) print("Plot.PartialDependence.Line --> AutoPlots::Plot.Line()")
  p1 <- AutoPlots::Plot.HeatMap(
    dt = dt1,
    PreAgg = TRUE,
    AggMethod = "mean",
    Engine = Engine,
    EchartsTheme = EchartsTheme,
    XVar = XVar,
    YVar = YVar,
    ZVar = ZVar,
    Title = paste0("Y-Axis: ", YVar, " \nevery 5th percentile"),
    BackGroundColor = BackGroundColor,
    ChartColor = ChartColor,
    FillColor = FillColor,
    TextColor = TextColor,
    X_Scroll = X_Scroll,
    Y_Scroll = Y_Scroll,
    Debug = Debug)

  # dt = dt1
  # PreAgg = TRUE
  # AggMethod = "mean"
  # Engine = Engine
  # EchartsTheme = EchartsTheme
  # XVar = XVar
  # YVar = YVar
  # ZVar = ZVar
  # Title = paste0("X-Axis: ", XVar, " - every 5th percentile")
  # BackGroundColor = BackGroundColor
  # ChartColor = ChartColor
  # FillColor = FillColor
  # TextColor = TextColor
  # X_Scroll = X_Scroll
  # Y_Scroll = Y_Scroll

  return(p1)
}

#' @title Plot.BinaryMetrics
#'
#' @description Line plot of evaluation metrics across thresholds
#'
#' @author Adrian Antico
#' @family Model Evaluation
#'
#' @param dt data.table
#' @param SampleSize numeric
#' @param XVar character
#' @param YVar character
#' @param ZVar character
#' @param Metrics Multiple selection "Utility","MCC","Accuracy","F1_Score","F2_Score","F0.5_Score","ThreatScore","TPR","TNR","FNR","FPR","FDR","FOR"
#' @param NumberBins numeric
#' @param PreAgg logical
#' @param Title character
#' @param Engine character
#' @param EchartsTheme character
#' @param EchartsLabels character
#' @param TimeLine logical
#' @param X_Scroll logical
#' @param Y_Scroll logical
#' @param BackGroundColor hex character
#' @param ChartColor hex character
#' @param FillColor hex character
#' @param FillColorReverse hex character
#' @param GridColor hex character
#' @param TextColor hex character
#' @param ZeroLineColor hex character
#' @param ZeroLineWidth numeric
#' @param AggMethod character
#' @param GroupVar character
#' @param Debug logical
#'
#' @return Partial dependence calibration plot
#' @examples
#' \dontrun{
#' # Query postgres
#' data <- AutoPlots::DM.pgQuery(
#'   Host = 'localhost',
#'   DataBase = 'KompsProcessed',
#'   SELECT = c('ARTICLE','BRAND','CHILLED_Liters_PerDay','CHILLED_Margin_PerDay','CHILLED_Net_Revenue_PerDay','CHILLED_Units_PerDay','CUSTOMER_COD_char','DATE_ISO'),
#'   AggStat = 'AVG',
#'   FROM = 'POS_Processed_Long_Daily_backward',
#'   GroupBy = NULL,
#'   SamplePercent = 1,
#'   User = 'postgres',
#'   Port = 5432,
#'   Password = 'Aa')
#'
#' # # Step Through Function
#' # library(AutoPlots)
#' # library(data.table)
#' # PreAgg = FALSE
#' # dt = data
#' # SampleSize = 100000L
#' # PlotEngineType = Engine =  "Echarts" # "Plotly"
#' # TimeLine = FALSE
#' # EchartsLabels = FALSE
#' # EchartsTheme = "purple-passion"
#' # XVar = "CHILLED_Liters_PerDay" # "Predict"
#' # YVar = "CHILLED_Margin_PerDay"
#' # ZVar = "CHILLED_Units_PerDay"
#' # GroupVar = NULL
#' # AggMethod = 'mean'
#' # GroupVar = NULL # "BRAND"
#' # NumberBins = 20
#' # AggStat = "mean"
#' # ZeroLineColor = '#ffff'
#' # ZeroLineWidth = 1.25
#' # Title = 'Bar Plot'
#' # X_Scroll = TRUE,
#' # Y_Scroll = FALSE,
#' # FillColor = "#0066ff"
#' # BackGroundColor = "#6a6969"
#' # ChartColor = '#001534'
#' # GridColor = 'white'
#' # TextColor = 'white'
#' # Debug = FALSE
#' }
#' @export
Plot.BinaryMetrics <- function(dt = NULL,
                               PreAgg = FALSE,
                               AggMethod = "mean",
                               SampleSize = 100000L,
                               XVar = NULL,
                               YVar = NULL,
                               ZVar = NULL,
                               Metrics = c("Utility","MCC","Accuracy","F1_Score","F2_Score","F0.5_Score","ThreatScore","TPR","TNR","FNR","FPR","FDR","FOR"),
                               GroupVar = NULL,
                               CostMatrixWeights = c(0,1,1,0),
                               NumberBins = 20,
                               Title = "Binary Metrics",
                               Engine = 'Plotly',
                               EchartsTheme = "macaron",
                               EchartsLabels = FALSE,
                               TimeLine = TRUE,
                               X_Scroll = TRUE,
                               Y_Scroll = FALSE,
                               BackGroundColor =  "#6a6969",
                               ChartColor =       "#001534",
                               FillColor =        "#0066ff",
                               FillColorReverse = "#97ff00",
                               GridColor =        "white",
                               TextColor =        "white",
                               ZeroLineColor = '#ffff',
                               ZeroLineWidth = 1.25,
                               Debug = FALSE) {

  # Minimize data before moving on
  if(Debug) print("Plot.PartialDependence.Box # Minimize data before moving on")
  Ncols <- ncol(dt)
  if(Ncols > 2L && length(GroupVar) == 0L) {
    dt1 <- data.table::copy(dt[, .SD, .SDcols = c(YVar, XVar, ZVar)])
  } else if(Ncols > 3L && length(GroupVar) > 0L) {
    dt1 <- data.table::copy(dt[, .SD, .SDcols = c(YVar, XVar, ZVar, GroupVar[1L])])
  } else {
    dt1 <- data.table::copy(dt)
  }

  # If actual is in factor form, convert to numeric
  if(Debug) print("Plot.PartialDependence.Box # If actual is in factor form, convert to numeric")
  if(!is.numeric(dt1[[YVar]])) {
    data.table::set(dt1, j = YVar, value = as.numeric(as.character(dt1[[YVar]])))
  }

  # Build Plot
  tl <- if(length(GroupVar) == 0L) FALSE else TimeLine
  dt2 <- AutoQuant:::BinaryMetrics(
    ValidationData. = dt1,
    TargetColumnName. = "BinaryTarget",
    CostMatrixWeights. = CostMatrixWeights,
    SaveModelObjects. = FALSE)
  dt3 <- data.table::melt.data.table(
    data = dt2,
    id.vars = "Threshold",
    measure.vars = Metrics)

  # Build
  if(Debug) print("AutoPlots::Plot.BinaryMetrics --> AutoPlots::Plot.Line()")
  p1 <- AutoPlots::Plot.Line(
    dt = dt3,
    PreAgg = TRUE,
    AggMethod = "mean",
    Area = FALSE,
    SampleSize = SampleSize,
    XVar = XVar,
    YVar = YVar,
    GroupVar = GroupVar,
    Title = Title,
    Engine = Engine,
    EchartsTheme = EchartsTheme,
    TimeLine = tl,
    BackGroundColor = BackGroundColor,
    ChartColor = ChartColor,
    FillColor = FillColor,
    FillColorReverse = FillColorReverse,
    GridColor = GridColor,
    TextColor = TextColor,
    X_Scroll = X_Scroll,
    Y_Scroll = Y_Scroll,
    ZeroLineColor = GridColor,
    ZeroLineWidth = 1.25,
    Debug = Debug)
  return(p1)
}

#' @title Plot.ShapImportance
#'
#' @description Plot.ShapImportance variable importance
#'
#' @family Model Evaluation
#' @author Adrian Antico
#'
#' @param dt Source data.table
#' @param Engine "plotly", "echarts4r"
#' @param EchartsTheme "dark-blue"
#' @param AggMethod "mean", "median", "sum", "sd", "skewness","kurtosis", "coeffvar", "meanabs", "medianabs", "sumabs", "sdabs", "skewnessabs", "kurtosisabs", "CoeffVarabs"
#' @param NumberBins = 21
#' @param NumLevels_Y = 20
#' @param NumLevels_X = 20
#' @param Title "Heatmap"
#' @param BackGroundColor = "#6a6969"
#' @param ChartColor = '#001534'
#' @param FillColor = "#0066ff"
#' @param FillColorReverse character hex
#' @param GridColor = '#ffffff'
#' @param Debug = FALSE
#'
#' @export
Plot.ShapImportance <- function(dt,
                                PreAgg = FALSE,
                                AggMethod = 'meanabs',
                                YVar = NULL,
                                GroupVar = NULL,
                                NumberBins = 21,
                                NumLevels_X = 33,
                                NumLevels_Y = 33,
                                Title = "Heatmap",
                                Engine = "Plotly",
                                EchartsTheme = "dark",
                                X_Scroll = TRUE,
                                Y_Scroll = TRUE,
                                BackGroundColor =  "#6a6969",
                                ChartColor =       "#001534",
                                FillColor =        "#0066ff",
                                FillColorReverse = "#97ff00",
                                GridColor =        "white",
                                TextColor =        "white",
                                Debug = FALSE) {

  if(Debug) print("ShapImportance Step 1")

  # Subset columns
  if(!PreAgg) {
    print("ShapImportance Step 2")
    if(length(GroupVar) > 1L) GroupVar <- GroupVar[1L]
    if(length(YVar) == 0L) YVar <- names(dt)[names(dt) %like% "Shap_"]
    dt1 <- dt[, .SD, .SDcols = c(YVar, GroupVar)]

    # Define Aggregation function
    if(Debug) print("Plot.ShapImportance # Define Aggregation function")
    if(Debug) print(AggMethod)
    aggFunc <- AutoPlots:::SummaryFunction(AggMethod)

    if(length(GroupVar) > 0L) {
      dt1 <- dt1[, lapply(.SD, FUN = noquote(aggFunc)), by = c(GroupVar)]
      dt2 <- data.table::melt.data.table(data = dt1, id.vars = c(GroupVar), measure.vars = YVar, variable.name = "Variable", value.name = "Importance")
    } else {
      dt1 <- dt1[, lapply(.SD, FUN = noquote(aggFunc))]
      dt2 <- data.table::melt.data.table(data = dt1, id.vars = NULL, measure.vars = YVar, variable.name = "Variable", value.name = "Importance")
    }
  } else {
    dt2 <- data.table::copy(dt)
  }

  # Add a column that ranks predicted values
  if(length(GroupVar) > 0L) {
    p1 <- AutoPlots::Plot.HeatMap(
      dt = dt2,
      PreAgg = TRUE,
      AggMethod = "mean",
      YVar = "Variable",
      XVar = GroupVar,
      ZVar = "Importance",
      NumberBins = 21,
      NumLevels_X = NumLevels_Y,
      NumLevels_Y = NumLevels_X,
      Title = paste0("Shap Importance: AggMethod = ", AggMethod),
      Engine = Engine,
      EchartsTheme = EchartsTheme,
      BackGroundColor = BackGroundColor,
      ChartColor = ChartColor,
      FillColor = FillColor,
      GridColor = GridColor,
      X_Scroll = X_Scroll,
      Y_Scroll = Y_Scroll)
    return(p1)
  } else {
    p1 <- AutoPlots::Plot.VariableImportance(
      Algo = "CatBoost",
      dt = dt2,
      AggMethod = 'mean',
      XVar = "Variable",
      YVar = "Importance",
      GroupVar = NULL,
      Title = paste0("Shap Importance: AggMethod = ", AggMethod),
      Engine = Engine,
      EchartsTheme = EchartsTheme,
      TimeLine = TimeLine,
      BackGroundColor = BackGroundColor,
      ChartColor = ChartColor,
      FillColor = FillColor,
      GridColor = GridColor,
      TextColor = TextColor,
      ZeroLineColor = GridColor,
      ZeroLineWidth = 1.25,
      Debug = Debug)
    return(p1)
  }
}

# ----

# ----

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
# > Experimental                                                              ----
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

#' @title PlotREsim
#'
#' @description Modified version of merTools::PlotREsim
#'
#' @author Adrian Antico
#' @family Model Evaluation
#'
#' @param data = AutoPlots::REsim(model),
#' @param level = 0.95
#' @param stat = "median
#' @param sd = TRUE
#' @param sigmaScale = NULL
#' @param oddsRatio = FALSE
#' @param labs = FALSE
#' @param facet = TRUE
#'
#' @examples
#' \dontrun{
#'
#' # Data, lmer model, output, plot
#' gpa <- data.table::fread(file = "C:/Users/Bizon/Documents/GitHub/gpa.csv")
#' model = lme4::lmer(gpa ~ occasion + (1 | student), data = gpa)
#' x <- AutoPlots::REsim(model)
#' AutoPlots::PlotREsim(x)
#' }
#'
#' @noRd
PlotREsim <- function(data = NULL,
                      level = 0.95,
                      stat = "median",
                      sd = TRUE,
                      sigmaScale = NULL,
                      oddsRatio = FALSE,
                      labs = FALSE,
                      facet = TRUE) {

  facet_logical <- is.logical(facet)
  if(!facet_logical) {
    data <- data[groupFctr == facet[[1]] & term == facet[[2]]]
  }
  if(!missing(sigmaScale)) {
    data[, sd := sd / sigmaScale]
    data[, eval(stat) := get(stat) / sigmaScale]
  }
  data[, sd := sd * qnorm(1 - ((1 - level)/2))]
  data[, ymax := get(stat) + sd]
  data[, ymin := get(stat) - sd]
  data[, sig := ymin > 0 | ymax < 0]
  hlineInt <- 0
  if(oddsRatio == TRUE) {
    data[, ymax := exp(ymax)]
    data[, eval(stat) := exp(get(stat))]
    data[, ymin := exp(ymin)]
    hlineInt <- 1
  }
  data.table::setorderv(data, c("groupFctr","term",stat), c(1,1,1))
  data[, xvar := factor(paste(groupFctr, groupID, sep = ""), levels = unique(paste(groupFctr, groupID, sep = "")), ordered = TRUE)]
  if(labs == TRUE) {
    xlabs.tmp <- ggplot2::element_text(
      face = "bold",
      angle = 90,
      vjust = 0.5)
  } else {
    data[, xvar := as.numeric(xvar)]
    xlabs.tmp <- ggplot2::element_blank()
  }
  p <- ggplot2::ggplot(data)
  p <- p + ggplot2::aes_string(x = "xvar", y = eval(stat), ymax = "ymax", ymin = "ymin")
  p <- p + AutoPlots::ChartTheme(ChartColor = "gray50")
  p <- p + ggplot2::geom_hline(yintercept = hlineInt, color = I("blue"), size = I(1.25))
  p <- p + ggplot2::geom_point(color = "purple", alpha = 1, size = I(2))
  p <- p + ggplot2::geom_point(data = subset(data, sig == TRUE), color = "yellow", size = I(3))
  p <- p + ggplot2::labs(x = "Group", y = "Effect Range", title = "Effect Ranges")
  p <- p + ggplot2::theme(axis.text.x = xlabs.tmp, axis.ticks.x = ggplot2::element_blank())

  # Add sd vertical lines to each point
  if(sd) {
    p <- p + ggplot2::geom_pointrange(alpha = 1/(nrow(data)^0.33)) + ggplot2::geom_pointrange(data = subset(data, sig == TRUE), alpha = 1)
  }

  if(facet_logical) p <- p + ggplot2::facet_grid(term ~ groupFctr, scales = "free_x")
  if(facet_logical) {
    return(eval(p))
  } else {
    return(eval(p))
  }
}
