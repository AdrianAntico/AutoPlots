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
#' @param BackGroundColor color outside of plot window. Rcolors and hex outside of plot window. Rcolors and hex character
#' @param LegendPosition Where to place legend
#' @param LegendBorderSize 0.50
#' @param LegendLineType 'solid'
#' @return An object to pass along to ggplot objects following the "+" sign
#' @noRd
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
#' @param dt source data.table
#' @param PreAgg FALSE
#' @param AggMethod character
#' @param SampleSize character
#' @param YVar Y-Axis variable name
#' @param XVar X-Axis variable name
#' @param ZVar Z-Axis variable name
#' @param GroupVar Character variable variable
#' @param YVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param XVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param ZVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param FacetRows Defaults to 1 which causes no faceting to occur vertically. Otherwise, supply a numeric value for the number of output grid rows
#' @param FacetCols Defaults to 1 which causes no faceting to occur horizontally. Otherwise, supply a numeric value for the number of output grid columns
#' @param FacetLevels Faceting rows x columns is the max number of levels allowed in a grid. If your GroupVar has more you can supply the levels to display.
#' @param NumberBins For histograms
#' @param Height NULL or valid css unit
#' @param Width NULL or valid css unit
#' @param PlotEngineType "Echarts" or "Plotly"
#' @param EchartsTheme "auritus","azul","bee-inspired","blue","caravan","carp","chalk","cool","dark-bold","dark","eduardo", #' "essos","forest","fresh-cut","fruit","gray","green","halloween","helianthus","infographic","inspired", #' "jazz","london","dark","macarons","macarons2","mint","purple-passion","red-velvet","red","roma","royal", #' "sakura","shine","tech-blue","vintage","walden","wef","weforum","westeros","wonderland"
#' @param TimeLine character
#' @param Title character
#' @param ShowLabels character
#' @param Title.YAxis character
#' @param Title.XAxis character
#' @param BackGroundColor color outside of plot window. Rcolors and hex outside of plot window. Rcolors and hex character
#' @param ChartColor character
#' @param FillColor character
#' @param FillColorReverse character
#' @param GridColor character
#' @param TextColor character
#' @param FontSize numeric
#' @param Debug Debugging purposes
#'
#' @export
Plot.StandardPlots <- function(dt = NULL,
                               PreAgg = FALSE,
                               PlotType = 'Scatter',
                               SampleSize = 100000L,
                               AggMethod = 'mean',
                               NumberBins = 30,
                               YVar = NULL,
                               XVar = NULL,
                               ZVar = NULL,
                               GroupVar = NULL,
                               YVarTrans = NULL,
                               XVarTrans = NULL,
                               ZVarTrans = NULL,
                               FacetRows = 1,
                               FacetCols = 1,
                               FacetLevels = NULL,
                               Height = NULL,
                               Width = NULL,
                               PlotEngineType = "Plotly",
                               EchartsTheme = "dark-blue",
                               TimeLine = FALSE,
                               Title = NULL,
                               ShowLabels = FALSE,
                               Title.YAxis = NULL,
                               Title.XAxis = NULL,
                               NumLevels_Y = 75,
                               NumLevels_X = 40,
                               BackGroundColor =  "#6a6969",
                               ChartColor =       "#001534",
                               FillColor =        "#0066ff",
                               FillColorReverse = "#97ff00",
                               GridColor =        "white",
                               TextColor =        "white",
                               FontSize = 14,
                               Debug = FALSE) {

  # Debug
  if(Debug) print(paste0('Plot.StandardPlots() begin, PlotType = ', PlotType))

  Title.FontSize <- FontSize + 8L

  # Pie Plot
  if(tolower(PlotType) == 'pieplot') {
    p1 <- AutoPlots:::Plot.Pie(
      dt = dt,
      PreAgg = PreAgg,
      AggMethod = AggMethod,
      XVar = if(length(XVar) == 0 && length(GroupVar) > 0L) GroupVar[1L] else XVar,
      YVar = YVar,
      GroupVar = NULL,
      YVarTrans = YVarTrans,
      XVarTrans = XVarTrans,
      FacetRows = FacetRows,
      FacetCols = FacetCols,
      FacetLevels = FacetLevels,
      Width = Width,
      Height = Height,
      Title = Title,
      ShowLabels = ShowLabels,
      Title.YAxis = Title.YAxis,
      Title.XAxis = Title.XAxis,
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
      title.fontSize = Title.FontSize,
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
      YVarTrans = YVarTrans,
      XVarTrans = XVarTrans,
      FacetRows = FacetRows,
      FacetCols = FacetCols,
      FacetLevels = FacetLevels,
      Width = Width,
      Height = Height,
      Title = Title,
      ShowLabels = ShowLabels,
      Title.YAxis = Title.YAxis,
      Title.XAxis = Title.XAxis,
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
      title.fontSize = Title.FontSize,
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
      YVarTrans = YVarTrans,
      XVarTrans = XVarTrans,
      FacetRows = FacetRows,
      FacetCols = FacetCols,
      FacetLevels = FacetLevels,
      Width = Width,
      Height = Height,
      Title = 'Violin Plot',
      FillColor = FillColor,
      ChartColor = ChartColor,
      BackGroundColor = BackGroundColor,
      TextColor = TextColor,
      GridColor = GridColor,
      title.fontSize = Title.FontSize,
      Debug = Debug)
    return(p1)
  }

  # Histogram Plot
  if(tolower(PlotType) == 'histogramplot') {
    p1 <- AutoPlots:::Plot.Histogram(
      dt = dt,
      SampleSize = SampleSize,
      XVar = XVar,
      YVar = YVar,
      GroupVar = GroupVar,
      YVarTrans = YVarTrans,
      XVarTrans = XVarTrans,
      FacetRows = FacetRows,
      FacetCols = FacetCols,
      FacetLevels = FacetLevels,
      NumberBins = NumberBins,
      Width = Width,
      Height = Height,
      Title = Title,
      ShowLabels = ShowLabels,
      Title.YAxis = Title.YAxis,
      Title.XAxis = Title.XAxis,
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
      title.fontSize = Title.FontSize,
      Debug = Debug)
    return(p1)
  }

  # Density Plot
  if(tolower(PlotType) == 'densityplot') {
    p1 <- AutoPlots:::Plot.Density(
      dt = dt,
      SampleSize = SampleSize,
      GroupVar=GroupVar,
      YVarTrans = YVarTrans,
      XVarTrans = XVarTrans,
      FacetRows = FacetRows,
      FacetCols = FacetCols,
      FacetLevels = FacetLevels,
      Title = Title,
      ShowLabels = ShowLabels,
      Title.YAxis = Title.YAxis,
      Title.XAxis = Title.XAxis,
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
      title.fontSize = Title.FontSize,
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
      YVarTrans = YVarTrans,
      XVarTrans = XVarTrans,
      FacetRows = FacetRows,
      FacetCols = FacetCols,
      FacetLevels = FacetLevels,
      Width = Width,
      Height = Height,
      Title = Title,
      ShowLabels = ShowLabels,
      Title.YAxis = Title.YAxis,
      Title.XAxis = Title.XAxis,
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
      title.fontSize = Title.FontSize,
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
      YVarTrans = YVarTrans,
      XVarTrans = XVarTrans,
      FacetRows = FacetRows,
      FacetCols = FacetCols,
      FacetLevels = FacetLevels,
      Width = Width,
      Height = Height,
      Title = Title,
      ShowLabels = ShowLabels,
      Title.YAxis = Title.YAxis,
      Title.XAxis = Title.XAxis,
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
      title.fontSize = Title.FontSize,
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
      YVarTrans = YVarTrans,
      XVarTrans = XVarTrans,
      FacetRows = FacetRows,
      FacetCols = FacetCols,
      FacetLevels = FacetLevels,
      Width = Width,
      Height = Height,
      Title = Title,
      ShowLabels = ShowLabels,
      Title.YAxis = Title.YAxis,
      Title.XAxis = Title.XAxis,
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
      title.fontSize = Title.FontSize,
      Debug = Debug)
    return(p1)
  }

  # River Plot
  if(tolower(PlotType) == 'riverplot') {
    p1 <- AutoPlots::Plot.River(
      dt = dt,
      PreAgg = PreAgg,
      AggMethod = AggMethod,
      XVar = XVar,
      YVar = YVar,
      GroupVar = GroupVar,
      YVarTrans = YVarTrans,
      XVarTrans = XVarTrans,
      FacetRows = FacetRows,
      FacetCols = FacetCols,
      FacetLevels = FacetLevels,
      Width = Width,
      Height = Height,
      Title = Title,
      ShowLabels = ShowLabels,
      Title.YAxis = Title.YAxis,
      Title.XAxis = Title.XAxis,
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
      title.fontSize = Title.FontSize,
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
      YVarTrans = YVarTrans,
      XVarTrans = XVarTrans,
      FacetRows = FacetRows,
      FacetCols = FacetCols,
      FacetLevels = FacetLevels,
      Width = Width,
      Height = Height,
      Title = Title,
      ShowLabels = ShowLabels,
      Title.YAxis = Title.YAxis,
      Title.XAxis = Title.XAxis,
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
      title.fontSize = Title.FontSize,
      Debug = Debug)
    return(p1)
  }

  # Stacked Bar Plot
  if(tolower(PlotType) == 'stackedbarplot') {
    p1 <- AutoPlots:::Plot.StackedBar(
      dt = dt,
      PreAgg = PreAgg,
      AggMethod = AggMethod,
      XVar = XVar,
      YVar = YVar,
      GroupVar = if(all(XVar == GroupVar)) NULL else GroupVar,
      YVarTrans = YVarTrans,
      XVarTrans = XVarTrans,
      FacetRows = FacetRows,
      FacetCols = FacetCols,
      FacetLevels = FacetLevels,
      Width = Width,
      Height = Height,
      Title = Title,
      ShowLabels = ShowLabels,
      Title.YAxis = Title.YAxis,
      Title.XAxis = Title.XAxis,
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
      title.fontSize = Title.FontSize,
      Debug = Debug)
    return(p1)
  }

  # 3D Bar Plot
  if(tolower(PlotType) %in% c('barplot3d','barplotd')) {
    p1 <- AutoPlots::Plot.BarPlot3D(
      PreAgg = PreAgg,
      dt = dt,
      YVar = YVar,
      XVar = XVar,
      ZVar = ZVar,
      YVarTrans = YVarTrans,
      XVarTrans = XVarTrans,
      ZVarTrans = ZVarTrans,
      FacetRows = FacetRows,
      FacetCols = FacetCols,
      FacetLevels = FacetLevels,
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
      Title = Title,
      ShowLabels = ShowLabels,
      Title.YAxis = Title.YAxis,
      Title.XAxis = Title.XAxis,
      BackGroundColor = BackGroundColor,
      ChartColor = ChartColor,
      FillColor = FillColor,
      GridColor = GridColor,
      TextColor = TextColor,
      title.fontSize = Title.FontSize,
      Debug = Debug)
    return(p1)
  }

  # Heat Map (Plotly & Echarts)
  if(tolower(PlotType) %in% 'heatmapplot') {
    p1 <- AutoPlots::Plot.HeatMap(
      PreAgg = PreAgg,
      dt = dt,
      YVar = YVar,
      XVar = XVar,
      ZVar = ZVar,
      YVarTrans = YVarTrans,
      XVarTrans = XVarTrans,
      ZVarTrans = ZVarTrans,
      FacetRows = FacetRows,
      FacetCols = FacetCols,
      FacetLevels = FacetLevels,
      AggMethod = AggMethod,
      NumberBins = 21,
      NumLevels_X = NumLevels_Y,
      NumLevels_Y = NumLevels_X,
      Width = Width,
      Height = Height,
      Engine = PlotEngineType,
      EchartsTheme = EchartsTheme,
      Title = Title,
      ShowLabels = ShowLabels,
      Title.YAxis = Title.YAxis,
      Title.XAxis = Title.XAxis,
      BackGroundColor = BackGroundColor,
      ChartColor = ChartColor,
      FillColor = FillColor,
      GridColor = GridColor,
      title.fontSize = Title.FontSize,
      Debug = Debug)
    return(p1)
  }

  # Correlation Matrix Plot
  if(tolower(PlotType) == 'correlogramplot') {
    p1 <- AutoPlots:::Plot.CorrMatrix(
      dt = dt,
      PreAgg = PreAgg,
      CorrVars = YVar,
      FacetRows = FacetRows,
      FacetCols = FacetCols,
      FacetLevels = FacetLevels,
      Width = Width,
      Height = Height,
      Engine = PlotEngineType,
      EchartsTheme = EchartsTheme,
      X_Scroll = TRUE,
      Y_Scroll = TRUE,
      Method = Title,
      ShowLabels = ShowLabels,
      Title.YAxis = Title.YAxis,
      Title.XAxis = Title.XAxis,
      BackGroundColor = BackGroundColor,
      ChartColor = ChartColor,
      FillColor = FillColor,
      GridColor = GridColor,
      TextColor = TextColor,
      title.fontSize = Title.FontSize,
      Debug = Debug)
    return(p1)
  }

  # Scatter Plot
  if(tolower(PlotType) %in% 'scatterplot') {
    if(SampleSize > 30000) SampleSize <- 30000
    p1 <- AutoPlots:::Plot.Scatter(
      dt = dt,
      SampleSize = SampleSize,
      XVar = XVar,
      YVar = YVar,
      GroupVar = if(all(XVar == GroupVar)) NULL else GroupVar,
      YVarTrans = YVarTrans,
      XVarTrans = XVarTrans,
      FacetRows = FacetRows,
      FacetCols = FacetCols,
      FacetLevels = FacetLevels,
      Width = Width,
      Height = Height,
      Title = Title,
      ShowLabels = ShowLabels,
      Title.YAxis = Title.YAxis,
      Title.XAxis = Title.XAxis,
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
      title.fontSize = Title.FontSize,
      Debug = Debug)
    return(p1)
  }

  # Copula Plot
  if(tolower(PlotType) %in% 'copulaplot') {
    if(SampleSize > 30000) SampleSize <- 30000
    p1 <- AutoPlots:::Plot.Copula(
      dt = dt,
      SampleSize = SampleSize,
      XVar = XVar,
      YVar = YVar,
      GroupVar = if(all(XVar == GroupVar)) NULL else GroupVar,
      YVarTrans = YVarTrans,
      XVarTrans = XVarTrans,
      FacetRows = FacetRows,
      FacetCols = FacetCols,
      FacetLevels = FacetLevels,
      Width = Width,
      Height = Height,
      Engine = PlotEngineType,
      EchartsTheme = EchartsTheme,
      TimeLine = TimeLine,
      X_Scroll = TRUE,
      Y_Scroll = TRUE,
      Title = Title,
      ShowLabels = ShowLabels,
      Title.YAxis = Title.YAxis,
      Title.XAxis = Title.XAxis,
      FillColor = FillColor,
      ChartColor = ChartColor,
      BackGroundColor = BackGroundColor,
      TextColor = TextColor,
      GridColor = GridColor,
      title.fontSize = Title.FontSize,
      Debug = Debug)
    return(p1)
  }

  # Scatter3D Plot
  if(tolower(PlotType) %in% c('scatterplot3d','scatterplotd')) {
    p1 <- AutoPlots:::Plot.Scatter3D(
      dt = dt,
      SampleSize = SampleSize,
      XVar = XVar,
      YVar = YVar,
      ZVar = ZVar,
      GroupVar = if(all(XVar == GroupVar)) NULL else GroupVar,
      YVarTrans = YVarTrans,
      XVarTrans = XVarTrans,
      ZVarTrans = ZVarTrans,
      FacetRows = FacetRows,
      FacetCols = FacetCols,
      FacetLevels = FacetLevels,
      Width = Width,
      Height = Height,
      Title = Title,
      ShowLabels = ShowLabels,
      Title.YAxis = Title.YAxis,
      Title.XAxis = Title.XAxis,
      Engine = PlotEngineType,
      EchartsTheme = EchartsTheme,
      TimeLine = TimeLine,
      FillColor = FillColor,
      ChartColor = ChartColor,
      BackGroundColor = BackGroundColor,
      TextColor = TextColor,
      GridColor = GridColor,
      title.fontSize = Title.FontSize,
      Debug = Debug)
    return(p1)
  }

  # Copula Plot
  if(tolower(PlotType) %in% c('copulaplot3d','copulaplotd')) {
    p1 <- AutoPlots:::Plot.Copula3D(
      dt = dt,
      SampleSize = SampleSize,
      XVar = XVar,
      YVar = YVar,
      ZVar = ZVar,
      GroupVar = if(all(XVar == GroupVar)) NULL else GroupVar,
      YVarTrans = YVarTrans,
      XVarTrans = XVarTrans,
      ZVarTrans = ZVarTrans,
      FacetRows = FacetRows,
      FacetCols = FacetCols,
      FacetLevels = FacetLevels,
      Width = Width,
      Height = Height,
      Title = Title,
      ShowLabels = ShowLabels,
      Title.YAxis = Title.YAxis,
      Title.XAxis = Title.XAxis,
      Engine = PlotEngineType,
      EchartsTheme = EchartsTheme,
      TimeLine = TimeLine,
      FillColor = FillColor,
      ChartColor = ChartColor,
      BackGroundColor = BackGroundColor,
      TextColor = TextColor,
      GridColor = GridColor,
      title.fontSize = Title.FontSize,
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
#' @param dt source data.table
#' @param AggMethod character
#' @param SampleSize 100000L
#' @param PlotEngineType "Echarts" or "Plotly"
#' @param EchartsTheme "auritus","azul","bee-inspired","blue","caravan","carp","chalk","cool","dark-bold","dark","eduardo", #' "essos","forest","fresh-cut","fruit","gray","green","halloween","helianthus","infographic","inspired", #' "jazz","london","dark","macarons","macarons2","mint","purple-passion","red-velvet","red","roma","royal", #' "sakura","shine","tech-blue","vintage","walden","wef","weforum","westeros","wonderland"
#' @param TimeLine logical
#' @param PlotType character
#' @param YVar Y-Axis variable name
#' @param XVar X-Axis variable name
#' @param ZVar Z-Axis variable name
#' @param GroupVar Character variable
#' @param YVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param XVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param ZVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param FacetRows Defaults to 1 which causes no faceting to occur vertically. Otherwise, supply a numeric value for the number of output grid rows
#' @param FacetCols Defaults to 1 which causes no faceting to occur horizontally. Otherwise, supply a numeric value for the number of output grid columns
#' @param FacetLevels Faceting rows x columns is the max number of levels allowed in a grid. If your GroupVar has more you can supply the levels to display.
#' @param NumLevels_Y = 75
#' @param NumLevels_X = 40
#' @param Height = NULL,
#' @param Width = NULL,
#' @param BackGroundColor color outside of plot window. Rcolors and hex outside of plot window. Rcolors and hex character
#' @param ChartColor hex
#' @param FillColor hex
#' @param FillColorReverse hex
#' @param GridColor hex
#' @param TextColor hex
#' @param NumberBins numeric
#' @param Debug Debugging purposes
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
                                  YVarTrans = "Identity",
                                  XVarTrans = "Identity",
                                  ZVarTrans = "Identity",
                                  FacetRows = 1,
                                  FacetCols = 1,
                                  FacetLevels = NULL,
                                  NumLevels_Y = 75,
                                  NumLevels_X = 40,
                                  Height = NULL,
                                  Width = NULL,
                                  Title = NULL,
                                  ShowLabels = FALSE,
                                  Title.YAxis = NULL,
                                  Title.XAxis = NULL,
                                  PlotEngineType = "Echarts",
                                  EchartsTheme = "dark-blue",
                                  TimeLine = FALSE,
                                  BackGroundColor =  "#6a6969",
                                  ChartColor =       "#001534",
                                  FillColor =        "#0066ff",
                                  FillColorReverse = "#97ff00",
                                  GridColor =        "white",
                                  TextColor =        "white",
                                  FontSize = 14L,
                                  NumberBins = 20,
                                  Debug = FALSE) {

  # Debugging
  if(Debug) {print('Running Plots.ModelEvaluation')}
  if(length(SampleSize) == 0L) SampleSize <- 30000L
  Title.FontSize = FontSize + 8L

  if(Debug) print(paste0("Plots.ModelEvaluation == ", PlotType))

  # Copula Plot
  if(PlotType %in% 'Residuals') {
    p1 <- AutoPlots::Plot.Residuals.Histogram(
      dt = dt,
      SampleSize = 50000L,
      XVar = XVar,
      YVar = YVar,
      GroupVar = GroupVar,
      YVarTrans = YVarTrans,
      XVarTrans = XVarTrans,
      FacetRows = FacetRows,
      FacetCols = FacetCols,
      FacetLevels = FacetLevels,
      Height = Height,
      Width = Width,
      NumberBins = NumberBins,
      Title = Title,
      ShowLabels = ShowLabels,
      Title.YAxis = Title.YAxis,
      Title.XAxis = Title.XAxis,
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
      title.fontSize = Title.FontSize,
      Debug = Debug)
    return(p1)
  }

  # ----
  # Residuals_2 Scatter Plot ----
  if(PlotType %chin% "ResidScatter") {
    p1 <- AutoPlots::Plot.Residuals.Scatter(
      dt = dt,
      SampleSize = min(SampleSize, 30000L),
      XVar = XVar,
      YVar = YVar,
      GroupVar = GroupVar,
      YVarTrans = YVarTrans,
      XVarTrans = XVarTrans,
      FacetRows = FacetRows,
      FacetCols = FacetCols,
      FacetLevels = FacetLevels,
      Height = Height,
      Width = Width,
      Title = Title,
      ShowLabels = ShowLabels,
      Title.YAxis = Title.YAxis,
      Title.XAxis = Title.XAxis,
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
    return(p1)
  }

  # ----

  # Evaluation Plot ----
  if(PlotType == "CalibrationLine") {
    p1 <- AutoPlots::Plot.Calibration.Line(
      dt = dt,
      Engine = PlotEngineType,
      EchartsTheme = EchartsTheme,
      TimeLine = TimeLine,
      XVar = XVar,
      YVar = YVar,
      GroupVar = GroupVar,
      YVarTrans = YVarTrans,
      XVarTrans = XVarTrans,
      FacetRows = FacetRows,
      FacetCols = FacetCols,
      FacetLevels = FacetLevels,
      Height = Height,
      Width = Width,
      AggMethod = AggMethod,
      NumberBins = 21,
      Title = Title,
      ShowLabels = ShowLabels,
      Title.YAxis = Title.YAxis,
      Title.XAxis = Title.XAxis,
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

  # Evaluation Heatmap ----
  if(PlotType == "CalibrationBox") {
    p1 <- AutoPlots::Plot.Calibration.Box(
      dt = dt,
      Engine = PlotEngineType,
      EchartsTheme = EchartsTheme,
      TimeLine = TimeLine,
      XVar = XVar,
      YVar = YVar,
      GroupVar = GroupVar,
      YVarTrans = YVarTrans,
      XVarTrans = XVarTrans,
      FacetRows = FacetRows,
      FacetCols = FacetCols,
      FacetLevels = FacetLevels,
      Height = Height,
      Width = Width,
      AggMethod = 'mean',
      NumberBins = 21,
      Title = Title,
      ShowLabels = ShowLabels,
      Title.YAxis = Title.YAxis,
      Title.XAxis = Title.XAxis,
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
  if(PlotType == "ROCPlot") {
    p1 <- tryCatch({AutoPlots::Plot.ROC(
      dt = dt,
      SampleSize = SampleSize,
      XVar = XVar,
      YVar = YVar,
      GroupVar = GroupVar,
      YVarTrans = YVarTrans,
      XVarTrans = XVarTrans,
      FacetRows = FacetRows,
      FacetCols = FacetCols,
      FacetLevels = FacetLevels,
      Height = Height,
      Width = Width,
      Title = Title,
      ShowLabels = ShowLabels,
      Title.YAxis = Title.YAxis,
      Title.XAxis = Title.XAxis,
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
      Debug = Debug)}, error = function(x) NULL)
    return(p1)
  }

  # ----

  # Gains Plot ----
  if(PlotType == "GainsPlot") {
    p1 <- AutoPlots::Plot.Gains(
      dt = dt,
      PreAgg = FALSE,
      XVar = XVar,
      YVar = YVar,
      ZVar = NULL,
      GroupVar = NULL,
      FacetLevels = FacetLevels,
      Height = Height,
      Width = Width,
      YVarTrans = YVarTrans,
      XVarTrans = XVarTrans,
      ZVarTrans = ZVarTrans,
      FacetRows = FacetRows,
      FacetCols = FacetCols,
      NumberBins = 20,
      Title = Title,
      ShowLabels = ShowLabels,
      Title.YAxis = Title.YAxis,
      Title.XAxis = Title.XAxis,
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
  if(PlotType == "LiftPlot") {
    p1 <- AutoPlots::Plot.Lift(
      dt = dt,
      PreAgg = FALSE,
      XVar = XVar,
      YVar = YVar,
      ZVar = NULL,
      GroupVar = NULL,
      YVarTrans = YVarTrans,
      XVarTrans = XVarTrans,
      ZVarTrans = ZVarTrans,
      FacetRows = FacetRows,
      FacetCols = FacetCols,
      FacetLevels = FacetLevels,
      Height = Height,
      Width = Width,
      NumberBins = 20,
      Title = Title,
      ShowLabels = ShowLabels,
      Title.YAxis = Title.YAxis,
      Title.XAxis = Title.XAxis,
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
  if(PlotType == "VariableImportance") {
    p1 <- AutoPlots::Plot.VariableImportance(
      dt = dt,
      AggMethod = 'mean',
      XVar = "Variable",
      YVar = "Importance",
      GroupVar = NULL,
      YVarTrans = YVarTrans,
      XVarTrans = XVarTrans,
      FacetRows = FacetRows,
      FacetCols = FacetCols,
      FacetLevels = FacetLevels,
      Height = Height,
      Width = Width,
      Title = Title,
      ShowLabels = ShowLabels,
      Title.YAxis = Title.YAxis,
      Title.XAxis = Title.XAxis,
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

  # Shap VI ----
  if(PlotType == 'ShapelyImportance') {
    p1 <- AutoPlots::Plot.ShapImportance(
      PreAgg = FALSE,
      dt = dt,
      YVar = NULL,
      GroupVar = GroupVar,
      YVarTrans = YVarTrans,
      FacetRows = FacetRows,
      FacetCols = FacetCols,
      FacetLevels = FacetLevels,
      Height = Height,
      Width = Width,
      AggMethod = AggMethod,
      NumberBins = 21,
      NumLevels_X = NumLevels_Y,
      NumLevels_Y = NumLevels_X,
      Engine = PlotEngineType,
      EchartsTheme = EchartsTheme,
      Title = Title,
      ShowLabels = ShowLabels,
      Title.YAxis = Title.YAxis,
      Title.XAxis = Title.XAxis,
      BackGroundColor = BackGroundColor,
      ChartColor = ChartColor,
      FillColor = FillColor,
      GridColor = GridColor)
    return(p1)
  }

  # ----

  # Confusion Matrix Heatmap ----
  if(PlotType == "ConfusionMatrixHeatmap") {
    p1 <- AutoPlots::Plot.ConfusionMatrix(
      dt = dt,
      Engine = PlotEngineType,
      EchartsTheme = EchartsTheme,
      TimeLine = TimeLine,
      XVar = XVar,
      YVar = YVar,
      ZVar = NULL,
      YVarTrans = YVarTrans,
      XVarTrans = XVarTrans,
      FacetRows = FacetRows,
      FacetCols = FacetCols,
      FacetLevels = FacetLevels,
      Height = Height,
      Width = Width,
      PreAgg = FALSE,
      NumberBins = 21,
      NumLevels_X = 50,
      NumLevels_Y = 50,
      Title = Title,
      ShowLabels = ShowLabels,
      Title.YAxis = Title.YAxis,
      Title.XAxis = Title.XAxis,
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
  if(PlotType == 'PartialDependenceLine' && length(XVar) > 0L) {
    p1 <- AutoPlots::Plot.PartialDependence.Line(
      dt = dt,
      XVar = XVar,
      YVar = YVar,
      ZVar = ZVar,
      GroupVar = GroupVar,
      YVarTrans = YVarTrans,
      XVarTrans = XVarTrans,
      ZVarTrans = ZVarTrans,
      FacetRows = FacetRows,
      FacetCols = FacetCols,
      FacetLevels = FacetLevels,
      Height = Height,
      Width = Width,
      AggMethod = 'mean',
      NumberBins = 21,
      Title = Title,
      ShowLabels = ShowLabels,
      Title.YAxis = Title.YAxis,
      Title.XAxis = Title.XAxis,
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
  if(PlotType == 'PartialDependenceHeatMap' && length(XVar) > 0L) {
    p1 <- tryCatch({AutoPlots::Plot.PartialDependence.HeatMap(
      dt = dt,
      AggMethod = 'mean',
      XVar = XVar,
      YVar = YVar,
      ZVar = ZVar,
      GroupVar = NULL,
      YVarTrans = YVarTrans,
      XVarTrans = XVarTrans,
      ZVarTrans = ZVarTrans,
      FacetRows = FacetRows,
      FacetCols = FacetCols,
      FacetLevels = FacetLevels,
      Height = Height,
      Width = Width,
      NumberBins = 21,
      Title = Title,
      ShowLabels = ShowLabels,
      Title.YAxis = Title.YAxis,
      Title.XAxis = Title.XAxis,
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
      Debug = Debug)}, error = function(x) NULL)
    return(p1)
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

#' @title Plot.Histogram
#'
#' @description Build a histogram plot by simply passing arguments to a single function. It will sample your data using SampleSize number of rows. Sampled data is randomized.
#'
#' @family Standard Plots
#'
#' @author Adrian Antico
#'
#' @param dt source data.table
#' @param SampleSize An integer for the number of rows to use. Sampled data is randomized. If NULL then ignored
#' @param YVar Y-Axis variable name
#' @param XVar X-Axis variable name
#' @param GroupVar Column name of Group Variable for distinct colored histograms by group levels
#' @param YVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param XVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param FacetRows Defaults to 1 which causes no faceting to occur vertically. Otherwise, supply a numeric value for the number of output grid rows
#' @param FacetCols Defaults to 1 which causes no faceting to occur horizontally. Otherwise, supply a numeric value for the number of output grid columns
#' @param FacetLevels Faceting rows x columns is the max number of levels allowed in a grid. If your GroupVar has more you can supply the levels to display.
#' @param NumberBins = 30
#' @param Height = NULL,
#' @param Width = NULL,
#' @param Engine "Echarts" or "Plotly"
#' @param EchartsTheme = EchartsTheme,
#' @param TimeLine logical
#' @param X_Scroll logical
#' @param Y_Scroll logical
#' @param FillColor 'gray'
#' @param FillColorReverse character
#' @param ChartColor 'lightsteelblue'
#' @param TextColor 'darkblue'
#' @param GridColor 'white'
#' @param BackGroundColor color outside of plot window. Rcolors and hex outside of plot window. Rcolors and hex character
#' @param ZeroLineWidth = 1.25,
#' @param ZeroLineColor = "white",
#' @param Debug Debugging purposes
#' @export
Plot.Histogram <- function(dt = NULL,
                           SampleSize = 30000L,
                           XVar = NULL,
                           YVar = NULL,
                           GroupVar = NULL,
                           YVarTrans = "Identity",
                           XVarTrans = "Identity",
                           FacetRows = 1,
                           FacetCols = 1,
                           FacetLevels = NULL,
                           NumberBins = 30,
                           Height = NULL,
                           Width = NULL,
                           Title = "Histogram",
                           ShowLabels = FALSE,
                           Title.YAxis = NULL,
                           Title.XAxis = NULL,
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
  if(length(YVar) == 0L) {
    YVar <- XVar
    YVarTrans <- XVarTrans
  }
  if(length(XVar) > 0L && length(GroupVar) == 0L) {
    GroupVar <- XVar
    XVar <- NULL
  }

  GroupVar <- tryCatch({GroupVar[1L]}, error = function(x) NULL)
  YVar <- tryCatch({YVar[1L]}, error = function(x) NULL)

  # Faceting shrink
  if(length(GroupVar) > 0L && (FacetRows > 1L || FacetCols > 1L)) {
    dt1 <- dt1[get(GroupVar) %in% c(eval(FacetLevels)), .SD, .SDcols = c(YVar,GroupVar)]
  } else {
    dt1 <- dt1[, .SD, .SDcols = c(YVar,GroupVar)]
  }

  # Transformation
  # "PercRank"  "Standardize"
  # "Asinh"  "Log"  "LogPlus1"  "Sqrt"  "Asin"  "Logit"  "BoxCox"  "YeoJohnson"
  if(YVarTrans != "Identity") {
    if(YVarTrans == "PercRank") {
      dt1 <- AutoQuant::PercRank(data = dt1, ColNames = YVar, GroupVars = GroupVar, Granularity = 0.0001, ScoreTable = FALSE)
    } else if(YVarTrans == "Standardize") {
      dt1 <- AutoQuant::Standardize(data = dt1, ColNames = YVar, GroupVars = GroupVar, Center = TRUE, Scale = TRUE, ScoreTable = FALSE)
    } else {
      dt1 <- tryCatch({AutoQuant::AutoTransformationCreate(data = dt1, ColumnNames = YVar, Methods = YVarTrans)$Data}, error = function(x) dt1)
    }
  }

  # Create base plot object
  if(Debug) print('Create Plot with only data')

  # Format
  if(Engine == "Plotly") {

    Width <- as.integer(gsub("[^\\d]+", "", Width, perl=TRUE))
    Height <- as.integer(gsub("[^\\d]+", "", Height, perl=TRUE))
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
      p1 <- echarts4r::e_charts_(
        dt1 |> dplyr::group_by(get(GroupVar)),
        timeline = TimeLine,
        dispose = TRUE,
        darkMode = TRUE,
        emphasis = list(focus = "series"),
        width = Width,
        height = Height)
    } else {
      p1 <- echarts4r::e_charts_(
        dt1,
        x = NULL,
        dispose = TRUE,
        darkMode = TRUE,
        emphasis = list(focus = "series"),
        width = Width,
        height = Height)
    }

    if(ShowLabels) {
      p1 <- echarts4r::e_histogram_(
        e = p1,
        YVar,
        breaks = NumberBins,
        bar_width = "100%",
        label = list(show = TRUE))
    } else {
      p1 <- echarts4r::e_histogram_(
        e = p1,
        YVar,
        breaks = NumberBins,
        bar_width = "100%")
    }

    if(FacetRows == 1L && FacetCols == 1L) {
      if(Y_Scroll) p1 <- echarts4r::e_datazoom(e = p1, y_Index = c(0,1))
    }
    p1 <- echarts4r::e_theme(e = p1, name = EchartsTheme)
    p1 <- echarts4r::e_aria(e = p1, enabled = TRUE)
    p1 <- echarts4r::e_tooltip(e = p1, trigger = "axis", backgroundColor = "aliceblue")
    p1 <- echarts4r::e_toolbox_feature(e = p1, feature = c("saveAsImage","dataZoom"))
    p1 <- echarts4r::e_show_loading(e = p1, hide_overlay = TRUE, text = "Calculating...", color = "#000", text_color = TextColor, mask_color = "#000")

    if(length(Title.XAxis) == 0L) {
      p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "x", name = XVar, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = xaxis.fontSize))
    } else {
      p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "x", name = Title.XAxis, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = xaxis.fontSize))
    }

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
    if(FacetRows > 1L || FacetCols > 1L) {
      p1 <- echarts4r::e_facet(
        e = p1,
        rows = FacetRows,
        cols = FacetCols,
        legend_space = 16,
        legend_pos = "top")
      p1 <- echarts4r::e_legend(e = p1, type = "scroll", orient = "horizontal", right = 50, top = 40, height = "240px", textStyle = list(color = TextColor, fontWeight = "bold"))
    } else {
      p1 <- echarts4r::e_legend(e = p1, type = "scroll", orient = "vertical", right = 50, top = 40, height = "240px", textStyle = list(color = TextColor, fontWeight = "bold"))
    }
  }
  return(p1)
}

#' @title Plot.Density
#'
#' @description Density plots, by groups, with transparent continuous plots
#'
#' @family Standard Plots
#'
#' @param dt source data.table
#' @param SampleSize = 100000L
#' @param GroupVar From App
#' @param YVar Y-Axis variable name
#' @param XVar X-Axis variable name
#' @param GroupVar Character variable
#' @param YVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param XVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param FacetRows Defaults to 1 which causes no faceting to occur vertically. Otherwise, supply a numeric value for the number of output grid rows
#' @param FacetCols Defaults to 1 which causes no faceting to occur horizontally. Otherwise, supply a numeric value for the number of output grid columns
#' @param FacetLevels Faceting rows x columns is the max number of levels allowed in a grid. If your GroupVar has more you can supply the levels to display.
#' @param Height = NULL,
#' @param Width = NULL,
#' @param Title = "Density Plot"
#' @param ShowLabels character
#' @param Title.YAxis character
#' @param Title.XAxis character
#' @param Engine "Echarts" or "Plotly"
#' @param EchartsTheme "auritus","azul","bee-inspired","blue","caravan","carp","chalk","cool","dark-bold","dark","eduardo", #' "essos","forest","fresh-cut","fruit","gray","green","halloween","helianthus","infographic","inspired", #' "jazz","london","dark","macarons","macarons2","mint","purple-passion","red-velvet","red","roma","royal", #' "sakura","shine","tech-blue","vintage","walden","wef","weforum","westeros","wonderland"
#' @param TimeLine logical
#' @param X_Scroll logical
#' @param Y_Scroll logical
#' @param BackGroundColor color outside of plot window. Rcolors and hex outside of plot window. Rcolors and hex character
#' @param ChartColor "#001534",
#' @param FillColor "#0066ff",
#' @param FillColorReverse "#97ff00",
#' @param GridColor "white",
#' @param TextColor "white",
#' @param Debug Debugging purposes
#' @export
Plot.Density <- function(dt = NULL,
                         SampleSize = 100000L,
                         YVar = NULL,
                         XVar = NULL,
                         GroupVar = NULL,
                         YVarTrans = "Identity",
                         XVarTrans = "Identity",
                         FacetRows = 1,
                         FacetCols = 1,
                         FacetLevels = NULL,
                         Height = NULL,
                         Width = NULL,
                         Title = "Density Plot",
                         ShowLabels = FALSE,
                         Title.YAxis = NULL,
                         Title.XAxis = NULL,
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
  if(length(YVar) == 0L) {
    YVar <- XVar
    YVarTrans <- XVarTrans
  }
  if(length(XVar) > 0L && length(GroupVar) == 0L) {
    GroupVar <- XVar
    XVar <- NULL
  }

  GroupVar <- tryCatch({GroupVar[1L]}, error = function(x) NULL)
  YVar <- tryCatch({YVar[1L]}, error = function(x) NULL)

  # Faceting shrink
  if(length(GroupVar) > 0L && (FacetRows > 1L || FacetCols > 1L)) {
    dt1 <- dt1[get(GroupVar) %in% c(eval(FacetLevels)), .SD, .SDcols = c(YVar,GroupVar)]
  } else {
    dt1 <- dt1[, .SD, .SDcols = c(YVar,GroupVar)]
  }

  # Transformation
  # "PercRank"  "Standardize"
  # "Asinh"  "Log"  "LogPlus1"  "Sqrt"  "Asin"  "Logit"  "BoxCox"  "YeoJohnson"
  if(YVarTrans != "Identity") {
    if(YVarTrans == "PercRank") {
      dt1 <- AutoQuant::PercRank(data = dt1, ColNames = YVar, GroupVars = GroupVar, Granularity = 0.0001, ScoreTable = FALSE)
    } else if(YVarTrans == "Standardize") {
      dt1 <- AutoQuant::Standardize(data = dt1, ColNames = YVar, GroupVars = GroupVar, Center = TRUE, Scale = TRUE, ScoreTable = FALSE)
    } else {
      dt1 <- AutoQuant::AutoTransformationCreate(data = dt1, ColumnNames = YVar, Methods = YVarTrans)$Data
    }
  }

  # Create base plot object
  if(Debug) print('Create Plot with only data')

  if(length(GroupVar) == 0L) {

    # Build plot
    if(Engine == "Plotly") {

      Width <- as.integer(gsub("[^\\d]+", "", Width, perl=TRUE))
      Height <- as.integer(gsub("[^\\d]+", "", Height, perl=TRUE))

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
      p1 <- echarts4r::e_charts_(
        dt1,
        x = NULL,
        dispose = TRUE,
        darkMode = TRUE,
        width = Width,
        height = Height)

      if(ShowLabels) {
        p1 <- echarts4r::e_density_(
          e = p1,
          YVar,
          areaStyle = list(opacity = .4),
          smooth = TRUE,
          y_index = 1,
          label = list(show = TRUE))
      } else {
        p1 <- echarts4r::e_density_(
          e = p1,
          YVar,
          areaStyle = list(opacity = .4),
          smooth = TRUE,
          y_index = 1)
      }

      if(FacetRows == 1L && FacetCols == 1L) {
        if(X_Scroll) p1 <- echarts4r::e_datazoom(e = p1, x_index = c(0,1))
        if(Y_Scroll) p1 <- echarts4r::e_datazoom(e = p1, y_Index = c(0,1))
      }
      p1 <- echarts4r::e_theme(e = p1, name = EchartsTheme)
      p1 <- echarts4r::e_aria(e = p1, enabled = TRUE)
      p1 <- echarts4r::e_tooltip(e = p1, trigger = "axis", backgroundColor = "aliceblue")
      p1 <- echarts4r::e_toolbox_feature(e = p1, feature = c("saveAsImage","dataZoom"))
      p1 <- echarts4r::e_show_loading(e = p1, hide_overlay = TRUE, text = "Calculating...", color = "#000", text_color = TextColor, mask_color = "#000")

      if(length(Title.XAxis) == 0L) {
        p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "x", name = XVar, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = xaxis.fontSize))
      } else {
        p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "x", name = Title.XAxis, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = xaxis.fontSize))
      }

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
      Height <- as.integer(gsub("[^\\d]+", "", Height, perl=TRUE))

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

      if(ShowLabels) {
        p1 <- echarts4r::e_charts_(
          dt1 |> dplyr::group_by(get(GroupVar[1L])),
          timeline = TimeLine,
          dispose = TRUE,
          darkMode = TRUE,
          emphasis = list(focus = "series"),
          width = Width,
          height = Height,
          label = list(show = TRUE))
      } else {
        p1 <- echarts4r::e_charts_(
          dt1 |> dplyr::group_by(get(GroupVar[1L])),
          timeline = TimeLine,
          dispose = TRUE,
          darkMode = TRUE,
          emphasis = list(focus = "series"),
          width = Width,
          height = Height)
      }

      p1 <- echarts4r::e_density_(e = p1, YVar, areaStyle = list(opacity = .4), smooth = TRUE, y_index = 1)
      if(FacetRows == 1L && FacetCols == 1L) {
        if(X_Scroll && length(GroupVar) == 0L) p1 <- echarts4r::e_datazoom(e = p1, x_index = c(0,1))
        if(Y_Scroll) p1 <- echarts4r::e_datazoom(e = p1, y_Index = c(0,1))
      }
      p1 <- echarts4r::e_theme(e = p1, name = EchartsTheme)
      p1 <- echarts4r::e_aria(e = p1, enabled = TRUE)
      p1 <- echarts4r::e_tooltip(e = p1, trigger = "axis", backgroundColor = "aliceblue")
      p1 <- echarts4r::e_toolbox_feature(e = p1, feature = c("saveAsImage","dataZoom"))
      p1 <- echarts4r::e_show_loading(e = p1, hide_overlay = TRUE, text = "Calculating...", color = "#000", text_color = TextColor, mask_color = "#000")

      if(length(Title.XAxis) == 0L) {
        p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "x", name = XVar, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = xaxis.fontSize))
      } else {
        p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "x", name = Title.XAxis, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = xaxis.fontSize))
      }

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
      if(FacetRows > 1L || FacetCols > 1L) {
        p1 <- echarts4r::e_facet(e = p1, rows = FacetRows, cols = FacetCols, legend_space = 16, legend_pos = "top")
        p1 <- echarts4r::e_legend(e = p1, type = "scroll", orient = "horizontal", right = 50, top = 40, height = "240px", textStyle = list(color = TextColor, fontWeight = "bold"))
      } else {
        p1 <- echarts4r::e_legend(e = p1, type = "scroll", orient = "vertical", right = 50, top = 40, height = "240px", textStyle = list(color = TextColor, fontWeight = "bold"))
      }

    }
    return(p1)
  }
}

#' @title Plot.Pie
#'
#' @description Build a pie chart by simply passing arguments to a single function
#'
#' @family Standard Plots
#'
#' @author Adrian Antico
#'
#' @param dt source data.table
#' @param PreAgg logical
#' @param YVar Y-Axis variable name
#' @param XVar X-Axis variable name
#' @param GroupVar Column name of Group Variable for distinct colored histograms by group levels
#' @param YVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param XVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param FacetRows Defaults to 1 which causes no faceting to occur vertically. Otherwise, supply a numeric value for the number of output grid rows
#' @param FacetCols Defaults to 1 which causes no faceting to occur horizontally. Otherwise, supply a numeric value for the number of output grid columns
#' @param FacetLevels Faceting rows x columns is the max number of levels allowed in a grid. If your GroupVar has more you can supply the levels to display.
#' @param AggMethod Choose from 'mean', 'sum', 'sd', and 'median'
#' @param Height = NULL,
#' @param Width = NULL,
#' @param Title title
#' @param ShowLabels character
#' @param Title.YAxis character
#' @param Title.XAxis character
#' @param Engine 'Plotly' or "Echarts"
#' @param EchartsTheme "auritus","azul","bee-inspired","blue","caravan","carp","chalk","cool","dark-bold","dark","eduardo","essos","forest","fresh-cut","fruit","gray","green","halloween","helianthus","infographic","inspired","jazz","london","dark","macarons","macarons2","mint","purple-passion","red-velvet","red","roma","royal","sakura","shine","tech-blue","vintage","walden","wef","weforum","westeros","wonderland"
#' @param TimeLine logical
#' @param X_Scroll logical
#' @param Y_Scroll logical
#' @param ZeroLineColor = '#ffff'
#' @param FillColor 'gray'
#' @param FillColorReverse character
#' @param ChartColor 'lightsteelblue'
#' @param TextColor 'darkblue'
#' @param GridColor 'white'
#' @param BackGroundColor color outside of plot window. Rcolors and hex outside of plot window. Rcolors and hex character
#' @param Debug Debugging purposes
#' @export
Plot.Pie <- function(Engine = 'Plotly',
                     dt = NULL,
                     PreAgg = FALSE,
                     XVar = NULL,
                     YVar = NULL,
                     GroupVar = NULL,
                     YVarTrans = "Identity",
                     XVarTrans = "Identity",
                     FacetRows = 1,
                     FacetCols = 1,
                     FacetLevels = NULL,
                     AggMethod = 'mean',
                     Height = NULL,
                     Width = NULL,
                     Title = 'Bar Plot',
                     ShowLabels = FALSE,
                     Title.YAxis = NULL,
                     Title.XAxis = NULL,
                     EchartsTheme = "macarons",
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
      numvars <- Rappture:::ColNameFilter(data = temp, Types = 'numeric')[[1L]]
      byvars <- Rappture:::ColNameFilter(data = temp, Types = "character")[[1L]]
    }

    yvar <- temp[[YVar]]
    xvar <- temp[[XVar]]

    # Transformation
    # "PercRank"  "Standardize"
    # "Asinh"  "Log"  "LogPlus1"  "Sqrt"  "Asin"  "Logit"  "BoxCox"  "YeoJohnson"
    if(YVarTrans != "Identity") {
      if(YVarTrans == "PercRank") {
        temp <- AutoQuant::PercRank(data = temp, ColNames = numvars, GroupVars = byvars, Granularity = 0.0001, ScoreTable = FALSE)
      } else if(YVarTrans == "Standardize") {
        temp <- AutoQuant::Standardize(data = temp, ColNames = numvars, GroupVars = byvars, Center = TRUE, Scale = TRUE, ScoreTable = FALSE)
      } else {
        temp <- AutoQuant::AutoTransformationCreate(data = temp, ColumnNames = numvars, Methods = YVarTrans)$Data
      }
    }

    # Plotly
    if(Engine == "Plotly") {

      Width <- as.integer(gsub("[^\\d]+", "", Width, perl=TRUE))
      Height <- as.integer(gsub("[^\\d]+", "", Height, perl=TRUE))

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
      p1 <- echarts4r::e_charts_(
        temp,
        x = XVar,
        dispose = TRUE,
        darkMode = TRUE,
        emphasis = list(focus = "series"),
        width = Width, height = Height)

      if(ShowLabels) {
        p1 <- echarts4r::e_pie_(e = p1, YVar, stack = XVar, label = list(show = TRUE))
      } else {
        p1 <- echarts4r::e_pie_(e = p1, YVar, stack = XVar)
      }

      if(FacetRows == 1L && FacetCols == 1L) {
        if(X_Scroll) p1 <- echarts4r::e_datazoom(e = p1, x_index = c(0,1))
        if(Y_Scroll) p1 <- echarts4r::e_datazoom(e = p1, y_Index = c(0,1))
      }
      p1 <- echarts4r::e_theme(e = p1, name = EchartsTheme)
      p1 <- echarts4r::e_aria(e = p1, enabled = TRUE)
      p1 <- echarts4r::e_tooltip(e = p1, trigger = "item", backgroundColor = "aliceblue")
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
#' @param dt source data.table
#' @param SampleSize numeric
#' @param YVar Y-Axis variable name
#' @param XVar X-Axis variable name
#' @param GroupVar Character variable
#' @param YVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param XVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param FacetRows Defaults to 1 which causes no faceting to occur vertically. Otherwise, supply a numeric value for the number of output grid rows
#' @param FacetCols Defaults to 1 which causes no faceting to occur horizontally. Otherwise, supply a numeric value for the number of output grid columns
#' @param FacetLevels Faceting rows x columns is the max number of levels allowed in a grid. If your GroupVar has more you can supply the levels to display.
#' @param Height = NULL,
#' @param Width = NULL,
#' @param Title character
#' @param ShowLabels character
#' @param Title.YAxis character
#' @param Title.XAxis character
#' @param Engine "Echarts" or "Plotly"
#' @param EchartsTheme "auritus","azul","bee-inspired","blue","caravan","carp","chalk","cool","dark-bold","dark","eduardo", #' "essos","forest","fresh-cut","fruit","gray","green","halloween","helianthus","infographic","inspired", #' "jazz","london","dark","macarons","macarons2","mint","purple-passion","red-velvet","red","roma","royal", #' "sakura","shine","tech-blue","vintage","walden","wef","weforum","westeros","wonderland"
#' @param TimeLine Logical
#' @param X_Scroll logical
#' @param Y_Scroll logical
#' @param BackGroundColor color outside of plot window. Rcolors and hex outside of plot window. Rcolors and hex character
#' @param ChartColor character hex
#' @param FillColor character hex
#' @param FillColorReverse character hex
#' @param GridColor character hex
#' @param TextColor character hex
#' @param ZeroLineColor character hex
#' @param ZeroLineWidth numeric
#' @param Debug Debugging purposes
#' @export
Plot.Box <- function(dt = NULL,
                     SampleSize = 100000L,
                     XVar = NULL,
                     YVar = NULL,
                     GroupVar = NULL,
                     YVarTrans = "Identity",
                     XVarTrans = "Identity",
                     FacetRows = 1,
                     FacetCols = 1,
                     FacetLevels = NULL,
                     Height = NULL,
                     Width = NULL,
                     Title = 'Box Plot',
                     ShowLabels = FALSE,
                     Title.YAxis = NULL,
                     Title.XAxis = NULL,
                     Engine = "Plotly",
                     EchartsTheme = "macarons",
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

  # Turn off Faceting until I can figure out how to supply it
  FacetRows <- 1L
  FacetCols <- 1L

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
    if(length(XVar) > 0L && class(dt1[[XVar]])[1L] %in% c("numeric","integer")) {
      YVarTrans <- XVarTrans
      YVar <- XVar
      XVar <- NULL
    }
  }

  if(length(GroupVar) > 0L) {
    dt1 <- dt1[get(GroupVar) %in% c(eval(FacetLevels)), .SD, .SDcols = c(YVar,XVar,GroupVar)]
  }

  # Transformation
  if(YVarTrans != "Identity") {
    if(YVarTrans == "PercRank") {
      dt1 <- AutoQuant::PercRank(data = dt1, ColNames = YVar, GroupVars = XVar, Granularity = 0.0001, ScoreTable = FALSE)
    } else if(YVarTrans == "Standardize") {
      dt1 <- AutoQuant::Standardize(data = dt1, ColNames = YVar, GroupVars = XVar, Center = TRUE, Scale = TRUE, ScoreTable = FALSE)
    } else {
      dt1 <- AutoQuant::AutoTransformationCreate(data = dt1, ColumnNames = YVar, Methods = YVarTrans)$Data
    }
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
      Height <- as.integer(gsub("[^\\d]+", "", Height, perl=TRUE))

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
      p1 <- echarts4r::e_charts_(
        dt1 |> dplyr::group_by(get(GroupVar[1L])),
        x = XVar,
        dispose = TRUE,
        darkMode = TRUE,
        emphasis = list(focus = "series"),
        width = Width,
        height = Height)

      if(ShowLabels) {
        p1 <- echarts4r::e_boxplot_(e = p1, YVar, label = list(show = TRUE))
      } else {
        p1 <- echarts4r::e_boxplot_(e = p1, YVar)
      }

      p1 <- echarts4r::e_visual_map_(e = p1, YVar, show = FALSE)
      if(FacetRows == 1L && FacetCols == 1L) {
        if(X_Scroll && length(GroupVar) == 0L) p1 <- echarts4r::e_datazoom(e = p1, x_index = c(0,1))
        if(Y_Scroll) p1 <- echarts4r::e_datazoom(e = p1, y_Index = c(0,1))
      }
      p1 <- echarts4r::e_theme(e = p1, name = EchartsTheme)
      p1 <- echarts4r::e_aria(e = p1, enabled = TRUE)
      p1 <- echarts4r::e_tooltip(e = p1, trigger = "item", backgroundColor = "aliceblue")
      p1 <- echarts4r::e_toolbox_feature(e = p1, feature = c("saveAsImage","dataZoom"))
      p1 <- echarts4r::e_show_loading(e = p1, hide_overlay = TRUE, text = "Calculating...", color = "#000", text_color = TextColor, mask_color = "#000")

      if(length(Title.XAxis) == 0L) {
        p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "x", name = XVar, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = xaxis.fontSize))
      } else {
        p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "x", name = Title.XAxis, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = xaxis.fontSize))
      }

      if(length(Title.YAxis) == 0L) {
        p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "y", name = YVar, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = yaxis.fontSize))
      } else {
        p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "y", name = Title.YAxis, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = yaxis.fontSize))
      }

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
      if(FacetRows > 1L || FacetCols > 1L) p1 <- echarts4r::e_facet(e = p1, rows = FacetRows, cols = FacetCols, legend_space = 16, legend_pos = "top")
      return(p1)
    }
  }

  # X,Y
  if(X_and_Y) {

    if(Debug) print("Plot.Box X_and_Y")
    if(Engine == "Plotly") {

      Width <- as.integer(gsub("[^\\d]+", "", Width, perl=TRUE))
      Height <- as.integer(gsub("[^\\d]+", "", Height, perl=TRUE))

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
      p1 <- echarts4r::e_charts_(
        dt1 |> dplyr::group_by(get(XVar)),
        x = YVar,
        darkMode = TRUE,
        dispose = TRUE,
        color = GroupVar,
        width = Width,
        height = Height)

      if(ShowLabels) {
        p1 <- echarts4r::e_boxplot_(e = p1, YVar, label = list(show = TRUE))
      } else {
        p1 <- echarts4r::e_boxplot_(e = p1, YVar)
      }

      p1 <- echarts4r::e_visual_map_(e = p1, YVar, show = FALSE)
      if(FacetRows == 1L && FacetCols == 1L) {
        if(X_Scroll && length(GroupVar) == 0L) p1 <- echarts4r::e_datazoom(e = p1, x_index = c(0,1))
        if(Y_Scroll) p1 <- echarts4r::e_datazoom(e = p1, y_Index = c(0,1))
      }
      p1 <- echarts4r::e_theme(e = p1, name = EchartsTheme)
      p1 <- echarts4r::e_aria(e = p1, enabled = TRUE)
      p1 <- echarts4r::e_tooltip(e = p1, trigger = "item", backgroundColor = "aliceblue")
      p1 <- echarts4r::e_toolbox_feature(e = p1, feature = c("saveAsImage","dataZoom"))
      p1 <- echarts4r::e_show_loading(e = p1, hide_overlay = TRUE, text = "Calculating...", color = "#000", text_color = TextColor, mask_color = "#000")

      if(length(Title.XAxis) == 0L) {
        p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "x", name = XVar, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = xaxis.fontSize))
      } else {
        p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "x", name = Title.XAxis, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = xaxis.fontSize))
      }

      if(length(Title.YAxis) == 0L) {
        p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "y", name = YVar, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = yaxis.fontSize))
      } else {
        p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "y", name = Title.YAxis, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = yaxis.fontSize))
      }

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
      if(FacetRows > 1L || FacetCols > 1L) p1 <- echarts4r::e_facet(e = p1, rows = FacetRows, cols = FacetCols, legend_space = 16, legend_pos = "top")

    }

    # Return
    return(p1)
  }

  # Y Only
  if(length(YVar) > 0L) {

    if(Debug) print("Plot.Box Y Only")

    if(Engine == "Plotly") {

      Width <- as.integer(gsub("[^\\d]+", "", Width, perl=TRUE))
      Height <- as.integer(gsub("[^\\d]+", "", Height, perl=TRUE))

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
      p1 <- echarts4r::e_charts_(
        dt1,
        dispose = TRUE,
        darkMode = TRUE,
        width = Width,
        height = Height)
      if(ShowLabels) {
        p1 <- echarts4r::e_boxplot_(e = p1, YVar, label = list(show = TRUE))
      } else {
        p1 <- echarts4r::e_boxplot_(e = p1, YVar)
      }
      p1 <- echarts4r::e_visual_map_(e = p1, YVar, show = FALSE)
      if(FacetRows == 1L && FacetCols == 1L) {
        if(X_Scroll) p1 <- echarts4r::e_datazoom(e = p1, x_index = c(0,1))
        if(Y_Scroll) p1 <- echarts4r::e_datazoom(e = p1, y_Index = c(0,1))
      }
      p1 <- echarts4r::e_theme(e = p1, name = EchartsTheme)
      p1 <- echarts4r::e_aria(e = p1, enabled = TRUE)
      p1 <- echarts4r::e_tooltip(e = p1, trigger = "item", backgroundColor = "aliceblue")
      p1 <- echarts4r::e_toolbox_feature(e = p1, feature = c("saveAsImage","dataZoom"))
      p1 <- echarts4r::e_show_loading(e = p1, hide_overlay = TRUE, text = "Calculating...", color = "#000", text_color = TextColor, mask_color = "#000")

      if(length(Title.XAxis) == 0L) {
        p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "x", name = XVar, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = xaxis.fontSize))
      } else {
        p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "x", name = Title.XAxis, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = xaxis.fontSize))
      }

      if(length(Title.YAxis) == 0L) {
        p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "y", name = YVar, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = yaxis.fontSize))
      } else {
        p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "y", name = Title.YAxis, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = yaxis.fontSize))
      }

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
      Height <- as.integer(gsub("[^\\d]+", "", Height, perl=TRUE))

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
      p1 <- echarts4r::e_charts_(
        dt1,
        dispose = TRUE,
        darkMode = TRUE,
        width = Width,
        height = Height)
      if(ShowLabels) {
        p1 <- echarts4r::e_boxplot_(e = p1, XVar, label = list(show = TRUE))
      } else {
        p1 <- echarts4r::e_boxplot_(e = p1, XVar)
      }
      p1 <- echarts4r::e_visual_map_(e = p1, XVar, show = FALSE)
      if(FacetRows == 1L && FacetCols == 1L) {
        if(X_Scroll) p1 <- echarts4r::e_datazoom(e = p1, x_index = c(0,1))
        if(Y_Scroll) p1 <- echarts4r::e_datazoom(e = p1, y_Index = c(0,1))
      }
      p1 <- echarts4r::e_theme(e = p1, name = EchartsTheme)
      p1 <- echarts4r::e_aria(e = p1, enabled = TRUE)
      p1 <- echarts4r::e_tooltip(e = p1, trigger = "item", backgroundColor = "aliceblue")
      p1 <- echarts4r::e_toolbox_feature(e = p1, feature = c("saveAsImage","dataZoom"))
      p1 <- echarts4r::e_show_loading(e = p1, hide_overlay = TRUE, text = "Calculating...", color = "#000", text_color = TextColor, mask_color = "#000")

      if(length(Title.XAxis) == 0L) {
        p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "x", name = XVar, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = xaxis.fontSize))
      } else {
        p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "x", name = Title.XAxis, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = xaxis.fontSize))
      }

      if(length(Title.YAxis) == 0L) {
        p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "y", name = YVar, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = yaxis.fontSize))
      } else {
        p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "y", name = Title.YAxis, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = yaxis.fontSize))
      }

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
#' @param dt source data.table
#' @param YVar Y-Axis variable name
#' @param XVar X-Axis variable name
#' @param GroupVar Requires an XVar and YVar already be defined
#' @param YVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param XVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param FacetRows Defaults to 1 which causes no faceting to occur vertically. Otherwise, supply a numeric value for the number of output grid rows
#' @param FacetCols Defaults to 1 which causes no faceting to occur horizontally. Otherwise, supply a numeric value for the number of output grid columns
#' @param FacetLevels Faceting rows x columns is the max number of levels allowed in a grid. If your GroupVar has more you can supply the levels to display.
#' @param Height = NULL,
#' @param Width = NULL,
#' @param Title 'Violin Plot'
#' @param ShowLabels character
#' @param Title.YAxis character
#' @param Title.XAxis character
#' @param SampleSize An integer for the number of rows to use. Sampled data is randomized. If NULL then ignored
#' @param FillColor 'gray'
#' @param FillColorReverse character
#' @param ZeroLineColor = '#ffff',
#' @param ZeroLineWidth = 2,
#' @param ChartColor 'lightsteelblue'
#' @param TextColor 'darkblue'
#' @param GridColor 'white'
#' @param BackGroundColor color outside of plot window. Rcolors and hex outside of plot window. Rcolors and hex character
#' @param Debug Debugging purposes
#' @export
Plot.Violin <- function(dt = NULL,
                        XVar = NULL,
                        YVar = NULL,
                        GroupVar = NULL,
                        YVarTrans = "Identity",
                        XVarTrans = "Identity",
                        FacetRows = 1,
                        FacetCols = 1,
                        FacetLevels = NULL,
                        Height = NULL,
                        Width = NULL,
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

  # Turn off Faceting until I can figure out how to supply it
  FacetRows <- 1L
  FacetCols <- 1L

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

  dt1 <- dt1[get(XVar) %in% eval(FacetLevels), .SD, .SDcols = c(YVar,XVar)]

  # Transformation
  # "PercRank"  "Standardize"
  # "Asinh"  "Log"  "LogPlus1"  "Sqrt"  "Asin"  "Logit"  "BoxCox"  "YeoJohnson"
  if(YVarTrans != "Identity") {
    if(YVarTrans == "PercRank") {
      dt1 <- AutoQuant::PercRank(data = dt1, ColNames = YVar, GroupVars = XVar, Granularity = 0.0001, ScoreTable = FALSE)
    } else if(YVarTrans == "Standardize") {
      dt1 <- AutoQuant::Standardize(data = dt1, ColNames = YVar, GroupVars = XVar, Center = TRUE, Scale = TRUE, ScoreTable = FALSE)
    } else {
      dt1 <- AutoQuant::AutoTransformationCreate(data = dt1, ColumnNames = YVar, Methods = YVarTrans)$Data
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
#' @param dt source data.table
#' @param PreAgg logical
#' @param AggMethod character
#' @param YVar Y-Axis variable name
#' @param XVar X-Axis variable name
#' @param GroupVar One Grouping Variable
#' @param YVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param XVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param FacetRows Defaults to 1 which causes no faceting to occur vertically. Otherwise, supply a numeric value for the number of output grid rows
#' @param FacetCols Defaults to 1 which causes no faceting to occur horizontally. Otherwise, supply a numeric value for the number of output grid columns
#' @param FacetLevels Faceting rows x columns is the max number of levels allowed in a grid. If your GroupVar has more you can supply the levels to display.
#' @param Height = NULL,
#' @param Width = NULL,
#' @param Title "Title"
#' @param ShowLabels character
#' @param Title.YAxis character
#' @param Title.XAxis character
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
#' @param BackGroundColor color outside of plot window. Rcolors and hex
#' @param ChartColor color
#' @param FillColor color
#' @param FillColorReverse character
#' @param GridColor color
#' @param TextColor "Not Implemented"
#' @param DarkMode FALSE
#' @param Debug Debugging purposes
#' @export
Plot.Line <- function(dt = NULL,
                      AggMethod = "mean",
                      PreAgg = TRUE,
                      Engine = 'Echarts',
                      XVar = NULL,
                      YVar = NULL,
                      GroupVar = NULL,
                      YVarTrans = "Identity",
                      XVarTrans = "Identity",
                      FacetRows = 1,
                      FacetCols = 1,
                      FacetLevels = NULL,
                      Height = NULL,
                      Width = NULL,
                      Title = 'Line Plot',
                      ShowLabels = FALSE,
                      Title.YAxis = NULL,
                      Title.XAxis = NULL,
                      EchartsTheme = "macarons",
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
                      DarkMode = FALSE,
                      Debug = FALSE) {

  if(TimeLine && length(FacetLevels) == 0L) X_Scroll <- FALSE
  if(length(GroupVar) == 0L) TimeLine <- FALSE
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
  } else if(length(GroupVar) > 0L) {
    if(Debug) print("Plot.Line() Ncols > 3L && length(GroupVar) > 0L")
    dt1 <- data.table::copy(dt[, .SD, .SDcols = c(YVar, XVar, GroupVar[1L])])
    if(length(FacetLevels) > 0) {
      dt1 <- dt1[get(GroupVar[1L]) %in% eval(FacetLevels)]
    }
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

  # Transformation
  if(YVarTrans != "Identity") {
    if(YVarTrans == "PercRank") {
      dt1 <- AutoQuant::PercRank(data = dt1, ColNames = YVar, GroupVars = GroupVar, Granularity = 0.0001, ScoreTable = FALSE)
    } else if(YVarTrans == "Standardize") {
      dt1 <- AutoQuant::Standardize(data = dt1, ColNames = YVar, GroupVars = GroupVar, Center = TRUE, Scale = TRUE, ScoreTable = FALSE)
    } else {
      dt1 <- AutoQuant::AutoTransformationCreate(data = dt1, ColumnNames = YVar, Methods = YVarTrans)$Data
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
        darkMode = TRUE,
        emphasis = list(focus = "series"),
        timeline = TimeLine, dispose = TRUE, width = Width, height = Height)

      # Finalize Plot Build
      if(Debug) print("Plot.Line() Build Echarts 4")
      if(ShowLabels) {
        p1 <- echarts4r::e_line_(e = p1, serie = YVar, smooth = Smooth, showSymbol = ShowSymbol, label = list(show = TRUE))
      } else {
        p1 <- echarts4r::e_line_(e = p1, serie = YVar, smooth = Smooth, showSymbol = ShowSymbol)
      }

      if(Debug) print("Plot.Line() Build Echarts 4 1")
      if(FacetRows == 1L && FacetCols == 1L) {
        if(Debug) print("Plot.Line() Build Echarts 4 2")
        if(X_Scroll && length(GroupVar) == 0L) p1 <- echarts4r::e_datazoom(e = p1, x_index = c(0,1))
        if(Y_Scroll) p1 <- echarts4r::e_datazoom(e = p1, y_Index = c(0,1))
      }
      if(Debug) print("Plot.Line() Build Echarts 5")
      p1 <- echarts4r::e_theme(e = p1, name = EchartsTheme)
      if(Debug) print("Plot.Line() Build Echarts 6")
      p1 <- echarts4r::e_aria(e = p1, enabled = TRUE)
      p1 <- echarts4r::e_tooltip(e = p1, trigger = "axis", backgroundColor = "aliceblue")
      p1 <- echarts4r::e_toolbox_feature(e = p1, feature = c("saveAsImage","dataZoom"))
      p1 <- echarts4r::e_show_loading(e = p1, hide_overlay = TRUE, text = "Calculating...", color = "#000", text_color = TextColor, mask_color = "#000")

      if(length(Title.XAxis) == 0L) {
        p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "x", name = XVar, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = xaxis.fontSize))
      } else {
        p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "x", name = Title.XAxis, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = xaxis.fontSize))
      }

      if(length(Title.YAxis) == 0L) {
        p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "y", name = YVar, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = yaxis.fontSize))
      } else {
        p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "y", name = Title.YAxis, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = yaxis.fontSize))
      }

      p1 <- echarts4r::e_brush(e = p1)
      if(Debug) print("Plot.Line() Build Echarts 6")
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
      if(Debug) print("Plot.Line() Build Echarts 8")
      if(length(FacetLevels) > 0L) {
        if(Debug) print("Plot.Line() Build Echarts 8 2")
        p1 <- echarts4r::e_facet(e = p1, rows = FacetRows, cols = FacetCols, legend_space = 16, legend_pos = "top")
        p1 <- echarts4r::e_legend(e = p1, type = "scroll", orient = "horizontal", right = 50, top = 40, height = "240px", textStyle = list(color = TextColor, fontWeight = "bold"))
        if(Debug) print("Plot.Line() Build Echarts 8 3")
      } else {
        p1 <- echarts4r::e_legend(e = p1, type = "scroll", orient = "vertical", right = 50, top = 40, height = "240px", textStyle = list(color = TextColor, fontWeight = "bold"))
      }

    } else {

      Width <- as.integer(gsub("[^\\d]+", "", Width, perl=TRUE))
      Height <- as.integer(gsub("[^\\d]+", "", Height, perl=TRUE))

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
      p1 <- echarts4r::e_charts_(
        data = dt1,
        x = XVar,
        dispose = TRUE,
        darkMode = TRUE,
        width = Width,
        height = Height)

      if(ShowLabels) {
        p1 <- echarts4r::e_line_(e = p1, serie = YVar, smooth = Smooth, showSymbol = ShowSymbol, label = list(show = TRUE))
      } else {
        p1 <- echarts4r::e_line_(e = p1, serie = YVar, smooth = Smooth, showSymbol = ShowSymbol)
      }

      # Finalize Plot Build
      if(FacetRows == 1L && FacetCols == 1L) {
        if(X_Scroll && length(GroupVar) == 0L) p1 <- echarts4r::e_datazoom(e = p1, x_index = c(0,1))
        if(Y_Scroll) p1 <- echarts4r::e_datazoom(e = p1, y_Index = c(0,1))
      }
      p1 <- echarts4r::e_theme(e = p1, name = EchartsTheme)
      p1 <- echarts4r::e_aria(e = p1, enabled = TRUE)
      p1 <- echarts4r::e_tooltip(e = p1, trigger = "axis", backgroundColor = "aliceblue")
      p1 <- echarts4r::e_toolbox_feature(e = p1, feature = c("saveAsImage","dataZoom"))
      p1 <- echarts4r::e_show_loading(e = p1, hide_overlay = TRUE, text = "Calculating...", color = "#000", text_color = TextColor, mask_color = "#000")

      if(length(Title.XAxis) == 0L) {
        p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "x", name = XVar, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = xaxis.fontSize))
      } else {
        p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "x", name = Title.XAxis, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = xaxis.fontSize))
      }

      if(length(Title.YAxis) == 0L) {
        p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "y", name = YVar, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = yaxis.fontSize))
      } else {
        p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "y", name = Title.YAxis, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = yaxis.fontSize))
      }

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
#' @param dt source data.table
#' @param PreAgg logical
#' @param AggMethod character
#' @param YVar Y-Axis variable name
#' @param XVar X-Axis variable name
#' @param GroupVar One Grouping Variable
#' @param YVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param XVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param FacetRows Defaults to 1 which causes no faceting to occur vertically. Otherwise, supply a numeric value for the number of output grid rows
#' @param FacetCols Defaults to 1 which causes no faceting to occur horizontally. Otherwise, supply a numeric value for the number of output grid columns
#' @param FacetLevels Faceting rows x columns is the max number of levels allowed in a grid. If your GroupVar has more you can supply the levels to display.
#' @param Height = NULL,
#' @param Width = NULL,
#' @param Title "Title"
#' @param ShowLabels character
#' @param Title.YAxis character
#' @param Title.XAxis character
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
#' @param BackGroundColor color outside of plot window. Rcolors and hex
#' @param ChartColor color
#' @param FillColor color
#' @param FillColorReverse character
#' @param GridColor color
#' @param TextColor "Not Implemented"
#' @param Debug Debugging purposes
#' @export
Plot.Area <- function(dt = NULL,
                      AggMethod = "mean",
                      PreAgg = TRUE,
                      Engine = 'Echarts',
                      XVar = NULL,
                      YVar = NULL,
                      GroupVar = NULL,
                      YVarTrans = "Identity",
                      XVarTrans = "Identity",
                      FacetRows = 1,
                      FacetCols = 1,
                      FacetLevels = NULL,
                      Height = NULL,
                      Width = NULL,
                      Title = 'Line Plot',
                      ShowLabels = FALSE,
                      Title.YAxis = NULL,
                      Title.XAxis = NULL,
                      EchartsTheme = "macarons",
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

  if(length(GroupVar) == 0L) TimeLine <- FALSE
  if(TimeLine && length(FacetLevels) > 0) X_Scroll <- FALSE

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
  } else if(length(GroupVar) > 0L) {
    if(Debug) print("Plot.Line() Ncols > 3L && length(GroupVar) > 0L")
    dt1 <- data.table::copy(dt[, .SD, .SDcols = c(YVar, XVar, GroupVar[1L])])
    if(length(FacetLevels) > 0) {
      dt1 <- dt1[get(GroupVar[1L]) %in% eval(FacetLevels)]
    }
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

  # Transformation
  if(YVarTrans != "Identity") {
    if(YVarTrans == "PercRank") {
      dt1 <- AutoQuant::PercRank(data = dt1, ColNames = YVar, GroupVars = GroupVar, Granularity = 0.0001, ScoreTable = FALSE)
    } else if(YVarTrans == "Standardize") {
      dt1 <- AutoQuant::Standardize(data = dt1, ColNames = YVar, GroupVars = GroupVar, Center = TRUE, Scale = TRUE, ScoreTable = FALSE)
    } else {
      dt1 <- AutoQuant::AutoTransformationCreate(data = dt1, ColumnNames = YVar, Methods = YVarTrans)$Data
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
        darkMode = TRUE,
        emphasis = list(focus = "series"),
        timeline = TimeLine,
        dispose = TRUE,
        width = Width,
        height = Height)

      # Finalize Plot Build
      if(Debug) print("Plot.Line() Build Echarts 4")
      if(ShowLabels) {
        p1 <- echarts4r::e_area_(e = p1, serie = YVar, smooth = Smooth, showSymbol = ShowSymbol, label = list(show = TRUE))
      } else {
        p1 <- echarts4r::e_area_(e = p1, serie = YVar, smooth = Smooth, showSymbol = ShowSymbol)
      }
      if(FacetRows == 1L && FacetCols == 1L) {
        if(X_Scroll && length(GroupVar) == 0L) p1 <- echarts4r::e_datazoom(e = p1, x_index = c(0,1))
        if(Y_Scroll) p1 <- echarts4r::e_datazoom(e = p1, y_Index = c(0,1))
      }
      p1 <- echarts4r::e_theme(e = p1, name = EchartsTheme)
      p1 <- echarts4r::e_aria(e = p1, enabled = TRUE)
      p1 <- echarts4r::e_tooltip(e = p1, trigger = "axis", backgroundColor = "aliceblue")
      p1 <- echarts4r::e_toolbox_feature(e = p1, feature = c("saveAsImage","dataZoom"))
      p1 <- echarts4r::e_show_loading(e = p1, hide_overlay = TRUE, text = "Calculating...", color = "#000", text_color = TextColor, mask_color = "#000")

      if(length(Title.XAxis) == 0L) {
        p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "x", name = XVar, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = xaxis.fontSize))
      } else {
        p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "x", name = Title.XAxis, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = xaxis.fontSize))
      }

      if(length(Title.YAxis) == 0L) {
        p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "y", name = YVar, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = yaxis.fontSize))
      } else {
        p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "y", name = Title.YAxis, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = yaxis.fontSize))
      }

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
      if(FacetRows > 1L || FacetCols > 1L) {
        p1 <- echarts4r::e_facet(e = p1, rows = FacetRows, cols = FacetCols, legend_space = 16, legend_pos = "top")
        p1 <- echarts4r::e_legend(e = p1, type = "scroll", orient = "horizontal", right = 50, top = 40, height = "240px", textStyle = list(color = TextColor, fontWeight = "bold"))
      } else {
        p1 <- echarts4r::e_legend(e = p1, type = "scroll", orient = "vertical", right = 50, top = 40, height = "240px", textStyle = list(color = TextColor, fontWeight = "bold"))
      }

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
      p1 <- echarts4r::e_charts_(
        data = dt1,
        x = XVar,
        darkMode = TRUE,
        dispose = TRUE,
        width = Width,
        height = Height)
      if(ShowLabels) {
        p1 <- echarts4r::e_area_(e = p1, serie = YVar, smooth = Smooth, showSymbol = ShowSymbol, label = list(show = TRUE))
      } else {
        p1 <- echarts4r::e_area_(e = p1, serie = YVar, smooth = Smooth, showSymbol = ShowSymbol)
      }

      # Finalize Plot Build
      if(FacetRows == 1L && FacetCols == 1L) {
        if(X_Scroll) p1 <- echarts4r::e_datazoom(e = p1, x_index = c(0,1))
        if(Y_Scroll) p1 <- echarts4r::e_datazoom(e = p1, y_Index = c(0,1))
      }
      p1 <- echarts4r::e_theme(e = p1, name = EchartsTheme)
      p1 <- echarts4r::e_aria(e = p1, enabled = TRUE)
      p1 <- echarts4r::e_tooltip(e = p1, trigger = "axis", backgroundColor = "aliceblue")
      p1 <- echarts4r::e_toolbox_feature(e = p1, feature = c("saveAsImage","dataZoom"))
      p1 <- echarts4r::e_show_loading(e = p1, hide_overlay = TRUE, text = "Calculating...", color = "#000", text_color = TextColor, mask_color = "#000")

      if(length(Title.XAxis) == 0L) {
        p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "x", name = XVar, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = xaxis.fontSize))
      } else {
        p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "x", name = Title.XAxis, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = xaxis.fontSize))
      }

      if(length(Title.YAxis) == 0L) {
        p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "y", name = YVar, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = yaxis.fontSize))
      } else {
        p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "y", name = Title.YAxis, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = yaxis.fontSize))
      }

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
#' @param dt source data.table
#' @param PreAgg logical
#' @param AggMethod character
#' @param YVar Y-Axis variable name
#' @param XVar X-Axis variable name
#' @param GroupVar One Grouping Variable
#' @param YVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param XVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param FacetRows Defaults to 1 which causes no faceting to occur vertically. Otherwise, supply a numeric value for the number of output grid rows
#' @param FacetCols Defaults to 1 which causes no faceting to occur horizontally. Otherwise, supply a numeric value for the number of output grid columns
#' @param FacetLevels Faceting rows x columns is the max number of levels allowed in a grid. If your GroupVar has more you can supply the levels to display.
#' @param Height = NULL,
#' @param Width = NULL,
#' @param Title "Title"
#' @param ShowLabels character
#' @param Title.YAxis character
#' @param Title.XAxis character
#' @param Engine "Echarts" or "Plotly"
#' @param EchartsTheme Provide an "Echarts" theme
#' @param TimeLine Logical
#' @param X_Scroll logical
#' @param Y_Scroll logical
#' @param ShowSymbol = FALSE
#' @param ZeroLineColor color
#' @param ZeroLineWidth 1
#' @param BackGroundColor color outside of plot window. Rcolors and hex
#' @param ChartColor color
#' @param FillColor color
#' @param FillColorReverse character
#' @param GridColor color
#' @param TextColor "Not Implemented"
#' @param Debug Debugging purposes
#' @export
Plot.Step <- function(dt = NULL,
                      AggMethod = "mean",
                      PreAgg = TRUE,
                      Engine = 'Echarts',
                      XVar = NULL,
                      YVar = NULL,
                      GroupVar = NULL,
                      YVarTrans = "Identity",
                      XVarTrans = "Identity",
                      FacetRows = 1,
                      FacetCols = 1,
                      FacetLevels = NULL,
                      Height = NULL,
                      Width = NULL,
                      Title = 'Line Plot',
                      ShowLabels = FALSE,
                      Title.YAxis = NULL,
                      Title.XAxis = NULL,
                      EchartsTheme = "macarons",
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

  if(length(GroupVar) == 0L) TimeLine <- FALSE
  if(TimeLine && length(FacetLevels) > 0) X_Scroll <- FALSE

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
  } else if(length(GroupVar) > 0L) {
    if(Debug) print("Plot.Line() Ncols > 3L && length(GroupVar) > 0L")
    dt1 <- data.table::copy(dt[, .SD, .SDcols = c(YVar, XVar, GroupVar[1L])])
    if(length(FacetLevels) > 0) {
      dt1 <- dt1[get(GroupVar[1L]) %in% eval(FacetLevels)]
    }
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

  # Transformation
  if(YVarTrans != "Identity") {
    if(YVarTrans == "PercRank") {
      dt1 <- AutoQuant::PercRank(data = dt1, ColNames = YVar, GroupVars = GroupVar, Granularity = 0.0001, ScoreTable = FALSE)
    } else if(YVarTrans == "Standardize") {
      dt1 <- AutoQuant::Standardize(data = dt1, ColNames = YVar, GroupVars = GroupVar, Center = TRUE, Scale = TRUE, ScoreTable = FALSE)
    } else {
      dt1 <- AutoQuant::AutoTransformationCreate(data = dt1, ColumnNames = YVar, Methods = YVarTrans)$Data
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
      timeline = TimeLine,
      darkMode = TRUE,
      emphasis = list(focus = "series"),
      dispose = TRUE,
      width = Width,
      height = Height)

    # Finalize Plot Build
    if(Debug) print("Plot.Line() Build Echarts 4")
    if(ShowLabels) {
      p1 <- echarts4r::e_step_(e = p1, serie = YVar, showSymbol = ShowSymbol, label = list(show = TRUE))
    } else {
      p1 <- echarts4r::e_step_(e = p1, serie = YVar, showSymbol = ShowSymbol)
    }
    if(FacetRows == 1L && FacetCols == 1L) {
      if(X_Scroll && length(GroupVar) == 0L) p1 <- echarts4r::e_datazoom(e = p1, x_index = c(0,1))
      if(Y_Scroll) p1 <- echarts4r::e_datazoom(e = p1, y_Index = c(0,1))
    }
    p1 <- echarts4r::e_theme(e = p1, name = EchartsTheme)
    p1 <- echarts4r::e_aria(e = p1, enabled = TRUE)
    p1 <- echarts4r::e_tooltip(e = p1, trigger = "axis", backgroundColor = "aliceblue")
    p1 <- echarts4r::e_toolbox_feature(e = p1, feature = c("saveAsImage","dataZoom"))
    p1 <- echarts4r::e_show_loading(e = p1, hide_overlay = TRUE, text = "Calculating...", color = "#000", text_color = TextColor, mask_color = "#000")

    if(length(Title.XAxis) == 0L) {
      p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "x", name = XVar, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = xaxis.fontSize))
    } else {
      p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "x", name = Title.XAxis, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = xaxis.fontSize))
    }

    if(length(Title.YAxis) == 0L) {
      p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "y", name = YVar, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = yaxis.fontSize))
    } else {
      p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "y", name = Title.YAxis, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = yaxis.fontSize))
    }

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
    if(FacetRows > 1L || FacetCols > 1L) {
      p1 <- echarts4r::e_facet(e = p1, rows = FacetRows, cols = FacetCols, legend_space = 16, legend_pos = "top")
      p1 <- echarts4r::e_legend(e = p1, type = "scroll", orient = "horizontal", right = 50, top = 40, height = "240px", textStyle = list(color = TextColor, fontWeight = "bold"))
    } else {
      p1 <- echarts4r::e_legend(e = p1, type = "scroll", orient = "vertical", right = 50, top = 40, height = "240px", textStyle = list(color = TextColor, fontWeight = "bold"))
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

    # Build base plot depending on GroupVar availability
    if(Debug) print("Plot.Line no group Echarts")
    p1 <- echarts4r::e_charts_(
      data = dt1,
      x = XVar,
      dispose = TRUE,
      darkMode = TRUE,
      width = Width,
      height = Height)

    if(ShowLabels) {
      p1 <- echarts4r::e_step_(e = p1, serie = YVar, showSymbol = ShowSymbol, label = list(show = TRUE))
    } else {
      p1 <- echarts4r::e_step_(e = p1, serie = YVar, showSymbol = ShowSymbol)
    }

    # Finalize Plot Build
    if(FacetRows == 1L && FacetCols == 1L) {
      if(X_Scroll) p1 <- echarts4r::e_datazoom(e = p1, x_index = c(0,1))
      if(Y_Scroll) p1 <- echarts4r::e_datazoom(e = p1, y_Index = c(0,1))
    }
    p1 <- echarts4r::e_theme(e = p1, name = EchartsTheme)
    p1 <- echarts4r::e_aria(e = p1, enabled = TRUE)
    p1 <- echarts4r::e_tooltip(e = p1, trigger = "axis", backgroundColor = "aliceblue")
    p1 <- echarts4r::e_toolbox_feature(e = p1, feature = c("saveAsImage","dataZoom"))
    p1 <- echarts4r::e_show_loading(e = p1, hide_overlay = TRUE, text = "Calculating...", color = "#000", text_color = TextColor, mask_color = "#000")

    if(length(Title.XAxis) == 0L) {
      p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "x", name = XVar, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = xaxis.fontSize))
    } else {
      p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "x", name = Title.XAxis, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = xaxis.fontSize))
    }

    if(length(Title.YAxis) == 0L) {
      p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "y", name = YVar, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = yaxis.fontSize))
    } else {
      p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "y", name = Title.YAxis, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = yaxis.fontSize))
    }

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
#' @param dt source data.table
#' @param PreAgg logical
#' @param AggMethod character
#' @param YVar Y-Axis variable name
#' @param XVar X-Axis variable name
#' @param GroupVar One Grouping Variable
#' @param YVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param XVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param FacetRows Defaults to 1 which causes no faceting to occur vertically. Otherwise, supply a numeric value for the number of output grid rows
#' @param FacetCols Defaults to 1 which causes no faceting to occur horizontally. Otherwise, supply a numeric value for the number of output grid columns
#' @param FacetLevels Faceting rows x columns is the max number of levels allowed in a grid. If your GroupVar has more you can supply the levels to display.
#' @param Height = NULL,
#' @param Width = NULL,
#' @param Title "Title"
#' @param ShowLabels character
#' @param Title.YAxis character
#' @param Title.XAxis character
#' @param Engine "Echarts" or "Plotly"
#' @param EchartsTheme Provide an "Echarts" theme
#' @param TimeLine Logical
#' @param X_Scroll logical
#' @param Y_Scroll logical
#' @param ShowSymbol = FALSE
#' @param ZeroLineColor color
#' @param ZeroLineWidth 1
#' @param BackGroundColor color outside of plot window. Rcolors and hex
#' @param ChartColor color
#' @param FillColor color
#' @param FillColorReverse character
#' @param GridColor color
#' @param TextColor "Not Implemented"
#' @param Debug Debugging purposes
#' @export
Plot.River <- function(dt = NULL,
                      AggMethod = "mean",
                      PreAgg = TRUE,
                      Engine = 'Echarts',
                      XVar = NULL,
                      YVar = NULL,
                      GroupVar = NULL,
                      YVarTrans = "Identity",
                      XVarTrans = "Identity",
                      FacetRows = 1,
                      FacetCols = 1,
                      FacetLevels = NULL,
                      Height = NULL,
                      Width = NULL,
                      Title = 'River Plot',
                      ShowLabels = FALSE,
                      Title.YAxis = NULL,
                      Title.XAxis = NULL,
                      EchartsTheme = "macarons",
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

  if(length(GroupVar) == 0L) TimeLine <- FALSE

  if(length(GroupVar) == 0L && length(YVar) <= 1L) {
    if(Debug) print("if(length(GroupVar) == 0L && length(YVar) <= 1L) return(NULL)")
    return(NULL)
  }

  if(Debug) print("Plot.River 2")

  if(!data.table::is.data.table(dt)) data.table::setDT(dt)
  Ncols <- ncol(dt)
  if(length(FacetLevels) > 0L) {
    dt1 <- data.table::copy(dt[get(GroupVar) %in% c(eval(FacetLevels)), .SD, .SDcols = c(YVar, XVar, GroupVar)])
  } else {
    dt1 <- data.table::copy(dt[, .SD, .SDcols = c(YVar, XVar, GroupVar)])
  }

  if(Debug) print("Plot.River 3")

  # Minimize data before moving on
  if(!PreAgg) {

    if(Debug) print("Plot.River 4")

    # DCast -> redefine YVar -> Proceed as normal
    if(length(YVar) == 1L && length(GroupVar) > 0L) {
      dt1 <- data.table::dcast.data.table(
        data = dt1,
        formula = get(XVar) ~ get(GroupVar[1L]),
        fun.aggregate = sum,
        value.var = eval(YVar))
      data.table::setnames(x = dt1, "XVar", c(XVar))
      YVar <- names(dt1)[-1L]
      GroupVar <- NULL
    }

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

  # Transformation
  if(YVarTrans != "Identity") {
    if(YVarTrans == "PercRank") {
      dt1 <- AutoQuant::PercRank(data = dt1, ColNames = YVar, GroupVars = GroupVar, Granularity = 0.0001, ScoreTable = FALSE)
    } else if(YVarTrans == "Standardize") {
      dt1 <- AutoQuant::Standardize(data = dt1, ColNames = YVar, GroupVars = GroupVar, Center = TRUE, Scale = TRUE, ScoreTable = FALSE)
    } else {
      dt1 <- AutoQuant::AutoTransformationCreate(data = dt1, ColumnNames = YVar, Methods = YVarTrans)$Data
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
  p1 <- echarts4r::e_charts_(
    data = dt1,
    x = XVar,
    dispose = TRUE,
    darkMode = TRUE,
    width = Width,
    height = Height)
  for(i in YVar) p1 <- echarts4r::e_river_(e = p1, serie = i)

  if(Debug) print("Plot.River 8b")

  # Finalize Plot Build
  if(FacetRows == 1L && FacetCols == 1L) {
    if(X_Scroll) p1 <- echarts4r::e_datazoom(e = p1, x_index = c(0,1))
    if(Y_Scroll) p1 <- echarts4r::e_datazoom(e = p1, y_Index = c(0,1))
  }
  p1 <- echarts4r::e_theme(e = p1, name = EchartsTheme)
  p1 <- echarts4r::e_aria(e = p1, enabled = TRUE)
  p1 <- echarts4r::e_tooltip(e = p1, trigger = "axis", backgroundColor = "aliceblue")
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
#' @param dt source data.table
#' @param PreAgg logical
#' @param YVar Y-Axis variable name
#' @param XVar X-Axis variable name
#' @param GroupVar Column name of Group Variable for distinct colored histograms by group levels
#' @param YVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param XVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param FacetRows Defaults to 1 which causes no faceting to occur vertically. Otherwise, supply a numeric value for the number of output grid rows
#' @param FacetCols Defaults to 1 which causes no faceting to occur horizontally. Otherwise, supply a numeric value for the number of output grid columns
#' @param FacetLevels Faceting rows x columns is the max number of levels allowed in a grid. If your GroupVar has more you can supply the levels to display.
#' @param AggMethod Choose from 'mean', 'sum', 'sd', and 'median'
#' @param Height = NULL,
#' @param Width = NULL,
#' @param Title title
#' @param Title.YAxis NULL. If NULL, YVar name will be used
#' @param Title.XAxis NULL. If NULL, XVar name will be used
#' @param ShowLabels logical
#' @param Engine 'Plotly' or "Echarts"
#' @param EchartsTheme "auritus","azul","bee-inspired","blue","caravan","carp","chalk","cool","dark-bold","dark","eduardo", #' "essos","forest","fresh-cut","fruit","gray","green","halloween","helianthus","infographic","inspired", #' "jazz","london","dark","macarons","macarons2","mint","purple-passion","red-velvet","red","roma","royal", #' "sakura","shine","tech-blue","vintage","walden","wef","weforum","westeros","wonderland"
#' @param TimeLine logical
#' @param X_Scroll logical
#' @param Y_Scroll logical
#' @param ZeroLineColor = '#ffff'
#' @param FillColor 'gray'
#' @param FillColorReverse character
#' @param ChartColor 'lightsteelblue'
#' @param TextColor 'darkblue'
#' @param GridColor 'white'
#' @param BackGroundColor color outside of plot window. Rcolors and hex outside of plot window. Rcolors and hex character
#' @param Debug Debugging purposes
#' @export
Plot.Bar <- function(dt = NULL,
                     PreAgg = FALSE,
                     XVar = NULL,
                     YVar = NULL,
                     GroupVar = NULL,
                     YVarTrans = "Identity",
                     XVarTrans = "Identity",
                     FacetRows = 1,
                     FacetCols = 1,
                     FacetLevels = NULL,
                     AggMethod = 'mean',
                     Height = NULL,
                     Width = NULL,
                     Title = 'Bar Plot',
                     ShowLabels = FALSE,
                     Title.YAxis = NULL,
                     Title.XAxis = NULL,
                     Engine = 'Echarts',
                     EchartsTheme = "macarons",
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

  if(length(GroupVar) == 0L) TimeLine <- FALSE

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

        if(length(FacetLevels) > 0L) {
          dt <- dt[get(GroupVar) %in% c(eval(FacetLevels)), .SD, .SDcols = c(YVar,XVar,GroupVar)]
        }

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
        numvars <- Rappture:::ColNameFilter(data = temp, Types = 'numeric')[[1L]]
        byvars <- Rappture:::ColNameFilter(data = temp, Types = "character")[[1L]]
      }

      # Transformation
      if(length(XVar) > 0L && class(temp[[XVar]])[1L] %in% c("numeric","integer")) {
        YVarTrans <- XVarTrans
      }
      if(YVarTrans != "Identity") {
        if(YVarTrans == "PercRank") {
          temp <- AutoQuant::PercRank(data = temp, ColNames = numvars, GroupVars = byvars, Granularity = 0.0001, ScoreTable = FALSE)
        } else if(YVarTrans == "Standardize") {
          temp <- AutoQuant::Standardize(data = temp, ColNames = numvars, GroupVars = byvars, Center = TRUE, Scale = TRUE, ScoreTable = FALSE)
        } else {
          temp <- AutoQuant::AutoTransformationCreate(data = temp, ColumnNames = numvars, Methods = YVarTrans)$Data
        }
      }

      # Plotly
      if(Engine == "Plotly") {

        Width <- as.integer(gsub("[^\\d]+", "", Width, perl=TRUE))
        Height <- as.integer(gsub("[^\\d]+", "", Height, perl=TRUE))

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
        if(length(GroupVar) > 0L) {
          p1 <- echarts4r::e_charts_(
            temp |> dplyr::group_by(get(GroupVar[1L])),
            x = XVar,
            darkMode = TRUE,
            emphasis = list(focus = "series"),
            dispose = TRUE,
            width = Width,
            height = Height)
        } else {
          p1 <- echarts4r::e_charts_(
            temp,
            x = XVar,
            dispose = TRUE,
            darkMode = TRUE,
            emphasis = list(focus = "series"),
            width = Width,
            height = Height)
        }

        if(ShowLabels) {
          p1 <- echarts4r::e_bar_(e = p1, YVar, label = list(show = TRUE))
        } else {
          p1 <- echarts4r::e_bar_(e = p1, YVar)
        }

        if(FacetRows == 1L && FacetCols == 1L) {
          if(X_Scroll) p1 <- echarts4r::e_datazoom(e = p1, x_index = c(0,1))
          if(Y_Scroll) p1 <- echarts4r::e_datazoom(e = p1, y_Index = c(0,1))
        }
        p1 <- echarts4r::e_theme(e = p1, name = EchartsTheme)
        p1 <- echarts4r::e_aria(e = p1, enabled = TRUE)
        p1 <- echarts4r::e_tooltip(e = p1, trigger = "axis", backgroundColor = "aliceblue")
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
        if(FacetRows > 1L || FacetCols > 1L) p1 <- echarts4r::e_facet(e = p1, rows = FacetRows, cols = FacetCols, legend_space = 16, legend_pos = "top")

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
        numvars <- Rappture:::ColNameFilter(data = temp, Types = 'numeric')[[1L]]
        byvars <- Rappture:::ColNameFilter(data = temp, Types = "character")[[1L]]
      }

      # Transformation
      if(length(XVar) > 0L && class(temp[[XVar]])[1L] %in% c("numeric","integer")) {
        YVarTrans <- XVarTrans
      }
      if(YVarTrans != "Identity") {
        if(YVarTrans == "PercRank") {
          temp <- AutoQuant::PercRank(data = temp, ColNames = numvars, GroupVars = GroupVar, Granularity = 0.0001, ScoreTable = FALSE)
        } else if(YVarTrans == "Standardize") {
          temp <- AutoQuant::Standardize(data = temp, ColNames = numvars, GroupVars = GroupVar, Center = TRUE, Scale = TRUE, ScoreTable = FALSE)
        } else {
          temp <- AutoQuant::AutoTransformationCreate(data = temp, ColumnNames = numvars, Methods = YVarTrans)$Data
        }
      }

      yvar <- temp[[YVar]]
      xvar <- temp[[XVar]]

      # Plotly
      if(Engine == "Plotly") {

        Width <- as.integer(gsub("[^\\d]+", "", Width, perl=TRUE))
        Height <- as.integer(gsub("[^\\d]+", "", Height, perl=TRUE))

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
        p1 <- echarts4r::e_charts_(
          temp,
          x = XVar,
          dispose = TRUE,
          darkMode = TRUE,
          width = Width,
          height = Height)
        if(ShowLabels) {
          p1 <- echarts4r::e_bar_(e = p1, YVar, label = list(show = TRUE))
        } else {
          p1 <- echarts4r::e_bar_(e = p1, YVar)
        }
        if(FacetRows == 1L && FacetCols == 1L) {
          if(X_Scroll) p1 <- echarts4r::e_datazoom(e = p1, x_index = c(0,1))
          if(Y_Scroll) p1 <- echarts4r::e_datazoom(e = p1, y_Index = c(0,1))
        }
        p1 <- echarts4r::e_theme(e = p1, name = EchartsTheme)
        p1 <- echarts4r::e_aria(e = p1, enabled = TRUE)
        p1 <- echarts4r::e_tooltip(e = p1, trigger = "axis", backgroundColor = "aliceblue")
        p1 <- echarts4r::e_toolbox_feature(e = p1, feature = c("saveAsImage","dataZoom"))
        p1 <- echarts4r::e_show_loading(e = p1, hide_overlay = TRUE, text = "Calculating...", color = "#000", text_color = TextColor, mask_color = "#000")

        if(length(Title.XAxis) == 0L) {
          p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "x", name = XVar, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = xaxis.fontSize))
        } else {
          p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "x", name = Title.XAxis, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = xaxis.fontSize))
        }

        if(length(Title.YAxis) == 0L) {
          p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "y", name = YVar, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = yaxis.fontSize))
        } else {
          p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "y", name = Title.YAxis, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = yaxis.fontSize))
        }

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
        if(FacetRows > 1L || FacetCols > 1L) {
          p1 <- echarts4r::e_facet(e = p1, rows = FacetRows, cols = FacetCols, legend_space = 16, legend_pos = "top")
          p1 <- echarts4r::e_legend(e = p1, type = "scroll", orient = "horizontal", right = 50, top = 40, height = "240px", textStyle = list(color = TextColor, fontWeight = "bold"))
        } else {
          p1 <- echarts4r::e_legend(e = p1, type = "scroll", orient = "vertical", right = 50, top = 40, height = "240px", textStyle = list(color = TextColor, fontWeight = "bold"))
        }

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
        numvars <- Rappture:::ColNameFilter(data = temp, Types = 'numeric')[[1L]]
        byvars <- Rappture:::ColNameFilter(data = temp, Types = "character")[[1L]]
      }

      # Transformation
      if(length(XVar) > 0L && class(temp[[XVar]])[1L] %in% c("numeric","integer")) {
        YVarTrans <- XVarTrans
      }
      if(YVarTrans != "Identity") {
        if(YVarTrans == "PercRank") {
          temp <- AutoQuant::PercRank(data = temp, ColNames = numvars, GroupVars = byvars, Granularity = 0.0001, ScoreTable = FALSE)
        } else if(YVarTrans == "Standardize") {
          temp <- AutoQuant::Standardize(data = temp, ColNames = numvars, GroupVars = byvars, Center = TRUE, Scale = TRUE, ScoreTable = FALSE)
        } else {
          temp <- AutoQuant::AutoTransformationCreate(data = temp, ColumnNames = numvars, Methods = YVarTrans)$Data
        }
      }

      if(Engine == "Plotly") {

        Width <- as.integer(gsub("[^\\d]+", "", Width, perl=TRUE))
        Height <- as.integer(gsub("[^\\d]+", "", Height, perl=TRUE))

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
        p1 <- echarts4r::e_charts_(
          temp, x = GroupVar[1L],
          dispose = TRUE,
          darkMode = TRUE,
          width = Width,
          height = Height)
        if(ShowLabels) {
          p1 <- echarts4r::e_bar_(e = p1, YVar, label = list(show = TRUE))
        } else {
          p1 <- echarts4r::e_bar_(e = p1, YVar)
        }
        if(FacetRows == 1L && FacetCols == 1L) {
          if(X_Scroll && length(GroupVar) == 0L) p1 <- echarts4r::e_datazoom(e = p1, x_index = c(0,1))
          if(Y_Scroll) p1 <- echarts4r::e_datazoom(e = p1, y_Index = c(0,1))
        }
        p1 <- echarts4r::e_theme(e = p1, name = EchartsTheme)
        p1 <- echarts4r::e_aria(e = p1, enabled = TRUE)
        p1 <- echarts4r::e_tooltip(e = p1, trigger = "axis", backgroundColor = "aliceblue")
        p1 <- echarts4r::e_toolbox_feature(e = p1, feature = c("saveAsImage","dataZoom"))
        p1 <- echarts4r::e_show_loading(e = p1, hide_overlay = TRUE, text = "Calculating...", color = "#000", text_color = TextColor, mask_color = "#000")

        if(length(Title.XAxis) == 0L) {
          p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "x", name = XVar, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = xaxis.fontSize))
        } else {
          p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "x", name = Title.XAxis, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = xaxis.fontSize))
        }

        if(length(Title.YAxis) == 0L) {
          p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "y", name = YVar, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = yaxis.fontSize))
        } else {
          p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "y", name = Title.YAxis, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = yaxis.fontSize))
        }

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
        if(FacetRows > 1L || FacetCols > 1L) {
          p1 <- echarts4r::e_facet(e = p1, rows = FacetRows, cols = FacetCols, legend_space = 16, legend_pos = "top")
          p1 <- echarts4r::e_legend(e = p1, type = "scroll", orient = "horizontal", right = 50, top = 40, height = "240px", textStyle = list(color = TextColor, fontWeight = "bold"))
        } else {
          p1 <- echarts4r::e_legend(e = p1, type = "scroll", orient = "vertical", right = 50, top = 40, height = "240px", textStyle = list(color = TextColor, fontWeight = "bold"))
        }

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
        numvars <- Rappture:::ColNameFilter(data = temp, Types = 'numeric')[[1L]]
        byvars <- Rappture:::ColNameFilter(data = temp, Types = "character")[[1L]]
      }

      # Transformation
      if(length(XVar) > 0L && class(temp[[XVar]])[1L] %in% c("numeric","integer")) {
        YVarTrans <- XVarTrans
      }
      if(YVarTrans != "Identity") {
        if(YVarTrans == "PercRank") {
          temp <- AutoQuant::PercRank(data = temp, ColNames = numvars, GroupVars = byvars, Granularity = 0.0001, ScoreTable = FALSE)
        } else if(YVarTrans == "Standardize") {
          temp <- AutoQuant::Standardize(data = temp, ColNames = numvars, GroupVars = byvars, Center = TRUE, Scale = TRUE, ScoreTable = FALSE)
        } else {
          temp <- AutoQuant::AutoTransformationCreate(data = temp, ColumnNames = numvars, Methods = YVarTrans)$Data
        }
      }

      # Plot
      if(Engine == "Plotly") {

        Width <- as.integer(gsub("[^\\d]+", "", Width, perl=TRUE))
        Height <- as.integer(gsub("[^\\d]+", "", Height, perl=TRUE))

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
        p1 <- echarts4r::e_charts_(
          temp,
          x = GroupVar[1L],
          dispose = TRUE,
          darkMode = TRUE,
          width = Width,
          height = Height)
        if(ShowLabels) {
          p1 <- echarts4r::e_bar_(e = p1, XVar, label = list(show = TRUE))
        } else {
          p1 <- echarts4r::e_bar_(e = p1, XVar)
        }
        if(FacetRows == 1L && FacetCols == 1L) {
          if(X_Scroll && length(GroupVar) == 0L) p1 <- echarts4r::e_datazoom(e = p1, x_index = c(0,1))
          if(Y_Scroll) p1 <- echarts4r::e_datazoom(e = p1, y_Index = c(0,1))
        }
        p1 <- echarts4r::e_theme(e = p1, name = EchartsTheme)
        p1 <- echarts4r::e_aria(e = p1, enabled = TRUE)
        p1 <- echarts4r::e_tooltip(e = p1, trigger = "axis", backgroundColor = "aliceblue")
        p1 <- echarts4r::e_toolbox_feature(e = p1, feature = c("saveAsImage","dataZoom"))
        p1 <- echarts4r::e_show_loading(e = p1, hide_overlay = TRUE, text = "Calculating...", color = "#000", text_color = TextColor, mask_color = "#000")

        if(length(Title.XAxis) == 0L) {
          p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "x", name = XVar, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = xaxis.fontSize))
        } else {
          p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "x", name = Title.XAxis, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = xaxis.fontSize))
        }

        if(length(Title.YAxis) == 0L) {
          p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "y", name = YVar, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = yaxis.fontSize))
        } else {
          p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "y", name = Title.YAxis, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = yaxis.fontSize))
        }

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
        if(FacetRows > 1L || FacetCols > 1L) p1 <- echarts4r::e_facet(e = p1, rows = FacetRows, cols = FacetCols, legend_space = 16, legend_pos = "top")

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
#' @param dt source data.table
#' @param PreAgg logical
#' @param YVar Y-Axis variable name
#' @param XVar X-Axis variable name
#' @param GroupVar Column name of Group Variable for distinct colored histograms by group levels
#' @param YVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param XVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param FacetRows Defaults to 1 which causes no faceting to occur vertically. Otherwise, supply a numeric value for the number of output grid rows
#' @param FacetCols Defaults to 1 which causes no faceting to occur horizontally. Otherwise, supply a numeric value for the number of output grid columns
#' @param FacetLevels Faceting rows x columns is the max number of levels allowed in a grid. If your GroupVar has more you can supply the levels to display.
#' @param AggMethod Choose from 'mean', 'sum', 'sd', and 'median'
#' @param Height NULL
#' @param Width NULL
#' @param Title title
#' @param Title.YAxis NULL. If NULL, YVar name will be used
#' @param Title.XAxis NULL. If NULL, XVar name will be used
#' @param ShowLabels logical
#' @param Engine 'Plotly' or "Echarts"
#' @param EchartsTheme "auritus","azul","bee-inspired","blue","caravan","carp","chalk","cool","dark-bold","dark","eduardo", #' "essos","forest","fresh-cut","fruit","gray","green","halloween","helianthus","infographic","inspired", #' "jazz","london","dark","macarons","macarons2","mint","purple-passion","red-velvet","red","roma","royal", #' "sakura","shine","tech-blue","vintage","walden","wef","weforum","westeros","wonderland"
#' @param TimeLine logical
#' @param X_Scroll logical
#' @param Y_Scroll logical
#' @param ZeroLineColor '#ffff'
#' @param FillColor 'gray'
#' @param FillColorReverse character
#' @param ChartColor 'lightsteelblue'
#' @param TextColor 'darkblue'
#' @param GridColor 'white'
#' @param BackGroundColor color outside of plot window. Rcolors and hex outside of plot window. Rcolors and hex character
#' @param Debug Debugging purposes
#' @export
Plot.StackedBar <- function(dt = NULL,
                            PreAgg = FALSE,
                            XVar = NULL,
                            YVar = NULL,
                            GroupVar = NULL,
                            YVarTrans = "Identity",
                            XVarTrans = "Identity",
                            FacetRows = 1,
                            FacetCols = 1,
                            FacetLevels = NULL,
                            AggMethod = 'mean',
                            Height = NULL,
                            Width = NULL,
                            Title = "Stacked Bar",
                            Title.YAxis = NULL,
                            Title.XAxis = NULL,
                            ShowLabels = FALSE,
                            Engine = 'Echarts',
                            EchartsTheme = "macarons",
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

  if(length(XVar) == 0L) return(NULL)
  if(length(YVar) == 0L) return(NULL)
  if(length(GroupVar) == 0L) return(NULL)

  if(class(dt[[YVar]])[1L] %in% c("character","factor") && class(dt[[XVar]])[1L] %in% c("numeric","integer")) {
    l <- YVar
    YVar <- XVar
    XVar <- l
    rm(l)
  }

  print("StackedBarPlot step 1")
  if(length(GroupVar) == 0L) TimeLine <- FALSE
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
    if(!PreAgg) {

      if(length(FacetLevels) > 0L) {
        dt <- dt[get(GroupVar) %in% c(eval(FacetLevels)), .SD, .SDcols = c(YVar,XVar,GroupVar)]
      }

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
      numvars <- Rappture:::ColNameFilter(data = temp, Types = 'numeric')[[1L]]
      byvars <- Rappture:::ColNameFilter(data = temp, Types = "character")[[1L]]
    }

    # Transformation
    if(length(XVar) > 0L && class(temp[[XVar]])[1L] %in% c("numeric","integer")) {
      YVarTrans <- XVarTrans
    }
    if(YVarTrans != "Identity") {
      if(YVarTrans == "PercRank") {
        temp <- AutoQuant::PercRank(data = temp, ColNames = numvars, GroupVars = byvars, Granularity = 0.0001, ScoreTable = FALSE)
      } else if(YVarTrans == "Standardize") {
        temp <- AutoQuant::Standardize(data = temp, ColNames = numvars, GroupVars = byvars, Center = TRUE, Scale = TRUE, ScoreTable = FALSE)
      } else {
        temp <- AutoQuant::AutoTransformationCreate(data = temp, ColumnNames = numvars, Methods = YVarTrans)$Data
      }
    }

    p1 <- echarts4r::e_charts_(
      data = temp |> dplyr::group_by(get(GroupVar[1L])),
      x = XVar,
      dispose = TRUE,
      darkMode = TRUE,
      emphasis = list(focus = "series"),
      width = Width,
      height = Height)
    if(ShowLabels) {
      p1 <- echarts4r::e_bar_(
        e = p1,
        YVar,
        stack = XVar,
        label = list(show = TRUE))
    } else {
      p1 <- echarts4r::e_bar_(
        e = p1,
        YVar,
        stack = XVar)
    }
    if(FacetRows == 1L && FacetCols == 1L) {
      if(X_Scroll && length(GroupVar) == 0L) p1 <- echarts4r::e_datazoom(e = p1, x_index = c(0,1))
      if(Y_Scroll) p1 <- echarts4r::e_datazoom(e = p1, y_Index = c(0,1))
    }
    p1 <- echarts4r::e_theme(e = p1, name = EchartsTheme)
    p1 <- echarts4r::e_aria(e = p1, enabled = TRUE)
    p1 <- echarts4r::e_tooltip(e = p1, trigger = "axis", backgroundColor = "aliceblue")
    p1 <- echarts4r::e_toolbox_feature(e = p1, feature = c("saveAsImage","dataZoom"))
    p1 <- echarts4r::e_show_loading(e = p1, hide_overlay = TRUE, text = "Calculating...", color = "#000", text_color = TextColor, mask_color = "#000")

    if(length(Title.XAxis) == 0L) {
      p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "x", name = XVar, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = xaxis.fontSize))
    } else {
      p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "x", name = Title.XAxis, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = xaxis.fontSize))
    }

    if(length(Title.YAxis) == 0L) {
      p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "y", name = YVar, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = yaxis.fontSize))
    } else {
      p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "y", name = Title.YAxis, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = yaxis.fontSize))
    }

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
    if(FacetRows > 1L || FacetCols > 1L) {
      p1 <- echarts4r::e_facet(e = p1, rows = FacetRows, cols = FacetCols, legend_space = 16, legend_pos = "top")
      p1 <- echarts4r::e_legend(e = p1, type = "scroll", orient = "horizontal", right = 50, top = 40, height = "240px", textStyle = list(color = TextColor, fontWeight = "bold"))
    } else {
      p1 <- echarts4r::e_legend(e = p1, type = "scroll", orient = "vertical", right = 50, top = 40, height = "240px", textStyle = list(color = TextColor, fontWeight = "bold"))
    }
    return(p1)

  } else {
    if(Debug) print("XVar, YVar, and GroupVar need to have length > 0")
  }
}

#' @title Plot.BarPlot3D
#'
#' @description Build a 3D Bar Plot
#'
#' @family Standard Plots
#' @author Adrian Antico
#'
#' @param dt source data.table
#' @param YVar Y-Axis variable name
#' @param XVar X-Axis variable name
#' @param ZVar Z-Axis variable name
#' @param YVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param XVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param ZVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param FacetRows Defaults to 1 which causes no faceting to occur vertically. Otherwise, supply a numeric value for the number of output grid rows
#' @param FacetCols Defaults to 1 which causes no faceting to occur horizontally. Otherwise, supply a numeric value for the number of output grid columns
#' @param FacetLevels Faceting rows x columns is the max number of levels allowed in a grid. If your GroupVar has more you can supply the levels to display.
#' @param Height = NULL,
#' @param Width = NULL,
#' @param Engine "plotly", "echarts4r"
#' @param EchartsTheme "dark-blue"
#' @param AggMethod 'mean', 'median', 'sum', 'sd', 'coeffvar', 'count'
#' @param NumberBins = 21
#' @param NumLevels_Y = 20
#' @param NumLevels_X = 20
#' @param Title "Heatmap"
#' @param ShowLabels character
#' @param Title.YAxis character
#' @param Title.XAxis character
#' @param BackGroundColor color outside of plot window. Rcolors and hex outside of plot window. Rcolors and hex character
#' @param ChartColor = '#001534'
#' @param FillColor = "#0066ff"
#' @param FillColorReverse character
#' @param GridColor = '#ffffff'
#' @param Debug Debugging purposes
#' @export
Plot.BarPlot3D <- function(dt,
                           PreAgg = FALSE,
                           AggMethod = 'mean',
                           XVar = NULL,
                           YVar = NULL,
                           ZVar = NULL,
                           YVarTrans = "Identity",
                           XVarTrans = "Identity",
                           ZVarTrans = "Identity",
                           FacetRows = 1,
                           FacetCols = 1,
                           FacetLevels = NULL,
                           NumberBins = 21,
                           NumLevels_Y = 33,
                           NumLevels_X = 33,
                           Height = NULL,
                           Width = NULL,
                           Title = "3D Bar Plot",
                           ShowLabels = FALSE,
                           Title.YAxis = NULL,
                           Title.XAxis = NULL,
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
                           Debug = FALSE) {

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

  TimeLine <- FALSE
  if(TimeLine && length(FacetLevels) > 0) X_Scroll <- FALSE

  if(!PreAgg) {
    aggFunc <- AutoPlots:::SummaryFunction(AggMethod)
  }

  # XVar == numeric or integer && YVar == numeric or integer
  if(x_y_num) {

    # rank XVar and YVar
    if(!PreAgg) {
      dt1[, eval(XVar) := round(data.table::frank(dt1[[XVar]]) * NumberBins /.N) / NumberBins]
      dt1[, eval(YVar) := round(data.table::frank(dt1[[YVar]]) * NumberBins /.N) / NumberBins]
      data.table::setnames(dt1, eval(ZVar), 'Measure_Variable')
      dt1 <- dt1[, lapply(.SD, noquote(aggFunc)), .SDcols = c(ZVar), by = c(XVar,YVar)]
    }

    # Transformation
    if(ZVarTrans != "Identity") {
      if(ZVarTrans == "PercRank") {
        dt1 <- AutoQuant::PercRank(data = dt1, ColNames = "Measure_Variable", GroupVars = c(XVar,YVar), Granularity = 0.0001, ScoreTable = FALSE)
      } else if(ZVarTrans == "Standardize") {
        dt1 <- AutoQuant::Standardize(data = dt1, ColNames = "Measure_Variable", GroupVars = c(XVar,YVar), Center = TRUE, Scale = TRUE, ScoreTable = FALSE)
      } else {
        dt1 <- AutoQuant::AutoTransformationCreate(data = dt1, ColumnNames = "Measure_Variable", Methods = ZVarTrans)$Data
      }
    }

    # Formatting
    vals <- unique(scales::rescale(c(dt1[['Measure_Variable']])))
    o <- order(vals, decreasing = FALSE)
    cols <- scales::col_numeric("Purples", domain = NULL)(vals)
    colz <- setNames(data.frame(vals[o], cols[o]), NULL)

    # Create final data for plot
    if(Engine == "Plotly") {

      Width <- as.integer(gsub("[^\\d]+", "", Width, perl=TRUE))
      Height <- as.integer(gsub("[^\\d]+", "", Height, perl=TRUE))

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
      p1 <- echarts4r::e_charts_(
        data = dt1,
        x = XVar,
        dispose = TRUE,
        darkMode = TRUE,
        width = Width,
        height = Height)

      if(ShowLabels) {
        p1 <- echarts4r::e_bar_3d_(e = p1, YVar, g, itemStyle = list(emphasis = list(shadowBlur = 10)), label = list(show = TRUE))
      } else {
        p1 <- echarts4r::e_bar_3d_(e = p1, YVar, g, itemStyle = list(emphasis = list(shadowBlur = 10)))
      }

      p1 <- echarts4r::e_visual_map_(e = p1, g, show = FALSE)
      p1 <- echarts4r::e_tooltip(e = p1, trigger = "axis", backgroundColor = "aliceblue")
      p1 <- echarts4r::e_theme(e = p1, name = EchartsTheme)
      if(FacetRows == 1L && FacetCols == 1L) {
        if(X_Scroll) p1 <- echarts4r::e_datazoom(e = p1, x_index = c(0,1))
        if(Y_Scroll) p1 <- echarts4r::e_datazoom(e = p1, y_Index = c(0,1))
      }
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
      if(FacetRows > 1L || FacetCols > 1L) p1 <- echarts4r::e_facet(e = p1, rows = FacetRows, cols = FacetCols, legend_space = 16, legend_pos = "top")

    }
    return(p1)
  }

  # XVar == character && YVar == numeric or integer
  if(x_char) {

    # rank YVar
    data.table::setnames(dt1, eval(ZVar), 'Measure_Variable')
    if(!PreAgg) {
      dt1[, eval(YVar) := round(data.table::frank(dt1[[YVar]]) * NumberBins /.N) / NumberBins]
      temp <- dt1[, lapply(.SD, mean, na.rm = TRUE), .SDcols = c('Measure_Variable'), by = c(YVar)][order(-Measure_Variable)]
      temp <- temp[seq_len(min(NumLevels_X, temp[, .N]))][[1L]]
      dt1 <- dt1[get(YVar) %in% eval(temp)]
      dt1 <- dt1[, lapply(.SD, noquote(aggFunc)), .SDcols = c(ZVar), by = c(XVar,YVar)]
    }

    # Formatting
    vals <- unique(scales::rescale(c(dt1[['Measure_Variable']])))
    o <- order(vals, decreasing = FALSE)
    cols <- scales::col_numeric("Purples", domain = NULL)(vals)
    colz <- setNames(data.frame(vals[o], cols[o]), NULL)

    # Transformation
    if(ZVarTrans != "Identity") {
      if(ZVarTrans == "PercRank") {
        dt1 <- AutoQuant::PercRank(data = dt1, ColNames = "Measure_Variable", GroupVars = c(XVar,YVar), Granularity = 0.0001, ScoreTable = FALSE)
      } else if(ZVarTrans == "Standardize") {
        dt1 <- AutoQuant::Standardize(data = dt1, ColNames = "Measure_Variable", GroupVars = c(XVar,YVar), Center = TRUE, Scale = TRUE, ScoreTable = FALSE)
      } else {
        dt1 <- AutoQuant::AutoTransformationCreate(data = dt1, ColumnNames = "Measure_Variable", Methods = ZVarTrans)$Data
      }
    }

    # Create final data for plot
    if(Engine == "Plotly") {

      Width <- as.integer(gsub("[^\\d]+", "", Width, perl=TRUE))
      Height <- as.integer(gsub("[^\\d]+", "", Height, perl=TRUE))

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
      p1 <- echarts4r::e_charts_(
        data = dt1,
        x = XVar,
        dispose = TRUE,
        darkMode = TRUE,
        width = Width,
        height = Height)

      if(ShowLabels) {
        p1 <- echarts4r::e_heatmap_(e = p1, YVar, g, itemStyle = list(emphasis = list(shadowBlur = 10)), label = list(show = TRUE))
      } else {
        p1 <- echarts4r::e_heatmap_(e = p1, YVar, g, itemStyle = list(emphasis = list(shadowBlur = 10)))
      }

      p1 <- echarts4r::e_visual_map_(e = p1, g, show = FALSE)
      if(FacetRows == 1L && FacetCols == 1L) {
        if(X_Scroll) p1 <- echarts4r::e_datazoom(e = p1, x_index = c(0,1))
        if(Y_Scroll) p1 <- echarts4r::e_datazoom(e = p1, y_Index = c(0,1))
      }
      p1 <- echarts4r::e_theme(e = p1, name = EchartsTheme)
      p1 <- echarts4r::e_aria(e = p1, enabled = TRUE)
      p1 <- echarts4r::e_show_loading(e = p1, hide_overlay = TRUE, text = "Calculating...", color = "#000", text_color = TextColor, mask_color = "#000")

      if(length(Title.XAxis) == 0L) {
        p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "x", name = XVar, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = xaxis.fontSize))
      } else {
        p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "x", name = Title.XAxis, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = xaxis.fontSize))
      }

      if(length(Title.YAxis) == 0L) {
        p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "y", name = YVar, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = yaxis.fontSize))
      } else {
        p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "y", name = Title.YAxis, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = yaxis.fontSize))
      }

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
      if(FacetRows > 1L || FacetCols > 1L) p1 <- echarts4r::e_facet(e = p1, rows = FacetRows, cols = FacetCols, legend_space = 16, legend_pos = "top")

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
      dt1 <- dt1[, lapply(.SD, noquote(aggFunc)), .SDcols = c(ZVar), by = c(XVar,YVar)]


      # Transformation
      if(ZVarTrans != "Identity") {
        if(ZVarTrans == "PercRank") {
          dt1 <- AutoQuant::PercRank(data = dt1, ColNames = "Measure_Variable", GroupVars = c(XVar,YVar), Granularity = 0.0001, ScoreTable = FALSE)
        } else if(ZVarTrans == "Standardize") {
          dt1 <- AutoQuant::Standardize(data = dt1, ColNames = "Measure_Variable", GroupVars = c(XVar,YVar), Center = TRUE, Scale = TRUE, ScoreTable = FALSE)
        } else {
          dt1 <- AutoQuant::AutoTransformationCreate(data = dt1, ColumnNames = "Measure_Variable", Methods = ZVarTrans)$Data
        }
      }

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
      Height <- as.integer(gsub("[^\\d]+", "", Height, perl=TRUE))

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
      p1 <- echarts4r::e_charts_(
        data = dt1,
        x = XVar,
        dispose = TRUE,
        darkMode = TRUE,
        width = Width,
        height = Height)

      if(ShowLabels) {
        p1 <- echarts4r::e_heatmap_(e = p1, YVar, g, itemStyle = list(emphasis = list(shadowBlur = 10)), label = list(show = TRUE))
      } else {
        p1 <- echarts4r::e_heatmap_(e = p1, YVar, g, itemStyle = list(emphasis = list(shadowBlur = 10)))
      }
      p1 <- echarts4r::e_visual_map_(e = p1, g, show = FALSE)
      p1 <- echarts4r::e_theme(e = p1, name = EchartsTheme)
      p1 <- echarts4r::e_datazoom(e = p1, x_index = c(0,1))
      p1 <- echarts4r::e_datazoom(e = p1, y_index = c(0,1))
      p1 <- echarts4r::e_aria(e = p1, enabled = TRUE)
      p1 <- echarts4r::e_show_loading(e = p1, hide_overlay = TRUE, text = "Calculating...", color = "#000", text_color = TextColor, mask_color = "#000")

      if(length(Title.XAxis) == 0L) {
        p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "x", name = XVar, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = xaxis.fontSize))
      } else {
        p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "x", name = Title.XAxis, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = xaxis.fontSize))
      }

      if(length(Title.YAxis) == 0L) {
        p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "y", name = YVar, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = yaxis.fontSize))
      } else {
        p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "y", name = Title.YAxis, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = yaxis.fontSize))
      }

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
      if(FacetRows > 1L || FacetCols > 1L) p1 <- echarts4r::e_facet(e = p1, rows = FacetRows, cols = FacetCols, legend_space = 16, legend_pos = "top")

    }
    return(p1)
  }

  # XVar == character or integer && YVar == character
  if(all_char) {

    # Starter pack
    if(!PreAgg) {
      temp1 <- dt1[, lapply(.SD, mean, na.rm = TRUE), .SDcols = c(ZVar), by = c(YVar)][order(-get(ZVar))]
      temp1 <- temp1[seq_len(min(NumLevels_Y, temp1[, .N]))][[1L]]
      temp2 <- dt1[, lapply(.SD, mean, na.rm = TRUE), .SDcols = c(ZVar), by = c(XVar)][order(-get(ZVar))]
      temp2 <- temp2[seq_len(min(NumLevels_X, temp2[, .N]))][[1L]]
      dt1 <- dt1[get(YVar) %in% eval(temp1) & get(XVar) %in% eval(temp2), lapply(.SD, noquote(aggFunc)), .SDcols = c(ZVar), by = c(XVar,YVar)]
    }

    # Transformation
    if(length(ZVarTrans) > 0 && ZVarTrans != "Identity") {
      if(ZVarTrans == "PercRank") {
        dt1 <- AutoQuant::PercRank(data = dt1, ColNames = ZVar, GroupVars = c(XVar,YVar), Granularity = 0.0001, ScoreTable = FALSE)
      } else if(ZVarTrans == "Standardize") {
        dt1 <- AutoQuant::Standardize(data = dt1, ColNames = ZVar, GroupVars = c(XVar,YVar), Center = TRUE, Scale = TRUE, ScoreTable = FALSE)
      } else {
        dt1 <- AutoQuant::AutoTransformationCreate(data = dt1, ColumnNames = ZVar, Methods = ZVarTrans)$Data
      }
    }

    if(XVar %in% c("Predict","p1")) data.table::setorderv(x = dt1, "Predict")
    p1 <- echarts4r::e_charts_(
      data = dt1,
      x = XVar,
      dispose = TRUE,
      darkMode = TRUE,
      width = Width,
      height = Height)

    if(ShowLabels) {
      p1 <- echarts4r::e_bar_3d_(e = p1, YVar, ZVar, coord_system = "cartesian3D", itemStyle = list(emphasis = list(shadowBlur = 10)), label = list(show = TRUE))
    } else {
      p1 <- echarts4r::e_bar_3d_(e = p1, YVar, ZVar, coord_system = "cartesian3D", itemStyle = list(emphasis = list(shadowBlur = 10)))
    }

    p1 <- echarts4r::e_visual_map_(e = p1, ZVar, show = FALSE)
    p1 <- echarts4r::e_theme(e = p1, name = EchartsTheme)
    # They do nothing for this plot type
    # p1 <- echarts4r::e_datazoom(e = p1, x_index = c(0,1))  # They do nothing for this plot type
    # p1 <- echarts4r::e_datazoom(e = p1, y_index = c(0,1))
    p1 <- echarts4r::e_aria(e = p1, enabled = TRUE)
    p1 <- echarts4r::e_show_loading(e = p1, hide_overlay = TRUE, text = "Calculating...", color = "#000", text_color = TextColor, mask_color = "#000")

    if(length(Title.XAxis) == 0L) {
      p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "x", name = XVar, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = xaxis.fontSize))
    } else {
      p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "x", name = Title.XAxis, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = xaxis.fontSize))
    }

    if(length(Title.YAxis) == 0L) {
      p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "y", name = YVar, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = yaxis.fontSize))
    } else {
      p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "y", name = Title.YAxis, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = yaxis.fontSize))
    }

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
    if(FacetRows > 1L || FacetCols > 1L) p1 <- echarts4r::e_facet(e = p1, rows = FacetRows, cols = FacetCols, legend_space = 16, legend_pos = "top")
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
#' @param dt source data.table
#' @param YVar Y-Axis variable name
#' @param XVar X-Axis variable name
#' @param ZVar Z-Axis variable name
#' @param YVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param XVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param ZVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param FacetRows Defaults to 1 which causes no faceting to occur vertically. Otherwise, supply a numeric value for the number of output grid rows
#' @param FacetCols Defaults to 1 which causes no faceting to occur horizontally. Otherwise, supply a numeric value for the number of output grid columns
#' @param FacetLevels Faceting rows x columns is the max number of levels allowed in a grid. If your GroupVar has more you can supply the levels to display.
#' @param Height = NULL,
#' @param Width = NULL,
#' @param Engine "plotly", "echarts4r"
#' @param EchartsTheme "dark-blue"
#' @param AggMethod 'mean', 'median', 'sum', 'sd', 'coeffvar', 'count'
#' @param NumberBins = 21
#' @param NumLevels_Y = 20
#' @param NumLevels_X = 20
#' @param Title "Heatmap"
#' @param ShowLabels character
#' @param Title.YAxis character
#' @param Title.XAxis character
#' @param BackGroundColor color outside of plot window. Rcolors and hex outside of plot window. Rcolors and hex character
#' @param ChartColor = '#001534'
#' @param FillColor = "#0066ff"
#' @param FillColorReverse character
#' @param GridColor = '#ffffff'
#' @export
Plot.HeatMap <- function(dt,
                         PreAgg = FALSE,
                         AggMethod = 'mean',
                         XVar = NULL,
                         YVar = NULL,
                         ZVar = NULL,
                         YVarTrans = "Identity",
                         XVarTrans = "Identity",
                         ZVarTrans = "Identity",
                         FacetRows = 1,
                         FacetCols = 1,
                         FacetLevels = NULL,
                         NumberBins = 21,
                         NumLevels_Y = 33,
                         NumLevels_X = 33,
                         Height = NULL,
                         Width = NULL,
                         Title = "Heatmap",
                         ShowLabels = FALSE,
                         Title.YAxis = NULL,
                         Title.XAxis = NULL,
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
                         Debug = FALSE) {

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

    # Transformation
    if(ZVarTrans != "Identity") {
      if(ZVarTrans == "PercRank") {
        dt1 <- AutoQuant::PercRank(data = dt1, ColNames = ZVar, GroupVars = c(XVar,YVar), Granularity = 0.0001, ScoreTable = FALSE)
      } else if(ZVarTrans == "Standardize") {
        dt1 <- AutoQuant::Standardize(data = dt1, ColNames = ZVar, GroupVars = c(XVar,YVar), Center = TRUE, Scale = TRUE, ScoreTable = FALSE)
      } else {
        dt1 <- AutoQuant::AutoTransformationCreate(data = dt1, ColumnNames = ZVar, Methods = ZVarTrans)$Data
      }
    }

    # Formatting
    vals <- unique(scales::rescale(c(dt1[[ZVar]])))
    o <- order(vals, decreasing = FALSE)
    cols <- scales::col_numeric("Purples", domain = NULL)(vals)
    colz <- setNames(data.frame(vals[o], cols[o]), NULL)
    data.table::setnames(dt1, ZVar, "Measure_Variable")
    data.table::setorderv(x = dt1, cols = c(XVar,YVar),c(1L,1L))

    # Create final data for plot
    if(Engine == "Plotly") {

      Width <- as.integer(gsub("[^\\d]+", "", Width, perl=TRUE))
      Height <- as.integer(gsub("[^\\d]+", "", Height, perl=TRUE))

      p1 <- plotly::plot_ly(
        dt1,
        x = ~get(XVar),
        y = ~get(YVar),
        z = ~Measure_Variable,
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
      g <- "Measure_Variable"
      p1 <- echarts4r::e_charts_(
        data = dt1,
        x = XVar,
        darkMode = TRUE,
        width = Width,
        height = Height)#, dispose = TRUE)

      if(ShowLabels) {
        p1 <- echarts4r::e_heatmap_(e = p1, YVar, g, itemStyle = list(emphasis = list(shadowBlur = 10)), label = list(show = TRUE))
      } else {
        p1 <- echarts4r::e_heatmap_(e = p1, YVar, g, itemStyle = list(emphasis = list(shadowBlur = 10)))
      }

      p1 <- echarts4r::e_visual_map_(e = p1, g, show = FALSE)
      p1 <- echarts4r::e_tooltip(e = p1, trigger = "axis", backgroundColor = "aliceblue")
      p1 <- echarts4r::e_theme(e = p1, name = EchartsTheme)
      p1 <- echarts4r::e_aria(e = p1, enabled = TRUE)
      p1 <- echarts4r::e_toolbox_feature(e = p1, feature = c("saveAsImage","dataZoom"))
      p1 <- echarts4r::e_show_loading(e = p1, hide_overlay = TRUE, text = "Calculating...", color = "#000", text_color = TextColor, mask_color = "#000")

      if(length(Title.XAxis) == 0L) {
        p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "x", name = XVar, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = xaxis.fontSize))
      } else {
        p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "x", name = Title.XAxis, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = xaxis.fontSize))
      }

      if(length(Title.YAxis) == 0L) {
        p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "y", name = YVar, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = yaxis.fontSize))
      } else {
        p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "y", name = Title.YAxis, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = yaxis.fontSize))
      }

      p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "z", name = ZVar, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = yaxis.fontSize))
      p1 <- echarts4r::e_brush(e = p1)
      p1 <- echarts4r::e_title(
        p1, Title,
        textStyle = list(
          color = TextColor,
          fontWeight = title.fontWeight,
          overflow = "truncate",
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

      # Top XVar Levels
      temp <- dt1[, lapply(.SD, mean, na.rm = TRUE), .SDcols = c('Measure_Variable'), by = c(XVar)][order(-Measure_Variable)]
      temp <- temp[seq_len(min(NumLevels_X, temp[, .N]))][[1L]]
      dt1 <- dt1[get(XVar) %in% eval(temp)]

      # Transformation
      if(ZVarTrans != "Identity") {
        if(ZVarTrans == "PercRank") {
          dt1 <- AutoQuant::PercRank(data = dt1, ColNames = "Measure_Variable", GroupVars = c(XVar,YVar), Granularity = 0.0001, ScoreTable = FALSE)
        } else if(ZVarTrans == "Standardize") {
          dt1 <- AutoQuant::Standardize(data = dt1, ColNames = "Measure_Variable", GroupVars = c(XVar,YVar), Center = TRUE, Scale = TRUE, ScoreTable = FALSE)
        } else {
          dt1 <- AutoQuant::AutoTransformationCreate(data = dt1, ColumnNames = "Measure_Variable", Methods = ZVarTrans)$Data
        }
      }

      # Formatting
      vals <- unique(scales::rescale(c(dt1[['Measure_Variable']])))
      o <- order(vals, decreasing = FALSE)
      cols <- scales::col_numeric("Purples", domain = NULL)(vals)
      colz <- setNames(data.frame(vals[o], cols[o]), NULL)
    }

    # Create final data for plot
    if(Engine == "Plotly") {

      Width <- as.integer(gsub("[^\\d]+", "", Width, perl=TRUE))
      Height <- as.integer(gsub("[^\\d]+", "", Height, perl=TRUE))

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
      p1 <- echarts4r::e_charts_(
        data = dt1,
        x = XVar,
        darkMode = TRUE,
        dispose = TRUE,
        width = Width,
        height = Height)

      if(ShowLabels) {
        p1 <- echarts4r::e_heatmap_(e = p1, YVar, g, itemStyle = list(emphasis = list(shadowBlur = 10)), label = list(show = TRUE))
      } else {
        p1 <- echarts4r::e_heatmap_(e = p1, YVar, g, itemStyle = list(emphasis = list(shadowBlur = 10)))
      }

      p1 <- echarts4r::e_visual_map_(e = p1, g, show = FALSE)
      if(FacetRows == 1L && FacetCols == 1L) {
        if(X_Scroll) p1 <- echarts4r::e_datazoom(e = p1, x_index = c(0,1))
        if(Y_Scroll) p1 <- echarts4r::e_datazoom(e = p1, y_Index = c(0,1))
      }
      p1 <- echarts4r::e_theme(e = p1, name = EchartsTheme)
      p1 <- echarts4r::e_aria(e = p1, enabled = TRUE)
      p1 <- echarts4r::e_show_loading(e = p1, hide_overlay = TRUE, text = "Calculating...", color = "#000", text_color = TextColor, mask_color = "#000")

      if(length(Title.XAxis) == 0L) {
        p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "x", name = XVar, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = xaxis.fontSize))
      } else {
        p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "x", name = Title.XAxis, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = xaxis.fontSize))
      }

      if(length(Title.YAxis) == 0L) {
        p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "y", name = YVar, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = yaxis.fontSize))
      } else {
        p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "y", name = Title.YAxis, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = yaxis.fontSize))
      }

      p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "z", name = ZVar, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = yaxis.fontSize))
      p1 <- echarts4r::e_brush(e = p1)
      p1 <- echarts4r::e_tooltip(e = p1, trigger = "axis", backgroundColor = "aliceblue")
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

      # Transformation
      if(ZVarTrans != "Identity") {
        if(ZVarTrans == "PercRank") {
          dt1 <- AutoQuant::PercRank(data = dt1, ColNames = "Measure_Variable", GroupVars = c(XVar,YVar), Granularity = 0.0001, ScoreTable = FALSE)
        } else if(ZVarTrans == "Standardize") {
          dt1 <- AutoQuant::Standardize(data = dt1, ColNames = "Measure_Variable", GroupVars = c(XVar,YVar), Center = TRUE, Scale = TRUE, ScoreTable = FALSE)
        } else {
          dt1 <- AutoQuant::AutoTransformationCreate(data = dt1, ColumnNames = "Measure_Variable", Methods = ZVarTrans)$Data
        }
      }

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
      Height <- as.integer(gsub("[^\\d]+", "", Height, perl=TRUE))

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
      p1 <- echarts4r::e_charts_(
        data = dt1,
        x = XVar,
        dispose = TRUE,
        darkMode = TRUE,
        width = Width,
        height = Height)

      if(ShowLabels) {
        p1 <- echarts4r::e_heatmap_(e = p1, YVar, g, itemStyle = list(emphasis = list(shadowBlur = 10)), label = list(show = TRUE))
      } else {
        p1 <- echarts4r::e_heatmap_(e = p1, YVar, g, itemStyle = list(emphasis = list(shadowBlur = 10)))
      }

      p1 <- echarts4r::e_visual_map_(e = p1, g, show = FALSE)
      p1 <- echarts4r::e_theme(e = p1, name = EchartsTheme)
      p1 <- echarts4r::e_datazoom(e = p1, x_index = c(0,1))
      p1 <- echarts4r::e_datazoom(e = p1, y_index = c(0,1))
      p1 <- echarts4r::e_aria(e = p1, enabled = TRUE)
      p1 <- echarts4r::e_show_loading(e = p1, hide_overlay = TRUE, text = "Calculating...", color = "#000", text_color = TextColor, mask_color = "#000")

      if(length(Title.XAxis) == 0L) {
        p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "x", name = XVar, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = xaxis.fontSize))
      } else {
        p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "x", name = Title.XAxis, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = xaxis.fontSize))
      }

      if(length(Title.YAxis) == 0L) {
        p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "y", name = YVar, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = yaxis.fontSize))
      } else {
        p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "y", name = Title.YAxis, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = yaxis.fontSize))
      }

      p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "z", name = ZVar, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = yaxis.fontSize))
      p1 <- echarts4r::e_brush(e = p1)
      p1 <- echarts4r::e_tooltip(e = p1, trigger = "axis", backgroundColor = "aliceblue")
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
      if(Debug) print("Echarts PreAgg 1")
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

    # Transformation
    if(ZVarTrans != "Identity") {
      if(ZVarTrans == "PercRank") {
        dt1 <- AutoQuant::PercRank(data = dt1, ColNames = ZVar, GroupVars = c(XVar,YVar), Granularity = 0.0001, ScoreTable = FALSE)
      } else if(ZVarTrans == "Standardize") {
        dt1 <- AutoQuant::Standardize(data = dt1, ColNames = ZVar, GroupVars = c(XVar,YVar), Center = TRUE, Scale = TRUE, ScoreTable = FALSE)
      } else {
        dt1 <- AutoQuant::AutoTransformationCreate(data = dt1, ColumnNames = ZVar, Methods = ZVarTrans)$Data
      }
    }

    # Create final dt1 for plot
    if(Engine == "Plotly") {

      Width <- as.integer(gsub("[^\\d]+", "", Width, perl=TRUE))
      Height <- as.integer(gsub("[^\\d]+", "", Height, perl=TRUE))

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
      if(XVar %in% c("Predict","p1")) data.table::setorderv(x = dt1, "Predict")
      p1 <- echarts4r::e_charts_(
        data = dt1,
        x = XVar,
        dispose = TRUE,
        darkMode = TRUE,
        width = Width,
        height = Height)

      if(ShowLabels) {
        p1 <- echarts4r::e_heatmap_(e = p1, YVar, ZVar, itemStyle = list(emphasis = list(shadowBlur = 10)), label = list(show = TRUE))
      } else {
        p1 <- echarts4r::e_heatmap_(e = p1, YVar, ZVar, itemStyle = list(emphasis = list(shadowBlur = 10)))
      }

      p1 <- echarts4r::e_visual_map_(e = p1, ZVar, show = FALSE)
      p1 <- echarts4r::e_theme(e = p1, name = EchartsTheme)
      p1 <- echarts4r::e_datazoom(e = p1, x_index = c(0,1))
      p1 <- echarts4r::e_datazoom(e = p1, y_index = c(0,1))
      p1 <- echarts4r::e_aria(e = p1, enabled = TRUE)
      p1 <- echarts4r::e_show_loading(e = p1, hide_overlay = TRUE, text = "Calculating...", color = "#000", text_color = TextColor, mask_color = "#000")

      if(length(Title.XAxis) == 0L) {
        p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "x", name = XVar, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = xaxis.fontSize))
      } else {
        p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "x", name = Title.XAxis, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = xaxis.fontSize))
      }

      if(length(Title.YAxis) == 0L) {
        p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "y", name = YVar, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = yaxis.fontSize))
      } else {
        p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "y", name = Title.YAxis, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = yaxis.fontSize))
      }

      p1 <- echarts4r::e_brush(e = p1)
      p1 <- echarts4r::e_tooltip(e = p1, trigger = "axis", backgroundColor = "aliceblue")
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
# > Relationships Plot Functions                                              ----
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

#' @title Plot.CorrMatrix
#'
#' @description Build a violin plot by simply passing arguments to a single function. It will sample your data using SampleSize number of rows. Sampled data is randomized.
#'
#' @family Standard Plots
#'
#' @author Adrian Antico
#'
#' @param dt source data.table
#' @param CorrVars vector of variable names
#' @param CorrVarsTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param FacetRows Defaults to 1 which causes no faceting to occur vertically. Otherwise, supply a numeric value for the number of output grid rows
#' @param FacetCols Defaults to 1 which causes no faceting to occur horizontally. Otherwise, supply a numeric value for the number of output grid columns
#' @param FacetLevels Faceting rows x columns is the max number of levels allowed in a grid. If your GroupVar has more you can supply the levels to display.
#' @param Method character
#' @param Height = NULL,
#' @param Width = NULL,
#' @param Title character
#' @param ShowLabels character
#' @param Title.YAxis character
#' @param Title.XAxis character
#' @param Engine "Echarts" or "Plotly"
#' @param EchartsTheme "auritus","azul","bee-inspired","blue","caravan","carp","chalk","cool","dark-bold","dark","eduardo", #' "essos","forest","fresh-cut","fruit","gray","green","halloween","helianthus","infographic","inspired", #' "jazz","london","dark","macarons","macarons2","mint","purple-passion","red-velvet","red","roma","royal", #' "sakura","shine","tech-blue","vintage","walden","wef","weforum","westeros","wonderland"
#' @param X_Scroll logical
#' @param Y_Scroll logical
#' @param PreAgg logical
#' @param BackGroundColor color outside of plot window. Rcolors and hex outside of plot window. Rcolors and hex character
#' @param ChartColor character hex
#' @param FillColor character hex
#' @param FillColorReverse character
#' @param GridColor character hex
#' @param TextColor character hex
#' @param Debug Debugging purposes
#' @export
Plot.CorrMatrix <- function(dt = NULL,
                            CorrVars = NULL,
                            CorrVarTrans = "Identity",
                            FacetRows = 1,
                            FacetCols = 1,
                            FacetLevels = NULL,
                            Method = 'spearman',
                            PreAgg = FALSE,
                            Height = NULL,
                            Width = NULL,
                            Title = "Correlation Matrix",
                            ShowLabels = FALSE,
                            Title.YAxis = NULL,
                            Title.XAxis = NULL,
                            Engine = "Plotly",
                            EchartsTheme = "macarons",
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

    # Transformation
    if(CorrVarTrans != "Identity") {
      if(CorrVarTrans == "PercRank") {
        dt1 <- AutoQuant::PercRank(data = dt1, ColNames = CorrVars, GroupVars = NULL, Granularity = 0.0001, ScoreTable = FALSE)
      } else if(CorrVarTrans == "Standardize") {
        dt1 <- AutoQuant::Standardize(data = dt1, ColNames = CorrVars, GroupVars = NULL, Center = TRUE, Scale = TRUE, ScoreTable = FALSE)
      } else {
        dt1 <- AutoQuant::AutoTransformationCreate(data = dt1, ColumnNames = CorrVars, Methods = CorrVarTrans)$Data
      }
    }
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
    Height <- as.integer(gsub("[^\\d]+", "", Height, perl=TRUE))

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
    if(Debug) print(Width)
    if(Debug) print(Height)
    if(Debug) print(corr_mat)
    p1 <- echarts4r::e_charts(data = corr_mat, width = Width, height = Height)
    p1 <- echarts4r::e_correlations(e = p1, order = "hclust")
    p1 <- echarts4r::e_tooltip(e = p1, trigger = "axis", backgroundColor = "aliceblue")
    if(FacetRows == 1L && FacetCols == 1L) {
      if(X_Scroll) p1 <- echarts4r::e_datazoom(e = p1, x_index = c(0,1))
      if(Y_Scroll) p1 <- echarts4r::e_datazoom(e = p1, y_Index = c(0,1))
    }
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
#' @param dt source data.table
#' @param SampleSize An integer for the number of rows to use. Sampled data is randomized. If NULL then ignored
#' @param YVar Y-Axis variable name
#' @param XVar X-Axis variable name
#' @param GroupVar Requires an XVar and YVar already be defined
#' @param YVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param XVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param FacetRows Defaults to 1 which causes no faceting to occur vertically. Otherwise, supply a numeric value for the number of output grid rows
#' @param FacetCols Defaults to 1 which causes no faceting to occur horizontally. Otherwise, supply a numeric value for the number of output grid columns
#' @param FacetLevels Faceting rows x columns is the max number of levels allowed in a grid. If your GroupVar has more you can supply the levels to display.
#' @param Height = NULL,
#' @param Width = NULL,
#' @param Title 'Copula Plot'
#' @param ShowLabels character
#' @param Title.YAxis character
#' @param Title.XAxis character
#' @param Engine = "Plotly",
#' @param EchartsTheme = "dark-blue",
#' @param TimeLine Logical
#' @param X_Scroll logical
#' @param Y_Scroll logical
#' @param ZeroLineColor = '#ffff',
#' @param ZeroLineWidth = 2,
#' @param BackGroundColor color outside of plot window. Rcolors and hex outside of plot window. Rcolors and hex character
#' @param ChartColor 'lightsteelblue'
#' @param FillColor 'gray'
#' @param GridColor 'white'
#' @param FillColorReverse character
#' @param TextColor 'darkblue'
#' @param Debug Debugging purposes
#' @export
Plot.Copula <- function(dt = NULL,
                        SampleSize = 30000L,
                        XVar = NULL,
                        YVar = NULL,
                        GroupVar = NULL,
                        YVarTrans = "Identity",
                        XVarTrans = "Identity",
                        FacetRows = 1,
                        FacetCols = 1,
                        FacetLevels = NULL,
                        Height = NULL,
                        Width = NULL,
                        Title = 'Copula Plot',
                        ShowLabels = FALSE,
                        Title.YAxis = NULL,
                        Title.XAxis = NULL,
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

  if(length(GroupVar) == 0L) TimeLine <- FALSE

  X.HoverFormat <- "%{xaxis.title.text}: %{x:,.2f}<br>"
  Y.HoverFormat <- "%{yaxis.title.text}: %{y:,.2f}<br>"

  if(TimeLine && length(FacetLevels) > 0) X_Scroll <- FALSE

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
      Height <- as.integer(gsub("[^\\d]+", "", Height, perl=TRUE))

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
      p1 <- echarts4r::e_charts_(
        dt1,
        x = XVar,
        dispose = TRUE,
        darkMode = TRUE,
        width = Width,
        height = Height)

      if(ShowLabels) {
        p1 <- echarts4r::e_scatter_(e = p1, YVar, color = YVar, label = list(show = TRUE))
      } else {
        p1 <- echarts4r::e_scatter_(e = p1, YVar, color = YVar)
      }

      p1 <- echarts4r::e_glm(e = p1, smooth = TRUE, formula = get(YVar) ~ get(XVar))
      p1 <- echarts4r::e_visual_map_(e = p1, scale = echarts4r::e_scale, show = FALSE)
      if(FacetRows == 1L && FacetCols == 1L) {
        if(X_Scroll) p1 <- echarts4r::e_datazoom(e = p1, x_index = c(0,1))
        if(Y_Scroll) p1 <- echarts4r::e_datazoom(e = p1, y_Index = c(0,1))
      }
      p1 <- echarts4r::e_theme(e = p1, name = EchartsTheme)
      p1 <- echarts4r::e_aria(e = p1, enabled = TRUE)
      p1 <- echarts4r::e_tooltip(e = p1, trigger = "axis", backgroundColor = "aliceblue")
      p1 <- echarts4r::e_toolbox_feature(e = p1, feature = c("saveAsImage","dataZoom"))
      p1 <- echarts4r::e_show_loading(e = p1, hide_overlay = TRUE, text = "Calculating...", color = "#000", text_color = TextColor, mask_color = "#000")
      p1 <- echarts4r::e_legend(e = p1, type = "scroll", orient = "vertical", right = 50, top = 40, height = "240px", textStyle = list(color = TextColor, fontWeight = "bold"))

      if(length(Title.XAxis) == 0L) {
        p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "x", name = XVar, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = xaxis.fontSize))
      } else {
        p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "x", name = Title.XAxis, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = xaxis.fontSize))
      }

      if(length(Title.YAxis) == 0L) {
        p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "y", name = YVar, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = yaxis.fontSize))
      } else {
        p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "y", name = Title.YAxis, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = yaxis.fontSize))
      }

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

    if(length(FacetLevels) > 0L) {
      dt1 <- dt1[get(GroupVar) %in% c(eval(FacetLevels)), .SD, .SDcols = c(YVar,XVar,GroupVar)]
    }

    if(Debug) print('Plot.Copula length(GroupVar) > 0L')
    if(Engine == "Plotly") {

      Width <- as.integer(gsub("[^\\d]+", "", Width, perl=TRUE))
      Height <- as.integer(gsub("[^\\d]+", "", Height, perl=TRUE))

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
        p1 <- echarts4r::e_charts_(
          dt1 |> dplyr::group_by(get(GroupVar[1L])),
          x = XVar,
          colorBy = GroupVar[1L],
          timeline = TRUE,
          darkMode = TRUE,
          emphasis = list(focus = "series"),
          dispose = TRUE,
          width = Width,
          height = Height)
      } else {
        p1 <- echarts4r::e_charts_(
          dt1 |> dplyr::group_by(get(GroupVar[1L])),
          x = XVar,
          dispose = TRUE,
          darkMode = TRUE,
          emphasis = list(focus = "series"),
          width = Width,
          height = Height)
      }
      p1 <- echarts4r::e_scatter_(e = p1, YVar)
      p1 <- echarts4r::e_glm(e = p1, smooth = TRUE, formula = get(YVar) ~ get(XVar))
      p1 <- echarts4r::e_visual_map_(e = p1, scale = echarts4r::e_scale, show = FALSE)
      if(FacetRows == 1L && FacetCols == 1L) {
        if(X_Scroll && length(GroupVar) == 0L) p1 <- echarts4r::e_datazoom(e = p1, x_index = c(0,1))
        if(Y_Scroll) p1 <- echarts4r::e_datazoom(e = p1, y_Index = c(0,1))
      }
      p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "x", name = XVar)
      p1 <- echarts4r::e_theme(e = p1, name = EchartsTheme)
      p1 <- echarts4r::e_aria(e = p1, enabled = TRUE)
      p1 <- echarts4r::e_tooltip(e = p1, trigger = "axis", backgroundColor = "aliceblue")
      p1 <- echarts4r::e_toolbox_feature(e = p1, feature = c("saveAsImage","dataZoom"))
      p1 <- echarts4r::e_show_loading(e = p1, hide_overlay = TRUE, text = "Calculating...", color = "#000", text_color = TextColor, mask_color = "#000")

      if(length(Title.XAxis) == 0L) {
        p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "x", name = XVar, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = xaxis.fontSize))
      } else {
        p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "x", name = Title.XAxis, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = xaxis.fontSize))
      }

      if(length(Title.YAxis) == 0L) {
        p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "y", name = YVar, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = yaxis.fontSize))
      } else {
        p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "y", name = Title.YAxis, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = yaxis.fontSize))
      }

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
      if(FacetRows > 1L || FacetCols > 1L) {
        p1 <- echarts4r::e_facet(e = p1, rows = FacetRows, cols = FacetCols, legend_space = 16, legend_pos = "top")
        p1 <- echarts4r::e_legend(e = p1, type = "scroll", orient = "horizontal", right = 50, top = 40, height = "240px", textStyle = list(color = TextColor, fontWeight = "bold"))
      } else {
        p1 <- echarts4r::e_legend(e = p1, type = "scroll", orient = "vertical", right = 50, top = 40, height = "240px", textStyle = list(color = TextColor, fontWeight = "bold"))
      }

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
#' @param dt source data.table
#' @param SampleSize An integer for the number of rows to use. Sampled data is randomized. If NULL then ignored
#' @param YVar Y-Axis variable name
#' @param XVar X-Axis variable name
#' @param ZVar Z-Axis variable name
#' @param GroupVar Requires an XVar and YVar already be defined
#' @param YVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param XVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param ZVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param FacetRows Defaults to 1 which causes no faceting to occur vertically. Otherwise, supply a numeric value for the number of output grid rows
#' @param FacetCols Defaults to 1 which causes no faceting to occur horizontally. Otherwise, supply a numeric value for the number of output grid columns
#' @param FacetLevels Faceting rows x columns is the max number of levels allowed in a grid. If your GroupVar has more you can supply the levels to display.
#' @param Height = NULL,
#' @param Width = NULL,
#' @param Title 'Copula3D Plot'
#' @param ShowLabels character
#' @param Title.YAxis character
#' @param Title.XAxis character
#' @param Engine = "Plotly"
#' @param EchartsTheme = "dark-blue"
#' @param TimeLine Logical
#' @param BackGroundColor color outside of plot window. Rcolors and hex outside of plot window. Rcolors and hex character
#' @param ChartColor 'lightsteelblue'
#' @param FillColor 'gray'
#' @param FillColorReverse character
#' @param TextColor 'darkblue'
#' @param GridColor 'white'
#' @param ZeroLineColor = '#ffff',
#' @param ZeroLineWidth = 2,
#' @param Debug Debugging purposes
#' @export
Plot.Copula3D <- function(dt = NULL,
                          SampleSize = 100000,
                          XVar = NULL,
                          YVar = NULL,
                          ZVar = NULL,
                          YVarTrans = "Identity",
                          XVarTrans = "Identity",
                          ZVarTrans = "Identity",
                          FacetRows = 1,
                          FacetCols = 1,
                          FacetLevels = NULL,
                          GroupVar = NULL,
                          Height = NULL,
                          Width = NULL,
                          Title = 'Copula 3D',
                          ShowLabels = FALSE,
                          Title.YAxis = NULL,
                          Title.XAxis = NULL,
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

  if(length(GroupVar) == 0L) TimeLine <- FALSE

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
      Height <- as.integer(gsub("[^\\d]+", "", Height, perl=TRUE))

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
      p1 <- p1 |> plotly::layout(
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
        darkMode = TRUE,
        emphasis = list(focus = "series"),
        timeline = TimeLine,
        colorBy = GroupVar[1L], dispose = TRUE, width = Width, height = Height)

      if(ShowLabels) {
        p1 <- echarts4r::e_scatter_3d_(e = p1, YVar, ZVar, ZVar, GroupVar[[1L]], label = list(show = TRUE))
      } else {
        p1 <- echarts4r::e_scatter_3d_(e = p1, YVar, ZVar, ZVar, GroupVar[[1L]])
      }

      p1 <- echarts4r::e_theme(e = p1, name = EchartsTheme)
      p1 <- echarts4r::e_aria(e = p1, enabled = TRUE)
      p1 <- echarts4r::e_legend(e = p1, type = "scroll", orient = "vertical", right = 50, top = 40, height = "240px", textStyle = list(color = GridColor))
      p1 <- echarts4r::e_tooltip(e = p1, trigger = "axis", backgroundColor = "aliceblue")
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

  } else {

    if(Debug) print('Plot.Copula3D length(GroupVar) == 0L')
    if(Engine == "Plotly") {

      Width <- as.integer(gsub("[^\\d]+", "", Width, perl=TRUE))
      Height <- as.integer(gsub("[^\\d]+", "", Height, perl=TRUE))

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
      p1 <- echarts4r::e_charts_(
        dt1,
        x = XVar,
        dispose = TRUE,
        darkMode = TRUE,
        width = Width,
        height = Height)

      if(ShowLabels) {
        p1 <- echarts4r::e_scatter_3d_(e = p1, YVar, ZVar, ZVar, label = list(show = TRUE))
      } else {
        p1 <- echarts4r::e_scatter_3d_(e = p1, YVar, ZVar, ZVar)
      }

      p1 <- echarts4r::e_theme(e = p1, name = EchartsTheme)
      p1 <- echarts4r::e_aria(e = p1, enabled = TRUE)
      p1 <- echarts4r::e_tooltip(e = p1, trigger = "axis", backgroundColor = "aliceblue")
      p1 <- echarts4r::e_toolbox_feature(e = p1, feature = c("saveAsImage","dataZoom"))
      p1 <- echarts4r::e_show_loading(e = p1, hide_overlay = TRUE, text = "Calculating...", color = "#000", text_color = TextColor, mask_color = "#000")

      if(length(Title.XAxis) == 0L) {
        p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "x", name = XVar, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = xaxis.fontSize))
      } else {
        p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "x", name = Title.XAxis, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = xaxis.fontSize))
      }

      if(length(Title.YAxis) == 0L) {
        p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "y", name = YVar, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = yaxis.fontSize))
      } else {
        p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "y", name = Title.YAxis, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = yaxis.fontSize))
      }

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
      if(FacetRows > 1L || FacetCols > 1L) p1 <- echarts4r::e_facet(e = p1, rows = FacetRows, cols = FacetCols, legend_space = 16, legend_pos = "top")

    }
  }

  # Return plot
  return(p1)
}

#' @title Plot.Scatter
#'
#' @description Build a copula plot by simply passing arguments to a single function. It will sample your data using SampleSize number of rows. Sampled data is randomized.
#'
#' @family Standard Plots
#'
#' @author Adrian Antico
#'
#' @param dt source data.table
#' @param SampleSize numeric
#' @param YVar Y-Axis variable name
#' @param XVar X-Axis variable name
#' @param GroupVar Character variable
#' @param YVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param XVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param FacetRows Defaults to 1 which causes no faceting to occur vertically. Otherwise, supply a numeric value for the number of output grid rows
#' @param FacetCols Defaults to 1 which causes no faceting to occur horizontally. Otherwise, supply a numeric value for the number of output grid columns
#' @param FacetLevels Faceting rows x columns is the max number of levels allowed in a grid. If your GroupVar has more you can supply the levels to display.
#' @param Height = NULL,
#' @param Width = NULL,
#' @param Title character
#' @param ShowLabels character
#' @param Title.YAxis character
#' @param Title.XAxis character
#' @param Engine "Echarts" or "Plotly"
#' @param EchartsTheme "auritus","azul","bee-inspired","blue","caravan","carp","chalk","cool","dark-bold","dark","eduardo", #' "essos","forest","fresh-cut","fruit","gray","green","halloween","helianthus","infographic","inspired", #' "jazz","london","dark","macarons","macarons2","mint","purple-passion","red-velvet","red","roma","royal", #' "sakura","shine","tech-blue","vintage","walden","wef","weforum","westeros","wonderland"
#' @param TimeLine logical
#' @param X_Scroll logical
#' @param Y_Scroll logical
#' @param FillColor character hex
#' @param FillColorReverse character
#' @param BackGroundColor color outside of plot window. Rcolors and hex outside of plot window. Rcolors and hex characterb
#' @param ChartColor character hex
#' @param TextColor character hex
#' @param GridColor character hex
#' @param ZeroLineColor character hex
#' @param ZeroLineWidth numeric
#' @param Debug Debugging purposes
#' @export
Plot.Scatter <- function(dt = NULL,
                         SampleSize = 30000L,
                         XVar = NULL,
                         YVar = NULL,
                         GroupVar = NULL,
                         YVarTrans = "Identity",
                         XVarTrans = "Identity",
                         FacetRows = 1,
                         FacetCols = 1,
                         FacetLevels = NULL,
                         Height = NULL,
                         Width = NULL,
                         Title = 'Scatter Plot',
                         ShowLabels = FALSE,
                         Title.YAxis = NULL,
                         Title.XAxis = NULL,
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
                         tooltip.trigger = "axis",
                         Debug = FALSE) {

  if(length(GroupVar) == 0L) TimeLine <- FALSE

  X.HoverFormat <- "%{xaxis.title.text}: %{x:,.2f}<br>"
  Y.HoverFormat <- "%{yaxis.title.text}: %{y:,.2f}<br>"

  if(TimeLine && length(FacetLevels) > 0) X_Scroll <- FALSE

  # Cap number of records
  if(Debug) print('Plot.Scatter # Cap number of records')
  if(length(SampleSize) == 0L) SampleSize <- 30000L
  if(!data.table::is.data.table(dt)) data.table::setDT(dt)
  if(dt[,.N] > SampleSize) {
    dt1 <- dt[order(runif(.N))][seq_len(SampleSize)]
  } else {
    dt1 <- data.table::copy(dt)
  }

  # Transformation
  if(YVarTrans != "Identity") {
    if(YVarTrans == "PercRank") {
      dt1 <- AutoQuant::PercRank(data = dt1, ColNames = YVar, GroupVars = GroupVar, Granularity = 0.0001, ScoreTable = FALSE)
    } else if(YVarTrans == "Standardize") {
      dt1 <- AutoQuant::Standardize(data = dt1, ColNames = YVar, GroupVars = GroupVar, Center = TRUE, Scale = TRUE, ScoreTable = FALSE)
    } else {
      dt1 <- AutoQuant::AutoTransformationCreate(data = dt1, ColumnNames = YVar, Methods = YVarTrans)$Data
    }
  }

  if(length(GroupVar) == 0L) {
    if(Debug) print('Plot.Scatter  length(GroupVar) == 0L')
    if(Engine == "Plotly") {

      Width <- as.integer(gsub("[^\\d]+", "", Width, perl=TRUE))
      Height <- as.integer(gsub("[^\\d]+", "", Height, perl=TRUE))

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
      p1 <- echarts4r::e_charts_(
        dt1,
        x = XVar,
        dispose = TRUE,
        darkMode = TRUE,
        width = Width,
        height = Height)

      if(ShowLabels) {
        p1 <- echarts4r::e_scatter_3d_(e = p1, YVar, ZVar, ZVar, label = list(show = TRUE))
      } else {
        p1 <- echarts4r::e_scatter_(e = p1, YVar)
      }

      p1 <- echarts4r::e_glm(e = p1, smooth = TRUE, formula = get(YVar) ~ get(XVar))
      p1 <- echarts4r::e_visual_map_(e = p1, scale = echarts4r::e_scale, show = FALSE)
      if(FacetRows == 1L && FacetCols == 1L) {
        if(X_Scroll) p1 <- echarts4r::e_datazoom(e = p1, x_index = c(0,1))
        if(Y_Scroll) p1 <- echarts4r::e_datazoom(e = p1, y_Index = c(0,1))
      }
      p1 <- echarts4r::e_theme(e = p1, name = EchartsTheme)
      p1 <- echarts4r::e_aria(e = p1, enabled = TRUE)
      p1 <- echarts4r::e_tooltip(e = p1, trigger = tooltip.trigger, backgroundColor = "aliceblue")
      p1 <- echarts4r::e_toolbox_feature(e = p1, feature = c("saveAsImage","dataZoom"))
      p1 <- echarts4r::e_show_loading(e = p1, hide_overlay = TRUE, text = "Calculating...", color = "#000", text_color = TextColor, mask_color = "#000")
      p1 <- echarts4r::e_legend(e = p1, type = "scroll", orient = "vertical", right = 50, top = 40, height = "240px", textStyle = list(color = TextColor, fontWeight = "bold"))

      if(length(Title.XAxis) == 0L) {
        p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "x", name = XVar, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = xaxis.fontSize))
      } else {
        p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "x", name = Title.XAxis, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = xaxis.fontSize))
      }

      if(length(Title.YAxis) == 0L) {
        p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "y", name = YVar, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = yaxis.fontSize))
      } else {
        p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "y", name = Title.YAxis, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = yaxis.fontSize))
      }

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

    if(length(FacetLevels) > 0L) {
      dt1 <- dt1[get(GroupVar) %in% c(eval(FacetLevels)), .SD, .SDcols = c(YVar,XVar,GroupVar)]
    }

    if(Debug) print('Plot.Scatter  length(GroupVar) > 0L')
    if(Engine == "Plotly") {

      Width <- as.integer(gsub("[^\\d]+", "", Width, perl=TRUE))
      Height <- as.integer(gsub("[^\\d]+", "", Height, perl=TRUE))

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
        darkMode = TRUE,
        emphasis = list(focus = "series"),
        colorBy = GroupVar[1L], dispose = TRUE, width = Width, height = Height)

      if(ShowLabels) {
        p1 <- echarts4r::e_scatter_3d_(e = p1, YVar, ZVar, ZVar, label = list(show = TRUE))
      } else {
        p1 <- echarts4r::e_scatter_(e = p1, YVar)
      }

      p1 <- echarts4r::e_visual_map_(e = p1, scale = echarts4r::e_scale, show = FALSE)
      if(FacetRows == 1L && FacetCols == 1L) {
        if(X_Scroll && length(GroupVar) == 0L) p1 <- echarts4r::e_datazoom(e = p1, x_index = c(0,1))
        if(Y_Scroll) p1 <- echarts4r::e_datazoom(e = p1, y_Index = c(0,1))
      }
      p1 <- echarts4r::e_theme(e = p1, name = EchartsTheme)
      p1 <- echarts4r::e_aria(e = p1, enabled = TRUE)
      p1 <- echarts4r::e_tooltip(e = p1, trigger = "axis", backgroundColor = "aliceblue")
      p1 <- echarts4r::e_toolbox_feature(e = p1, feature = c("saveAsImage","dataZoom"))
      p1 <- echarts4r::e_show_loading(e = p1, hide_overlay = TRUE, text = "Calculating...", color = "#000", text_color = TextColor, mask_color = "#000")

      if(length(Title.XAxis) == 0L) {
        p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "x", name = XVar, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = xaxis.fontSize))
      } else {
        p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "x", name = Title.XAxis, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = xaxis.fontSize))
      }

      if(length(Title.YAxis) == 0L) {
        p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "y", name = YVar, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = yaxis.fontSize))
      } else {
        p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "y", name = Title.YAxis, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = yaxis.fontSize))
      }

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
      if(FacetRows > 1L || FacetCols > 1L) {
        p1 <- echarts4r::e_facet(e = p1, rows = FacetRows, cols = FacetCols, legend_space = 16, legend_pos = "top")
        p1 <- echarts4r::e_legend(e = p1, type = "scroll", orient = "horizontal", right = 50, top = 40, height = "240px", textStyle = list(color = TextColor, fontWeight = "bold"))
      } else {
        p1 <- echarts4r::e_legend(e = p1, type = "scroll", orient = "vertical", right = 50, top = 40, height = "240px", textStyle = list(color = TextColor, fontWeight = "bold"))
      }

    }
  }

  # Return plot
  return(p1)
}

#' @title Plot.Scatter3D
#'
#' @description Build a 3D-copula plot by simply passing arguments to a single function. It will sample your data using SampleSize number of rows. Sampled data is randomized.
#'
#' @family Standard Plots
#'
#' @author Adrian Antico
#'
#' @param dt source data.table
#' @param SampleSize An integer for the number of rows to use. Sampled data is randomized. If NULL then ignored
#' @param YVar Y-Axis variable name
#' @param XVar X-Axis variable name
#' @param ZVar Z-Axis variable name
#' @param GroupVar Requires an XVar and YVar already be defined
#' @param YVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param XVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param ZVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param FacetRows Defaults to 1 which causes no faceting to occur vertically. Otherwise, supply a numeric value for the number of output grid rows
#' @param FacetCols Defaults to 1 which causes no faceting to occur horizontally. Otherwise, supply a numeric value for the number of output grid columns
#' @param FacetLevels Faceting rows x columns is the max number of levels allowed in a grid. If your GroupVar has more you can supply the levels to display.
#' @param Height = NULL,
#' @param Width = NULL,
#' @param Title 'Violin Plot'
#' @param ShowLabels character
#' @param Title.YAxis character
#' @param Title.XAxis character
#' @param Engine = "Plotly"
#' @param EchartsTheme = "macaron"
#' @param TimeLine Logical
#' @param FillColor 'gray'
#' @param FillColorReverse character
#' @param BackGroundColor color outside of plot window. Rcolors and hex outside of plot window. Rcolors and hex character
#' @param ChartColor 'lightsteelblue'
#' @param GridColor 'white'
#' @param TextColor 'darkblue'
#' @param ZeroLineColor = '#ffff',
#' @param ZeroLineWidth = 2,
#' @param Debug Debugging purposes
#' @export
Plot.Scatter3D <- function(dt = NULL,
                           SampleSize = 100000,
                           XVar = NULL,
                           YVar = NULL,
                           ZVar = NULL,
                           GroupVar = NULL,
                           YVarTrans = "Identity",
                           XVarTrans = "Identity",
                           ZVarTrans = "Identity",
                           FacetRows = 1,
                           FacetCols = 1,
                           FacetLevels = NULL,
                           Height = NULL,
                           Width = NULL,
                           Title = '3D Scatter',
                           ShowLabels = FALSE,
                           Title.YAxis = NULL,
                           Title.XAxis = NULL,
                           Engine = "Plotly",
                           EchartsTheme = "macarons",
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

  if(length(GroupVar) == 0L) TimeLine <- FALSE

  # Cap number of records
  if(Debug) print('Plot.Scatter3D # Cap number of records')
  if(length(SampleSize) == 0L) SampleSize <- 30000L
  if(dt[,.N] > SampleSize) {
    dt1 <- dt[order(runif(.N))][seq_len(SampleSize)]
  } else {
    dt1 <- data.table::copy(dt)
  }

  # Transformation
  if(YVarTrans != "Identity") {
    if(YVarTrans == "PercRank") {
      dt1 <- AutoQuant::PercRank(data = dt1, ColNames = YVar, GroupVars = GroupVar, Granularity = 0.0001, ScoreTable = FALSE)
    } else if(YVarTrans == "Standardize") {
      dt1 <- AutoQuant::Standardize(data = dt1, ColNames = YVar, GroupVars = GroupVar, Center = TRUE, Scale = TRUE, ScoreTable = FALSE)
    } else {
      dt1 <- AutoQuant::AutoTransformationCreate(data = dt1, ColumnNames = YVar, Methods = YVarTrans)$Data
    }
  }

  # Transformation
  if(XVarTrans != "Identity") {
    if(XVarTrans == "PercRank") {
      dt1 <- AutoQuant::PercRank(data = dt1, ColNames = XVar, GroupVars = GroupVar, Granularity = 0.0001, ScoreTable = FALSE)
    } else if(YVarTrans == "Standardize") {
      dt1 <- AutoQuant::Standardize(data = dt1, ColNames = XVar, GroupVars = GroupVar, Center = TRUE, Scale = TRUE, ScoreTable = FALSE)
    } else {
      dt1 <- AutoQuant::AutoTransformationCreate(data = dt1, ColumnNames = XVar, Methods = XVarTrans)$Data
    }
  }

  # Transformation
  if(ZVarTrans != "Identity") {
    if(ZVarTrans == "PercRank") {
      dt1 <- AutoQuant::PercRank(data = dt1, ColNames = ZVar, GroupVars = GroupVar, Granularity = 0.0001, ScoreTable = FALSE)
    } else if(YVarTrans == "Standardize") {
      dt1 <- AutoQuant::Standardize(data = dt1, ColNames = ZVar, GroupVars = GroupVar, Center = TRUE, Scale = TRUE, ScoreTable = FALSE)
    } else {
      dt1 <- AutoQuant::AutoTransformationCreate(data = dt1, ColumnNames = ZVar, Methods = ZVarTrans)$Data
    }
  }

  if(length(GroupVar) > 0L) {
    if(Debug) print('Plot.Scatter3D length(GroupVar) > 0L')
    if(Engine == "Plotly") {

      Width <- as.integer(gsub("[^\\d]+", "", Width, perl=TRUE))
      Height <- as.integer(gsub("[^\\d]+", "", Height, perl=TRUE))

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
      p1 <- echarts4r::e_charts_(
        dt1 |> dplyr::group_by(get(GroupVar[1L])),
        x = XVar,
        timeline = TimeLine,
        colorBy = GroupVar[1L],
        dispose = TRUE,
        darkMode = TRUE,
        emphasis = list(focus = "series"),
        width = Width,
        height = Height)

      if(ShowLabels) {
        p1 <- echarts4r::e_scatter_3d_(e = p1, YVar, ZVar, ZVar, GroupVar[1L], label = list(show = TRUE))
      } else {
        p1 <- echarts4r::e_scatter_3d_(e = p1, YVar, ZVar, ZVar, GroupVar[1L])
      }

      p1 <- echarts4r::e_theme(e = p1, name = EchartsTheme)
      p1 <- echarts4r::e_aria(e = p1, enabled = TRUE)
      p1 <- echarts4r::e_tooltip(e = p1, trigger = "axis", backgroundColor = "aliceblue")
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
      if(FacetRows > 1L || FacetCols > 1L) {
        p1 <- echarts4r::e_facet(e = p1, rows = FacetRows, cols = FacetCols, legend_space = 16, legend_pos = "top")
        p1 <- echarts4r::e_legend(e = p1, type = "scroll", orient = "horizontal", right = 50, top = 40, height = "240px", textStyle = list(color = TextColor, fontWeight = "bold"))
      } else {
        p1 <- echarts4r::e_legend(e = p1, type = "scroll", orient = "vertical", right = 50, top = 40, height = "240px", textStyle = list(color = TextColor, fontWeight = "bold"))
      }

    }

  } else {

    if(Debug) print('Plot.Scatter3D length(GroupVar) == 0L')
    if(Engine == "Plotly") {

      Width <- as.integer(gsub("[^\\d]+", "", Width, perl=TRUE))
      Height <- as.integer(gsub("[^\\d]+", "", Height, perl=TRUE))

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
      p1 <- echarts4r::e_charts_(
        dt1 |> dplyr::group_by(GroupVar[[1L]]),
        x = XVar,
        timeline = TRUE,
        dispose = TRUE,
        darkMode = TRUE,
        emphasis = list(focus = "series"),
        width = Width,
        height = Height)

      if(ShowLabels) {
        p1 <- echarts4r::e_scatter_3d_(e = p1, YVar, ZVar, ZVar, GroupVar[[1L]], label = list(show = TRUE))
      } else {
        p1 <- echarts4r::e_scatter_3d_(e = p1, YVar, ZVar, ZVar, GroupVar[[1L]])
      }

      p1 <- echarts4r::e_theme(e = p1, name = EchartsTheme)
      p1 <- echarts4r::e_aria(e = p1, enabled = TRUE)
      p1 <- echarts4r::e_tooltip(e = p1, trigger = "axis", backgroundColor = "aliceblue")
      p1 <- echarts4r::e_toolbox_feature(e = p1, feature = c("saveAsImage","dataZoom"))
      p1 <- echarts4r::e_show_loading(e = p1, hide_overlay = TRUE, text = "Calculating...", color = "#000", text_color = TextColor, mask_color = "#000")

      if(length(Title.XAxis) == 0L) {
        p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "x", name = XVar, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = xaxis.fontSize))
      } else {
        p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "x", name = Title.XAxis, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = xaxis.fontSize))
      }

      if(length(Title.YAxis) == 0L) {
        p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "y", name = YVar, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = yaxis.fontSize))
      } else {
        p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "y", name = Title.YAxis, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = yaxis.fontSize))
      }

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
    if(Debug) print(paste0('Working on first ', counter, ' ticker symbols'))
    x <- tryCatch({jsonlite::fromJSON(paste0(x$next_url, "&apiKey=hvyL7ZOsKK_5PNplOmv55tBTRd8rdA20"))}, error = function(x) 1)
    if(x != 1) {
      xx <- data.table::rbindlist(list(xx, data.table::setDT(x$results)), fill = TRUE, use.names = TRUE)
      counter <- counter + 1000L
      Sys.sleep(12L)
    } else {
      break
    }
  }
  #xx <- xx[, .SD, .SDcols = c(names(xx)[c(1,2,5,6,12)])]
  data.table::fwrite(xx, file = file.path('C:/Users/Bizon/Documents/GitHub/Rappture/inst/shiny-apps/Rappture/ticker_data.csv'))
  AutoQuant::PostGRE_RemoveCreateAppend(
    data = xx,
    TableName = "ticker_data",
    CloseConnection = TRUE,
    CreateSchema = NULL,
    Host = "localhost",
    DBName = "RemixAutoML",
    User = "postgres",
    Port = 5432,
    Password = "Aa1028#@",
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
#' @family Stock Plots
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
#' @param timeElapsed = 60
#'
#' @export
StockData <- function(PolyOut = NULL,
                      Symbol = 'TSLA',
                      CompanyName = 'Tesla Inc. Common Stock',
                      Metric = 'Stock Price',
                      TimeAgg = 'days',
                      StartDate = '2022-01-01',
                      EndDate = Sys.Date(),
                      APIKey = NULL,
                      timeElapsed = 61,
                      Debug = FALSE) {
  StartDate <- as.Date(StartDate)
  EndDate <- min(Sys.Date()-1, as.Date(EndDate))

  # Use data if provided
  #if(!data.table::is.data.table(PolyOut)) {
  print("data.table check here")
  print(data.table::is.data.table(PolyOut))
  if(!data.table::is.data.table(PolyOut)) {
    if(Debug) print("here 1a")
    PolyOut <- jsonlite::fromJSON(paste0("https://api.polygon.io/v2/aggs/ticker/",Symbol,"/range/1/day/",StartDate, "/", EndDate, "?adjusted=true&sort=asc&limit=10000&apiKey=", APIKey))

    data <- data.table::as.data.table(PolyOut$results)
    data[, Date := as.Date(lubridate::as_datetime((t+10800000)/1000, origin = "1970-01-01"))]
    if(Debug) print(head(data))

    tryCatch({
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

      if(Metric == '% Returns') {
        for(i in c('o','c','h','l')) data[, paste0(i) := get(i) / data.table::shift(x = get(i)) - 1]
        data <- data[seq_len(.N)[-1L]]
      } else if(Metric  == '% Log Returns') {
        for(i in c('o','c','h','l')) data[, paste0(i) := log(get(i)) - log(data.table::shift(x = get(i)))]
        data <- data[seq_len(.N)[-1L]]
      } else if(Metric  == 'Index') {
        for(i in c('o','c','h','l')) data[, paste0(i) := get(i) / data.table::first(get(i))]
      } else if(Metric  == 'Quadratic Variation') {
        for(i in c('o','c','h','l')) data[, temp_temp := data.table::shift(x = get(i), n = 1L, fill = NA, type = 'lag')][, paste0(i) := (get(i) - temp_temp)^2][, temp_temp := NULL]
        data <- data[seq_len(.N)[-1L]]
      }
    }, error = function(x) NULL)

  } else {
    if(Debug) print("here 1b")
    data <- PolyOut
    if(Debug) print(head(data))
  }

  return(list(results = data, PolyOut = PolyOut, CompanyName = CompanyName, Symbol = Symbol, Metric = Metric, StartDate = StartDate, EndDate = EndDate, APIKey = APIKey))
}

#' @title Plot.Stock
#'
#' @description  Create a candlestick plot for stocks. See https://plotly.com/r/figure-labels/
#'
#' @family Stock Plots
#' @author Adrian Antico
#'
#' @param Type 'candlestick', 'ohlc'
#' @param StockDataOutput PolyOut returned from StockData()
#' @param PlotEngineType = "Echarts" or "Plotly"
#' @param Width = "1450px"
#' @param Height = "600px"
#' @param EchartsTheme = "macarons"
#' @param ShadowBlur = 5. Chart boxes' shadow blur amount. This attribute should be used along with shadowColor,shadowOffsetX, shadowOffsetY to set shadow to component
#' @param ShadowColor "black"
#' @param ShadowOffsetX 0
#' @param ShadowOffsetY 0
#' @param TextColor = "white"
#' @param title.fontSize = 22
#' @param title.fontWeight = "bold", # norma
#' @param title.textShadowColor = '#63aeff'
#' @param title.textShadowBlur = 3
#' @param title.textShadowOffsetY = 1
#' @param title.textShadowOffsetX = -1
#' @param xaxis.fontSize = 14
#' @param yaxis.fontSize = 14
#'
#' @export
Plot.Stock <- function(StockDataOutput,
                       Type = 'candlestick',
                       Metric = "Stock Price",
                       PlotEngineType = "Echarts",
                       Width = NULL,
                       Height = NULL,
                       EchartsTheme = "macarons",
                       TextColor = "white",
                       ShadowBlur = 0,
                       ShadowColor = "black",
                       ShadowOffsetX = 0,
                       ShadowOffsetY = 0,
                       title.fontSize = 14,
                       title.fontWeight = "bold",
                       title.textShadowColor = '#63aeff',
                       title.textShadowBlur = 3,
                       title.textShadowOffsetY = 1,
                       title.textShadowOffsetX = -1,
                       Color = "green",
                       Color0 = "red",
                       BorderColor = "transparent",
                       BorderColor0 = "transparent",
                       BorderColorDoji = "transparent",
                       xaxis.fontSize = 14,
                       yaxis.fontSize = 14,
                       Debug = FALSE) {

# Width = "1450px"
# Height = "600px"
# EchartsTheme = "macarons"
# TextColor = "white"
# ShadowBlur = 5
# title.fontSize = 22
# title.fontWeight = "bold"
# title.textShadowColor = '#63aeff'
# title.textShadowBlur = 3
# title.textShadowOffsetY = 1
# title.textShadowOffsetX = -1
# Color = "green"
# Color0 = "red"
# BorderColor = "transparent"
# BorderColor0 = "transparent"
# BorderColorDoji = "transparent"
# xaxis.fontSize = 14
  print(StockDataOutput$results)
  if(missing(StockDataOutput)) stop('StockDataOutput cannot be missing')
  if(Type == 'CandlestickPlot') Type <- 'candlestick'
  if(PlotEngineType == "Plotly") {
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
    if(Debug) print('Plot.Stock 3')
    p1 <- plotly::layout(
      p = p1,
      font = AutoPlots:::font_(),
      title = if(length(StockDataOutput$CompanyName) == 0L) list(text = paste0(StockDataOutput$Symbol, ": ", StockDataOutput$StartDate, " to ", StockDataOutput$EndDate), font = 'Segoe UI') else list(text = paste0(StockDataOutput$CompanyName, " - ", StockDataOutput$Symbol, ": ", StockDataOutput$StartDate, " to ", StockDataOutput$EndDate), font = 'Segoe UI'),
      plot_bgcolor = '#001534',
      paper_bgcolor = "#6a6969",
      yaxis = list(title = AutoPlots:::bold_(StockDataOutput$Metric)),
      xaxis = list(title = AutoPlots:::bold_('Date')))
    if(Debug) print('Plot.Stock 3: done')
    return(p1)

  } else if(PlotEngineType == "Echarts") {

    # Build base plot depending on GroupVar availability
    dt <- StockDataOutput$results
    dt[, Date := as.character(Date)]
    p1 <- echarts4r::e_charts_(
      data = dt,
      x = "Date",
      dispose = TRUE,
      darkMode = TRUE,
      width = Width,
      height = Height)
    p1 <- echarts4r::e_candle_(
      e = p1,
      high = "h",
      low = "l",
      closing = "c",
      opening = "o",
      itemStyle = list(
        #shadowBlur = ShadowBlur,
        #shadowColor = ShadowColor,
        #shadowOffsetX = ShadowOffsetX,
        #shadowOffsetY = ShadowOffsetY,
        color = Color,
        color0 = Color0,
        backgroundColor = "white",
        borderColor = BorderColor,
        borderColor0 = BorderColor0,
        borderColorDoji = BorderColorDoji
      ),
      name = StockDataOutput$Symbol)

    # Finalize Plot Build
    p1 <- echarts4r::e_legend(e = p1, show = FALSE)
    p1 <- echarts4r::e_theme(e = p1, name = EchartsTheme)
    p1 <- echarts4r::e_aria(e = p1, enabled = TRUE)
    p1 <- echarts4r::e_tooltip(e = p1 , trigger = "axis")
    p1 <- echarts4r::e_toolbox_feature(e = p1, feature = c("saveAsImage","dataZoom"))
    p1 <- echarts4r::e_show_loading(e = p1, hide_overlay = TRUE, text = "Calculating...", color = "#000", text_color = TextColor, mask_color = "#000")

    p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "x", name = "Date", nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = xaxis.fontSize))
    p1 <- echarts4r::e_brush(e = p1)
    p1 <- echarts4r::e_datazoom(e = p1, x_index = c(0,1))
    p1 <- echarts4r::e_title(
      p1,
      text = if(length(StockDataOutput$CompanyName) == 0L) paste0(StockDataOutput$Symbol, ": ", StockDataOutput$StartDate, " to ", StockDataOutput$EndDate) else paste0(StockDataOutput$CompanyName, " - ", StockDataOutput$Symbol, ": ", StockDataOutput$StartDate, " to ", StockDataOutput$EndDate, " :: Measure: ", Metric),
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
    if(Debug) print("Plot.Line no group Echarts 9")
    return(p1)
  }
}

# ----

# ----

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
# > Model Evaluation Plots                                                    ----
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

#' @title Plot.Residuals.Histogram
#'
#' @description Residuals Plot
#'
#' @author Adrian Antico
#' @family Model Evaluation
#'
#' @param dt source data.table
#' @param AggMethod character
#' @param SampleSize numeric
#' @param XVar X-Axis variable name
#' @param YVar Y-Axis variable name
#' @param GroupVar Character variable
#' @param YVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param XVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param FacetRows Defaults to 1 which causes no faceting to occur vertically. Otherwise, supply a numeric value for the number of output grid rows
#' @param FacetCols Defaults to 1 which causes no faceting to occur horizontally. Otherwise, supply a numeric value for the number of output grid columns
#' @param FacetLevels Faceting rows x columns is the max number of levels allowed in a grid. If your GroupVar has more you can supply the levels to display.
#' @param NumberBins numeric
#' @param ZeroLineColor character hex
#' @param ZeroLineWidth numeric
#' @param Title character
#' @param ShowLabels character
#' @param Title.YAxis character
#' @param Title.XAxis character
#' @param Engine "Echarts" or "Plotly"
#' @param EchartsTheme "auritus","azul","bee-inspired","blue","caravan","carp","chalk","cool","dark-bold","dark","eduardo", #' "essos","forest","fresh-cut","fruit","gray","green","halloween","helianthus","infographic","inspired", #' "jazz","london","dark","macarons","macarons2","mint","purple-passion","red-velvet","red","roma","royal", #' "sakura","shine","tech-blue","vintage","walden","wef","weforum","westeros","wonderland"
#' @param TimeLine logical
#' @param X_Scroll logical
#' @param Y_Scroll logical
#' @param BackGroundColor color outside of plot window. Rcolors and hex outside of plot window. Rcolors and hex character
#' @param ChartColor character hex
#' @param FillColor character hex
#' @param FillColorReverse character hex
#' @param GridColor character hex
#' @param TextColor Not Implemented
#' @param Debug Debugging purposes
#' @export
Plot.Residuals.Histogram <- function(dt = NULL,
                                     AggMethod = 'mean',
                                     SampleSize = 100000,
                                     XVar = NULL,
                                     YVar = NULL,
                                     GroupVar = NULL,
                                     YVarTrans = "Identity",
                                     XVarTrans = "Identity",
                                     FacetRows = 1,
                                     FacetCols = 1,
                                     FacetLevels = NULL,
                                     NumberBins = 20,
                                     Height = NULL,
                                     Width = NULL,
                                     Title = 'Calibration Plot',
                                     ShowLabels = FALSE,
                                     Title.YAxis = NULL,
                                     Title.XAxis = NULL,
                                     Engine = 'Echarts',
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

  # Subset cols, define Target - Predicted, NULL YVar in data, Update YVar def, Ensure GroupVar is length(1)
  if(length(SampleSize) == 0L) SampleSize <- 30000L
  if(!data.table::is.data.table(dt)) data.table::setDT(dt)
  dt1 <- dt[, .SD, .SDcols = c(XVar,YVar,GroupVar)]
  dt1[, `Target - Predicted` := get(YVar) - get(XVar)]
  data.table::set(dt1, j = c(YVar), value = NULL)
  YVar <- "Target - Predicted"
  if(length(GroupVar) > 0L) GroupVar <- GroupVar[1L]

  # Faceting shrink
  if(length(GroupVar) > 0L) {
    data.table::setorderv(x = dt1, cols = c(GroupVar))
  }
  if(length(GroupVar) > 0L && (FacetRows > 1L || FacetCols > 1L)) {
    dt1 <- dt1[get(GroupVar) %in% c(eval(FacetLevels)), .SD, .SDcols = c(YVar,GroupVar)]
  } else {
    dt1 <- dt1[, .SD, .SDcols = c(YVar,GroupVar)]
  }

  # Data Prep2
  if(Debug) print("Plot.Residuals.Histogram")
  tl <- if(length(GroupVar) == 0L || length(FacetLevels) > 0) FALSE else TimeLine

  # Transformation
  # "PercRank"  "Standardize"
  # "Asinh"  "Log"  "LogPlus1"  "Sqrt"  "Asin"  "Logit"  "BoxCox"  "YeoJohnson"
  if(YVarTrans != "Identity") {
    if(YVarTrans == "PercRank") {
      dt1 <- AutoQuant::PercRank(data = dt1, ColNames = YVar, GroupVars = GroupVar, Granularity = 0.0001, ScoreTable = FALSE)
    } else if(YVarTrans == "Standardize") {
      dt1 <- AutoQuant::Standardize(data = dt1, ColNames = YVar, GroupVars = GroupVar, Center = TRUE, Scale = TRUE, ScoreTable = FALSE)
    } else {
      dt1 <- tryCatch({AutoQuant::AutoTransformationCreate(data = dt1, ColumnNames = YVar, Methods = YVarTrans)$Data}, error = function(x) dt1)
    }
  }

  # Create base plot object
  if(Debug) print('Create Plot with only data')

  # Format
  if(Engine == "Plotly") {

    X.HoverFormat <- "%{xaxis.title.text}: %{x:,.2f}<br>"
    Y.HoverFormat <- "%{yaxis.title.text}: %{y:,.2f}<br>"

    Width <- as.integer(gsub("[^\\d]+", "", Width, perl=TRUE))
    Height <- as.integer(gsub("[^\\d]+", "", Height, perl=TRUE))

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
      p1 <- echarts4r::e_charts_(
        dt1 |> dplyr::group_by(get(GroupVar)),
        x = NULL,
        timeline = TimeLine,
        dispose = TRUE,
        darkMode = TRUE,
        emphasis = list(focus = "series"),
        width = Width,
        height = Height)
    } else {
      if(Debug) print("here 1b")
      p1 <- echarts4r::e_charts_(
        dt1,
        x = NULL,
        darkMode = TRUE,
        dispose = TRUE,
        width = Width,
        height = Height)
    }
    p1 <- echarts4r::e_histogram_(e = p1, YVar, breaks = NumberBins, bar_width = "100%")
    if(FacetRows == 1L && FacetCols == 1L && Y_Scroll) {
      p1 <- echarts4r::e_datazoom(e = p1, y_Index = c(0,1))
    }
    p1 <- echarts4r::e_theme(e = p1, name = EchartsTheme)
    p1 <- echarts4r::e_aria(e = p1, enabled = TRUE)
    p1 <- echarts4r::e_tooltip(e = p1, trigger = "axis", backgroundColor = "aliceblue")
    p1 <- echarts4r::e_toolbox_feature(e = p1, feature = c("saveAsImage","dataZoom"))
    p1 <- echarts4r::e_show_loading(e = p1, hide_overlay = TRUE, text = "Calculating...", color = "#000", text_color = TextColor, mask_color = "#000")

    if(length(Title.XAxis) == 0L) {
      p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "x", name = XVar, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = xaxis.fontSize))
    } else {
      p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "x", name = Title.XAxis, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = xaxis.fontSize))
    }

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
    if(FacetRows > 1L || FacetCols > 1L) {
      p1 <- echarts4r::e_facet(
        e = p1,
        rows = FacetRows,
        cols = FacetCols,
        legend_space = 16,
        legend_pos = "top")
      p1 <- echarts4r::e_legend(e = p1, type = "scroll", orient = "horizontal", right = 50, top = 40, height = "240px", textStyle = list(color = TextColor, fontWeight = "bold"))
    } else {
      p1 <- echarts4r::e_legend(e = p1, type = "scroll", orient = "vertical", right = 50, top = 40, height = "240px", textStyle = list(color = TextColor, fontWeight = "bold"))
    }
  }
  return(p1)
}

#' @title Plot.Residuals.Scatter
#'
#' @description Residuals_2 Plot
#'
#' @author Adrian Antico
#' @family Model Evaluation
#'
#' @param dt source data.table
#' @param AggMethod character
#' @param SampleSize numeric
#' @param XVar X-Axis variable name
#' @param YVar Y-Axis variable name
#' @param GroupVar Character variable
#' @param YVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param XVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param FacetRows Defaults to 1 which causes no faceting to occur vertically. Otherwise, supply a numeric value for the number of output grid rows
#' @param FacetCols Defaults to 1 which causes no faceting to occur horizontally. Otherwise, supply a numeric value for the number of output grid columns
#' @param FacetLevels Faceting rows x columns is the max number of levels allowed in a grid. If your GroupVar has more you can supply the levels to display.
#' @param NumberBins numeric
#' @param Title character
#' @param ShowLabels character
#' @param Title.YAxis character
#' @param Title.XAxis character
#' @param Engine "Echarts" or "Plotly"
#' @param EchartsTheme "auritus","azul","bee-inspired","blue","caravan","carp","chalk","cool","dark-bold","dark","eduardo", #' "essos","forest","fresh-cut","fruit","gray","green","halloween","helianthus","infographic","inspired", #' "jazz","london","dark","macarons","macarons2","mint","purple-passion","red-velvet","red","roma","royal", #' "sakura","shine","tech-blue","vintage","walden","wef","weforum","westeros","wonderland"
#' @param TimeLine logical
#' @param X_Scroll logical
#' @param Y_Scroll logical
#' @param BackGroundColor color outside of plot window. Rcolors and hex outside of plot window. Rcolors and hex character
#' @param ChartColor character hex
#' @param FillColor character hex
#' @param FillColorReverse character hex
#' @param GridColor character hex
#' @param TextColor "Not Implemented"
#' @param ZeroLineColor character hex
#' @param ZeroLineWidth numeric
#' @param Debug Debugging purposes
#' @export
Plot.Residuals.Scatter <- function(dt = NULL,
                                   AggMethod = 'mean',
                                   SampleSize = 100000,
                                   XVar = NULL,
                                   YVar = NULL,
                                   GroupVar = NULL,
                                   YVarTrans = "Identity",
                                   XVarTrans = "Identity",
                                   FacetRows = 1,
                                   FacetCols = 1,
                                   FacetLevels = NULL,
                                   Height = NULL,
                                   Width = NULL,
                                   Title = 'Calibration Plot',
                                   ShowLabels = FALSE,
                                   Title.YAxis = NULL,
                                   Title.XAxis = NULL,
                                   Engine = 'Echarts',
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
    YVarTrans = YVarTrans,
    XVarTrans = XVarTrans,
    FacetRows = FacetRows,
    FacetCols = FacetCols,
    FacetLevels = FacetLevels,
    Height = Height,
    Width = Width,
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
    tooltip.trigger = "item",
    Debug = Debug)
  return(p1)
}

#' @title Plot.Calibration.Line
#'
#' @description This function automatically builds calibration plots and calibration boxplots for model evaluation using regression, quantile regression, and binary and multinomial classification
#'
#' @author Adrian Antico
#' @family Model Evaluation
#'
#' @param dt source data.table
#' @param AggMethod character
#' @param SampleSize numeric
#' @param XVar X-Axis variable name
#' @param YVar Y-Axis variable name
#' @param GroupVar Character variable
#' @param YVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param XVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param FacetRows Defaults to 1 which causes no faceting to occur vertically. Otherwise, supply a numeric value for the number of output grid rows
#' @param FacetCols Defaults to 1 which causes no faceting to occur horizontally. Otherwise, supply a numeric value for the number of output grid columns
#' @param FacetLevels Faceting rows x columns is the max number of levels allowed in a grid. If your GroupVar has more you can supply the levels to display.
#' @param NumberBins numeric
#' @param ZeroLineColor character hex
#' @param ZeroLineWidth numeric
#' @param Title character
#' @param ShowLabels character
#' @param Title.YAxis character
#' @param Title.XAxis character
#' @param Engine "Echarts" or "Plotly"
#' @param EchartsTheme "auritus","azul","bee-inspired","blue","caravan","carp","chalk","cool","dark-bold","dark","eduardo", #' "essos","forest","fresh-cut","fruit","gray","green","halloween","helianthus","infographic","inspired", #' "jazz","london","dark","macarons","macarons2","mint","purple-passion","red-velvet","red","roma","royal", #' "sakura","shine","tech-blue","vintage","walden","wef","weforum","westeros","wonderland"
#' @param TimeLine logical
#' @param X_Scroll logical
#' @param Y_Scroll logical
#' @param BackGroundColor color outside of plot window. Rcolors and hex outside of plot window. Rcolors and hex character
#' @param ChartColor character hex
#' @param FillColor character hex
#' @param FillColorReverse character hex
#' @param GridColor character hex
#' @param TextColor "Not Implemented"
#' @param Debug Debugging purposes
#' @export
Plot.Calibration.Line <- function(dt = NULL,
                                  AggMethod = 'mean',
                                  XVar = NULL,
                                  YVar = NULL,
                                  GroupVar = NULL,
                                  YVarTrans = "Identity",
                                  XVarTrans = "Identity",
                                  FacetRows = 1,
                                  FacetCols = 1,
                                  FacetLevels = NULL,
                                  NumberBins = 21,
                                  Height = NULL,
                                  Width = NULL,
                                  Title = 'Calibration Plot',
                                  ShowLabels = FALSE,
                                  Title.YAxis = NULL,
                                  Title.XAxis = NULL,
                                  Engine = 'Echarts',
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
                                  ZeroLineColor = '#ffff',
                                  ZeroLineWidth = 1.25,
                                  Debug = FALSE) {

  # YVar check
  y_class <- class(dt[[YVar]])[1L]

  # Define Aggregation function
  if(Debug) print("Plot.PartialDependence.Line # Define Aggregation function")
  aggFunc <- AutoPlots:::SummaryFunction(AggMethod)

  # Regression and Classification else MultiClass
  if(!y_class %in% c("character","factor")) {

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

    # If actual is in factor form, convert to numeric
    if(Debug) print("Plot.Calibration.Line # If actual is in factor form, convert to numeric")
    if(!is.numeric(dt1[[YVar]])) {
      data.table::set(dt1, j = YVar, value = as.numeric(as.character(dt1[[YVar]])))
    }

    # Add a column that ranks predicted values
    if(length(GroupVar) > 0L) {
      if(Debug) print("Plot.Calibration.Line # if(length(GroupVar) > 0L)")

      if(length(FacetLevels) > 0L) {
        dt1 <- dt1[get(GroupVar) %in% c(eval(FacetLevels)), .SD, .SDcols = c(YVar,XVar,GroupVar)]
      }

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
      YVarTrans = YVarTrans,
      XVarTrans = XVarTrans,
      FacetRows = FacetRows,
      FacetCols = FacetCols,
      FacetLevels = FacetLevels,
      Height = Height,
      Width = Width,
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

  } else { # multiclass model

    # Minimize data before moving on
    if(Debug) print("Plot.PartialDependence.Line # Minimize data before moving on")
    GroupVar <- tryCatch({GroupVar[1L]}, error = function(x) NULL)

    # Shrink data
    yvar_levels <- dt[, unique(get(YVar))]
    dt1 <- data.table::copy(dt[, .SD, .SDcols = c(GroupVar, XVar, YVar, yvar_levels)])

    # Dummify Target
    nam <- data.table::copy(names(dt1))
    dt1 <- AutoQuant::DummifyDT(data = dt1, cols = YVar, TopN = length(yvar_levels), KeepFactorCols = FALSE, OneHot = FALSE, SaveFactorLevels = FALSE, SavePath = getwd(), ImportFactorLevels = FALSE, FactorLevelsList = NULL, ClustScore = FALSE, ReturnFactorLevels = FALSE)
    nam <- setdiff(names(dt1), nam)

    # Melt Predict Cols
    dt2 <- data.table::melt.data.table(
      data = if(length(GroupVar) == 0L) dt1[, .SD, .SDcols = c(names(dt1)[!names(dt1) %in% XVar])] else dt1,
      id.vars = c(GroupVar),
      measure.vars = names(dt1)[!names(dt1) %in% c(GroupVar, YVar, XVar, nam)],
      variable.name = "Level",
      value.name = XVar,
      na.rm = TRUE,
      variable.factor = FALSE)

    # Melt Target Cols
    dt3 <- data.table::melt.data.table(
      data = dt1,
      id.vars = c(GroupVar,XVar),
      measure.vars = nam,
      variable.name = "Level",
      value.name = YVar,
      na.rm = TRUE,
      variable.factor = FALSE)

    # Join data
    dt2[, eval(YVar) := dt3[[YVar]]]

    # Add New Target
    yvar <- "Target - Predicted"
    dt2[, eval(yvar) := get(YVar) - get(XVar)]

    if(length(GroupVar) > 0L) {
      dt2[, GroupVariables := do.call(paste, c(.SD, sep = ' :: ')), .SDcols = c(GroupVar, "Level")]
      GroupVar <- "GroupVariables"
      if(FacetRows > 1L || FacetCols > 1L) {
        FacetLevels <- as.character(dt2[, unique(GroupVariables)])
        FacetLevels <- FacetLevels[seq_len(min(length(FacetLevels),FacetRows*FacetCols))]
        dt2 <- dt2[GroupVariables %chin% c(eval(FacetLevels))]
      }
    } else if(length(GroupVar) == 0L && (FacetRows > 1L || FacetCols > 1L)) {
      FacetLevels <- yvar_levels[seq_len(min(length(yvar_levels), FacetRows * FacetCols))]
      dt2 <- dt2[Level %chin% c(eval(FacetLevels))]
    }

    # Subset Cols
    if(length(GroupVar) > 0L) {
      dt2 <- dt2[, .SD, .SDcols = c("GroupVariables", yvar, XVar)]
      dt2[, eval(XVar) := round(data.table::frank(get(XVar)) * NumberBins / .N) / NumberBins, by = c(GroupVar[1L])]
      dt2 <- dt2[, lapply(.SD, noquote(aggFunc)), by = c(XVar,GroupVar)]
    } else {
      dt2 <- dt2[, .SD, .SDcols = c(yvar, XVar, "Level")]
      dt2[, eval(XVar) := round(data.table::frank(get(XVar)) * NumberBins / .N) / NumberBins]
      dt2 <- dt2[, lapply(.SD, noquote(aggFunc)), by = c(XVar,"Level")]
    }

    # Build
    if(Debug) print("Plot.PartialDependence.Line --> AutoPlots::Plot.Line()")
    p1 <- AutoPlots::Plot.Line(
      dt = dt2,
      PreAgg = TRUE,
      AggMethod = "mean",
      Engine = Engine,
      EchartsTheme = EchartsTheme,
      TimeLine = FALSE,
      XVar = XVar,
      YVar = yvar,
      GroupVar = if(length(GroupVar) > 0L) "GroupVariables" else "Level",
      YVarTrans = YVarTrans,
      XVarTrans = XVarTrans,
      FacetRows = FacetRows,
      FacetCols = FacetCols,
      FacetLevels = FacetLevels,
      Area = FALSE,
      Smooth = TRUE,
      ShowSymbol = FALSE,
      Height = Height,
      Width = Width,
      Title = "Partial Dependence",
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
}

#' @title Plot.Calibration.Box
#'
#' @description This function automatically builds calibration plots and calibration boxplots for model evaluation using regression, quantile regression, and binary and multinomial classification
#'
#' @author Adrian Antico
#' @family Model Evaluation
#'
#' @param dt source data.table
#' @param SampleSize numeric
#' @param XVar X-Axis variable name
#' @param YVar Y-Axis variable name
#' @param AggMethod character
#' @param GroupVar Character variable
#' @param YVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param XVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param FacetRows Defaults to 1 which causes no faceting to occur vertically. Otherwise, supply a numeric value for the number of output grid rows
#' @param FacetCols Defaults to 1 which causes no faceting to occur horizontally. Otherwise, supply a numeric value for the number of output grid columns
#' @param FacetLevels Faceting rows x columns is the max number of levels allowed in a grid. If your GroupVar has more you can supply the levels to display.
#' @param NumberBins numeric
#' @param Title character
#' @param ShowLabels character
#' @param Title.YAxis character
#' @param Title.XAxis character
#' @param Engine "Echarts" or "Plotly"
#' @param EchartsTheme "auritus","azul","bee-inspired","blue","caravan","carp","chalk","cool","dark-bold","dark","eduardo", #' "essos","forest","fresh-cut","fruit","gray","green","halloween","helianthus","infographic","inspired", #' "jazz","london","dark","macarons","macarons2","mint","purple-passion","red-velvet","red","roma","royal", #' "sakura","shine","tech-blue","vintage","walden","wef","weforum","westeros","wonderland"
#' @param TimeLine logical
#' @param X_Scroll logical
#' @param Y_Scroll logical
#' @param BackGroundColor color outside of plot window. Rcolors and hex outside of plot window. Rcolors and hex character
#' @param ChartColor character hex
#' @param FillColor character hex
#' @param FillColorReverse character hex
#' @param GridColor character hex
#' @param TextColor "Not Implemented"
#' @param ZeroLineColor character hex
#' @param ZeroLineWidth numeric
#' @param Debug Debugging purposes
#' @export
Plot.Calibration.Box <- function(dt = NULL,
                                 SampleSize = 100000L,
                                 AggMethod = 'mean',
                                 XVar = NULL,
                                 YVar = NULL,
                                 GroupVar = NULL,
                                 YVarTrans = "Identity",
                                 XVarTrans = "Identity",
                                 FacetRows = 1,
                                 FacetCols = 1,
                                 FacetLevels = NULL,
                                 NumberBins = 21,
                                 Height = NULL,
                                 Width = NULL,
                                 Title = 'Calibration Plot',
                                 ShowLabels = FALSE,
                                 Title.YAxis = NULL,
                                 Title.XAxis = NULL,
                                 Engine = 'Echarts',
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

  dt1 <- dt1[, .SD, .SDcols = c("Target - Predicted",GroupVar)]

  # Plot
  if(Debug) print(paste0("TimeLine for AutoPlots:::Plot.Box=", TimeLine))
  p1 <- AutoPlots:::Plot.Box(
    dt = dt1,
    SampleSize = SampleSize,
    XVar = NULL,
    YVar = "Target - Predicted",
    GroupVar = GroupVar,
    YVarTrans = YVarTrans,
    XVarTrans = XVarTrans,
    FacetRows = FacetRows,
    FacetCols = FacetCols,
    FacetLevels = NULL,
    Height = Height,
    Width = Width,
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

#' @title Plot.PartialDependence.Line
#'
#' @description This function automatically builds partial dependence calibration plots
#'
#' @author Adrian Antico
#' @family Model Evaluation
#'
#' @param dt source data.table
#' @param XVar X-Axis variable name
#' @param YVar Y-Axis variable name
#' @param ZVar character
#' @param NumberBins numeric
#' @param Title character
#' @param ShowLabels character
#' @param Title.YAxis character
#' @param Title.XAxis character
#' @param Engine "Echarts" or "Plotly"
#' @param YVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param XVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param ZVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param FacetRows Defaults to 1 which causes no faceting to occur vertically. Otherwise, supply a numeric value for the number of output grid rows
#' @param FacetCols Defaults to 1 which causes no faceting to occur horizontally. Otherwise, supply a numeric value for the number of output grid columns
#' @param FacetLevels Faceting rows x columns is the max number of levels allowed in a grid. If your GroupVar has more you can supply the levels to display.
#' @param EchartsTheme "auritus","azul","bee-inspired","blue","caravan","carp","chalk","cool","dark-bold","dark","eduardo", #' "essos","forest","fresh-cut","fruit","gray","green","halloween","helianthus","infographic","inspired", #' "jazz","london","dark","macarons","macarons2","mint","purple-passion","red-velvet","red","roma","royal", #' "sakura","shine","tech-blue","vintage","walden","wef","weforum","westeros","wonderland"
#' @param EchartsLabels character
#' @param TimeLine logical
#' @param X_Scroll = TRUE,
#' @param Y_Scroll = TRUE,
#' @param BackGroundColor color outside of plot window. Rcolors and hex outside of plot window. Rcolors and hex character
#' @param ChartColor hex character
#' @param FillColor hex character
#' @param FillColorReverse hex character
#' @param GridColor hex character
#' @param TextColor hex character
#' @param ZeroLineColor hex character
#' @param ZeroLineWidth numeric
#' @param AggMethod character
#' @param GroupVar Character variable
#' @param Debug Debugging purposes
#' @export
Plot.PartialDependence.Line <- function(dt = NULL,
                                        XVar = NULL,
                                        YVar = NULL,
                                        ZVar = NULL,
                                        YVarTrans = "Identity",
                                        XVarTrans = "Identity",
                                        ZVarTrans = "Identity",
                                        FacetRows = 1,
                                        FacetCols = 1,
                                        FacetLevels = NULL,
                                        GroupVar = NULL,
                                        NumberBins = 20,
                                        AggMethod = "mean",
                                        Height = NULL,
                                        Width = NULL,
                                        Title = "Gains Plot",
                                        ShowLabels = FALSE,
                                        Title.YAxis = NULL,
                                        Title.XAxis = NULL,
                                        Engine = 'Plotly',
                                        EchartsTheme = "macarons",
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

  # YVar check
  yvar_class <- class(dt[[YVar]])[1L]
  xvar_class <- class(dt[[XVar]][1L])

  # Define Aggregation function
  if(Debug) print("Plot.PartialDependence.Line # Define Aggregation function")
  aggFunc <- AutoPlots:::SummaryFunction(AggMethod)

  # Regression and Classification else MultiClass
  if(yvar_class %in% c("numeric","integer")) {

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

    # If actual is in factor form, convert to numeric
    if(Debug) print("Plot.PartialDependence.Line # If actual is in factor form, convert to numeric")
    if(!is.numeric(dt1[[YVar]])) {
      data.table::set(dt1, j = YVar, value = as.numeric(as.character(dt1[[YVar]])))
    }

    # Data Mgt
    if(length(GroupVar) > 0L) {
      if(Debug) print("Plot.PartialDependence.Line # if(length(GroupVar) > 0L)")
      if(!xvar_class %in%  c("factor","character","Date","IDate","POSIXct","IDateTime")) {
        dt1[, eval(XVar) := round(data.table::frank(get(XVar)) * NumberBins / .N) / NumberBins, by = c(GroupVar[1L])]
      }
      dt1 <- dt1[, lapply(.SD, noquote(aggFunc)), by = c(XVar,GroupVar[1L])]
      dt1[, `Target - Predicted` := get(YVar) - get(ZVar)]
      data.table::setorderv(x = dt1, cols = c(XVar,GroupVar[1L]), c(1L,1L))
      yvar <- "Target - Predicted"
      gv <- GroupVar
      tl <- TimeLine
    } else {
      if(Debug) print("Plot.PartialDependence.Line # if(length(GroupVar) == 0L)")
      if(!xvar_class %in%  c("factor","character","Date","IDate","POSIXct","IDateTime")) {
        dt1[, eval(XVar) := round(data.table::frank(get(XVar)) * NumberBins / .N) / NumberBins]
      }
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
      YVarTrans = YVarTrans,
      XVarTrans = XVarTrans,
      FacetRows = FacetRows,
      FacetCols = FacetCols,
      FacetLevels = FacetLevels,
      Height = Height,
      Width = Width,
      Title = "Partial Dependence",
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

  } else { # multiclass model

    # Minimize data before moving on
    if(Debug) print("Plot.PartialDependence.Line # Minimize data before moving on")
    GroupVar <- tryCatch({GroupVar[1L]}, error = function(x) NULL)

    # Shrink data
    yvar_levels <- dt[, unique(get(YVar))]
    dt1 <- data.table::copy(dt[, .SD, .SDcols = c(GroupVar, XVar, YVar, yvar_levels)])

    # Dummify Target
    nam <- data.table::copy(names(dt1))
    dt1 <- AutoQuant::DummifyDT(data = dt1, cols = YVar, TopN = length(yvar_levels), KeepFactorCols = FALSE, OneHot = FALSE, SaveFactorLevels = FALSE, SavePath = getwd(), ImportFactorLevels = FALSE, FactorLevelsList = NULL, ClustScore = FALSE, ReturnFactorLevels = FALSE)
    nam <- setdiff(names(dt1), nam)

    # Melt Predict Cols
    dt2 <- data.table::melt.data.table(
      data = dt1,
      id.vars = c(GroupVar, XVar),
      measure.vars = names(dt1)[!names(dt1) %in% c(GroupVar, XVar, YVar, nam)],
      variable.name = "Level",
      value.name = ZVar,
      na.rm = TRUE,
      variable.factor = FALSE)

    # Melt Target Cols
    dt3 <- data.table::melt.data.table(
      data = dt1,
      id.vars = c(GroupVar, XVar),
      measure.vars = nam,
      variable.name = "Level",
      value.name = YVar,
      na.rm = TRUE,
      variable.factor = FALSE)

    # Join data
    dt2[, eval(YVar) := dt3[[YVar]]]

    # Update Args
    if(length(GroupVar) > 0L) {
      dt2[, GroupVariables := do.call(paste, c(.SD, sep = ' :: ')), .SDcols = c(GroupVar, "Level")]
      GroupVar <- "GroupVariables"
      if(FacetRows > 1L && FacetCols > 1L) {
        FacetLevels <- as.character(dt2[, unique(GroupVariables)])
        FacetLevels <- FacetLevels[seq_len(min(length(FacetLevels),FacetRows*FacetCols))]
        dt2 <- dt2[GroupVariables %chin% c(eval(FacetLevels))]
      }
    } else if(length(GroupVar) == 0L && (FacetRows > 1L || FacetCols > 1L)) {
      FacetLevels <- yvar_levels[seq_len(min(length(yvar_levels), FacetRows * FacetCols))]
      dt2 <- dt2[Level %chin% c(eval(FacetLevels))]
    }

    # Add New Target
    yvar <- "Target - Predicted"
    dt2[, eval(yvar) := get(YVar) - get(ZVar)]

    # Subset Cols
    if(length(GroupVar) > 0L) {
      dt2 <- dt2[, .SD, .SDcols = c("GroupVariables", yvar, XVar)]
      if(!xvar_class %in%  c("factor","character","Date","IDate","POSIXct","IDateTime")) {
        dt2[, eval(XVar) := round(data.table::frank(get(XVar)) * NumberBins / .N) / NumberBins, by = c(GroupVar[1L])]
      }
      dt2 <- dt2[, lapply(.SD, noquote(aggFunc)), by = c(XVar,GroupVar)]
    } else {
      dt2 <- dt2[, .SD, .SDcols = c(yvar, XVar, "Level")]
      if(!xvar_class %in%  c("factor","character","Date","IDate","POSIXct","IDateTime")) {
        dt2[, eval(XVar) := round(data.table::frank(get(XVar)) * NumberBins / .N) / NumberBins]
      }
      dt2 <- dt2[, lapply(.SD, noquote(aggFunc)), by = c(XVar,"Level")]
    }

    # Build
    if(Debug) print("Plot.PartialDependence.Line --> AutoPlots::Plot.Line()")
    p1 <- AutoPlots::Plot.Line(
      dt = dt2,
      PreAgg = TRUE,
      AggMethod = "mean",
      Engine = Engine,
      EchartsTheme = EchartsTheme,
      TimeLine = FALSE,
      XVar = XVar,
      YVar = yvar,
      GroupVar = if(length(GroupVar) > 0L) "GroupVariables" else "Level",
      YVarTrans = YVarTrans,
      XVarTrans = XVarTrans,
      FacetRows = FacetRows,
      FacetCols = FacetCols,
      FacetLevels = FacetLevels,
      Area = FALSE,
      Smooth = TRUE,
      ShowSymbol = FALSE,
      Height = Height,
      Width = Width,
      Title = "Partial Dependence",
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
}

#' @title Plot.PartialDependence.Box
#'
#' @description This function automatically builds partial dependence calibration plots
#'
#' @author Adrian Antico
#' @family Model Evaluation
#'
#' @param dt source data.table
#' @param SampleSize numeric
#' @param XVar X-Axis variable name
#' @param YVar Y-Axis variable name
#' @param ZVar character
#' @param YVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param XVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param ZVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param FacetRows Defaults to 1 which causes no faceting to occur vertically. Otherwise, supply a numeric value for the number of output grid rows
#' @param FacetCols Defaults to 1 which causes no faceting to occur horizontally. Otherwise, supply a numeric value for the number of output grid columns
#' @param FacetLevels Faceting rows x columns is the max number of levels allowed in a grid. If your GroupVar has more you can supply the levels to display.
#' @param NumberBins numeric
#' @param PreAgg logical
#' @param Title character
#' @param ShowLabels character
#' @param Title.YAxis character
#' @param Title.XAxis character
#' @param Engine "Echarts" or "Plotly"
#' @param EchartsTheme "auritus","azul","bee-inspired","blue","caravan","carp","chalk","cool","dark-bold","dark","eduardo", #' "essos","forest","fresh-cut","fruit","gray","green","halloween","helianthus","infographic","inspired", #' "jazz","london","dark","macarons","macarons2","mint","purple-passion","red-velvet","red","roma","royal", #' "sakura","shine","tech-blue","vintage","walden","wef","weforum","westeros","wonderland"
#' @param EchartsLabels character
#' @param TimeLine logical
#' @param X_Scroll logical
#' @param Y_Scroll logical
#' @param BackGroundColor color outside of plot window. Rcolors and hex outside of plot window. Rcolors and hex character
#' @param ChartColor hex character
#' @param FillColor hex character
#' @param FillColorReverse hex character
#' @param GridColor hex character
#' @param TextColor hex character
#' @param ZeroLineColor hex character
#' @param ZeroLineWidth numeric
#' @param AggMethod character
#' @param GroupVar Character variable
#' @param Debug Debugging purposes
#' @export
Plot.PartialDependence.Box <- function(dt = NULL,
                                       PreAgg = FALSE,
                                       SampleSize = 100000L,
                                       XVar = NULL,
                                       YVar = NULL,
                                       ZVar = NULL,
                                       GroupVar = NULL,
                                       YVarTrans = "Identity",
                                       XVarTrans = "Identity",
                                       ZVarTrans = "Identity",
                                       FacetRows = 1,
                                       FacetCols = 1,
                                       FacetLevels = NULL,
                                       NumberBins = 20,
                                       AggMethod = "mean",
                                       Height = NULL,
                                       Width = NULL,
                                       Title = "Gains Plot",
                                       ShowLabels = FALSE,
                                       Title.YAxis = NULL,
                                       Title.XAxis = NULL,
                                       Engine = 'Plotly',
                                       EchartsTheme = "macarons",
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

  GroupVar <- NULL

  # Minimize data before moving on
  if(Debug) print("Plot.PartialDependence.Box # Minimize data before moving on")
  Ncols <- ncol(dt)
  if(Ncols > 3L) {
    dt1 <- data.table::copy(dt[, .SD, .SDcols = c(YVar, XVar, ZVar)])
  } else {
    dt1 <- data.table::copy(dt)
  }

  # If actual is in factor form, convert to numeric
  if(Debug) print("Plot.PartialDependence.Box # If actual is in factor form, convert to numeric")
  if(!is.numeric(dt1[[YVar]])) {
    data.table::set(dt1, j = YVar, value = as.numeric(as.character(dt1[[YVar]])))
  }

  # Add a column that ranks predicted values
  dt1[, eval(XVar) := as.character(round(data.table::frank(get(XVar)) * (NumberBins) / .N) / NumberBins)]
  dt1[, `Target - Predicted` := get(YVar) - get(ZVar)]
  data.table::setorderv(x = dt1, cols = XVar, 1L)

  # Build Plot
  tl <- if(length(GroupVar) == 0L) FALSE else TimeLine

  # Build
  if(Debug) print("Plot.PartialDependence.Box --> AutoPlots::Plot.Box()")
  p1 <- AutoPlots::Plot.Box(
    dt = dt1,
    SampleSize = SampleSize,
    XVar = XVar,
    YVar = "Target - Predicted",
    GroupVar = NULL,
    YVarTrans = YVarTrans,
    XVarTrans = XVarTrans,
    FacetRows = 1,
    FacetCols = 1,
    FacetLevels = NULL,
    Height = Height,
    Width = Width,
    Title = "Partial Dependence",
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

#' @title Plot.PartialDependence.HeatMap
#'
#' @description This function automatically builds partial dependence calibration plots
#'
#' @author Adrian Antico
#' @family Model Evaluation
#'
#' @param dt source data.table
#' @param XVar X-Axis variable name
#' @param YVar Y-Axis variable name
#' @param ZVar character
#' @param YVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param XVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param ZVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param FacetRows Defaults to 1 which causes no faceting to occur vertically. Otherwise, supply a numeric value for the number of output grid rows
#' @param FacetCols Defaults to 1 which causes no faceting to occur horizontally. Otherwise, supply a numeric value for the number of output grid columns
#' @param FacetLevels Faceting rows x columns is the max number of levels allowed in a grid. If your GroupVar has more you can supply the levels to display.
#' @param NumberBins numeric
#' @param Title character
#' @param ShowLabels character
#' @param Title.YAxis character
#' @param Title.XAxis character
#' @param Engine "Echarts" or "Plotly"
#' @param EchartsTheme "auritus","azul","bee-inspired","blue","caravan","carp","chalk","cool","dark-bold","dark","eduardo", #' "essos","forest","fresh-cut","fruit","gray","green","halloween","helianthus","infographic","inspired", #' "jazz","london","dark","macarons","macarons2","mint","purple-passion","red-velvet","red","roma","royal", #' "sakura","shine","tech-blue","vintage","walden","wef","weforum","westeros","wonderland"
#' @param EchartsLabels character
#' @param TimeLine logical
#' @param X_Scroll = TRUE,
#' @param Y_Scroll = TRUE,
#' @param BackGroundColor color outside of plot window. Rcolors and hex outside of plot window. Rcolors and hex character
#' @param ChartColor hex character
#' @param FillColor hex character
#' @param FillColorReverse hex character
#' @param GridColor hex character
#' @param TextColor hex character
#' @param ZeroLineColor hex character
#' @param ZeroLineWidth numeric
#' @param AggMethod character
#' @param GroupVar Character variable
#' @param Debug Debugging purposes
#' @export
Plot.PartialDependence.HeatMap <- function(dt = NULL,
                                           XVar = NULL,
                                           YVar = NULL,
                                           ZVar = NULL,
                                           GroupVar = NULL,
                                           YVarTrans = "Identity",
                                           XVarTrans = "Identity",
                                           ZVarTrans = "Identity",
                                           FacetRows = 1,
                                           FacetCols = 1,
                                           FacetLevels = NULL,
                                           NumberBins = 21,
                                           AggMethod = "mean",
                                           Height = NULL,
                                           Width = NULL,
                                           Title = "Gains Plot",
                                           ShowLabels = FALSE,
                                           Title.YAxis = NULL,
                                           Title.XAxis = NULL,
                                           Engine = 'Plotly',
                                           EchartsTheme = "macarons",
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

  # YVar check
  yvar_class <- class(dt[[YVar]])[1L]
  xvar_class <- class(dt[[XVar]][1L])

  # Define Aggregation function
  if(Debug) print("Plot.PartialDependence.Line # Define Aggregation function")
  aggFunc <- AutoPlots:::SummaryFunction(AggMethod)

  # Regression and Classification else MultiClass
  if(yvar_class %in% c("numeric","integer")) {

    GroupVar <- NULL

    # Minimize data before moving on
    if(Debug) print("Plot.PartialDependence.HeatMap # Minimize data before moving on")
    Ncols <- ncol(dt)
    dt1 <- data.table::copy(dt[, .SD, .SDcols = c(YVar, XVar, ZVar)])

    if(Debug) print("Plot.PartialDependence.HeatMap # Define Aggregation function")
    aggFunc <- AutoPlots:::SummaryFunction(AggMethod)
    if(Debug) print("Plot.PartialDependence.HeatMap # if(length(GroupVar) == 0L)")
    for(i in seq_along(XVar)) dt1[, eval(XVar[i]) := as.character(round(data.table::frank(get(XVar[i])) * NumberBins / .N / NumberBins, 1L))]
    dt1 <- dt1[, lapply(.SD, noquote(aggFunc)), by = c(eval(XVar))]
    dt1[, `Target - Predicted` := get(YVar) - get(ZVar)]
    ZVar <- "Target - Predicted"
    YVar <- XVar[2L]
    XVar <- XVar[1L]

    data.table::setorderv(x = dt1, cols = c(XVar,YVar),c(1L,1L))
    for(i in c(XVar,YVar)) dt1[, eval(i) := get(i)]

    # Build
    if(Debug) print("Plot.PartialDependence.HeatMap --> AutoPlots::Plot.HeatMap()")
    p1 <- AutoPlots::Plot.HeatMap(
      dt = dt1,
      PreAgg = TRUE,
      AggMethod = "mean",
      Engine = Engine,
      EchartsTheme = EchartsTheme,
      XVar = XVar,
      YVar = YVar,
      ZVar = ZVar,
      YVarTrans = YVarTrans,
      XVarTrans = XVarTrans,
      ZVarTrans = ZVarTrans,
      Height = Height,
      Width = Width,
      Title = "Heatmap: Target - Predicted",
      BackGroundColor = BackGroundColor,
      ChartColor = ChartColor,
      FillColor = FillColor,
      TextColor = TextColor,
      X_Scroll = X_Scroll,
      Y_Scroll = Y_Scroll,
      NumberBins = NumberBins,
      FillColorReverse = FillColorReverse,
      GridColor = GridColor,
      Debug = Debug)
    return(p1)
  } else {

    # Minimize data before moving on
    if(Debug) print("Plot.PartialDependence.Line # Minimize data before moving on")

    # Shrink data
    yvar_levels <- dt[, unique(get(YVar))]
    dt1 <- data.table::copy(dt[, .SD, .SDcols = c(XVar, YVar, yvar_levels)])

    # Dummify Target
    nam <- data.table::copy(names(dt1))
    dt1 <- AutoQuant::DummifyDT(data = dt1, cols = YVar, TopN = length(yvar_levels), KeepFactorCols = FALSE, OneHot = FALSE, SaveFactorLevels = FALSE, SavePath = getwd(), ImportFactorLevels = FALSE, FactorLevelsList = NULL, ClustScore = FALSE, ReturnFactorLevels = FALSE)
    nam <- setdiff(names(dt1), nam)

    # Melt Predict Cols
    dt2 <- data.table::melt.data.table(
      data = dt1,
      id.vars = XVar,
      measure.vars = names(dt1)[!names(dt1) %in% c(XVar, YVar, nam)],
      variable.name = "Level",
      value.name = ZVar,
      na.rm = TRUE,
      variable.factor = FALSE)

    # Melt Target Cols
    dt3 <- data.table::melt.data.table(
      data = dt1,
      id.vars = XVar,
      measure.vars = nam,
      variable.name = "Level",
      value.name = YVar,
      na.rm = TRUE,
      variable.factor = FALSE)

    # Join data
    dt2[, eval(YVar) := dt3[[YVar]]]

    # Add New Target
    yvar <- "Target - Predicted"
    dt2[, eval(yvar) := get(YVar) - get(ZVar)]

    # Subset Cols
    dt2 <- dt2[, .SD, .SDcols = c(yvar, XVar, "Level")]
    if(!xvar_class %in%  c("factor","character","Date","IDate","POSIXct","IDateTime")) {
      dt2[, eval(XVar) := round(data.table::frank(get(XVar)) * NumberBins / .N) / NumberBins]
    }

    dt2 <- dt2[, lapply(.SD, noquote(aggFunc)), by = c(XVar,"Level")]

    # Build
    if(Debug) print("Plot.PartialDependence.HeatMap --> AutoPlots::Plot.HeatMap()")
    p1 <- AutoPlots::Plot.HeatMap(
      dt = dt2,
      PreAgg = TRUE,
      AggMethod = "mean",
      Engine = Engine,
      EchartsTheme = EchartsTheme,
      XVar = XVar,
      YVar = "Level",
      ZVar = yvar,
      YVarTrans = YVarTrans,
      XVarTrans = XVarTrans,
      ZVarTrans = ZVarTrans,
      Height = Height,
      Width = Width,
      Title = "Heatmap: Target - Predicted",
      BackGroundColor = BackGroundColor,
      ChartColor = ChartColor,
      FillColor = FillColor,
      TextColor = TextColor,
      X_Scroll = X_Scroll,
      Y_Scroll = Y_Scroll,
      NumberBins = NumberBins,
      FillColorReverse = FillColorReverse,
      GridColor = GridColor,
      Debug = Debug)
    return(p1)
  }
}

#' @title Plot.VariableImportance
#'
#' @description Generate variable importance plots
#'
#' @author Adrian Antico
#' @family Model Evaluation
#'
#' @param dt source data.table
#' @param AggMethod Choose from 'mean', 'sum', 'sd', and 'median'
#' @param XVar Column name of X-Axis variable. If NULL then ignored
#' @param YVar Column name of Y-Axis variable. If NULL then ignored
#' @param GroupVar Column name of Group Variable for distinct colored histograms by group levels
#' @param YVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param XVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param FacetRows Defaults to 1 which causes no faceting to occur vertically. Otherwise, supply a numeric value for the number of output grid rows
#' @param FacetCols Defaults to 1 which causes no faceting to occur horizontally. Otherwise, supply a numeric value for the number of output grid columns
#' @param FacetLevels Faceting rows x columns is the max number of levels allowed in a grid. If your GroupVar has more you can supply the levels to display.
#' @param Title title
#' @param ShowLabels character
#' @param Title.YAxis character
#' @param Title.XAxis character
#' @param Engine 'Plotly' or "Echarts"
#' @param EchartsTheme "auritus","azul","bee-inspired","blue","caravan","carp","chalk","cool","dark-bold","dark","eduardo", #' "essos","forest","fresh-cut","fruit","gray","green","halloween","helianthus","infographic","inspired", #' "jazz","london","dark","macarons","macarons2","mint","purple-passion","red-velvet","red","roma","royal", #' "sakura","shine","tech-blue","vintage","walden","wef","weforum","westeros","wonderland"
#' @param TimeLine logical
#' @param X_Scroll logical
#' @param Y_Scroll logical
#' @param BackGroundColor color outside of plot window. Rcolors and hex outside of plot window. Rcolors and hex character
#' @param ChartColor 'lightsteelblue'
#' @param FillColor 'gray'
#' @param FillColorReverse character hex
#' @param GridColor 'white'
#' @param TextColor 'darkblue'
#' @param ZeroLineColor = '#ffff'
#' @param Debug Debugging purposes
#' @export
Plot.VariableImportance <- function(dt = NULL,
                                    XVar = NULL,
                                    YVar = NULL,
                                    GroupVar = NULL,
                                    YVarTrans = "Identity",
                                    XVarTrans = "Identity",
                                    FacetRows = 1,
                                    FacetCols = 1,
                                    FacetLevels = NULL,
                                    AggMethod = 'mean',
                                    Height = NULL,
                                    Width = NULL,
                                    Title = 'Variable Importance Plot',
                                    ShowLabels = FALSE,
                                    Title.YAxis = NULL,
                                    Title.XAxis = NULL,
                                    Engine = 'Echarts',
                                    EchartsTheme = "macarons",
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

  # Plotly
  if(Engine == "Plotly") {

    X.HoverFormat <- "%{xaxis.title.text}: %{x:,.2f}<br>"
    Y.HoverFormat <- "%{yaxis.title.text}: %{y:,.2f}<br>"

    Width <- as.integer(gsub("[^\\d]+", "", Width, perl=TRUE))
    Height <- as.integer(gsub("[^\\d]+", "", Height, perl=TRUE))
    dt <- dt[order(Importance)]
    Var <- names(which(unlist(lapply(dt, is.character))))
    if(length(Var) == 0L) {
      Var <- names(which(unlist(lapply(dt, is.factor))))
    }
    Vals <- dt$Variable
    dt[, eval(Var) := factor(get(Var), levels = c(Vals))]

    p1 <- plotly::plot_ly(
      data = dt,
      x = ~get(XVar),
      y = ~get(YVar),
      type = "bar",
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
        gridcolor = GridColor))
  } else {
    dt <- dt[order(Importance)]
    Var <- names(which(unlist(lapply(dt, is.character))))
    Var2 <- names(which(unlist(lapply(dt, is.numeric))))
    if(length(Var) == 0L) {
      Var <- names(which(unlist(lapply(dt, is.factor))))
      dt[, eval(Var) := as.character(get(Var))]
    }
    p1 <- echarts4r::e_charts_(
      dt,
      x = Var,
      dispose = TRUE,
      darkMode = TRUE,
      width = Width,
      height = Height)
    p1 <- echarts4r::e_bar_(e = p1, Var2)
    p1 <- echarts4r::e_theme(e = p1, name = EchartsTheme)
    p1 <- echarts4r::e_aria(e = p1, enabled = TRUE)
    p1 <- echarts4r::e_tooltip(e = p1, trigger = "axis", backgroundColor = "aliceblue")
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
    p1 <- echarts4r::e_flip_coords(e = p1)
  }

  if(class(p1)[1L] == "plotly") {
    p1 <- plotly::layout(p1, yaxis = list(autorange = "reversed"))
  }
  return(p1)
}

#' @title Plot.ROC
#'
#' @description ROC Plot
#'
#' @author Adrian Antico
#' @family Model Evaluation
#'
#' @param dt source data.table
#' @param XVar X-Axis variable name
#' @param YVar Y-Axis variable name
#' @param AggMethod character
#' @param GroupVar Character variable
#' @param YVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param XVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param FacetRows Defaults to 1 which causes no faceting to occur vertically. Otherwise, supply a numeric value for the number of output grid rows
#' @param FacetCols Defaults to 1 which causes no faceting to occur horizontally. Otherwise, supply a numeric value for the number of output grid columns
#' @param FacetLevels Faceting rows x columns is the max number of levels allowed in a grid. If your GroupVar has more you can supply the levels to display.
#' @param NumberBins numeric
#' @param Title character
#' @param ShowLabels character
#' @param Title.YAxis character
#' @param Title.XAxis character
#' @param Engine "Echarts" or "Plotly"
#' @param EchartsTheme "auritus","azul","bee-inspired","blue","caravan","carp","chalk","cool","dark-bold","dark","eduardo", #' "essos","forest","fresh-cut","fruit","gray","green","halloween","helianthus","infographic","inspired", #' "jazz","london","dark","macarons","macarons2","mint","purple-passion","red-velvet","red","roma","royal", #' "sakura","shine","tech-blue","vintage","walden","wef","weforum","westeros","wonderland"
#' @param TimeLine logical
#' @param X_Scroll logical
#' @param Y_Scroll logical
#' @param SampleSize numeric
#' @param BackGroundColor color outside of plot window. Rcolors and hex outside of plot window. Rcolors and hex character
#' @param ChartColor character hex
#' @param FillColor character hex
#' @param FillColorReverse character hex
#' @param GridColor character hex
#' @param TextColor character hex
#' @param ZeroLineColor character hex
#' @param ZeroLineWidth numeric
#' @param Debug Debugging purposes
#' @export
Plot.ROC <- function(dt = NULL,
                     SampleSize = 100000,
                     XVar = NULL,
                     YVar = NULL,
                     GroupVar = NULL,
                     YVarTrans = "Identity",
                     XVarTrans = "Identity",
                     FacetRows = 1,
                     FacetCols = 1,
                     FacetLevels = NULL,
                     AggMethod = 'mean',
                     Height = NULL,
                     Width = NULL,
                     Title = 'Calibration Plot',
                     ShowLabels = FALSE,
                     Title.YAxis = NULL,
                     Title.XAxis = NULL,
                     Engine = 'Echarts',
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
                     ZeroLineColor = '#ffff',
                     ZeroLineWidth = 1.25,
                     Debug = FALSE) {

  # YVar check
  yvar_class <- class(dt[[YVar]])[1L]
  if(yvar_class %in% c("factor","character")) {

    # Shrink data
    yvar_levels <- dt[, unique(get(YVar))]
    dt1 <- data.table::copy(dt[, .SD, .SDcols = c(XVar, YVar, yvar_levels, GroupVar)])

    # Dummify Target
    nam <- data.table::copy(names(dt1))
    dt1 <- AutoQuant::DummifyDT(data = dt1, cols = YVar, TopN = length(yvar_levels), KeepFactorCols = FALSE, OneHot = FALSE, SaveFactorLevels = FALSE, SavePath = getwd(), ImportFactorLevels = FALSE, FactorLevelsList = NULL, ClustScore = FALSE, ReturnFactorLevels = FALSE)
    nam <- setdiff(names(dt1), nam)

    # Melt Predict Cols
    dt2 <- data.table::melt.data.table(
      data = dt1[, .SD, .SDcols = c(names(dt1)[!names(dt1) %in% c(nam,XVar)])],
      id.vars = GroupVar,
      measure.vars = names(dt1)[!names(dt1) %in% c(nam,XVar,GroupVar)],
      variable.name = "Level",
      value.name = XVar,
      na.rm = TRUE,
      variable.factor = FALSE)

    # Melt Target Cols
    dt3 <- data.table::melt.data.table(
      data = dt1[, .SD, .SDcols = c(names(dt1)[!names(dt1) %in% c(yvar_levels,XVar)])],
      id.vars = GroupVar,
      measure.vars = nam,
      variable.name = "Level",
      value.name = YVar,
      na.rm = TRUE,
      variable.factor = FALSE)

    # Join data
    dt2[, eval(YVar) := dt3[[YVar]]]

    # Update Args
    if(length(GroupVar) > 0L) {
      dt2[, GroupVariables := do.call(paste, c(.SD, sep = ' :: ')), .SDcols = c(GroupVar, "Level")]
      GroupVar <- "GroupVariables"
      if(FacetRows > 1L && FacetCols > 1L) {
        FacetLevels <- as.character(dt2[, unique(GroupVariables)])
        FacetLevels <- FacetLevels[seq_len(min(length(FacetLevels),FacetRows*FacetCols))]
        dt2 <- dt2[GroupVariables %chin% c(eval(FacetLevels))]
      }
    } else if(length(GroupVar) == 0L && (FacetRows > 1L || FacetCols > 1L)) {
      FacetLevels <- yvar_levels[seq_len(min(length(yvar_levels), FacetRows * FacetCols))]
      dt2 <- dt2[Level %chin% c(eval(FacetLevels))]
      GroupVar <- "Level"
    } else {
      GroupVar <- "Level"
    }

  } else {
    dt2 <- data.table::copy(dt)
  }

  # Data Prep1
  if(length(GroupVar) > 0L) {
    vals <- sort(unique(dt2[[GroupVar]]))
    for(i in seq_along(vals)) { # i = 2
      temp <- dt2[get(GroupVar) %in% eval(vals[i])]
      if(Debug) print(i)
      AUC_Metrics <- tryCatch({pROC::roc(
        response = temp[[YVar]],
        predictor = temp[[XVar]],
        na.rm = TRUE,
        algorithm = 3L,
        auc = TRUE,
        ci = TRUE)}, error = function(x) NULL)
      if(i == 1L && length(AUC_Metrics) > 0L) {
        data <- data.table::data.table(
          GroupLevels = vals[i],
          Sensitivity = AUC_Metrics$sensitivities,
          Specificity = AUC_Metrics$specificities)
      } else if(length(AUC_Metrics) > 0L) {
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
      response = dt2[[YVar]],
      predictor = dt2[[XVar]],
      na.rm = TRUE,
      algorithm = 3L,
      auc = TRUE,
      ci = TRUE)

  } else {
    AUC_Metrics <- pROC::roc(
      response = dt2[[YVar]],
      predictor = dt2[[XVar]],
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
  if(length(GroupVar) > 0L && (FacetRows > 1L && FacetCols > 1L)) {
    title <- paste0(Title, ":\nMicro-AUC: ", 100 * round(AUC_Metrics$auc, 3), "%\n*Excluding cases of all 1's or 0's")
  }
  title <- paste0(Title, ":\nMicro-AUC: ", 100 * round(AUC_Metrics$auc, 3), "%")
  gv <- if(length(GroupVar) > 0L) "GroupLevels" else NULL
  data.table::setorderv(x = data, cols = c(gv, "Sensitivity"))

  # Build Plot (Line or Area)
  if(length(GroupVar) > 0L && FacetRows == 1L && FacetCols == 1L) {
    p1 <- AutoPlots::Plot.Line(
      dt = data,
      PreAgg = TRUE,
      Smooth = TRUE,
      Area = FALSE,
      ShowSymbol = FALSE,
      Alpha = 0.50,
      Engine = Engine,
      EchartsTheme = EchartsTheme,
      TimeLine = tl,
      YVar = YVar,
      XVar = XVar,
      GroupVar = gv,
      YVarTrans = YVarTrans,
      XVarTrans = XVarTrans,
      FacetRows = FacetRows,
      FacetCols = FacetCols,
      FacetLevels = FacetLevels,
      Height = Height,
      Width = Width,
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
  } else {
    p1 <- AutoPlots::Plot.Area(
      dt = data,
      PreAgg = TRUE,
      Smooth = TRUE,
      ShowSymbol = FALSE,
      Alpha = 0.50,
      Engine = Engine,
      EchartsTheme = EchartsTheme,
      TimeLine = tl,
      YVar = YVar,
      XVar = XVar,
      GroupVar = gv,
      YVarTrans = YVarTrans,
      XVarTrans = XVarTrans,
      FacetRows = FacetRows,
      FacetCols = FacetCols,
      FacetLevels = FacetLevels,
      Height = Height,
      Width = Width,
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
  }

  # Y == X dashed line
  if(class(p1)[1L] == "plotly") p1 <- plotly::add_segments(p = p1, x = 0, xend = 1, y = 0, yend = 1, line = list(dash = "dash", color = TextColor),inherit = FALSE, showlegend = FALSE)

  # Return
  return(p1)
}

#' @title Plot.ConfusionMatrix
#'
#' @description Generate variable importance plots
#'
#' @author Adrian Antico
#' @family Model Evaluation
#'
#' @param Engine 'Plotly' or "Echarts"
#' @param EchartsTheme "auritus","azul","bee-inspired","blue","caravan","carp","chalk","cool","dark-bold","dark","eduardo", #' "essos","forest","fresh-cut","fruit","gray","green","halloween","helianthus","infographic","inspired", #' "jazz","london","dark","macarons","macarons2","mint","purple-passion","red-velvet","red","roma","royal", #' "sakura","shine","tech-blue","vintage","walden","wef","weforum","westeros","wonderland"
#' @param TimeLine logical
#' @param dt source data.table
#' @param PreAgg FALSE
#' @param XVar Column name of X-Axis variable. If NULL then ignored
#' @param YVar Column name of Y-Axis variable. If NULL then ignored
#' @param ZVar = "N"
#' @param YVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param XVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param ZVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param FacetRows Defaults to 1 which causes no faceting to occur vertically. Otherwise, supply a numeric value for the number of output grid rows
#' @param FacetCols Defaults to 1 which causes no faceting to occur horizontally. Otherwise, supply a numeric value for the number of output grid columns
#' @param FacetLevels Faceting rows x columns is the max number of levels allowed in a grid. If your GroupVar has more you can supply the levels to display.
#' @param NumberBins = 21,
#' @param NumLevels_X = NumLevels_Y,
#' @param NumLevels_Y = NumLevels_X,
#' @param GroupVar Column name of Group Variable for distinct colored histograms by group levels
#' @param Title title
#' @param ShowLabels character
#' @param Title.YAxis character
#' @param Title.XAxis character
#' @param GroupVar = NULL
#' @param ZeroLineColor = '#ffff'
#' @param AggMethod Choose from 'mean', 'sum', 'sd', and 'median'
#' @param FillColor 'gray'
#' @param FillColorReverse character hex
#' @param ChartColor 'lightsteelblue'
#' @param TextColor 'darkblue'
#' @param GridColor 'white'
#' @param BackGroundColor color outside of plot window. Rcolors and hex outside of plot window. Rcolors and hex character
#' @param Debug Debugging purposes
#' @export
Plot.ConfusionMatrix <- function(dt = NULL,
                                 PreAgg = FALSE,
                                 XVar = NULL,
                                 YVar = NULL,
                                 ZVar = "N",
                                 YVarTrans = "Identity",
                                 XVarTrans = "Identity",
                                 ZVarTrans = "Identity",
                                 FacetRows = 1,
                                 FacetCols = 1,
                                 FacetLevels = NULL,
                                 NumberBins = 21,
                                 NumLevels_X = 50,
                                 NumLevels_Y = 50,
                                 Height = NULL,
                                 Width = NULL,
                                 Title = "Confusion Matrix",
                                 ShowLabels = FALSE,
                                 Title.YAxis = NULL,
                                 Title.XAxis = NULL,
                                 Engine = 'Plotly',
                                 EchartsTheme = "macarons",
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

  # YVar check
  yvar_class <- class(dt[[YVar]])[1L]

  if(yvar_class %in% c("factor","character")) {
    dt1 <- data.table::copy(dt[, .SD, .SDcols = c(XVar, YVar, GroupVar)])
    dt1[, paste0(XVar,"_") := .N, by = XVar]
    dt1[, paste0(YVar,"_") := .N, by = YVar]
    dt4 <- dt1[, list(N = .N, Mean.X = mean(get(paste0(XVar,"_")), na.rm = TRUE)), by = c(YVar,XVar)]
    dt4[, `Mean.X` := N / Mean.X]
    ZVar <- "Mean.X"
  } else if(!PreAgg) {

    if(length(unique(dt2[[XVar]])) > 2L) {
      dt2[, classPredict := data.table::fifelse(get(XVar) > 0.5, 1, 0)]
    }
    dt4 <- data.table::CJ(unique(dt2[[YVar]]), unique(dt2[["classPredict"]]))
    data.table::setnames(dt4, c("V1","V2"), c(YVar, XVar))
    dt3 <- dt2[, list(Metric = .N), by = c(YVar, "classPredict")]
    data.table::setkeyv(x = dt3, cols = c(YVar, "classPredict"))
    data.table::setkeyv(x = dt4, cols = c(YVar, XVar))
    dt4[dt3, Metric := i.Metric]
    data.table::set(dt4, i = which(is.na(dt4[["Metric"]])), j = "Metric", value = 0)
    if(Debug) print("Confusion Matrix Plot.Heatmap")
    dt4[, `Proportion in Target` := sum(Metric), by = eval(YVar)]
    dt4[, `Proportion in Target` := data.table::fifelse(`Proportion in Target` > 0, Metric / `Proportion in Target`, 0)]
    ZVar = "Proportion in Target"
  } else {
    dt4 <- data.table::copy(dt)
  }

  # Corr Matrix for the automatic ordering
  data.table::setorderv(dt4, c(XVar,YVar), c(1L,1L))
  p1 <- AutoPlots:::Plot.HeatMap(
    PreAgg = TRUE,
    Engine = Engine,
    EchartsTheme = EchartsTheme,
    Title = Title,
    dt = dt4,
    YVar = YVar,
    XVar = XVar,
    ZVar = ZVar,
    Height = Height,
    Width = Width,
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
}

#' @title Plot.Lift
#'
#' @description Create a cumulative gains chart
#'
#' @family Model Evaluation
#'
#' @author Adrian Antico
#'
#' @param dt source data.table
#' @param XVar X-Axis variable name
#' @param YVar Y-Axis variable name
#' @param ZVar character
#' @param GroupVar Character variable
#' @param YVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param XVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param ZVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param FacetRows Defaults to 1 which causes no faceting to occur vertically. Otherwise, supply a numeric value for the number of output grid rows
#' @param FacetCols Defaults to 1 which causes no faceting to occur horizontally. Otherwise, supply a numeric value for the number of output grid columns
#' @param FacetLevels Faceting rows x columns is the max number of levels allowed in a grid. If your GroupVar has more you can supply the levels to display.
#' @param NumberBins numeric
#' @param PreAgg logical
#' @param NumberBins numeric
#' @param Title character
#' @param ShowLabels character
#' @param Title.YAxis character
#' @param Title.XAxis character
#' @param Engine "Echarts" or "Plotly"
#' @param EchartsTheme "auritus","azul","bee-inspired","blue","caravan","carp","chalk","cool","dark-bold","dark","eduardo", #' "essos","forest","fresh-cut","fruit","gray","green","halloween","helianthus","infographic","inspired", #' "jazz","london","dark","macarons","macarons2","mint","purple-passion","red-velvet","red","roma","royal", #' "sakura","shine","tech-blue","vintage","walden","wef","weforum","westeros","wonderland"
#' @param TimeLine logical
#' @param X_Scroll logical
#' @param Y_Scroll logical
#' @param BackGroundColor color outside of plot window. Rcolors and hex outside of plot window. Rcolors and hex character
#' @param ChartColor character hex
#' @param FillColor character hex
#' @param FillColorReverse character hex
#' @param GridColor character hex
#' @param TextColor character hex
#' @param ZeroLineColor character hex
#' @param ZeroLineWidth numeric
#' @param Debug Debugging purposes
#'
#' @export
Plot.Lift <- function(dt = NULL,
                      PreAgg = FALSE,
                      XVar = NULL,
                      YVar = NULL,
                      ZVar = "N",
                      GroupVar = NULL,
                      YVarTrans = "Identity",
                      XVarTrans = "Identity",
                      ZVarTrans = "Identity",
                      FacetRows = 1,
                      FacetCols = 1,
                      FacetLevels = NULL,
                      NumberBins = 20,
                      Height = NULL,
                      Width = NULL,
                      Title = "Confusion Matrix",
                      ShowLabels = FALSE,
                      Title.YAxis = NULL,
                      Title.XAxis = NULL,
                      Engine = 'Plotly',
                      EchartsTheme = "macarons",
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

  if(Debug) print("here 1")

  # YVar check
  yvar_class <- class(dt[[YVar]])[1L]
  print(yvar_class)
  if(yvar_class %in% c("factor","character")) {

    if(Debug) print("here 2")

    # Shrink data
    yvar_levels <- as.character(dt[, unique(get(YVar))])
    dt1 <- data.table::copy(dt[, .SD, .SDcols = c(XVar, YVar, yvar_levels, GroupVar)])

    if(Debug) print("here 3")

    # Dummify Target
    nam <- data.table::copy(names(dt1))
    dt1 <- AutoQuant::DummifyDT(data = dt1, cols = YVar, TopN = length(yvar_levels), KeepFactorCols = FALSE, OneHot = FALSE, SaveFactorLevels = FALSE, SavePath = getwd(), ImportFactorLevels = FALSE, FactorLevelsList = NULL, ClustScore = FALSE, ReturnFactorLevels = FALSE)
    nam <- setdiff(names(dt1), nam)

    if(Debug) print("here 4")

    # Melt Predict Cols
    dt2 <- data.table::melt.data.table(
      data = dt1[, .SD, .SDcols = c(names(dt1)[!names(dt1) %in% c(nam,XVar)])],
      id.vars = GroupVar,
      measure.vars = names(dt1)[!names(dt1) %in% c(nam,XVar,GroupVar)],
      variable.name = "Level",
      value.name = XVar,
      na.rm = TRUE,
      variable.factor = FALSE)

    if(Debug) print("here 5")

    # Melt Target Cols
    dt3 <- data.table::melt.data.table(
      data = dt1[, .SD, .SDcols = c(names(dt1)[!names(dt1) %in% c(yvar_levels,XVar)])],
      id.vars = GroupVar,
      measure.vars = nam,
      variable.name = "Level",
      value.name = YVar,
      na.rm = TRUE,
      variable.factor = FALSE)

    if(Debug) print("here 6")

    # Join data
    dt2[, eval(YVar) := dt3[[YVar]]]

    if(Debug) print("here 7")

    # Update Args
    if(length(GroupVar) > 0L) {
      dt2[, GroupVariables := do.call(paste, c(.SD, sep = ' :: ')), .SDcols = c(GroupVar, "Level")]
      GroupVar <- "GroupVariables"
      if(FacetRows > 1L && FacetCols > 1L) {
        FacetLevels <- as.character(dt2[, unique(GroupVariables)])
        FacetLevels <- FacetLevels[seq_len(min(length(FacetLevels),FacetRows*FacetCols))]
        dt2 <- dt2[GroupVariables %chin% c(eval(FacetLevels))]
      } else {
        FacetLevels <- yvar_levels
      }
    } else {
      if(FacetRows > 1L && FacetCols > 1L) {
        FacetLevels <- yvar_levels
        FacetLevels <- FacetLevels[seq_len(min(length(FacetLevels),FacetRows*FacetCols))]
        dt2 <- dt2[Level %chin% c(eval(FacetLevels))]
      } else {
        FacetLevels <- yvar_levels
      }
      GroupVar <- "Level"
      dt2 <- dt2[Level %chin% c(eval(FacetLevels))]
      GroupVar <- "Level"
    }

  } else {
    dt2 <- data.table::copy(dt)
  }

  if(Debug) print("here 9")

  if(yvar_class %in% c("factor","character") || length(GroupVar) > 0L) {
    dl <- list()
    if(Debug) print("Start For-Loop")
    if(length(NumberBins) == 0L) NumberBins <- 21
    if(max(NumberBins) > 1L) NumberBins <- c(seq(1/NumberBins, 1 - 1/NumberBins, 1/NumberBins), 1)
    for(i in FacetLevels) {# i = 1
      if(Debug) print("iter")
      if(Debug) print(i)
      dt_ <- dt2[get(GroupVar) %in% eval(i)]
      if(Debug) print(" iter 2")
      dt_[, NegScore := -get(XVar)]
      if(Debug) print(" iter 3")
      if(Debug) print(" iter 4")
      Cuts <- quantile(x = dt_[["NegScore"]], probs = NumberBins)
      if(Debug) print(" iter 5")
      dt_[, eval(YVar) := as.character(get(YVar))]
      if(Debug) print(" iter 6")
      grp <- dt_[, .N, by = eval(YVar)][order(N)]
      if(Debug) print(" iter 7")
      smaller_class <- grp[1L, 1L][[1L]]
      if(Debug) print(" iter 8")
      dt3 <- round(100 * sapply(Cuts, function(x) {
        dt_[NegScore <= x & get(YVar) == eval(smaller_class), .N] / dt_[get(YVar) == eval(smaller_class), .N]
      }), 2)
      if(Debug) print(" iter 9")
      dt3 <- rbind(dt3, -Cuts)
      if(Debug) print(" iter 10")
      rownames(dt3) <- c("Gain", "Score.Point")
      if(Debug) print(" iter 11")
      dt4 <- grp[1,2] / (grp[2,2] + grp[1,2])
      if(Debug) print(" iter 12")
      dt5 <- data.table::as.data.table(t(dt3))
      if(Debug) print(" iter 13")
      dt5[, Population := as.numeric(100 * eval(NumberBins))]
      if(Debug) print(" iter 14")
      dt5[, Lift := round(Gain / 100 / NumberBins, 2)]
      if(Debug) print(" iter 15")
      dt5[, Level := eval(i)]
      if(Debug) print(" iter 16")
      if(data.table::is.data.table(dt5)) {
        if(Debug) print(" iter rbindlist")
        dl[[i]] <- data.table::rbindlist(list(
          data.table::data.table(Gain = 0, Score.Point = 0, Population = 0, Lift = 0, Level = eval(i)),
          dt5
        ))
      }
    }
    if(Debug) print(" For Loop Done: rbindlist")
    dt6 <- data.table::rbindlist(dl)

  } else {

    if(Debug) print("here 10")

    # Data Prep
    dt2[, NegScore := -get(XVar)]
    NumberBins <- c(seq(1/NumberBins, 1 - 1/NumberBins, 1/NumberBins), 1)
    Cuts <- quantile(x = dt2[["NegScore"]], probs = NumberBins)
    dt2[, eval(YVar) := as.character(get(YVar))]
    grp <- dt2[, .N, by = eval(YVar)][order(N)]
    smaller_class <- grp[1L, 1L][[1L]]
    dt3 <- round(100 * sapply(Cuts, function(x) {
      dt2[NegScore <= x & get(YVar) == eval(smaller_class), .N] / dt2[get(YVar) == eval(smaller_class), .N]
    }), 2)
    dt3 <- rbind(dt3, -Cuts)
    rownames(dt3) <- c("Gain", "Score.Point")
    dt4 <- grp[1,2] / (grp[2,2] + grp[1,2])
    dt5 <- data.table::as.data.table(t(dt3))
    dt5[, Population := as.numeric(100 * eval(NumberBins))]
    dt5[, Lift := round(Gain / 100 / NumberBins, 2)]
    if(data.table::is.data.table(dt5)) {
      dt6 <- data.table::rbindlist(list(
        data.table::data.table(Gain = 0, Score.Point = 0, Population = 0, Lift = 0),
        dt5
      ))
    }
  }

  if(Debug) print("here 11")

  # Build
  if(Debug) print(names(dt6))
  dt6 <- dt6[Population > 0, .SD, .SDcols = c("Population","Lift", GroupVar)]

  if(Debug) print("here 12")

  if(FacetRows == 1L && FacetCols == 1L && length(GroupVar) > 0L) {

    if(Debug) print("here 13")

    p1 <- AutoPlots::Plot.Line(
      dt = dt6,
      PreAgg = TRUE,
      XVar = "Population",
      YVar = "Lift",
      GroupVar = GroupVar,
      YVarTrans = YVarTrans,
      XVarTrans = XVarTrans,
      FacetRows = FacetRows,
      FacetCols = FacetCols,
      FacetLevels = NULL,
      Height = Height,
      Width = Width,
      Title = Title,
      Area = FALSE,
      Smooth = TRUE,
      ShowSymbol = FALSE,
      Engine = Engine,
      EchartsTheme = EchartsTheme,
      TimeLine = FALSE,
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
  } else {

    if(Debug) print("here 14")

    p1 <- AutoPlots::Plot.Area(
      dt = dt6,
      PreAgg = TRUE,
      XVar = "Population",
      YVar = "Lift",
      GroupVar = GroupVar,
      YVarTrans = YVarTrans,
      XVarTrans = XVarTrans,
      FacetRows = FacetRows,
      FacetCols = FacetCols,
      FacetLevels = NULL,
      Height = Height,
      Width = Width,
      Title = Title,
      Smooth = TRUE,
      ShowSymbol = FALSE,
      Engine = Engine,
      EchartsTheme = EchartsTheme,
      TimeLine = FALSE,
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
  }

  if(Debug) print("here 16")

  g <- class(p1)[1L]
  if(g == "plotly") {
    p1 <- plotly::layout(p = p1, uniformtext = list(minsize=8, mode='hide', color="white"))
  } else if(g == "echarts4r") {
    if(Debug) print("here 17")
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
#' @param dt source data.table
#' @param XVar X-Axis variable name
#' @param YVar Y-Axis variable name
#' @param ZVar character
#' @param GroupVar Character variable
#' @param YVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param XVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param ZVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param FacetRows Defaults to 1 which causes no faceting to occur vertically. Otherwise, supply a numeric value for the number of output grid rows
#' @param FacetCols Defaults to 1 which causes no faceting to occur horizontally. Otherwise, supply a numeric value for the number of output grid columns
#' @param FacetLevels Faceting rows x columns is the max number of levels allowed in a grid. If your GroupVar has more you can supply the levels to display.
#' @param NumberBins numeric
#' @param PreAgg logical
#' @param NumberBins numeric
#' @param Title character
#' @param ShowLabels character
#' @param Title.YAxis character
#' @param Title.XAxis character
#' @param Engine "Echarts" or "Plotly"
#' @param EchartsTheme "auritus","azul","bee-inspired","blue","caravan","carp","chalk","cool","dark-bold","dark","eduardo", #' "essos","forest","fresh-cut","fruit","gray","green","halloween","helianthus","infographic","inspired", #' "jazz","london","dark","macarons","macarons2","mint","purple-passion","red-velvet","red","roma","royal", #' "sakura","shine","tech-blue","vintage","walden","wef","weforum","westeros","wonderland"
#' @param TimeLine logical
#' @param X_Scroll logical
#' @param Y_Scroll logical
#' @param BackGroundColor color outside of plot window. Rcolors and hex outside of plot window. Rcolors and hex character
#' @param ChartColor character hex
#' @param FillColor character hex
#' @param FillColorReverse character hex
#' @param GridColor character hex
#' @param TextColor character hex
#' @param ZeroLineColor character hex
#' @param ZeroLineWidth numeric
#' @param Debug Debugging purposes
#'
#' @export
Plot.Gains <- function(dt = NULL,
                       PreAgg = FALSE,
                       XVar = NULL,
                       YVar = NULL,
                       ZVar = "N",
                       GroupVar = NULL,
                       YVarTrans = "Identity",
                       XVarTrans = "Identity",
                       ZVarTrans = "Identity",
                       FacetRows = 1,
                       FacetCols = 1,
                       FacetLevels = NULL,
                       NumberBins = 20,
                       Height = NULL,
                       Width = NULL,
                       Title = "Gains Plot",
                       ShowLabels = FALSE,
                       Title.YAxis = NULL,
                       Title.XAxis = NULL,
                       Engine = 'Plotly',
                       EchartsTheme = "macarons",
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

  if(Debug) print("here 1")

  # YVar check
  yvar_class <- class(dt[[YVar]])[1L]
  print(yvar_class)
  if(yvar_class %in% c("factor","character")) {

    if(Debug) print("here 2")

    # Shrink data
    yvar_levels <- as.character(dt[, unique(get(YVar))])
    dt1 <- data.table::copy(dt[, .SD, .SDcols = c(XVar, YVar, yvar_levels, GroupVar)])

    if(Debug) print("here 3")

    # Dummify Target
    nam <- data.table::copy(names(dt1))
    dt1 <- AutoQuant::DummifyDT(data = dt1, cols = YVar, TopN = length(yvar_levels), KeepFactorCols = FALSE, OneHot = FALSE, SaveFactorLevels = FALSE, SavePath = getwd(), ImportFactorLevels = FALSE, FactorLevelsList = NULL, ClustScore = FALSE, ReturnFactorLevels = FALSE)
    nam <- setdiff(names(dt1), nam)

    if(Debug) print("here 4")

    # Melt Predict Cols
    dt2 <- data.table::melt.data.table(
      data = dt1[, .SD, .SDcols = c(names(dt1)[!names(dt1) %in% c(nam,XVar)])],
      id.vars = GroupVar,
      measure.vars = names(dt1)[!names(dt1) %in% c(nam,XVar,GroupVar)],
      variable.name = "Level",
      value.name = XVar,
      na.rm = TRUE,
      variable.factor = FALSE)

    if(Debug) print("here 5")

    # Melt Target Cols
    dt3 <- data.table::melt.data.table(
      data = dt1[, .SD, .SDcols = c(names(dt1)[!names(dt1) %in% c(yvar_levels,XVar)])],
      id.vars = GroupVar,
      measure.vars = nam,
      variable.name = "Level",
      value.name = YVar,
      na.rm = TRUE,
      variable.factor = FALSE)

    if(Debug) print("here 6")

    # Join data
    dt2[, eval(YVar) := dt3[[YVar]]]

    if(Debug) print("here 7")

    # Update Args
    if(length(GroupVar) > 0L) {
      dt2[, GroupVariables := do.call(paste, c(.SD, sep = ' :: ')), .SDcols = c(GroupVar, "Level")]
      GroupVar <- "GroupVariables"
      if(FacetRows > 1L && FacetCols > 1L) {
        FacetLevels <- as.character(dt2[, unique(GroupVariables)])
        FacetLevels <- FacetLevels[seq_len(min(length(FacetLevels),FacetRows*FacetCols))]
        dt2 <- dt2[GroupVariables %chin% c(eval(FacetLevels))]
      } else {
        FacetLevels <- yvar_levels
      }
    } else {
      if(FacetRows > 1L && FacetCols > 1L) {
        FacetLevels <- yvar_levels
        FacetLevels <- FacetLevels[seq_len(min(length(FacetLevels),FacetRows*FacetCols))]
        dt2 <- dt2[Level %chin% c(eval(FacetLevels))]
      } else {
        FacetLevels <- yvar_levels
      }
      GroupVar <- "Level"
      dt2 <- dt2[Level %chin% c(eval(FacetLevels))]
      GroupVar <- "Level"
    }

  } else {
    dt2 <- data.table::copy(dt)
  }

  if(Debug) print("here 9")

  if(yvar_class %in% c("factor","character") || length(GroupVar) > 0L) {
    dl <- list()
    if(Debug) print("Start For-Loop")
    if(length(NumberBins) == 0L) NumberBins <- 21
    if(max(NumberBins) > 1L) NumberBins <- c(seq(1/NumberBins, 1 - 1/NumberBins, 1/NumberBins), 1)
    for(i in FacetLevels) {# i = 1
      if(Debug) print("iter")
      if(Debug) print(i)
      dt_ <- dt2[get(GroupVar) %in% eval(i)]
      if(Debug) print(" iter 2")
      dt_[, NegScore := -get(XVar)]
      if(Debug) print(" iter 3")
      if(Debug) print(" iter 4")
      Cuts <- quantile(x = dt_[["NegScore"]], probs = NumberBins)
      if(Debug) print(" iter 5")
      dt_[, eval(YVar) := as.character(get(YVar))]
      if(Debug) print(" iter 6")
      grp <- dt_[, .N, by = eval(YVar)][order(N)]
      if(Debug) print(" iter 7")
      smaller_class <- grp[1L, 1L][[1L]]
      if(Debug) print(" iter 8")
      dt3 <- round(100 * sapply(Cuts, function(x) {
        dt_[NegScore <= x & get(YVar) == eval(smaller_class), .N] / dt_[get(YVar) == eval(smaller_class), .N]
      }), 2)
      if(Debug) print(" iter 9")
      dt3 <- rbind(dt3, -Cuts)
      if(Debug) print(" iter 10")
      rownames(dt3) <- c("Gain", "Score.Point")
      if(Debug) print(" iter 11")
      dt4 <- grp[1,2] / (grp[2,2] + grp[1,2])
      if(Debug) print(" iter 12")
      dt5 <- data.table::as.data.table(t(dt3))
      if(Debug) print(" iter 13")
      dt5[, Population := as.numeric(100 * eval(NumberBins))]
      if(Debug) print(" iter 14")
      dt5[, Lift := round(Gain / 100 / NumberBins, 2)]
      if(Debug) print(" iter 15")
      dt5[, Level := eval(i)]
      if(Debug) print(" iter 16")
      if(data.table::is.data.table(dt5)) {
        if(Debug) print(" iter rbindlist")
        dl[[i]] <- data.table::rbindlist(list(
          data.table::data.table(Gain = 0, Score.Point = 0, Population = 0, Lift = 0, Level = eval(i)),
          dt5
        ))
      }
    }
    if(Debug) print(" For Loop Done: rbindlist")
    dt6 <- data.table::rbindlist(dl)

  } else {

    if(Debug) print("here 10")

    # Data Prep
    dt2[, NegScore := -get(XVar)]
    NumberBins <- c(seq(1/NumberBins, 1 - 1/NumberBins, 1/NumberBins), 1)
    Cuts <- quantile(x = dt2[["NegScore"]], probs = NumberBins)
    dt2[, eval(YVar) := as.character(get(YVar))]
    grp <- dt2[, .N, by = eval(YVar)][order(N)]
    smaller_class <- grp[1L, 1L][[1L]]
    dt3 <- round(100 * sapply(Cuts, function(x) {
      dt2[NegScore <= x & get(YVar) == eval(smaller_class), .N] / dt2[get(YVar) == eval(smaller_class), .N]
    }), 2)
    dt3 <- rbind(dt3, -Cuts)
    rownames(dt3) <- c("Gain", "Score.Point")
    dt4 <- grp[1,2] / (grp[2,2] + grp[1,2])
    dt5 <- data.table::as.data.table(t(dt3))
    dt5[, Population := as.numeric(100 * eval(NumberBins))]
    dt5[, Lift := round(Gain / 100 / NumberBins, 2)]
    if(data.table::is.data.table(dt5)) {
      dt6 <- data.table::rbindlist(list(
        data.table::data.table(Gain = 0, Score.Point = 0, Population = 0, Lift = 0),
        dt5
      ))
    }
  }

  if(Debug) print("here 11")

  # Build
  if(Debug) print(names(dt6))
  dt6 <- dt6[Population > 0, .SD, .SDcols = c("Population","Gain", GroupVar)]

  if(Debug) print("here 12")

  if(FacetRows == 1L && FacetCols == 1L && length(GroupVar) > 0L) {

    if(Debug) print("here 13")

    p1 <- AutoPlots::Plot.Line(
      dt = dt6,
      PreAgg = TRUE,
      XVar = "Population",
      YVar = "Gain",
      GroupVar = GroupVar,
      YVarTrans = YVarTrans,
      XVarTrans = XVarTrans,
      FacetRows = FacetRows,
      FacetCols = FacetCols,
      FacetLevels = NULL,
      Height = Height,
      Width = Width,
      Title = Title,
      Area = FALSE,
      Smooth = TRUE,
      ShowSymbol = FALSE,
      Engine = Engine,
      EchartsTheme = EchartsTheme,
      TimeLine = FALSE,
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
  } else {

    if(Debug) print("here 14")

    p1 <- AutoPlots::Plot.Area(
      dt = dt6,
      PreAgg = TRUE,
      XVar = "Population",
      YVar = "Gain",
      GroupVar = GroupVar,
      YVarTrans = YVarTrans,
      XVarTrans = XVarTrans,
      FacetRows = FacetRows,
      FacetCols = FacetCols,
      FacetLevels = NULL,
      Height = Height,
      Width = Width,
      Title = Title,
      Smooth = TRUE,
      ShowSymbol = FALSE,
      Engine = Engine,
      EchartsTheme = EchartsTheme,
      TimeLine = FALSE,
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
  }

  if(Debug) print("here 16")

  g <- class(p1)[1L]
  if(g == "plotly") {
    p1 <- plotly::layout(p = p1, uniformtext = list(minsize=8, mode='hide', color="white"))
  } else if(g == "echarts4r") {
    if(Debug) print("here 17")
    p1 <- echarts4r::e_labels(e = p1, show = TRUE)
  }

  # Return
  return(p1)
}

#' @title Plot.BinaryMetrics
#'
#' @description Line plot of evaluation metrics across thresholds
#'
#' @author Adrian Antico
#' @family Model Evaluation
#'
#' @param dt source data.table
#' @param SampleSize numeric
#' @param XVar X-Axis variable name
#' @param YVar Y-Axis variable name
#' @param ZVar character
#' @param YVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param XVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param ZVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param FacetRows Defaults to 1 which causes no faceting to occur vertically. Otherwise, supply a numeric value for the number of output grid rows
#' @param FacetCols Defaults to 1 which causes no faceting to occur horizontally. Otherwise, supply a numeric value for the number of output grid columns
#' @param FacetLevels Faceting rows x columns is the max number of levels allowed in a grid. If your GroupVar has more you can supply the levels to display.
#' @param Metrics Multiple selection "Utility","MCC","Accuracy","F1_Score","F2_Score","F0.5_Score","ThreatScore","TPR","TNR","FNR","FPR","FDR","FOR"
#' @param NumberBins numeric
#' @param PreAgg logical
#' @param Title character
#' @param ShowLabels character
#' @param Title.YAxis character
#' @param Title.XAxis character
#' @param Engine "Echarts" or "Plotly"
#' @param EchartsTheme "auritus","azul","bee-inspired","blue","caravan","carp","chalk","cool","dark-bold","dark","eduardo", #' "essos","forest","fresh-cut","fruit","gray","green","halloween","helianthus","infographic","inspired", #' "jazz","london","dark","macarons","macarons2","mint","purple-passion","red-velvet","red","roma","royal", #' "sakura","shine","tech-blue","vintage","walden","wef","weforum","westeros","wonderland"
#' @param EchartsLabels character
#' @param TimeLine logical
#' @param X_Scroll logical
#' @param Y_Scroll logical
#' @param BackGroundColor color outside of plot window. Rcolors and hex outside of plot window. Rcolors and hex character
#' @param ChartColor hex character
#' @param FillColor hex character
#' @param FillColorReverse hex character
#' @param GridColor hex character
#' @param TextColor hex character
#' @param ZeroLineColor hex character
#' @param ZeroLineWidth numeric
#' @param AggMethod character
#' @param GroupVar Character variable
#' @param Debug Debugging purposes
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
                               YVarTrans = "Identity",
                               XVarTrans = "Identity",
                               ZVarTrans = "Identity",
                               FacetRows = 1,
                               FacetCols = 1,
                               FacetLevels = NULL,
                               CostMatrixWeights = c(0,1,1,0),
                               NumberBins = 20,
                               Height = NULL,
                               Width = NULL,
                               Title = "Binary Metrics",
                               ShowLabels = FALSE,
                               Title.YAxis = NULL,
                               Title.XAxis = NULL,
                               Engine = 'Plotly',
                               EchartsTheme = "macarons",
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
    YVarTrans = YVarTrans,
    XVarTrans = XVarTrans,
    FacetRows = FacetRows,
    FacetCols = FacetCols,
    FacetLevels = FacetLevels,
    Height = Height,
    Width = Width,
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
#' @param dt source data.table
#' @param Engine "plotly", "echarts4r"
#' @param EchartsTheme "dark-blue"
#' @param YVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param XVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param ZVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param FacetRows Defaults to 1 which causes no faceting to occur vertically. Otherwise, supply a numeric value for the number of output grid rows
#' @param FacetCols Defaults to 1 which causes no faceting to occur horizontally. Otherwise, supply a numeric value for the number of output grid columns
#' @param FacetLevels Faceting rows x columns is the max number of levels allowed in a grid. If your GroupVar has more you can supply the levels to display.
#' @param AggMethod "mean", "median", "sum", "sd", "skewness","kurtosis", "coeffvar", "meanabs", "medianabs", "sumabs", "sdabs", "skewnessabs", "kurtosisabs", "CoeffVarabs"
#' @param NumberBins = 21
#' @param NumLevels_Y = 20
#' @param NumLevels_X = 20
#' @param Title "Heatmap"
#' @param ShowLabels character
#' @param Title.YAxis character
#' @param Title.XAxis character
#' @param BackGroundColor color outside of plot window. Rcolors and hex outside of plot window. Rcolors and hex character
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
                                YVarTrans = "Identity",
                                XVarTrans = "Identity",
                                ZVarTrans = "Identity",
                                FacetRows = 1,
                                FacetCols = 1,
                                FacetLevels = NULL,
                                NumberBins = 21,
                                NumLevels_X = 33,
                                NumLevels_Y = 33,
                                Height = NULL,
                                Width = NULL,
                                Title = "Shap Importance",
                                ShowLabels = FALSE,
                                Title.YAxis = NULL,
                                Title.XAxis = NULL,
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
    if(Debug) print("ShapImportance Step 2")
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
      Height = Height,
      Width = Width,
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
      dt = dt2,
      AggMethod = 'mean',
      XVar = "Variable",
      YVar = "Importance",
      GroupVar = NULL,
      YVarTrans = YVarTrans,
      XVarTrans = XVarTrans,
      FacetRows = FacetRows,
      FacetCols = FacetCols,
      FacetLevels = FacetLevels,
      Height = Height,
      Width = Width,
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

