![Version:1.0.0](https://img.shields.io/static/v1?label=Version&message=1.0.0&color=blue&?style=plastic)
[![PRsWelcome](https://img.shields.io/badge/PRs-welcome-brightgreen.svg?style=default)](http://makeapullrequest.com)

<img src="https://raw.githubusercontent.com/AdrianAntico/AutoPlots/master/inst/Logo2.PNG" align="center" width="800" />

### Motivation
I want to make building the best plots as easy as possible. I've never really been a fan of incrementally building a plot by calling function after function, mostly because I have to keep going to stackoverflow to get the syntax or flip through entire documentation just to see what's possible. I'm sorry but that is a gigantic waste of everyone's time, especially when a simple API solution is possible.

This package is intended to reduce or eliminate that behavior (hence the "Auto" part of the name "AutoPlots"). The plots returned in AutoPlots are sufficiently good for 99% of plotting purposes. There are two broad classes of plots available in AutoPlots: Standard Plots and Model Evaluation Plots. If other users find additional plots that this package can support I'm open to having them incorporated.

### Standard plots 
These plot types should be known to most, although there are some that might not fit that category, such as river plots.
- Histogram Plots
- Density Plots
- Box Plots
- Violin Plots
- Pie Charts
- Bar Plots
- 3D Bar Plots
- Stacked Bar Plots
- Line Plots
- Step Plots
- Area Plots
- River Plots
- Scatter Plots
- 3D Scatter Plots
- Copula Plots
- 3D Copula Plots
- Correlation Matrix Plots
- Heatmaps
- Candlestick Plots


### Model evaluation 
These plot types are most useful for those looking to evaluate the performance of regression, binary classification, and multiclass models. Designing plots for multiclass models are rather challenging but I've abstracted all that work away so the user only has to pass their categorical target variable along with their categorical predicted value, and the plots will display all the levels appropriately without requiring the user to do the data manipulation ahead of time. Same goes for regression and classification, which are easier, but still requires time and energy.

Additionaly, all model evaluation plots supports grouping variables for by-analysis of models, even for multiclass models! 
- Calibration Plots
- Calibration Scatter Plots
- Partital Dependence Plots
- Partital Dependence Heatmaps
- Variable Importance Plots
- Shapely Importance Plots
- ROC Plots
- Confusion Matrix Heatmaps
- Lift Plots
- Gain Plots

### Data Management
Another giant bonus is that the user can either pre-aggregate their data and pass that through to these functions (using PreAgg = TRUE) or they can leave their data in raw form and let my optimized data.table code manage it for them. This means you can develop plots from giant data sets without having to wait for long running data operations. Further, there is a SampleSize parameter in the functions to limit the number of records to display, for the giant data cases (or for scatter / copula plots). This sampling takes place AFTER data aggregation, not before.

### Features
- Choose from Echarts or Plotly via functions parameter
- Common API across all functions, regardless of Echarts usage or Plotly usage
- Automatic data management via data.table operations
- Large variety of aggregation statistics options
- Large number of numeric transformations options
- Easy faceting by specifying FacetRows and FacetCols via function parameters
- Automatic formatting from Echarts and Plotly (Echarts has some really great features!)
- Updating Titles, Axes Labels, and Values displayed on plots
- There are 30+ plot types (18+ standard and 12 model evaluation) including 3D Plots
- Display size sampling (sampled right before plot building, not before data management)
- Model evaluation plots available by grouping variables (or faceted)


### Available Plots

<img src="https://raw.githubusercontent.com/AdrianAntico/AutoPlots/master/inst/Available_Plots1.PNG" align="center" width="800" />


### Histogram, Density, BoxPlot

<img src="https://raw.githubusercontent.com/AdrianAntico/AutoPlots/master/inst/Distribution.PNG" align="center" width="800" />

### Area, Steam, Pie

<img src="https://raw.githubusercontent.com/AdrianAntico/AutoPlots/master/inst/Area_Steam_Pie.PNG" align="center" width="800" />

### Bar3D, Stacked Bar, Heatmap

<img src="https://raw.githubusercontent.com/AdrianAntico/AutoPlots/master/inst/Bar3D_StackBar_Heatmap.PNG" align="center" width="800" />

### Stocks: Stock Price, % Log Returns, Quadratic Variation

<img src="https://raw.githubusercontent.com/AdrianAntico/AutoPlots/master/inst/Stocks.PNG" align="center" width="800" />

### ScatterPlot, CopulaPlot, 3D CopulaPlot

<img src="https://raw.githubusercontent.com/AdrianAntico/AutoPlots/master/inst/Relationships.PNG" align="center" width="800" />

### Installation

```r
devtools::install_github("AdrianAntico/AutoPlots")
```

### Getting Started

```r
# Create fake data
data <- AutoQuant::FakeDataGenerator(N = 100000)

# Echarts Bar Chart
AutoPlots::Plot.Bar(
  dt = data,
  PreAgg = FALSE,
  XVar = "Factor_1",
  YVar = "Adrian",
  YVarTrans = "Identity",
  Engine = 'Echarts',
  EchartsTheme = "macarons")
  
# Plotly Bar Chart
AutoPlots::Plot.Bar(
  dt = data,
  PreAgg = FALSE,
  XVar = "Factor_1",
  YVar = "Adrian",
  YVarTrans = "Identity",
  Engine = 'Plotly')
```

### Bigger Data

```r
# Create fake data
data <- AutoQuant::FakeDataGenerator(N = 1000000)

# Build bar plot
AutoPlots::Plot.Bar(
  dt = data,
  PreAgg = FALSE,
  XVar = "Factor_1",
  YVar = "Adrian",
  YVarTrans = "Identity",
  Engine = 'Echarts',
  EchartsTheme = "macarons")

# Plotly Bar Chart
AutoPlots::Plot.Bar(
  dt = data,
  PreAgg = FALSE,
  XVar = "Factor_1",
  YVar = "Adrian",
  YVarTrans = "Identity",
  Engine = 'Plotly')
```

### Even Bigger Data

```r
# Create fake data
data <- AutoQuant::FakeDataGenerator(N = 10000000)

# Build bar plot
AutoPlots::Plot.Bar(
  dt = data,
  PreAgg = FALSE,
  XVar = "Factor_1",
  YVar = "Adrian",
  YVarTrans = "Identity",
  Engine = 'Echarts',
  EchartsTheme = "macarons")
  
# Plotly Bar Chart
AutoPlots::Plot.Bar(
  dt = data,
  PreAgg = FALSE,
  XVar = "Factor_1",
  YVar = "Adrian",
  YVarTrans = "Identity",
  Engine = 'Plotly')
```
