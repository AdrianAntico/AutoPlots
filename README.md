![Version:1.0.0](https://img.shields.io/static/v1?label=Version&message=1.0.0&color=blue&?style=plastic)
[![PRsWelcome](https://img.shields.io/badge/PRs-welcome-brightgreen.svg?style=default)](http://makeapullrequest.com)

<img src="https://raw.githubusercontent.com/AdrianAntico/AutoPlots/master/inst/Logo2.PNG" align="center" width="800" />

### Motivation
I want to make building the best plots as easy as possible. I've never really been a fan of incrementally building a plot by calling function after function, mostly because I have to keep going to stackoverflow to get the syntax or flip through entire documentation just to see what's possible.

This package is intended to reduce or eliminate that behavior (hence the "Auto" part of the name "AutoPlots"). The plots returned in AutoPlots are sufficiently good for 99% of plotting purposes. There are two broad classes of plots available in AutoPlots: Standard Plots and Model Evaluation Plots. If other users find additional plots that this package can support I'm open to having them incorporated.

### Standard plots 
- Histogram Plots
- Density Plots
- Box Plots
- Probability Plots
- Word Cloud
- Pie Charts
- Donut Plot
- Rosetype Plot
- Bar Plots
- 3D Bar Plots
- Stacked Bar Plots
- Radar Plots
- Line Plots
- Step Plots
- Area Plots
- River Plots
- Autocorrelation Plot
- Partial Autocorrelation Plot
- Scatter Plots
- 3D Scatter Plots
- Copula Plots
- 3D Copula Plots
- Correlation Matrix Plots
- Parallel Plots
- Heatmaps


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
- Common API across all functions, regardless of Echarts usage or Plotly usage
- Automatic data management via data.table operations
- Large variety of aggregation statistics options
- Large number of numeric transformations options
- Easy faceting by specifying FacetRows and FacetCols via function parameters
- Automatic formatting from Echarts
- Updating Titles, Axes Labels, and Values displayed on plots
- There are 37 plot types (25 standard and 12 model evaluation) including 3D Plots
- Display size sampling (sampled right before plot building, not before data management)
- Model evaluation plots available by grouping variables (or faceted)


# Getting Started

### Installation

```r
install.packages("bit64")
install.packages("data.table")
install.packages("echarts4r")
install.packages("dplyr")
install.packages("quanteda")
install.packages("quanteda.textstats")
devtools::install_github("AdrianAntico/Rodeo", upgrade = FALSE, force = TRUE)
devtools::install_github("AdrianAntico/AutoPlots", upgrade = FALSE, force = TRUE)
```


<img src="https://github.com/AdrianAntico/AutoPlots/blob/master/Images/AreaPlot.PNG?raw=true" align="center" width="800" />
<img src="https://github.com/AdrianAntico/AutoPlots/blob/master/Images/AutocorrelationPlot.PNG?raw=true" align="center" width="800" />
<img src="https://github.com/AdrianAntico/AutoPlots/blob/master/Images/BarPlot.PNG?raw=true" align="center" width="800" />
<img src="https://github.com/AdrianAntico/AutoPlots/blob/master/Images/BarPlot3D.PNG?raw=true" align="center" width="800" />
<img src="https://github.com/AdrianAntico/AutoPlots/blob/master/Images/CopulaPlot.PNG?raw=true" align="center" width="800" />
<img src="https://github.com/AdrianAntico/AutoPlots/blob/master/Images/CopulaPlot3D.PNG?raw=true" align="center" width="800" />
<img src="https://github.com/AdrianAntico/AutoPlots/blob/master/Images/CorrelogramPlot.PNG?raw=true" align="center" width="800" />
<img src="https://github.com/AdrianAntico/AutoPlots/blob/master/Images/Density.PNG?raw=true" align="center" width="800" />
<img src="https://github.com/AdrianAntico/AutoPlots/blob/master/Images/DonutPlot.PNG?raw=true" align="center" width="800" />
<img src="https://github.com/AdrianAntico/AutoPlots/blob/master/Images/Heatmap.PNG?raw=true" align="center" width="800" />
<img src="https://github.com/AdrianAntico/AutoPlots/blob/master/Images/Histogram.PNG?raw=true" align="center" width="800" />
<img src="https://github.com/AdrianAntico/AutoPlots/blob/master/Images/RiverPlot.PNG?raw=true" align="center" width="800" />
<img src="https://github.com/AdrianAntico/AutoPlots/blob/master/Images/ParallelPlot.PNG?raw=true" align="center" width="800" />
<img src="https://github.com/AdrianAntico/AutoPlots/blob/master/Images/PartialAutocorrelationPlot.PNG?raw=true" align="center" width="800" />
<img src="https://github.com/AdrianAntico/AutoPlots/blob/master/Images/PiePlot.PNG?raw=true" align="center" width="800" />
<img src="https://github.com/AdrianAntico/AutoPlots/blob/master/Images/ProbabilityPlot.PNG?raw=true" align="center" width="800" />
<img src="https://github.com/AdrianAntico/AutoPlots/blob/master/Images/RosetypePlot.PNG?raw=true" align="center" width="800" />
<img src="https://github.com/AdrianAntico/AutoPlots/blob/master/Images/ScatterPlot.PNG?raw=true" align="center" width="800" />
<img src="https://github.com/AdrianAntico/AutoPlots/blob/master/Images/ScatterPlot3D.PNG?raw=true" align="center" width="800" />
<img src="https://github.com/AdrianAntico/AutoPlots/blob/master/Images/StepPlot.PNG?raw=true" align="center" width="800" />
<img src="https://github.com/AdrianAntico/AutoPlots/blob/master/Images/StackedBarPlot.PNG?raw=true" align="center" width="800" />
<img src="https://github.com/AdrianAntico/AutoPlots/blob/master/Images/WordCloud.PNG?raw=true" align="center" width="800" />
<img src="https://github.com/AdrianAntico/AutoPlots/blob/master/Images/zz_CalibrationBox.PNG?raw=true" align="center" width="800" />
<img src="https://github.com/AdrianAntico/AutoPlots/blob/master/Images/zz_CalibrationLine.PNG?raw=true" align="center" width="800" />
<img src="https://github.com/AdrianAntico/AutoPlots/blob/master/Images/zz_ConfusionMatrix.PNG?raw=true" align="center" width="800" />
<img src="https://github.com/AdrianAntico/AutoPlots/blob/master/Images/zz_Gains.PNG?raw=true" align="center" width="800" />
<img src="https://github.com/AdrianAntico/AutoPlots/blob/master/Images/zz_Lift.PNG?raw=true" align="center" width="800" />
<img src="https://github.com/AdrianAntico/AutoPlots/blob/master/Images/zz_PartialDependenceBarPlot.PNG?raw=true" align="center" width="800" />
<img src="https://github.com/AdrianAntico/AutoPlots/blob/master/Images/zz_PartialDependenceLine.PNG?raw=true" align="center" width="800" />
<img src="https://github.com/AdrianAntico/AutoPlots/blob/master/Images/zz_ResidualHistogram.PNG?raw=true" align="center" width="800" />
<img src="https://github.com/AdrianAntico/AutoPlots/blob/master/Images/zz_ResidualScatter.PNG?raw=true" align="center" width="800" />
<img src="https://github.com/AdrianAntico/AutoPlots/blob/master/Images/zz_ROC.PNG?raw=true" align="center" width="800" />
<img src="https://github.com/AdrianAntico/AutoPlots/blob/master/Images/zz_ShapleyImportance.PNG?raw=true" align="center" width="800" />
<img src="https://github.com/AdrianAntico/AutoPlots/blob/master/Images/zz_VariableImportance.PNG?raw=true" align="center" width="800" />



### Histogram

```r
# Create fake data
data <- AutoPlots::FakeDataGenerator(N = 1000000)

# Build Histogram plot
AutoPlots::Plot.Histogram(
  dt = data,
  XVar = NULL,
  YVar = "Independent_Variable4",
  YVarTrans = "Identity",
  EchartsTheme = "macarons")
```

### Density

```r
# Create fake data
data <- AutoPlots::FakeDataGenerator(N = 1000000)

# Build Density plot
AutoPlots::Plot.Density(
  dt = data,
  XVar = NULL,
  YVar = "Independent_Variable4",
  YVarTrans = "Identity",
  EchartsTheme = "macarons")
```

### Box Plot

```r
# Create fake data
data <- AutoPlots::FakeDataGenerator(N = 1000000)

# Build Box plot
AutoPlots::Plot.Box(
  dt = data,
  XVar = "Factor_1",
  YVar = "Independent_Variable1",
  YVarTrans = "Identity",
  EchartsTheme = "macarons")
```

### Pie Plot

```r
# Create fake data
data <- AutoPlots::FakeDataGenerator(N = 1000000)

# Build Pie plot
AutoPlots::Plot.Pie(
  dt = data,
  XVar = "Factor_1",
  YVar = "Independent_Variable1",
  YVarTrans = "Identity",
  EchartsTheme = "macarons")
```

### Area Plot

```r
# Create fake data
data <- AutoPlots::FakeDataGenerator(N = 1000)

# Build Area plot
AutoPlots::Plot.Area(
  dt = data,
  PreAgg = FALSE,
  AggMethod = "mean",
  XVar = "DateTime",
  YVar = "Independent_Variable1",
  YVarTrans = "Identity",
  EchartsTheme = "macarons")
```

### Line Plot

```r
# Create fake data
data <- AutoPlots::FakeDataGenerator(N = 1000)

# Build Line plot
AutoPlots::Plot.Line(
  dt = data,
  PreAgg = FALSE,
  AggMethod = "mean",
  XVar = "DateTime",
  YVar = "Independent_Variable1",
  YVarTrans = "Identity",
  EchartsTheme = "macarons")
```

### Step Plot

```r
# Create fake data
data <- AutoPlots::FakeDataGenerator(N = 1000)

# Build Step plot
AutoPlots::Plot.Step(
  dt = data,
  PreAgg = FALSE,
  AggMethod = "mean",
  XVar = "DateTime",
  YVar = "Independent_Variable1",
  YVarTrans = "Identity",
  EchartsTheme = "macarons")
```

### River Plot

```r
# Create fake data
data <- AutoPlots::FakeDataGenerator(N = 1000)

# Build River plot
AutoPlots::Plot.River(
  dt = data,
  PreAgg = FALSE,
  AggMethod = "mean",
  XVar = "DateTime",
  YVar = c(
    "Independent_Variable1",
    "Independent_Variable2",
    "Independent_Variable3",
    "Independent_Variable4",
    "Independent_Variable5"),
  YVarTrans = "Identity",
  EchartsTheme = "macarons")
```

### Bar Plots

```r
# Create fake data
data <- AutoPlots::FakeDataGenerator(N = 100000)

# Echarts Bar Chart
AutoPlots::Plot.Bar(
  dt = data,
  PreAgg = FALSE,
  XVar = "Factor_1",
  YVar = "Adrian",
  YVarTrans = "Identity",
  EchartsTheme = "macarons")
```

### Stacked Bar Plots

```r
# Create fake data
data <- AutoPlots::FakeDataGenerator(N = 100000)

# Echarts Stacked Bar Chart
AutoPlots::Plot.StackedBar(
  dt = data,
  PreAgg = FALSE,
  XVar = "Factor_1",
  YVar = "Adrian",
  GroupVar = "Factor_2",
  YVarTrans = "Identity",
  EchartsTheme = "macarons")
```

### 3D Bar Plots

```r
# Create fake data
data <- AutoPlots::FakeDataGenerator(N = 100000)

# Echarts 3D Bar Chart
AutoPlots::Plot.BarPlot3D(
  dt = data,
  PreAgg = FALSE,
  XVar = "Factor_1",
  YVar = "Factor_2",
  ZVar = "Adrian",
  YVarTrans = "Identity",
  EchartsTheme = "macarons")
```

### Scatter Plot

```r
# Create fake data
data <- AutoPlots::FakeDataGenerator(N = 100000)

# Echarts Scatter Plot Chart
AutoPlots::Plot.Scatter(
  dt = data,
  SampleSize = 10000,
  XVar = "Adrian",
  YVar = "Independent_Variable8",
  YVarTrans = "Identity",
  EchartsTheme = "macarons")
```

### 3D Scatter Plot

```r
# Create fake data
data <- AutoPlots::FakeDataGenerator(N = 100000)

# Echarts Scatter Plot Chart
AutoPlots::Plot.Scatter3D(
  dt = data,
  SampleSize = 10000,
  XVar = "Adrian",
  YVar = "Independent_Variable8",
  ZVar = "Independent_Variable6",
  YVarTrans = "Identity",
  EchartsTheme = "macarons")
```

### Copula Plot

```r
# Create fake data
data <- AutoPlots::FakeDataGenerator(N = 100000)

# Echarts Copula Plot Chart
AutoPlots::Plot.Copula(
  dt = data,
  SampleSize = 10000,
  XVar = "Adrian",
  YVar = "Independent_Variable8",
  YVarTrans = "Identity",
  EchartsTheme = "macarons")
```

### 3D Copula Plot

```r
# Create fake data
data <- AutoPlots::FakeDataGenerator(N = 100000)

# Echarts Copula Plot Chart
AutoPlots::Plot.Copula3D(
  dt = data,
  SampleSize = 10000,
  XVar = "Adrian",
  YVar = "Independent_Variable9",
  ZVar = "Independent_Variable6",
  YVarTrans = "Identity",
  EchartsTheme = "macarons")
```

### Heatmap Plot

```r
# Create fake data
data <- AutoPlots::FakeDataGenerator(N = 100000)

# Echarts Heatmap Plot Chart
AutoPlots::Plot.HeatMap(
  dt = data,
  XVar = "Factor_1",
  YVar = "Factor_2",
  ZVar = "Independent_Variable6",
  YVarTrans = "Identity",
  EchartsTheme = "macarons")
```

### CorrMatrix Plot

```r
# Create fake data
data <- AutoPlots::FakeDataGenerator(N = 100000)

# Echarts CorrMatrix Plot Chart
AutoPlots::Plot.CorrMatrix(
  dt = data,
  CorrVars = c(
    "Adrian",
    "Independent_Variable1",
    "Independent_Variable2",
    "Independent_Variable3",
    "Independent_Variable4",
    "Independent_Variable5"),
  EchartsTheme = "macarons")
```
