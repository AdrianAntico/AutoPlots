![Version:1.0.0](https://img.shields.io/static/v1?label=Version&message=1.0.0&color=blue&?style=plastic)
[![metacran downloads](https://cranlogs.r-pkg.org/badges/last-week/AutoPlots)](https://cran.r-project.org/package=AutoPlots)


<img src="https://raw.githubusercontent.com/AdrianAntico/AutoPlots/master/inst/Logo2.PNG" align="center" width="800" />

<br> 

A simple api for visualizing Echarts. Plotting functions expose most Echarts customization options. See documentation for details.

<br> 

### Basic Plots
- Area
- Bar
- Box
- Copula
- Correlogram
- Density Plots
- Donut Plot
- Heatmaps
- Histogram Plots
- Line Plots
- Parallel
- Pie
- Probability
- Radar
- River
- Rosetype
- Scatter
- Stacked Bar
- Step
- 3D Bar
- 3D Scatter
- 3D Copula
- Word Cloud

### Modeling Plots
- Autocorrelation
- Calibration
- Calibration Scatter
- Confusion Matrix Heatmaps
- Gain
- Lift
- Partial Autocorrelation
- Partial Dependence
- Partial Dependence Heatmaps
- ROC
- Shapely Importance
- Variable Importance



## Installation

```r
install.packages("combinat")
install.packages("data.table")
install.packages("devtools")
install.packages("dplyr")
install.packages("e1071")
install.packages("echarts4r")
install.packages("lubridate")
install.packages("nortest")
install.packages("quanteda")
install.packages("quanteda.textstats")
install.packages("scales")
install.packages("stats")
install.packages("utils")
devtools::install_github("AdrianAntico/AutoPlots", upgrade = FALSE, force = TRUE)
```

<br>

## Code Examples


### Area Plot

<details><summary>Area Plot Examples</summary>

```r
# Create fake data
data <- AutoPlots::FakeDataGenerator(N = 1000)
data <- data[, .(
  IndepVar = mean(Independent_Variable8)
), by = c("DateTime")]

# Build plot
AutoPlots::Area(
  dt = data,
  XVar = "DateTime",
  YVar = "IndepVar",
  areaStyle.color = c("#80AAFF","#BDD5FF","#CFCFCF"),
  areaStyle.opacity = c(0.9,0.4,0.05),
  legend.show = FALSE)
```

<br>

<img src="https://raw.githubusercontent.com/AdrianAntico/AutoPlots/master/inst/AreaPlot.PNG" align="center" width="800" />

<br>

```r
# Create fake data
data <- AutoPlots::FakeDataGenerator(N = 1000)
data <- data[, .(
  IndepVar = mean(Independent_Variable8)
), by = c("DateTime", "Factor_1")]

# Build plot
ch <- as.character(sort(unique(data$Factor_1)))
plot_list <- lapply(ch, function(x) {
  AutoPlots::Area(
    dt = data[Factor_1 == x],
    XVar = "DateTime",
    YVar = "IndepVar",
    Height = "300px",
    title.text = paste0("Factor_1: ", x),
    areaStyle.color = c("#80AAFF","#BDD5FF","#CFCFCF"),
    areaStyle.opacity = c(0.9,0.4,0.05),
    legend.show = FALSE)
})

AutoPlots::display_plots_grid(
  plot_list,
  cols = 2
)
```

<br>

<img src="https://raw.githubusercontent.com/AdrianAntico/AutoPlots/master/inst/AreaPlot_grid.PNG" align="center" width="800" />

</details>

<br>


### Bar Plots

<details><summary>Bar Plot Examples</summary>

```r
# Create fake data
data <- AutoPlots::FakeDataGenerator(N = 1000)
data <- data[, .(
  IndepVar = mean(Independent_Variable8)
), by = c("DateTime")]

# Build plot
AutoPlots::Bar(
  dt = data,
  XVar = "DateTime",
  YVar = "IndepVar",
  backgroundStyle.color = c("#80AAFF","#BDD5FF","#CFCFCF"),
  backgroundStyle.opacity = c(0.9,0.4,0.05),
  legend.show = FALSE)
```

<br>

<img src="https://raw.githubusercontent.com/AdrianAntico/AutoPlots/master/inst/BarPlot.PNG" align="center" width="800" />

<br>

```r
# Create fake data
data <- AutoPlots::FakeDataGenerator(N = 1000)
data <- data[, .(
  IndepVar = mean(Independent_Variable8)
), by = c("DateTime", "Factor_1")]

# Build plot
ch <- as.character(sort(unique(data$Factor_1)))
plot_list <- lapply(ch, function(x) {
  AutoPlots::Bar(
    dt = data[Factor_1 == x],
    XVar = "DateTime",
    YVar = "IndepVar",
    Height = "300px",
    title.text = paste0("Factor_1: ", x),
    backgroundStyle.color = c("#80AAFF","#BDD5FF","#CFCFCF"),
    backgroundStyle.opacity = c(0.9,0.4,0.05),
    legend.show = FALSE)
})

AutoPlots::display_plots_grid(
  plot_list,
  cols = 2
)

```

<br>

<img src="https://raw.githubusercontent.com/AdrianAntico/AutoPlots/master/inst/BarPlot_grid.PNG" align="center" width="800" />

<br>


</details>

<br>


### Box Plots

<details><summary>Box Plot Examples</summary>

```r
# Create fake data
data <- AutoPlots::FakeDataGenerator(N = 1000)
data[1, Independent_Variable8 := 0.6]
data[2, Independent_Variable8 := 0.7]
data[3, Independent_Variable8 := 0.8]
data[4, Independent_Variable8 := 0.9]

# Build plot
AutoPlots::Box(
  dt = data,
  XVar = "Factor_1",
  YVar = "Independent_Variable8",
  yAxis.title = "IndepVar",
  itemStyle.color = c("red","#BDD5FF","blue"),
  itemStyle.opacity = c(0.9,0.4,0.05),
  legend.show = FALSE)
```

<br>

<img src="https://raw.githubusercontent.com/AdrianAntico/AutoPlots/master/inst/BoxPlot.PNG" align="center" width="800" />

<br>

```r
# Create fake data
data <- AutoPlots::FakeDataGenerator(N = 1000)
data[1, Independent_Variable8 := 0.6]
data[2, Independent_Variable8 := 0.7]
data[3, Independent_Variable8 := 0.8]
data[4, Independent_Variable8 := 0.9]

# Build plot
ch <- c("Factor_1", "Factor_2")
plot_list <- lapply(ch, function(x) {
  AutoPlots::Box(
    dt = data,
    XVar = x,
    YVar = "Independent_Variable8",
    Height = "300px",
    title.text = paste0("Factor_1: ", x),
    itemStyle.color = c("red","#BDD5FF","blue"),
    itemStyle.opacity = c(0.9,0.4,0.05),
    legend.show = FALSE)
})

AutoPlots::display_plots_grid(
  plot_list,
  cols = 1
)

```

<br>

<img src="https://raw.githubusercontent.com/AdrianAntico/AutoPlots/master/inst/BoxPlot_grid.PNG" align="center" width="800" />

<br>


</details>

<br>
