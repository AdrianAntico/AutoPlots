![Version:1.5.0](https://img.shields.io/static/v1?label=Version&message=1.5.0&color=blue&?style=plastic)


<img src="https://raw.githubusercontent.com/AdrianAntico/AutoPlots/master/inst/Logo2.PNG" align="center" width="800" />

<br> 

## 🚀 AutoPlots

AutoPlots is a comprehensive R visualization toolkit built on top of echarts4r, providing a consistent, ultra-customizable API across dozens of chart types — from classical analytics plots to advanced modeling visualizations and 3D charts.

AutoPlots exposes the full power of ECharts.js without requiring users to write JavaScript.
If a feature exists in ECharts, AutoPlots aims to make it available.

## ⭐ Why AutoPlots?

30+ high-quality plot types — all with consistent argument naming

Full access to ECharts options using the xxx.* naming convention

No JavaScript required

Shiny-ready out of the box

Supports multi-plot layouts via display_plots_grid()

3D visualizations, modeling plots, river charts, radar, parallel, contour, density, and more

Designed for data science, EDA, dashboards, and model evaluation


## 🔍 Philosophy

AutoPlots is built around these principles:

Every plot is a standalone object
Modify, tweak, and filter each one independently.

Composition happens outside the plot
Use display_plots_grid() to arrange multiple plots into dashboards.

Expose full ECharts power without JS
Legend, toolbox, tooltip, axis, series, grid, animation, theme — everything is adjustable.

Beautiful defaults, infinite customization
Beginners get great plots immediately; experts can tune everything.

<br>

## Installation

```r
# Install CRAN dependencies
install.packages(c(
  "combinat", "data.table", "devtools", "dplyr", "e1071", "echarts4r",
  "lubridate", "nortest", "quanteda", "quanteda.textstats", "scales"
))

# Install AutoPlots from GitHub
devtools::install_github("AdrianAntico/AutoPlots", upgrade = FALSE, force = TRUE)
```

<br>

## Quick Start
```r
library(AutoPlots)
dt <- data.table::data.table(x = rnorm(1000))

AutoPlots::Density(
  dt,
  XVar = "x",
  legend.show = FALSE
)

```

<br> 

### Basic Plots
- Area
- Bar
- Box
- Copula
- Correlogram
- Density
- Donut
- Heatmaps
- Histogram
- Line
- Parallel
- Pie
- Radar
- River
- Rosetype
- Scatter
- Stacked Bar
- Step
- Treemap
- 3D Bar
- 3D Scatter
- 3D Copula
- Word Cloud

### Modeling Plots
- Autocorrelation
- Calibration
- Calibration Scatter
- Confusion Matrix Heatmap
- Gain
- Lift
- Partial Autocorrelation
- Partial Dependence
- Partial Dependence Heatmap
- Probability
- ROC
- Shapely Importance
- Variable Importance





<br>

## 📚 Plot Gallery

Below are the full set of examples for standard plots.


### 📐 Area Plots

<details><summary>Area Plot Examples</summary>

```r
# Create example data (no FakeDataGenerator)
data <- data.table::data.table(
  DateTime = seq.Date(
    from = Sys.Date() - 89,
    to   = Sys.Date(),
    by   = "day"
  )
)

# Smooth wave + a bit of noise for a nice area shape
data[, IndepVar := 50 +
       30 * sin(seq(0, 2 * pi, length.out = .N)) +
       stats::rnorm(.N, mean = 0, sd = 3)]

# Optional: ensure values stay positive for cleaner visuals
data[IndepVar < 0, IndepVar := 0]

# Build plot
AutoPlots::Area(
  dt = data,
  XVar = "DateTime",
  YVar = "IndepVar",
  areaStyle.color = c("#80AAFF","#BDD5FF","#8B008B"),
  areaStyle.opacity = c(1,0.6,0.05),
  legend.show = FALSE,
  title.textStyle.textShadowColor = "purple",
  title.textStyle.textShadowBlur = 4,
  title.textStyle.textShadowOffsetX = 2,
  title.textStyle.textShadowOffsetY = 1,
  yAxis.nameTextStyle.fontSize = 20,
  yAxis.nameTextStyle.padding = 60,
  xAxis.nameTextStyle.fontSize = 20,
  tooltip.backgroundColor = "#80AAFFAA",
  tooltip.textStyle.color = "black"
)

```

<br>

<img src="https://raw.githubusercontent.com/AdrianAntico/AutoPlots/master/inst/AreaPlot.PNG" align="center" width="800" />

<br>
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


### 📊 Bar Plots

<details><summary>Bar Plot Examples</summary>

```r
# Create example data (no FakeDataGenerator)
data <- data.table::data.table(
  DateTime = seq.Date(
    from = Sys.Date() - 89,
    to   = Sys.Date(),
    by   = "day"
  )
)

# Smooth wave + a bit of noise for a nice area shape
data[, IndepVar := 50 +
       30 * sin(seq(0, 2 * pi, length.out = .N)) +
       stats::rnorm(.N, mean = 0, sd = 3)]

# Optional: ensure values stay positive for cleaner visuals
data[IndepVar < 0, IndepVar := 0]

# Build plot
AutoPlots::Bar(
  dt = data,
  XVar = "DateTime",
  YVar = "IndepVar",
  backgroundStyle.color = c("#80AAFF","#BDD5FF","#8B008B"),
  backgroundStyle.opacity = c(1,0.6,0.05),
  legend.show = FALSE,
  title.textStyle.textShadowColor = "purple",
  title.textStyle.textShadowBlur = 4,
  title.textStyle.textShadowOffsetX = 2,
  title.textStyle.textShadowOffsetY = 1,
  yAxis.nameTextStyle.fontSize = 20,
  yAxis.nameTextStyle.padding = 60,
  xAxis.nameTextStyle.fontSize = 20,
  tooltip.backgroundColor = "#80AAFFAA",
  tooltip.textStyle.color = "black",
  title.text = "Bar Plot"
)
```

<br>

<img src="https://raw.githubusercontent.com/AdrianAntico/AutoPlots/master/inst/BarPlot.PNG" align="center" width="800" />

<br>
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


### 📦 Box Plots

<details><summary>Box Plot Examples</summary>

```r
# Create boxplot demo data with intentional outliers
set.seed(123)
n_per_group <- 60

data <- data.table::data.table(
  Factor_1 = factor(rep(c("Group A", "Group B", "Group C", "Group D"), each = n_per_group))
)

# Base distributions
data[, Independent_Variable8 := c(
  rnorm(n_per_group, mean = 40, sd = 4),
  rnorm(n_per_group, mean = 55, sd = 5),
  rnorm(n_per_group, mean = 70, sd = 6),
  rnorm(n_per_group, mean = 85, sd = 7)
)]

# Add controlled outliers per group
outliers <- data.table::data.table(
  Factor_1 = factor(c(
    rep("Group A", 6),
    rep("Group B", 7),
    rep("Group C", 8),
    rep("Group D", 6)
  ), levels = levels(data$Factor_1)),
  Independent_Variable8 = c(
    rnorm(6, mean = 25, sd = 2),   # A: low outliers
    rnorm(7, mean = 45, sd = 3),   # B: low-ish outliers
    rnorm(8, mean = 100, sd = 3),  # C: high outliers
    rnorm(6, mean = 115, sd = 4)   # D: high outliers
  )
)

data <- rbind(data, outliers)

# Safety: no negatives
data[Independent_Variable8 < 0, Independent_Variable8 := 0]

# Final box plot
AutoPlots::Box(
  dt = data,
  XVar = "Factor_1",
  YVar = "Independent_Variable8",
  yAxis.title = "IndepVar",
  itemStyle.color = c("#80AAFF", "#BDD5FF", "#8B008B"),
  itemStyle.opacity = c(0.98, 0.85, 0.55),
  legend.show = FALSE,
  tooltip.backgroundColor = "#80AAFFDD",
  tooltip.textStyle.color = "black",
  yAxis.nameTextStyle.fontSize = 20,
  yAxis.nameTextStyle.padding = 60,
  yAxis.axisLabel.color = "#CCCCCC",
  xAxis.nameTextStyle.fontSize = 20,
  xAxis.axisLabel.color = "#CCCCCC",
  xAxis.axisLabel.fontSize = 14,
  title.textStyle.textShadowColor = "purple",
  title.textStyle.textShadowBlur = 4,
  title.textStyle.textShadowOffsetX = 2,
  title.textStyle.textShadowOffsetY = 1
)

```

<br>

<img src="https://raw.githubusercontent.com/AdrianAntico/AutoPlots/master/inst/BoxPlot.PNG" align="center" width="800" />

<br>
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


### 🍥 Copula

<details><summary>Copula Plot Examples</summary>

```r
# Create correlated data
n   <- 1000        # rows
p   <- 4           # number of variables
rho <- 0.8         # correlation between neighbors (AR(1))
mu  <- c(50, 100, 0, 10)     # means (length p)
sds <- c(10, 5, 3, 1)        # standard deviations (length p)
Sigma_cor <- outer(1:p, 1:p, \(i, j) rho^abs(i - j))   # AR(1) correlation matrix
L <- chol(Sigma_cor)                                   # Cholesky factor (upper-tri)
Z <- matrix(rnorm(n * p), nrow = n, ncol = p)          # iid N(0,1)
X_cor <- Z %*% L                                       # correlated, unit variance
X <- X_cor %*% diag(sds)                               # set std devs
X <- sweep(X, 2, mu, `+`)                              # set means
dt <- data.table::as.data.table(X)
data.table::setnames(dt, paste0("x", seq_len(p)))

# Build plot
p1 <- AutoPlots::Copula(
  dt = dt,
  XVar = "x1",
  YVar = "x2",
  legend.show = FALSE
)
```

<br>

<img src="https://raw.githubusercontent.com/AdrianAntico/AutoPlots/master/inst/CopulaPlot.PNG" align="center" width="800" />

<br>
<br>

```r
# Create correlated data
n   <- 1000        # rows
p   <- 4           # number of variables
rho <- 0.8         # correlation between neighbors (AR(1))
mu  <- c(50, 100, 0, 10)     # means (length p)
sds <- c(10, 5, 3, 1)        # standard deviations (length p)
Sigma_cor <- outer(1:p, 1:p, \(i, j) rho^abs(i - j))   # AR(1) correlation matrix
L <- chol(Sigma_cor)                                   # Cholesky factor (upper-tri)
Z <- matrix(rnorm(n * p), nrow = n, ncol = p)          # iid N(0,1)
X_cor <- Z %*% L                                       # correlated, unit variance
X <- X_cor %*% diag(sds)                               # set std devs
X <- sweep(X, 2, mu, `+`)                              # set means
dt <- data.table::as.data.table(X)
data.table::setnames(dt, paste0("x", seq_len(p)))

# Build plot
p1 <- AutoPlots::Copula(
  dt = dt,
  XVar = "x1",
  YVar = "x2",
  title.text = "x1 vs. x2",
  legend.show = FALSE
)

# Build plot
p2 <- AutoPlots::Copula(
  dt = dt,
  XVar = "x1",
  YVar = "x3",
  title.text = "x1 vs. x3",
  legend.show = FALSE
)

# Build plot
p3 <- AutoPlots::Copula(
  dt = dt,
  XVar = "x2",
  YVar = "x3",
  title.text = "x2 vs. x3",
  legend.show = FALSE
)

# Build plot
p4 <- AutoPlots::Copula(
  dt = dt,
  XVar = "x3",
  YVar = "x4",
  title.text = "x3 vs. x4",
  legend.show = FALSE
)

AutoPlots::display_plots_grid(
  list(p1,p2,p3,p4),
  cols = 2
)
```

<br>

<img src="https://raw.githubusercontent.com/AdrianAntico/AutoPlots/master/inst/CopulaPlot_grid.PNG" align="center" width="800" />

<br>


</details>

<br>



### 📈 Correlogram

<details><summary>Correlogram Examples</summary>

```r
n   <- 1000        # rows
p   <- 4           # number of variables
rho <- 0.8         # correlation between neighbors (AR(1))
mu  <- c(50, 100, 0, 10)     # means (length p)
sds <- c(10, 5, 3, 1)        # standard deviations (length p)
Sigma_cor <- outer(1:p, 1:p, \(i, j) rho^abs(i - j))   # AR(1) correlation matrix
L <- chol(Sigma_cor)                                   # Cholesky factor (upper-tri)
Z <- matrix(rnorm(n * p), nrow = n, ncol = p)          # iid N(0,1)
X_cor <- Z %*% L                                       # correlated, unit variance
X <- X_cor %*% diag(sds)                               # set std devs
X <- sweep(X, 2, mu, `+`)                              # set means
dt <- data.table::as.data.table(X)
data.table::setnames(dt, paste0("x", seq_len(p)))

# Build Plot
AutoPlots::CorrMatrix(
  dt = dt,
  PreAgg = FALSE,
  Method = "Spearman",
  CorrVars = c("x1","x2","x3","x4"),
  ShowLabels = TRUE,
  visualMap.InRange.color = c("white", "gray", "darkblue")
)
```

<br>

<img src="https://raw.githubusercontent.com/AdrianAntico/AutoPlots/master/inst/CorrelogramPlot.PNG" align="center" width="800" />

<br>
<br>

```r
n   <- 1000        # rows
p   <- 4           # number of variables
rho <- 0.8         # correlation between neighbors (AR(1))
mu  <- c(50, 100, 0, 10)     # means (length p)
sds <- c(10, 5, 3, 1)        # standard deviations (length p)
Sigma_cor <- outer(1:p, 1:p, \(i, j) rho^abs(i - j))   # AR(1) correlation matrix
L <- chol(Sigma_cor)                                   # Cholesky factor (upper-tri)
Z <- matrix(rnorm(n * p), nrow = n, ncol = p)          # iid N(0,1)
X_cor <- Z %*% L                                       # correlated, unit variance
X <- X_cor %*% diag(sds)                               # set std devs
X <- sweep(X, 2, mu, `+`)                              # set means
dt <- data.table::as.data.table(X)
data.table::setnames(dt, paste0("x", seq_len(p)))

# Build Plot
p1 <- AutoPlots::CorrMatrix(
  dt = dt,
  PreAgg = FALSE,
  Method = "Spearman",
  CorrVars = c("x1","x2","x3","x4"),
  ShowLabels = TRUE,
  visualMap.InRange.color = c("white", "gray", "darkblue"),
  title.text = "Spearman Correlation"
)

p2 <- AutoPlots::CorrMatrix(
  dt = dt,
  PreAgg = FALSE,
  Method = "Pearson",
  CorrVars = c("x1","x2","x3","x4"),
  ShowLabels = TRUE,
  visualMap.InRange.color = c("white", "gray", "darkgreen"),
  title.text = "Pearson Correlation"
)

p3 <- AutoPlots::CorrMatrix(
  dt = dt,
  PreAgg = FALSE,
  Method = "Kendall",
  CorrVars = c("x1","x2","x3","x4"),
  ShowLabels = TRUE,
  visualMap.InRange.color = c("white", "gray", "darkred"),
  title.text = "Kendall's Tau Correlation"
)

AutoPlots::display_plots_grid(
  list(p1,p2,p3),
  cols = 2
)
```

<br>

<img src="https://raw.githubusercontent.com/AdrianAntico/AutoPlots/master/inst/CorrelogramPlot_grid.PNG" align="center" width="800" />

<br>


</details>

<br>



### 🌡 Density

<details><summary>Density Plot Examples</summary>

```r
# Create fake data
data <- data.table::data.table(IndepVar = rnorm(1000, mean = 0, sd = 1))

# Build plot
AutoPlots::Density(
  dt = data,
  XVar = "IndepVar",
  areaStyle.color = c("#697AFF","#A1ADFF","#D9DEFF"),
  areaStyle.opacity = c(0.9,0.4,0.05),
  legend.show = FALSE
)
```

<br>

<img src="https://raw.githubusercontent.com/AdrianAntico/AutoPlots/master/inst/Density.PNG" align="center" width="800" />

<br>
<br>

```r
# Create fake data
data <- data.table::data.table(IndepVar = rnorm(1000, mean = 5, sd = 2))
data[, IndepVar2 := rnorm(1000, mean = 10, sd = 5)]
data[, IndepVar3 := rnorm(1000, mean = 10, sd = 15)]
data[, IndepVar4 := rnorm(1000, mean = 10, sd = 20)]

# Build Plots
p1 <- AutoPlots::Density(
  dt = data,
  XVar = "IndepVar",
  title.text = "IndepVar",
  areaStyle.color = c("#697AFF","#A1ADFF","#D9DEFF"),
  areaStyle.opacity = c(0.9,0.4,0.05),
  legend.show = FALSE
)

p2 <- AutoPlots::Density(
  dt = data,
  XVar = "IndepVar2",
  title.text = "IndepVar2",
  areaStyle.color = c("#697AFF","#A1ADFF","#D9DEFF"),
  areaStyle.opacity = c(0.9,0.4,0.05),
  legend.show = FALSE
)

p3 <- AutoPlots::Density(
  dt = data,
  XVar = "IndepVar3",
  title.text = "IndepVar3",
  areaStyle.color = c("#697AFF","#A1ADFF","#D9DEFF"),
  areaStyle.opacity = c(0.9,0.4,0.05),
  legend.show = FALSE
)

p4 <- AutoPlots::Density(
  dt = data,
  XVar = "IndepVar4",
  title.text = "IndepVar4",
  areaStyle.color = c("#697AFF","#A1ADFF","#D9DEFF"),
  areaStyle.opacity = c(0.9,0.4,0.05),
  legend.show = FALSE
)

AutoPlots::display_plots_grid(
  list(p1,p2,p3,p4),
  cols = 2
)
```

<br>

<img src="https://raw.githubusercontent.com/AdrianAntico/AutoPlots/master/inst/Density_grid.PNG" align="center" width="800" />

<br>


</details>

<br>


### 🍩 Donut

<details><summary>Donut Plot Examples</summary>

```r
# Create fake data
data <- AutoPlots::FakeDataGenerator(N = 1000)
data <- data[, .(
  IndepVar = round(mean(Independent_Variable8), 3)
), by = c("Factor_1")]

# Build Plot
AutoPlots::Donut(
  dt = data,
  XVar = "Factor_1",
  YVar = "IndepVar"
)
```

<br>

<img src="https://raw.githubusercontent.com/AdrianAntico/AutoPlots/master/inst/DonutPlot.PNG" align="center" width="800" />

<br>
<br>

```r
# Create fake data
data <- AutoPlots::FakeDataGenerator(N = 1000)
dt1 <- data[, .(
  IndepVar = round(mean(Independent_Variable8), 3)
), by = c("Factor_1")]
dt2 <- data[, .(
  IndepVar = round(mean(Independent_Variable8), 3)
), by = c("Factor_2")]

# Build Plots
p1 <- AutoPlots::Donut(
  dt = dt1,
  XVar = "Factor_1",
  YVar = "IndepVar",
  title.text = "Factor_1"
)

p2 <- AutoPlots::Donut(
  dt = dt2,
  XVar = "Factor_2",
  YVar = "IndepVar",
  title.text = "Factor_2"
)

AutoPlots::display_plots_grid(
  list(p1, p2),
  cols = 1
)
```

<br>

<img src="https://raw.githubusercontent.com/AdrianAntico/AutoPlots/master/inst/DonutPlot_grid.PNG" align="center" width="800" />

<br>


</details>

<br>



### 🔥 Heatmap

<details><summary>Heatmap Plot Examples</summary>

```r
# Create fake data
data <- AutoPlots::FakeDataGenerator(N = 100000, Correlation = 0.1)
data <- data[, .(
  IndepVar = round(mean(Independent_Variable8), 3)
), by = c("Factor_1", "Factor_2")]

# Build Plots
AutoPlots::HeatMap(
  dt = data,
  XVar = "Factor_1",
  YVar = "Factor_2",
  ZVar = "IndepVar",
  title.text = "V1",
  label.show = TRUE,
  label.fontWeight = "bolder",
  emphasis.shadowColor = "white",
  emphasis.shadowBlur = 10, 
  visualMap.InRange.color = c("blue", "white", "red")
)
```

<br>

<img src="https://raw.githubusercontent.com/AdrianAntico/AutoPlots/master/inst/Heatmap.PNG" align="center" width="800" />

<br>
<br>

```r
# Create fake data
data <- AutoPlots::FakeDataGenerator(N = 100000, Correlation = 0.1)
data <- data[, .(
  IndepVar = round(mean(Independent_Variable8), 3)
), by = c("Factor_1", "Factor_2")]

# Build Plots
p1 <- AutoPlots::HeatMap(
  dt = data,
  XVar = "Factor_1",
  YVar = "Factor_2",
  ZVar = "IndepVar",
  title.text = "V1",
  label.show = TRUE,
  label.fontWeight = "bolder",
  emphasis.shadowColor = "white",
  emphasis.shadowBlur = 10, 
  visualMap.InRange.color = c("blue", "white", "red")
)

# Create fake data
data <- AutoPlots::FakeDataGenerator(N = 100000, Correlation = 0.1)
data <- data[, .(
  IndepVar = round(mean(Independent_Variable8), 3)
), by = c("Factor_1", "Factor_2")]

# Build Plots
p2 <- AutoPlots::HeatMap(
  dt = data,
  XVar = "Factor_1",
  YVar = "Factor_2",
  ZVar = "IndepVar",
  title.text = "V2",
  label.show = TRUE,
  label.fontWeight = "bolder",
  emphasis.shadowColor = "white",
  emphasis.shadowBlur = 10, 
  visualMap.InRange.color = c("green", "white", "orange")
)

AutoPlots::display_plots_grid(
  list(p1, p2),
  cols = 1
)

```

<br>

<img src="https://raw.githubusercontent.com/AdrianAntico/AutoPlots/master/inst/Heatmap_grid.PNG" align="center" width="800" />

<br>


</details>

<br>


### 🧱 Histogram

<details><summary>Histogram Examples</summary>

```r
# Create fake data
data <- data.table::data.table(Variable = rnorm(n = 1000, mean = 5, sd = 2))

# Build Plots
AutoPlots::Histogram(
  dt = data,
  XVar = "Variable",
  backgroundStyle.color = c("lightblue","darkblue","blue","blue"),
  backgroundStyle.opacity = c(0.9,0.7,0.6,0.05),
  legend.show = FALSE
)
```

<br>

<img src="https://raw.githubusercontent.com/AdrianAntico/AutoPlots/master/inst/Histogram.PNG" align="center" width="800" />

<br>
<br>

```r
# Create fake data
data <- data.table::data.table(Variable = rnorm(n = 1000, mean = 5, sd = 2))
data[, Variable2 := rnorm(n = 1000, mean = 5, sd = 5)]
data[, Variable3 := rnorm(n = 1000, mean = 5, sd = 10)]
data[, Variable4 := rnorm(n = 1000, mean = 5, sd = 20)]

# Build Plots
p1 <- AutoPlots::Histogram(
  dt = data,
  XVar = "Variable",
  title.text = "Var",
  backgroundStyle.color = c("lightblue","darkblue","blue","blue"),
  backgroundStyle.opacity = c(0.9,0.7,0.6,0.05),
  legend.show = FALSE
)

p2 <- AutoPlots::Histogram(
  dt = data,
  XVar = "Variable2",
  title.text = "Var2",
  backgroundStyle.color = c("lightblue","darkblue","blue","blue"),
  backgroundStyle.opacity = c(0.9,0.7,0.6,0.05),
  legend.show = FALSE
)

p3 <- AutoPlots::Histogram(
  dt = data,
  XVar = "Variable3",
  title.text = "Var3",
  backgroundStyle.color = c("lightblue","darkblue","blue","blue"),
  backgroundStyle.opacity = c(0.9,0.7,0.6,0.05),
  legend.show = FALSE
)

p4 <- AutoPlots::Histogram(
  dt = data,
  XVar = "Variable4",
  title.text = "Var4",
  backgroundStyle.color = c("lightblue","darkblue","blue","blue"),
  backgroundStyle.opacity = c(0.9,0.7,0.6,0.05),
  legend.show = FALSE
)
  
AutoPlots::display_plots_grid(
  list(p1,p2,p3,p4),
  cols = 2
)
```

<br>

<img src="https://raw.githubusercontent.com/AdrianAntico/AutoPlots/master/inst/Histogram_grid.PNG" align="center" width="800" />

<br>


</details>

<br>


### 💡 Line

<details><summary>Line Plot Examples</summary>

```r
# Create data
data <- AutoPlots::FakeDataGenerator(N = 1000, AddDate = T)
data <- data[, .(
  IndepVar = mean(Independent_Variable8)
), by = c("DateTime")]

# Build plot
AutoPlots::Line(
  dt = data,
  XVar = "DateTime",
  YVar = "IndepVar",
  title.text = "IndepVar",
  lineStyle.color = c("red","#3848FF","blue"),
  legend.show = FALSE
)
```

<br>

<img src="https://raw.githubusercontent.com/AdrianAntico/AutoPlots/master/inst/LinePlot.PNG" align="center" width="800" />

<br>
<br>

```r
# Create data
data <- AutoPlots::FakeDataGenerator(N = 1000, AddDate = T)
data <- data[, .(
  IndepVar = mean(Independent_Variable8),
  IndepVar2 = mean(Independent_Variable7),
  IndepVar3 = mean(Independent_Variable6),
  IndepVar4 = mean(Independent_Variable5)
), by = c("DateTime")]

# Build plot
p1 <- AutoPlots::Line(
  dt = data,
  XVar = "DateTime",
  YVar = "IndepVar",
  title.text = "IndepVar",
  lineStyle.color = c("red","#3848FF","blue"),
  legend.show = FALSE
)

p2 <- AutoPlots::Line(
  dt = data,
  XVar = "DateTime",
  YVar = "IndepVar2",
  title.text = "IndepVar2",
  lineStyle.color = c("red","#3848FF","blue"),
  legend.show = FALSE
)

p3 <- AutoPlots::Line(
  dt = data,
  XVar = "DateTime",
  YVar = "IndepVar3",
  title.text = "IndepVar3",
  lineStyle.color = c("red","#3848FF","blue"),
  legend.show = FALSE
)

p4 <- AutoPlots::Line(
  dt = data,
  XVar = "DateTime",
  YVar = "IndepVar4",
  title.text = "IndepVar4",
  lineStyle.color = c("red","#3848FF","blue"),
  legend.show = FALSE
)

AutoPlots::display_plots_grid(
  list(p1,p2,p3,p4),
  cols = 2
)
```

<br>

<img src="https://raw.githubusercontent.com/AdrianAntico/AutoPlots/master/inst/LinePlot_grid.PNG" align="center" width="800" />

<br>


</details>

<br>


### 🧭 Parallel

<details><summary>Parallel Plot Examples</summary>

```r
# Create data
n   <- 1000        # rows
p   <- 8           # number of variables
rho <- 0.8         # correlation between neighbors (AR(1))
mu  <- c(50, 100, 0, 10,1,2,3,4)     # means (length p)
sds <- c(10, 5, 3, 1, 1,2,3,4)        # standard deviations (length p)
Sigma_cor <- outer(1:p, 1:p, \(i, j) rho^abs(i - j))   # AR(1) correlation matrix
L <- chol(Sigma_cor)                                   # Cholesky factor (upper-tri)
Z <- matrix(rnorm(n * p), nrow = n, ncol = p)          # iid N(0,1)
X_cor <- Z %*% L                                       # correlated, unit variance
X <- X_cor %*% diag(sds)                               # set std devs
X <- sweep(X, 2, mu, `+`)                              # set means
dt <- data.table::as.data.table(X)
data.table::setnames(dt, paste0("x", seq_len(p)))
data.table::setnames(dt, "x1", "bl bl bl")

# Build Plot
AutoPlots::Parallel(
  dt = dt,
  CorrVars = names(dt),
  ShowLabels = T,
  lineStyle.color = "#00C7FF",
  lineStyle.width = 0.2
)
```

<br>

<img src="https://raw.githubusercontent.com/AdrianAntico/AutoPlots/master/inst/ParallelPlot.PNG" align="center" width="800" />

<br>
<br>

```r
# Create data
data <- AutoPlots::FakeDataGenerator(N = 1000, ID = 0)
data.table::setnames(
  data,
  paste0("Independent_Variable", 1:8),
  paste0("x", 1:8)
)

# Build Plots
group_vars <- sort(as.character(unique(data$Factor_1)))[1:4]
plot_list <- lapply(group_vars, function(x) {
  AutoPlots::Parallel(
    dt = data[Factor_1 == x],
    CorrVars = paste0("x", 1:8),
    ShowLabels = T,
    lineStyle.color = "#00C7FF",
    lineStyle.width = 0.2,
    title.text = x
  )
})

AutoPlots::display_plots_grid(
  plot_list,
  cols = 2
)
```

<br>

<img src="https://raw.githubusercontent.com/AdrianAntico/AutoPlots/master/inst/ParallelPlot_grid.PNG" align="center" width="800" />

<br>


</details>

<br>



### 🥧 Pie

<details><summary>Pie Plot Examples</summary>

```r
# Create fake data
data <- AutoPlots::FakeDataGenerator(N = 1000)
data <- data[, .(
  IndepVar = round(mean(Independent_Variable8), 3)
), by = c("Factor_1")]

# Build Plot
AutoPlots::Pie(
  dt = data,
  XVar = "Factor_1",
  YVar = "IndepVar",
  ShowLabels = T)
```

<br>

<img src="https://raw.githubusercontent.com/AdrianAntico/AutoPlots/master/inst/Pie.PNG" align="center" width="800" />

<br>
<br>

```r
# Create fake data
data <- AutoPlots::FakeDataGenerator(N = 1000)
dt1 <- data[, .(
  IndepVar = round(mean(Independent_Variable8), 3)
), by = c("Factor_1")]
dt2 <- data[, .(
  IndepVar = round(mean(Independent_Variable8), 3)
), by = c("Factor_2")]

# Build Plots
p1 <- AutoPlots::Pie(
  dt = dt1,
  XVar = "Factor_1",
  YVar = "IndepVar",
  title.text = "Factor_1",
  ShowLabels = T)

p2 <- AutoPlots::Pie(
  dt = dt2,
  XVar = "Factor_2",
  YVar = "IndepVar",
  title.text = "Factor_2",
  ShowLabels = T)

AutoPlots::display_plots_grid(
  list(p1, p2),
  cols = 1
)
```

<br>

<img src="https://raw.githubusercontent.com/AdrianAntico/AutoPlots/master/inst/Pie_grid.PNG" align="center" width="800" />

<br>


</details>

<br>


### 🎯 Radar

<details><summary>Radar Plot Examples</summary>

```r
# Create Data
dt <- data.table::data.table(
  Y1 = pnorm(q = runif(10)),
  Y2 = pnorm(q = runif(10)),
  Y3 = pnorm(q = runif(10)),
  GV = sample(LETTERS[1:10], 10, TRUE))

# Create plot
AutoPlots::Radar(
  dt = dt,
  AggMethod = "mean",
  ShowLabels = TRUE,
  PreAgg = FALSE,
  YVar = c("Y1","Y2","Y3"),
  GroupVar = "GV",
  lineStyle.color = c("#00BFFF", "#FF69B4", "#32CD32"))
```

<br>

<img src="https://raw.githubusercontent.com/AdrianAntico/AutoPlots/master/inst/Radar.PNG" align="center" width="800" />

<br>
<br>

```r
# Create Data
dt <- data.table::data.table(
  Y1 = pnorm(q = runif(10)),
  Y2 = pnorm(q = runif(10)),
  Y3 = pnorm(q = runif(10)),
  GV = sample(LETTERS[1:10], 10, TRUE))

dt2 <- data.table::data.table(
  Y1 = pnorm(q = runif(10)),
  Y2 = pnorm(q = runif(10)),
  Y3 = pnorm(q = runif(10)),
  GV = sample(LETTERS[11:20], 10, TRUE))

# Create plot
p1 <- AutoPlots::Radar(
  dt = dt,
  AggMethod = "mean",
  ShowLabels = TRUE,
  PreAgg = FALSE,
  YVar = c("Y1","Y2","Y3"),
  GroupVar = "GV",
  title.text = "Data 1",
  lineStyle.color = c("#00BFFF", "#FF69B4", "#32CD32"))

p2 <- AutoPlots::Radar(
  dt = dt2,
  AggMethod = "mean",
  ShowLabels = TRUE,
  PreAgg = FALSE,
  YVar = c("Y1","Y2","Y3"),
  GroupVar = "GV",
  title.text = "Data 2",
  lineStyle.color = c("#00BFFF", "#FF69B4", "#32CD32"))

AutoPlots::display_plots_grid(
  list(p1,p2),
  cols = 1
)
```

<br>

<img src="https://raw.githubusercontent.com/AdrianAntico/AutoPlots/master/inst/Radar_grid.PNG" align="center" width="800" />

<br>


</details>

<br>



### 🌊 River

<details><summary>River Plot Examples</summary>

```r
# Create fake data
dates <- seq.Date(Sys.Date() - 30, Sys.Date(), by = "day")
grps <- lapply(LETTERS[1:5], rep, 31) |> unlist()
dt <- data.table::data.table(
  dates = rep(dates, 5),
  groups = grps,
  values = runif(length(grps), 1, 50)
)

# Build Plot
AutoPlots::River(
  dt = dt,
  PreAgg = TRUE,
  XVar = "dates",
  YVar = "values",
  GroupVar = "groups",
  legend.orient = "horizontal",
  itemStyle.color = c("#FF4C4C", "#00BFFF", "#FFD700", "#32CD32", "#FF69B4")
  )
```

<br>

<img src="https://raw.githubusercontent.com/AdrianAntico/AutoPlots/master/inst/RiverPlot.PNG" align="center" width="800" />

<br>
<br>

```r
# Create fake data
dates <- seq.Date(Sys.Date() - 30, Sys.Date(), by = "day")
grps <- lapply(LETTERS[1:5], rep, 31) |> unlist()
dt1 <- data.table::data.table(
  dates = rep(dates, 5),
  groups = grps,
  values = runif(length(grps), 1, 50)
)

dates <- seq.Date(Sys.Date() - 30, Sys.Date(), by = "day")
grps <- lapply(LETTERS[6:10], rep, 31) |> unlist()
dt2 <- data.table::data.table(
  dates = rep(dates, 5),
  groups = grps,
  values = runif(length(grps), 1, 50)
)
+-
# Build Plot
p1 <- AutoPlots::River(
  dt = dt1,
  PreAgg = TRUE,
  XVar = "dates",
  YVar = "values",
  GroupVar = "groups",
  title.text = "Data 1",
  legend.orient = "horizontal",
  itemStyle.color = c("#FF4C4C", "#00BFFF", "#FFD700", "#32CD32", "#FF69B4")
  )

p2 <- AutoPlots::River(
  dt = dt2,
  PreAgg = TRUE,
  XVar = "dates",
  YVar = "values",
  GroupVar = "groups"
  title.text = "Data 2",
  legend.orient = "horizontal",
  itemStyle.color = c("#00FFFF", "#FF7F50", "#8A2BE2", "#00FF7F", "#FF4500"))

AutoPlots::display_plots_grid(
  list(p1, p2),
  cols = 1
)
```

<br>

<img src="https://raw.githubusercontent.com/AdrianAntico/AutoPlots/master/inst/RiverPlot_grid.PNG" align="center" width="800" />

<br>


</details>

<br>


### 🌹 Rosetype

<details><summary>Rosetype Plot Examples</summary>

```r
# Create fake data
data <- AutoPlots::FakeDataGenerator(N = 1000)
data <- data[, .(
  IndepVar = round(mean(Independent_Variable8), 3)
), by = c("Factor_1")]

# Build Plot
AutoPlots::Rosetype(
  dt = data,
  XVar = "Factor_1",
  YVar = "IndepVar"
)
```

<br>

<img src="https://raw.githubusercontent.com/AdrianAntico/AutoPlots/master/inst/RosetypePlot.PNG" align="center" width="800" />

<br>
<br>

```r
# Create fake data
data <- AutoPlots::FakeDataGenerator(N = 1000)
dt1 <- data[, .(
  IndepVar = round(mean(Independent_Variable8), 3)
), by = c("Factor_1")]
dt2 <- data[, .(
  IndepVar = round(mean(Independent_Variable8), 3)
), by = c("Factor_2")]

# Build Plots
p1 <- AutoPlots::Rosetype(
  dt = dt1,
  XVar = "Factor_1",
  YVar = "IndepVar",
  title.text = "Factor_1"
)

p2 <- AutoPlots::Rosetype(
  dt = dt2,
  XVar = "Factor_2",
  YVar = "IndepVar",
  title.text = "Factor_2"
)

AutoPlots::display_plots_grid(
  list(p1, p2),
  cols = 1
)
```

<br>

<img src="https://raw.githubusercontent.com/AdrianAntico/AutoPlots/master/inst/RosetypePlot_grid.PNG" align="center" width="800" />

<br>


</details>

<br>


### ☁ Scatter

<details><summary>Scatter Plot Examples</summary>

```r
# Create data
n   <- 1000        # rows
p   <- 4           # number of variables
rho <- 0.8         # correlation between neighbors (AR(1))
mu  <- c(50, 100, 0, 10)     # means (length p)
sds <- c(10, 5, 3, 1)        # standard deviations (length p)
Sigma_cor <- outer(1:p, 1:p, \(i, j) rho^abs(i - j))   # AR(1) correlation matrix
L <- chol(Sigma_cor)                                   # Cholesky factor (upper-tri)
Z <- matrix(rnorm(n * p), nrow = n, ncol = p)          # iid N(0,1)
X_cor <- Z %*% L                                       # correlated, unit variance
X <- X_cor %*% diag(sds)                               # set std devs
X <- sweep(X, 2, mu, `+`)                              # set means
dt <- data.table::as.data.table(X)
data.table::setnames(dt, paste0("x", seq_len(p)))

# Build Plot
AutoPlots::Scatter(
  dt = dt,
  XVar = "x2",
  YVar = "x1",
  legend.show = FALSE
)
```

<br>

<img src="https://raw.githubusercontent.com/AdrianAntico/AutoPlots/master/inst/ScatterPlot.PNG" align="center" width="800" />

<br>
<br>

```r
# Create data
n   <- 1000        # rows
p   <- 4           # number of variables
rho <- 0.8         # correlation between neighbors (AR(1))
mu  <- c(50, 100, 0, 10)     # means (length p)
sds <- c(10, 5, 3, 1)        # standard deviations (length p)
Sigma_cor <- outer(1:p, 1:p, \(i, j) rho^abs(i - j))   # AR(1) correlation matrix
L <- chol(Sigma_cor)                                   # Cholesky factor (upper-tri)
Z <- matrix(rnorm(n * p), nrow = n, ncol = p)          # iid N(0,1)
X_cor <- Z %*% L                                       # correlated, unit variance
X <- X_cor %*% diag(sds)                               # set std devs
X <- sweep(X, 2, mu, `+`)                              # set means
dt <- data.table::as.data.table(X)
data.table::setnames(dt, paste0("x", seq_len(p)))

# Build Plots
p1 <- AutoPlots::Scatter(
  dt = dt,
  XVar = "x2",
  YVar = "x1",
  legend.show = FALSE,
  title.text = "x1 vs x2"
)

p2 <- AutoPlots::Scatter(
  dt = dt,
  XVar = "x3",
  YVar = "x1",
  legend.show = FALSE,
  title.text = "x1 vs x3"
)

p3 <- AutoPlots::Scatter(
  dt = dt,
  XVar = "x3",
  YVar = "x2",
  legend.show = FALSE,
  title.text = "x2 vs x3"
)

p4 <- AutoPlots::Scatter(
  dt = dt,
  XVar = "x4",
  YVar = "x3",
  legend.show = FALSE,
  title.text = "x3 vs x4"
)

AutoPlots::display_plots_grid(
  list(p1,p2,p3,p4),
  cols = 2
)
```

<br>

<img src="https://raw.githubusercontent.com/AdrianAntico/AutoPlots/master/inst/ScatterPlot_grid.PNG" align="center" width="800" />

<br>


</details>

<br>


### 🧱 Stacked Bar

<details><summary>Stacked Bar Plot Examples</summary>

```r
# Create fake data
data <- AutoPlots::FakeDataGenerator(N = 100000)
data <- data[, .(
  IndepVar = sum(Independent_Variable1)
), by = c("Factor_1","Factor_2")]


# Echarts Stacked Bar Chart
AutoPlots::StackedBar(
  dt = data,
  XVar = "Factor_1",
  YVar = "IndepVar",
  GroupVar = "Factor_2",
  legend.right = 35,
  legend.top = 45,
  yAxis.nameTextStyle.padding = 40
)
```

<br>

<img src="https://raw.githubusercontent.com/AdrianAntico/AutoPlots/master/inst/StackedBarPlot.PNG" align="center" width="800" />


<br>


</details>

<br>


### 📉 Step

<details><summary>Step Plot Examples</summary>

```r
# Create data
data <- AutoPlots::FakeDataGenerator(N = 1000, AddDate = T)
data <- data[, .(
  IndepVar = mean(Independent_Variable8)
), by = c("DateTime")]

# Build plot
AutoPlots::Step(
  dt = data,
  XVar = "DateTime",
  YVar = "IndepVar",
  title.text = "IndepVar",
  lineStyle.color = c("red","#3848FF","blue"),
  legend.show = FALSE
)
```

<br>

<img src="https://raw.githubusercontent.com/AdrianAntico/AutoPlots/master/inst/StepPlot.PNG" align="center" width="800" />

<br>
<br>

```r
# Create data
data <- AutoPlots::FakeDataGenerator(N = 1000, AddDate = T)
data <- data[, .(
  IndepVar = mean(Independent_Variable8),
  IndepVar2 = mean(Independent_Variable7),
  IndepVar3 = mean(Independent_Variable6),
  IndepVar4 = mean(Independent_Variable5)
), by = c("DateTime")]

# Build plot
p1 <- AutoPlots::Step(
  dt = data,
  XVar = "DateTime",
  YVar = "IndepVar",
  title.text = "IndepVar",
  lineStyle.color = c("red","#3848FF","blue"),
  legend.show = FALSE
)

p2 <- AutoPlots::Step(
  dt = data,
  XVar = "DateTime",
  YVar = "IndepVar2",
  title.text = "IndepVar2",
  lineStyle.color = c("red","#3848FF","blue"),
  legend.show = FALSE
)

p3 <- AutoPlots::Step(
  dt = data,
  XVar = "DateTime",
  YVar = "IndepVar3",
  title.text = "IndepVar3",
  lineStyle.color = c("red","#3848FF","blue"),
  legend.show = FALSE
)

p4 <- AutoPlots::Step(
  dt = data,
  XVar = "DateTime",
  YVar = "IndepVar4",
  title.text = "IndepVar4",
  lineStyle.color = c("red","#3848FF","blue"),
  legend.show = FALSE
)

AutoPlots::display_plots_grid(
  list(p1,p2,p3,p4),
  cols = 2
)
```

<br>

<img src="https://raw.githubusercontent.com/AdrianAntico/AutoPlots/master/inst/StepPlot_grid.PNG" align="center" width="800" />

<br>


</details>

<br>

### 🧩 Treemap

<details><summary>Treemap Plot Examples</summary>

```r
library(data.table)
set.seed(123)

# Define hierarchy
continents  <- c("Americas", "Europe", "Asia")
countries   <- list(
  Americas = c("USA", "Canada", "Mexico"),
  Europe   = c("UK", "Germany", "Spain"),
  Asia     = c("China", "Japan", "India")
)
cities <- list(
  USA     = c("New York", "Los Angeles", "Chicago"),
  Canada  = c("Toronto", "Vancouver"),
  Mexico  = c("Mexico City", "Guadalajara"),
  UK      = c("London", "Manchester"),
  Germany = c("Berlin", "Hamburg"),
  Spain   = c("Madrid", "Barcelona"),
  China   = c("Beijing", "Shanghai"),
  Japan   = c("Tokyo", "Osaka"),
  India   = c("Mumbai", "Delhi")
)

neighborhoods <- list(
  "New York"     = c("Manhattan", "Brooklyn"),
  "Los Angeles"  = c("Hollywood", "Venice"),
  "Chicago"      = c("Loop", "Hyde Park"),
  "Toronto"      = c("North York", "Scarborough"),
  "Vancouver"    = c("Downtown", "Kitsilano"),
  "Mexico City"  = c("Centro", "Roma"),
  "Guadalajara"  = c("Zapopan", "Tlaquepaque"),
  "London"       = c("Camden", "Chelsea"),
  "Manchester"   = c("Salford", "Didsbury"),
  "Berlin"       = c("Mitte", "Kreuzberg"),
  "Hamburg"      = c("Altona", "Eimsbüttel"),
  "Madrid"       = c("Centro", "Chamartín"),
  "Barcelona"    = c("Gràcia", "Eixample"),
  "Beijing"      = c("Chaoyang", "Haidian"),
  "Shanghai"     = c("Pudong", "Xuhui"),
  "Tokyo"        = c("Shinjuku", "Shibuya"),
  "Osaka"        = c("Kita", "Namba"),
  "Mumbai"       = c("Bandra", "Andheri"),
  "Delhi"        = c("Karol Bagh", "Saket")
)

# Build the full dataset
dt <- rbindlist(lapply(names(countries), function(cont) {
  rbindlist(lapply(countries[[cont]], function(ctry) {
    rbindlist(lapply(cities[[ctry]], function(city) {
      data.table(
        Continent = cont,
        Country = ctry,
        City = city,
        Neighborhood = neighborhoods[[city]],
        Value = sample(50:500, length(neighborhoods[[city]]), replace = TRUE)
      )
    }))
  }))
}), use.names = TRUE)


# Build plot
AutoPlots::Treemap(
  dt = dt,
  YVar = "Value",
  GroupVar = c("Continent","Country","City","Neighborhood")
)
```

<br>

<img src="https://raw.githubusercontent.com/AdrianAntico/AutoPlots/master/inst/TreemapPlot.PNG" align="center" width="800" />

<br>
<br>

```r
library(data.table)
set.seed(123)

# ----- Define hierarchy -------------------------------------------------------
continents  <- c("Americas", "Europe", "Asia")
countries   <- list(
  Americas = c("USA", "Canada", "Mexico"),
  Europe   = c("UK", "Germany", "Spain"),
  Asia     = c("China", "Japan", "India")
)
cities <- list(
  USA     = c("New York", "Los Angeles", "Chicago"),
  Canada  = c("Toronto", "Vancouver"),
  Mexico  = c("Mexico City", "Guadalajara"),
  UK      = c("London", "Manchester"),
  Germany = c("Berlin", "Hamburg"),
  Spain   = c("Madrid", "Barcelona"),
  China   = c("Beijing", "Shanghai"),
  Japan   = c("Tokyo", "Osaka"),
  India   = c("Mumbai", "Delhi")
)

neighborhoods <- list(
  "New York"     = c("Manhattan", "Brooklyn"),
  "Los Angeles"  = c("Hollywood", "Venice"),
  "Chicago"      = c("Loop", "Hyde Park"),
  "Toronto"      = c("North York", "Scarborough"),
  "Vancouver"    = c("Downtown", "Kitsilano"),
  "Mexico City"  = c("Centro", "Roma"),
  "Guadalajara"  = c("Zapopan", "Tlaquepaque"),
  "London"       = c("Camden", "Chelsea"),
  "Manchester"   = c("Salford", "Didsbury"),
  "Berlin"       = c("Mitte", "Kreuzberg"),
  "Hamburg"      = c("Altona", "Eimsbüttel"),
  "Madrid"       = c("Centro", "Chamartín"),
  "Barcelona"    = c("Gràcia", "Eixample"),
  "Beijing"      = c("Chaoyang", "Haidian"),
  "Shanghai"     = c("Pudong", "Xuhui"),
  "Tokyo"        = c("Shinjuku", "Shibuya"),
  "Osaka"        = c("Kita", "Namba"),
  "Mumbai"       = c("Bandra", "Andheri"),
  "Delhi"        = c("Karol Bagh", "Saket")
)

# ----- Build the full dataset -------------------------------------------------
dt <- rbindlist(
  lapply(names(countries), function(cont) {
    rbindlist(lapply(countries[[cont]], function(ctry) {
      rbindlist(lapply(cities[[ctry]], function(city) {
        n_nbhd <- length(neighborhoods[[city]])
        
        # base volume metric (e.g. revenue, signups)
        value <- sample(50:500, n_nbhd, replace = TRUE)
        
        # impressions & clicks for rate example
        imps  <- sample(1e3:1e4, n_nbhd, replace = TRUE)
        ctr   <- runif(n_nbhd, 0.01, 0.20) # 1%–20% click-through
        clicks <- pmin(round(imps * ctr), imps)
        
        data.table(
          Continent     = cont,
          Country       = ctry,
          City          = city,
          Neighborhood  = neighborhoods[[city]],
          Value         = value,
          Impressions   = imps,
          Clicks        = clicks
        )
      }))
    }))
  }),
  use.names = TRUE
)

# --------------------------------------------------------------------
# 2) Rate-based treemap – area by Click-through-rate (Clicks / Impressions)
#    (uses your new RateNumer / RateDenom / AreaMetric wiring)
# --------------------------------------------------------------------
AutoPlots::Treemap(
  dt       = dt,
  YVar     = c("Value", "Clicks", "Impressions"),
  GroupVar = c("Continent", "Country", "City", "Neighborhood"),
  RateNumer  = "Clicks",
  RateDenom  = "Impressions",
  AreaMetric = "rate"
)

```

<br>

<img src="https://raw.githubusercontent.com/AdrianAntico/AutoPlots/master/inst/TreemapPlotRate.PNG" align="center" width="800" />

<br>


</details>

<br>
