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
- Density
- Donut
- Heatmaps
- Histogram
- Line
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
- Confusion Matrix Heatmap
- Gain
- Lift
- Partial Autocorrelation
- Partial Dependence
- Partial Dependence Heatmap
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


### Area

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


### Bar

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


### Box

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


### Copula

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



### Correlogram

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



### Density

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


### Donut

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
  YVar = "IndepVar",
  ShowLabels = T)
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
  title.text = "Factor_1",
  ShowLabels = T)

p2 <- AutoPlots::Donut(
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

<img src="https://raw.githubusercontent.com/AdrianAntico/AutoPlots/master/inst/DonutPlot_grid.PNG" align="center" width="800" />

<br>


</details>

<br>



### Heatmap

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



