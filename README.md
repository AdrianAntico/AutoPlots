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
- Probability
- ROC
- Shapely Importance
- Variable Importance



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


### Histogram

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


### Line

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


### Parallel

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



### Pie

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


### Radar

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



### River

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


### Rosetype

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


### Scatter

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


### Stacked Bar

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


