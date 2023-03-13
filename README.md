![Version:1.0.0](https://img.shields.io/static/v1?label=Version&message=1.0.0&color=blue&?style=plastic)
[![PRsWelcome](https://img.shields.io/badge/PRs-welcome-brightgreen.svg?style=default)](http://makeapullrequest.com)

### Caution!!!
There is no license in this repository, yet. I'm not sure yet whether I'll go the APGL route or an MIT route.

<img src="https://raw.githubusercontent.com/AdrianAntico/AutoPlots/master/inst/logo.PNG" align="center" width="800" />

### Motivation
I'm sick of looking up plotting syntax every time I build a plot. Even after thousands of plots developed, I still have to look up syntax. This package is intended to stop that behavior. The plots returned in AutoPlots are sufficiently good for 99% of plotting purposes. Further customizations can be handling by going back to source packages if needed. There are two broad classes of plots available: standard and model evaluation. 

### Standard plots 
These plot types should be known to most, although there are some that might not fit that category, such as river plots.

### Model evaluation 
These plot types are most useful for those looking to evaluate the performance of regression, binary classification, and multiclass models. Designing plots for multiclass models are rather challenging but I've abstracted all that work away so the user only has to pass their categorical target variable along with their categorical predicted value, and the plots will display all the levels appropriately without requiring the user to do the data manipulation ahead of time.

### Data Management
Another giant bonus is that the user can either pre-aggregate their data and pass that through to these functions (using PreAgg = TRUE) or they can leave their data in transaction form and let my optimized data.table code to manage it for them. This means you can develop plots from giant data sets without having to wait for long running data operations. Further, there is a SampleSize parameter in the functions to limit the number of records to display, in the giant data cases (or for scatter / copula plots).

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

<img src="https://raw.githubusercontent.com/AdrianAntico/AutoPlots/master/inst/AvailablePlots.PNG" align="center" width="800" />


### Histogram, Density, BoxPlot

<img src="https://raw.githubusercontent.com/AdrianAntico/AutoPlots/master/inst/Distribution.PNG" align="center" width="800" />

### Area, Steam, Pie

<img src="https://raw.githubusercontent.com/AdrianAntico/AutoPlots/master/inst/Area_Steam_Pie.PNG" align="center" width="800" />
