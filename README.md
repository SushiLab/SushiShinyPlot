# *SushiShinyPlot*: Package for plotting through the construction of Shiny Apps

The package SushiShinyPlot provides pre-built Shiny Apps for the most common plots based on ggplot2 and plotly.

## Content
The current version contains the following functions:

+ **ShinyScatter()** Shiny App for a Scatterplot.  
+ **ShinyViolin()** Shiny App for a Violinplot.  
+ **ShinyHistogram()** Shiny App for a Histogram.  
+ **ShinyDensity()** Shiny App for a Densityplot.  
+ **ShinyBar()** Shiny App for a Barplot.  


## Installation of the package

* Install the latest version of **plotly** and **shiny**:

```r
install.packages("plotly")  
install.packages("shiny")
```

* Install FastaUtils' current development version from Github:

```r
devtools::install_github("SushiLab/SushiShinyPlot")
```


## Citation

To see the preferable citation of the package, type:

```r
citation("SushiShinyPlot")
```

## Usage

```r
# Load some data
library(vegan)
data(mite)
data(mite.env)
newdat<-cbind(mite[,order(colSums(mite),decreasing = T)[1:5]],mite.env)

# Make a Scatterplot Shiny App
library(SushiShinyPlot)
shinyScatter(newdat)
```
