---
title: "AirTemperature and Precipitation in SouthTyrol | Data Availability and Quality"
author: "Johannes Brenner"
date: "17. Juli 2015"
output: html_document
runtime: shiny
---
  
***
  
This R Markdown document is made interactive using Shiny. To learn more, see [Interactive Documents](http://rmarkdown.rstudio.com/authoring_shiny.html).

***

####Data Origin And Pupose Of Use
 
In South Tyrol a dense network of micro-climate stations is set up since long term. Maintainer of these station is the [Hydrographic Office of the Province Bozen/Bolzano](http://www.provinz.bz.it/wetter/home.asp). Minimum, mean and maximum air temperature and precipitation data has been downloaded on daily basis from their WISKI database. This data is intended to be used for statistical downscaling of regional climate projections, an important step towards realistic forcing for climate impact assessment models.
This document provides

* quality check of the whole data set (calculation of quality indices)
* visualisation of gaps in the time series (monthly basis),
* visualisation of the time series (daily scale),
* interpolation of the time series, and calculation of, accordingly, enhanced quality measures.

***



####Where are the climate stations?

Below one can get an impression of the density of the South Tyrolean meteo network for air temperature. Here available stations providing daily data are shown, high elevation stations are shown in darker grey. The station you choose to discover is marked within a red circle.


```
## function (...) 
## {
##     if (length(outputArgs) != 0 && !hasExecuted$get()) {
##         warning("Unused argument: outputArgs. The argument outputArgs is only ", 
##             "meant to be used when embedding snippets of Shiny code in an ", 
##             "R Markdown code chunk (using runtime: shiny). When running a ", 
##             "full Shiny app, please set the output arguments directly in ", 
##             "the corresponding output function of your UI code.")
##         hasExecuted$set(TRUE)
##     }
##     if (is.null(formals(origRenderFunc))) 
##         origRenderFunc()
##     else origRenderFunc(...)
## }
## <environment: 0x3e7f2c0>
## attr(,"class")
## [1] "shiny.render.function" "function"             
## attr(,"outputFunc")
## function (outputId, width = "100%", height = 400) 
## {
##     htmlwidgets::shinyWidgetOutput(outputId, "leaflet", width, 
##         height, "leaflet")
## }
## <environment: namespace:leaflet>
## attr(,"outputArgs")
## list()
## attr(,"hasExecuted")
## <Mutable>
##   Public:
##     clone: function (deep = FALSE) 
##     get: function () 
##     set: function (value) 
##   Private:
##     value: FALSE
```

```
## Error in eval(expr, envir, enclos): konnte Funktion "inputPanel" nicht finden
```




***

####First data quality overview

A short summary table on data quality is provided for the whole data set. Below a short description of the colomns:

* _SufficientObsPeriod_:    Has time series a consecutive length longer than 20 years? [TRUE=1]
* _#ConsecutiveYears_:      How long is the consecutive time series [YEARs]?
* _SufficientNAqual_:       Has the consecutive time series less than specified percentage of NA values?
* _%NAs4consecutiveYears_:  Percent of NAs in consecutive time series.
* _#NAs4consecutiveYears_:  Sum of NAs in consecutive time series.


```
## Error in eval(expr, envir, enclos): konnte Funktion "renderDataTable" nicht finden
```

Choose a variable you want to discover and define thresholds for high quality time series for the summary table above:


```
## Error in eval(expr, envir, enclos): konnte Funktion "inputPanel" nicht finden
```

***

####Which months do have an adequate data quality?

As for statistical downscaling a time series of at least 20 years is necessary the more general question to ask is: For how many years good quality data is available for the specific station? The heatmap below gives an answer for the choosen station. For each month the percentage of days the choosen variable was measured are shown. 
Highlighting of rows and colomns as well as zooming is provided.

***


```
## function (...) 
## {
##     if (length(outputArgs) != 0 && !hasExecuted$get()) {
##         warning("Unused argument: outputArgs. The argument outputArgs is only ", 
##             "meant to be used when embedding snippets of Shiny code in an ", 
##             "R Markdown code chunk (using runtime: shiny). When running a ", 
##             "full Shiny app, please set the output arguments directly in ", 
##             "the corresponding output function of your UI code.")
##         hasExecuted$set(TRUE)
##     }
##     if (is.null(formals(origRenderFunc))) 
##         origRenderFunc()
##     else origRenderFunc(...)
## }
## <environment: 0x39f5770>
## attr(,"class")
## [1] "shiny.render.function" "function"             
## attr(,"outputFunc")
## function (outputId, width = "100%", height = "400px") 
## {
##     shinyWidgetOutput(outputId, "d3heatmap", width, height, package = "d3heatmap")
## }
## <environment: namespace:d3heatmap>
## attr(,"outputArgs")
## list()
## attr(,"hasExecuted")
## <Mutable>
##   Public:
##     clone: function (deep = FALSE) 
##     get: function () 
##     set: function (value) 
##   Private:
##     value: FALSE
```

***

####How does the series look in detail?

Looking to the series of mean temperature in detail gives you a guess if interpolation of values can help to create sufficent data quality. If you choose _add interpolated time series_ the time series is interpolated with a spline methodology. You can adjust the maximum number of consecutive NAs to fill. Zooming the graph or adding a rolling mean (left bottom corner) to better discover the time series is provided. Moreover, this option is changing the results of the summary table and the heatmap accordingly. Instead of the raw data sets, now the interpolated data sets are used for calculations.


```
## Error in eval(expr, envir, enclos): konnte Funktion "inputPanel" nicht finden
```

```
## function (...) 
## {
##     if (length(outputArgs) != 0 && !hasExecuted$get()) {
##         warning("Unused argument: outputArgs. The argument outputArgs is only ", 
##             "meant to be used when embedding snippets of Shiny code in an ", 
##             "R Markdown code chunk (using runtime: shiny). When running a ", 
##             "full Shiny app, please set the output arguments directly in ", 
##             "the corresponding output function of your UI code.")
##         hasExecuted$set(TRUE)
##     }
##     if (is.null(formals(origRenderFunc))) 
##         origRenderFunc()
##     else origRenderFunc(...)
## }
## <environment: 0x7464c88>
## attr(,"class")
## [1] "shiny.render.function" "function"             
## attr(,"outputFunc")
## function (outputId, width = "100%", height = "400px") 
## {
##     htmlwidgets::shinyWidgetOutput(outputId, "dygraphs", width, 
##         height)
## }
## <environment: namespace:dygraphs>
## attr(,"outputArgs")
## list()
## attr(,"hasExecuted")
## <Mutable>
##   Public:
##     clone: function (deep = FALSE) 
##     get: function () 
##     set: function (value) 
##   Private:
##     value: FALSE
```

***
