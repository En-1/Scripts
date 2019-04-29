library(googleVis)
########### turn on flash ############
 AnnoTimeLine  <- gvisAnnotatedTimeLine(Stock, datevar="Date",numvar="Value",idvar="Device",
titlevar="Title",annotationvar="Annotation"); plot(AnnoTimeLine)
########### motion chart ############# 
 M1 <- gvisMotionChart(Fruits, idvar="Fruit", timevar="Year")
 plot(M1)
 
library(plotly)
 plot_ly(mtcars, x = mtcars$wt, y = mtcars$mpg, mode="markers",color = as.factor(mtcars$cyl))
 with(mtcars, plot_ly(x = wt, y = mpg, mode="markers",color = as.factor(cyl)))
 with(mtcars, plot_ly(x = wt, y = mpg, mode="markers",color = disp)) 
 with(mtcars, plot_ly(x = wt, y = mpg, mode="markers",color = as.factor(cyl), size = hp)) 
 
 library(tidyverse)
 data("EuStockMarkets")
 stocks <- as.data.frame(EuStockMarkets) %>% gather(index, price) %>% mutate(time = rep(time(EuStockMarkets), 4))
 plot_ly(stocks, x = ~time, y = ~price, color = ~index, type = "scatter", mode = "lines")      
 
 library(leaflet)
leaflet() %>% addTiles() 
df<-data.frame(lat = runif(20,58.74,58.76),lng = runif(20,42.66,42.70))
df %>% leaflet() %>% addTiles() %>% addMarkers(clusterOptions = markerClusterOptions())
