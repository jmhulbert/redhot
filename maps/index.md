---
title: "Maps"
author: "Joey Hulbert"
date: "2024-02-06"
output: 
  html_document:
    toc: true
    toc_float:
      collapsed: true
    toc_depth: 3
    keep_md: yes
---

|            |            |            |            |
|:----------:|:----------:|:----------:|:----------:|
|[Welcome](https://jmhulbert.github.io/redhot)|[Data](https://jmhulbert.github.io/redhot/data)|[Analyses](https://jmhulbert.github.io/redhot/analyses)|[Discussion](https://jmhulbert.github.io/redcedar/discussion)|
|             |           |            |            |




```r
library(tidyverse)
library(knitr)
library(kableExtra)
library(gghalves)
library(patchwork)
library(scales)
library(ggmap)
library(osmdata)
```


# Purpose

The purpose of this page is to generate maps of data used in analyses. 


# Read Data


```r
data <- read.csv('https://raw.githubusercontent.com/jmhulbert/redhot/main/data/urban-data-modified.csv')
```

# Filter into Areas


```r
portland <- data %>% filter(Area=="Portland")
king <- data %>% filter(Area=="King County")
tacoma <-data %>% filter(Area=="Tacoma")
```


## Portland


```r
portbb <- c(left = min(portland$longitude.x), 
                              bottom = min(portland$latitude.x), 
                              right = max(portland$longitude.x), 
                              top = max(portland$latitude.x))
```



```r
portmap <- get_map(portbb, zoom = 11, scale = 2, maptype="terrain",source="google")
```

```
## ! Bounding box given to Google - spatial extent only approximate.
```

```
## ℹ <https://maps.googleapis.com/maps/api/staticmap?center=45.548448,-122.637458&zoom=11&size=640x640&scale=2&maptype=terrain&language=en-EN&key=xxx>
```

```r
portland.map <- ggmap(portmap) + geom_point(data = portland, aes(x = longitude.x, y = latitude.x,fill=DN_AF1), color = "black",pch=21, size = 3) + theme_minimal() +scale_fill_viridis_c(option = "inferno")+labs(title="Portland",x="Longitude",y="Latitude",fill="Afternoon\nTemp (F)") +theme(plot.title = element_text(size = 14, hjust = .5,face = "bold.italic"))
```


```r
portland.map
```

![](index_files/figure-html/portland-map-1.png)<!-- -->

## King County


```r
kingbb <- c(left = min(king$longitude.x), 
                              bottom = min(king$latitude.x), 
                              right = max(king$longitude.x), 
                              top = max(king$latitude.x))
```



```r
kingmap <- get_map(kingbb, zoom = 10, scale = 2,maptype="terrain",source="google")
```

```
## ! Bounding box given to Google - spatial extent only approximate.
```

```
## ℹ <https://maps.googleapis.com/maps/api/staticmap?center=47.521806,-122.07004&zoom=10&size=640x640&scale=2&maptype=terrain&language=en-EN&key=xxx>
```


```r
king.map <- ggmap(kingmap) + geom_point(data = king, aes(x = longitude.x, y = latitude.x,fill=DN_AF1), color = "black",pch=21, size = 3) + theme_minimal() +scale_fill_viridis_c(option = "inferno")+labs(title="King County",x="Longitude",y="Latitude",fill="Afternoon\nTemp (F)") +theme(plot.title = element_text(size = 14, hjust = .5,face = "bold.italic"))
```


```r
king.map
```

![](index_files/figure-html/king-county-map-1.png)<!-- -->


## Tacoma


```r
tacomabb <- c(left = min(tacoma$longitude.x), 
                              bottom = min(tacoma$latitude.x), 
                              right = max(tacoma$longitude.x), 
                              top = max(tacoma$latitude.x))
```



```r
tacomamap <- get_map(tacomabb, zoom = 12, scale = 2,maptype="terrain",source="google")
```

```
## ! Bounding box given to Google - spatial extent only approximate.
```

```
## ℹ <https://maps.googleapis.com/maps/api/staticmap?center=47.248009,-122.474169&zoom=12&size=640x640&scale=2&maptype=terrain&language=en-EN&key=xxx>
```


```r
tacoma.map <- ggmap(tacomamap) + geom_point(data = tacoma, aes(x = longitude.x, y = latitude.x,fill=DN_AF1), color = "black",pch=21, size = 3) + theme_minimal() +scale_fill_viridis_c(option = "inferno")+labs(title="Tacoma",x="Longitude",y="Latitude",fill="Afternoon\nTemp (F)") +theme(plot.title = element_text(size = 14, hjust = .5,face = "bold.italic"))
```


```r
tacoma.map
```

![](index_files/figure-html/tacoma-map-1.png)<!-- -->


```r
portland.map <- portland.map + theme(legend.position = "none")
tacoma.map <- tacoma.map + theme(legend.position = "none")
king.map <- king.map
```




```r
portland.map + tacoma.map + king.map
```

![](index_files/figure-html/all-areas-map-1.png)<!-- -->




