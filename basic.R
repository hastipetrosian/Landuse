# landuse 2018
library(geosample)
library(sf)
library(rgeos)
library(readxl)
library(raster)
library(tidyverse) ## includes purrr, readr, dplyr, ...
library(readxl)
library(fasterize)  ## may need to install this
library(rasterVis)
library(foreign)
library(cowplot)

source("functions.R")

## check what's going on as you go through the code
## useful functions for more complicated objects:
##   names(.)
##   str(.)

## all shape files:
shapefiles <- list.files(pattern="*.shp$",recursive=TRUE)
## dd_list <- lapply(v2shp, read_sf)
dd_list <- map(shapefiles, read_sf)  ## reading all of the files into a list
## extract the first number from each file name
year_vec <- parse_number(shapefiles)
names(dd_list) <- year_vec
plot(dd_list[["2014"]])

## draw all of the vector maps
op <- par(mfrow=c(2,3))   ## set up a 2x3 grid of plots
## (2 rows, 3 columns)
map(dd_list,
    ## . represents the current element in the list
    ## key.pos and reset are necessary so we can plot all of the maps
    ## together (see ?plot.sf)
    ~ plot(.["descrip"],key.pos=NULL,reset=FALSE))
par(op)  ## restore old parameters

## dd_list[[1]]["descrip"]

all_descrip <- map(dd_list, ~ sort(.["descrip"]$descrip))
sort(unique(unlist(all_descrip)))

## acquaculture -> aquaculture

length(dd_list)  ## how many maps?

## only reading the landuse rasters
rasterfiles <- list.files(pattern="*.tif$",recursive=TRUE)
years <- parse_number(rasterfiles)
years <- years[years>1900] ## leave out DEM file

rr_list <- map(years, get_categorical_raster)  ## reading all of the files into a list
names(rr_list) <- years

dem <- raster("dem/Extract_dem11.tif")

## ?plot.raster : help on the 'plot' method for 'raster' objects
## that points us to ?rasterImage for most information
## draw all of the raster maps
## (2 rows, 3 columns)
plots <- map(rr_list,levelplot,colorkey=FALSE)
plot_grid(plotlist=plots)

## ?"levelplot-methods"


## playing with rasterizing: not really necessary any more
## x <- dd_list[[1]]
## r <- raster(x,res=10)
## x$dn <- as.numeric(as.factor(x$descrip))
## r <- fasterize(x, r, field = "dn")
## plot(r)
## spplot(r) ## ugly
clim_data <- (
    read_excel("climate/climate data.xlsx", col_names=TRUE)
    ## convert Farsi numbers to Western ...
mutate_if(contains_slash, make_number
#raster maps          
<-a=raster("1987R/1987raster.tif")
<-b=raster("1997R/1997raster.tif")
<-c=raster("2003R/2003raster.tif")
<-d=raster("2008R/2008raster.tif")
<-e=raster("2014R/2014raster.tif")
<-f=raster("2018R/2018raster.tif")
<-g=raster("dem/Extract_dem11.tif")
#aggregate fact=2
<-g1=aggregate(g,fact=2,fun=modal)
#slope and aspect
<-slope=terrain(g1, opt="slope",unit="radians")
<-aspect=terrain(g1, opt="aspect",unit="radians")
<-plot(aspect)
<-plot(slope)
