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
library(lulcc)

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
    read_excel("climate/climate_data.xlsx", col_names=TRUE)
)


### land-use change raster stack
## ObsLulcRasterStack(rr_list)  ## doesn't know what to do
ObsLulcRasterStack(rr_list,pattern="*") ## *=use all rasters
ObsLulcRasterStack(rr_list,
                   pattern="[0-9]+", ## use all numbers
                   ## this only works if we only have the
                   ##  land-use rasters; if we have climate,
                   ##  DEM, etc. then we have to give just
                   ##  the vector of years
                   t=as.numeric(names(rr_list)))
                   
