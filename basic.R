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

do_shape_files <- FALSE
source("functions.R")

## check what's going on as you go through the code
## useful functions for more complicated objects:
##   names(.)
##   str(.)

if (do_shape_files) {
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
}

## only reading the landuse rasters
rasterfiles <- list.files(pattern="*.tif$",recursive=TRUE)
years <- parse_number(rasterfiles)
years <- years[years>1900] ## leave out DEM file

## reading all of the files into a list
rr_list <- map(years, get_categorical_raster, list_cats=TRUE)  
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
<<<<<<< HEAD
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
=======
    read_excel("climate/climate_data.xlsx", col_names=TRUE)
)
>>>>>>> 446cf762d1eed16341df5b2b2272f852e30f83db


### land-use change raster stack
## ObsLulcRasterStack(rr_list)  ## doesn't know what to do
rs <- ObsLulcRasterStack(rr_list,
                   pattern="[0-9]+", ## use all numbers
                   ## this only works if we only have the
                   ##  land-use rasters; if we have climate,
                   ##  DEM, etc. then we have to give just
                   ##  the vector of years
                   t=as.numeric(names(rr_list)))

crosstab(stack(rr_list[[1]],rr_list[[2]]))
crosstab(stack(rs[[1]],rs[[2]]))

crossTabulate(rs,times=c(1987,2014))

## find attribute table
## attributes(x@data)$attributes

## this stuff doesn't work
x <- rr_list[[1]]
y <- rr_list[[2]]
z <- x==3 & y!=3  ## places where erg was lost between 1987 and 1997 (i.e. nowhere)
levelplot(z)
summary(x)

x2 <- rr_list[["2014"]]
## plots of erg and change
levelplot(x==3)  ## erg in 1987
levelplot(y==3)  ## erg in 1997
levelplot(x==3 & y==3)  ## erg in both years
levelplot(x!=3)       ## non-erg in 19878
levelplot(x==3 & y!=3)  ## weird-looking because all zero (no loss of erg)
levelplot(x!=3 & y==3)  ## gain of erg

levelplot(x==3 & x2!=3)

a <- rr_list[[1]]
levelplot(a)
## can't aggregate too much or we lose categories
r2 <- make_categorical(aggregate(rr_list[[1]],fact=6,fun=modal),
                       rat=get_rat(rr_list[[1]]))
levelplot(r2)
dd <- as_tibble(as.data.frame(r2))
table(dd$landuse)

before_after <- function(x,y) {
    2*as.numeric(x=="erg")+as.numeric(y=="erg")
}

## 3=both, 2=before, 1=after, 0=neither
before_after(c("erg","other"),c("other","erg"))
## test: work on re-categorizing ...
## BMB: not sure this works yet ...
r3 <- overlay(rr_list[[3]], rr_list[[4]],
              fun = before_after)
levelplot(r3)
r4 <- overlay(rr_list[[1]], rr_list[[6]],
              fun = before_after)
levelplot(r4)
table(as.data.frame(r4)$layer)

#slope and aspect
m=raster("dem/Extract_dem11.tif")
m1=aggregate(m,fact=2,fun=modal)
k=terrain(m1, opt="slope", unit="radians", neighbors=8)
plot(k)
k=terrain(m1, opt="aspect", unit="radians", neighbors=8)
k2=terrain(m1, opt="aspect", unit="radians", neighbors=8)
plot(k2)
#focal with matrix 3*3
library(raster)
fa=focal(a,matrix(1/9,nrow=3,ncol=3))

