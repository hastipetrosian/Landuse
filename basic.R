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
library(sp)

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
clim_data <-  read_excel("climate/climate_data.xlsx", col_names=TRUE)

##aggregate fact=2
## BMB: modal aggregation usually doesn't make sense for numeric rasters
## dem_agg=aggregate(dem,fact=2,fun=modal)

#slope and aspect
slope <- terrain(dem, opt="slope",unit="radians")
aspect <- terrain(dem, opt="aspect",unit="radians")
levelplot(aspect)
levelplot(slope)


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
a= ## erg in 1987
levelplot(y==3)  ## erg in 1997
levelplot(x==3 & y==3)  ## erg in both years
levelplot(x!=3)       ## non-erg in 19878
levelplot(x==3 & y!=3)  ## weird-looking because all zero (no loss of erg)
levelplot(x!=3 & y==3)  ## gain of erg

levelplot(x==3 & x2!=3)

a <- rr_list[[1]]
levelplot(a)
## can't aggregate too much or we lose categories
r2 <- make_categorical(gate(rr_list[[1]],fact=6,fun=modal),
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

##slope and aspect

slope <- terrain(dem, opt="slope", unit="radians", neighbors=8)
levelplot(slope)
aspect <- terrain(dem, opt="aspect", unit="radians", neighbors=8)
levelplot(aspect)


a <- rr_list[[1]]
##focal with matrix 3*3 with sum (default)
## BMB: numeric operations on categorical rasters don't usually make sense
prop <- focal(a,matrix(1/9,nrow=3,ncol=3))
##focal with matrix 3*3 with modal (BMB: not mode!)
prop2 <- focal(a,matrix(1,nrow=3,ncol=3), fun=modal)
## BMB: 
##data fram of 1987 raster layer (recived error)
xx <- as.data.frame(rasterToPoints(prop2))
## BMB: we've lost the categorical labels again ...
table(xx$layer)

## BMB::as.data.frame.matrix is not what we want;
## we need the data in long format (one row per pixel)
## the NaN values are there because they represent the
## 'undefined' corners of the map

xx2 <- as.data.frame.matrix(prop2)
image(as.matrix(xx2)) ## rotated, ugly
> before_after <- function(x,y) {
  +     2*as.numeric(x=="erg")+as.numeric(y=="erg")
  + }
> before_after(c("erg","other"),c("other","erg"))
[1] 2 1
> r3 <- overlay(rr_list[[3]], rr_list[[4]],
                +               fun = before_after)
> levelplot(r3)
Warning messages:
  1: In min(x) : no non-missing arguments to min; returning Inf
2: In max(x) : no non-missing arguments to max; returning -Inf
3: In min(x) : no non-missing arguments to min; returning Inf
4: In max(x) : no non-missing arguments to max; returning -Inf
> r4 <- overlay(rr_list[[1]], rr_list[[6]],
                +               fun = before_after)
> levelplot(r4)
Warning messages:
  1: In min(x) : no non-missing arguments to min; returning Inf
2: In max(x) : no non-missing arguments to max; returning -Inf
3: In min(x) : no non-missing arguments to min; returning Inf
4: In max(x) : no non-missing arguments to max; returning -Inf
> ergba=function(x,y){as.numeric(x=="erg")+as.numeric(y=="erg")}
> a=rr_list[[1]]
> a
class      : RasterLayer 
dimensions : 616, 891, 548856  (nrow, ncol, ncell)
resolution : 50, 50  (x, y)
extent     : 592280.5, 636830.5, 2827154, 2857954  (xmin, xmax, ymin, ymax)
crs        : +proj=utm +zone=40 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 
source     : C:/Users/asus/Documents/R2/landuse-raster/1987R/1987raster.tif 
names      : X1987raster 
values     : 1, 10  (min, max)
attributes :
  ID          landuse
from:  1 agriculture land
to : 10 vegetation cover

> b=rrlist[[2]]
Error: object 'rrlist' not found
> b=rr_list[[2]]
> b
class      : RasterLayer 
dimensions : 616, 891, 548856  (nrow, ncol, ncell)
resolution : 50, 50  (x, y)
extent     : 592280.5, 636830.5, 2827154, 2857954  (xmin, xmax, ymin, ymax)
crs        : +proj=utm +zone=40 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 
source     : C:/Users/asus/Documents/R2/landuse-raster/1997R/1997raster.tif 
names      : X1997raster 
values     : 1, 10  (min, max)
attributes :
  ID          landuse
from:  1 agriculture land
to : 10 vegetation cover

> levelplot(a==3)
> levelplot(b==3)
> a1=levelplot(a==3)
> b1=levelplot(b==3)
> ergba=function(x,y,code=3){as.numeric(x==code)+as.numeric(y==code)}
> overlay(a,b,fun=ergba)
class      : RasterLayer 
dimensions : 616, 891, 548856  (nrow, ncol, ncell)
resolution : 50, 50  (x, y)
extent     : 592280.5, 636830.5, 2827154, 2857954  (xmin, xmax, ymin, ymax)
crs        : +proj=utm +zone=40 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 
source     : memory
names      : layer 
values     : 0, 2  (min, max)

> ab=overlay(a,b,fun=ergba)
> plot(ab)
> c=rr_list[[3]]
> c
class      : RasterLayer 
dimensions : 616, 891, 548856  (nrow, ncol, ncell)
resolution : 50, 50  (x, y)
extent     : 592280.5, 636830.5, 2827154, 2857954  (xmin, xmax, ymin, ymax)
crs        : +proj=utm +zone=40 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 
source     : C:/Users/asus/Documents/R2/landuse-raster/2003R/2003raster.tif 
names      : X2003raster 
values     : 1, 11  (min, max)
attributes :
  ID          landuse
from:  1 agriculture land
to : 11     acquaculture

> levelplot(c==3)
> c1=levelplot(c==3)
> plot(ab)
> a1b1=overlay(a1,b1,fun=ergba)
Error in (function (classes, fdef, mtable)  : 
            unable to find an inherited method for function ‘overlay’ for signature ‘"trellis", "trellis"’
          > bc=overlay(b,c,fun=ergba)
          > plot(bc)
          > ergba=function(x,y,code=3){2*as.numeric(x==code)+as.numeric(y==code)}
          > ab=overlay(a,b,fun=ergba)
          > plot(ab)
          > bc=overlay(b,c,fun=ergba)
          > plot(bc)
          > d=rr_list[[4]]
          > cd=overlay(c,d,fun=ergba)
          > plot(cd)
          > plot(bc)
          > e=rr_list[[5]]
          > de=overlay(d,e,fun=ergba)
          > plot(de)
          > f=rr_list[[6]]
          > ef=overlay(e,f,fun=ergba)
          > help("rename")
          > rename(a,1987)
          Error in UseMethod("rename_") : 
            no applicable method for 'rename_' applied to an object of class "c('RasterLayer', 'Raster', 'RasterLayerOrNULL', 'BasicRaster')"
          > View(a)
          > View(a)
          > View(a)
          > file.rename(a,1987)
          Error in file.rename(a, 1987) : invalid 'from' argument
          > file.rename(from = a, to=1987)
          Error in file.rename(from = a, to = 1987) : invalid 'from' argument
          > help("file.rename")
          > file.rename(from=a, to=1987)
          Error in file.rename(from = a, to = 1987) : invalid 'from' argument
          > file.rename("from=a", "to=1987")
          [1] FALSE
          > file.rename(from = a, to = 1987)
          Error in file.rename(from = a, to = 1987) : invalid 'from' argument
          > file.rename(from = "a", to = "1987")
          [1] FALSE