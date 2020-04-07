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

## FIXME: do we still need this stuff?
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
r2 <- make_categorical(aggregate(rr_list[[1]],fact=6,fun=modal),
                       rat=get_rat(rr_list[[1]]))
levelplot(r2)
dd <- as_tibble(as.data.frame(r2))
table(dd$landuse)

## too clever:
##   as.numeric converts a logical variable to 0 or 1
##   2*as.numeric(x=="erg")  -> 2 for erg, or 0 for not-erg
##   as.numeric(y=="erg)     -> 1 for erg, or 0 for not-erg
## so if x and y are both erg -> 3  ("both")
## if x is erg and y is not   -> 2  ("lost")
## if x is not-erg and y is erg -> 1 ("gained")
## if neither is erg -> 0 ("neither")
before_after <- function(x,y,code=3) {
    2*as.numeric(x==code)+as.numeric(y==code)
}

## 3=both, 2=before, 1=after, 0=neither
before_after(c(3,10),c(10,3))
## test: work on re-categorizing ...
## BMB: not sure this works yet ...
r3 <- overlay(rr_list[["2003"]], rr_list[["2008"]],
              fun = before_after)

## BMB: FIXME: make this categorical again, with
##   0 -> "neither", 1 -> "gained", 2->"lost"
levelplot(r3)

## make this into a long-format tibble
change1 <- as_tibble(as.data.frame(rasterToPoints(r3)))

##slope and aspect
## FIXME: this is a repeat from above.  We should clean up!
slope <- terrain(dem, opt="slope", unit="radians", neighbors=8)
levelplot(slope)
aspect <- terrain(dem, opt="aspect", unit="radians", neighbors=8)
levelplot(aspect)


a <- rr_list[[1]]
##focal with matrix 3*3 with sum (default)
## BMB: numeric operations on categorical rasters don't usually make sense
prop <- focal(a,matrix(1/9,nrow=3,ncol=3))
##focal with matrix 3*3 with modal (BMB: not mode!)
## using 1/9 as weights instead of 1 as weights gives weird answers
## (it divides all of the numbers, which correspond to land-use types,
## by 9)
## I don't think  modal is what we actually want ...
prop2 <- focal(a,matrix(1,nrow=3,ncol=3), fun=modal)
## BMB: 
##data frame of 1987 raster layer (recived error)
xx <- as_tibble(as.data.frame(rasterToPoints(prop2)))
## BMB: we've lost the categorical labels again ...
head(xx)
table(xx$layer)
str(xx$layer)
## make it back into a categorical variable
xx$layer <- factor(xx$layer,
                   levels=as.numeric(landuse_cats), ## numeric values
                   ## use labels from the land use categories
                   labels=levels(landuse_cats)
)
str(xx$layer)
table(xx$layer)

xx_lost <- (full_join(xx,change1,by=c("x","y"))
            ## keep only points where there is change 
            ## %>% filter(layer.y %in% c(1,2))
            %>% filter(layer.y %in% c(2,3))
)
xx_lost


## BMB::as.data.frame.matrix is not what we want;
## we need the data in long format (one row per pixel)
## the NaN values are there because they represent the
## 'undefined' corners of the map

## don't do this! for illustration only; might as well leave it as a raster
xx2 <- as.data.frame.matrix(prop2)
image(as.matrix(xx2)) ## rotated, ugly
<<<<<<< HEAD
<<<<<<< HEAD
before_after <- function(x,y) {2*as.numeric(x=="erg")+as.numeric(y=="erg")}
before_after(c("erg","other"),c("other","erg"))
ergba=function(x,y){as.numeric(x=="erg")+as.numeric(y=="erg")}
=======
>>>>>>> 01e37f4b6b55d2a2ef55e1a77a6c1b85cba78815

# a=1987,b=1197,c=2003,d=2008,e=2014,f=2018
a=rr_list[[1]]
b=rr_list[[2]]
c=rr_list[[3]]
d=rr_list[[4]]
e=rr_list[[5]]
f=rr_list[[6]]

#Change in the erg, every two consecutive years
change=function(x,y,code=3) {
    2*as.numeric(x==code)+as.numeric(y==code)
}
abchange=overlay(a,b,fun=change)
levelplot(abchange)
bcchange=overlay(b,c,fun=change)
levelplot(bcchange)
cdchange=overlay(c,d,fun=change)         
levelplot(cdchange)
dechange=overlay(d,e,fun=change)
levelplot(dechange)
efchange=overlay(e,f,fun=change)
<<<<<<< HEAD
plot(efchange)
#PLOT_GRID(I think it used for vector maps? I think I have to use grid.raster)
plot_grid(abchange,labels = "AUTO")
Warning message:
In as_grob.default(plot) :
Cannot convert object of class RasterLayer into a grob.
plot_ab <- levelplot(abchange) 
plot_ab
plot_cd <- levelplot(cdchange)
plot_cd
plot_bc <- levelplot(bcchange)
plot_bc
plot_de <- levelplot(dechange)
plot_de
plot_ef <- levelplot(efchange)
plot_ef
#H-P: all plots are correct but plot grid result is rotated?
plot_grid(plot_ab,plot_cd,ncol=1)
plot_grid(plot_ab,plot_cd,plot_bc,plot_de,plot_ef) 
#focal modal(I have to compute neighburs value just around dunes?)
af=focal(a==3, matrix(1/9,nrow=3,ncol=3), fun=modal)
as_tibble(as.data.frame(rasterToPoints(af)))
#focal mean
af=focal(a==3, matrix(1, nrow=3, ncol=3), fun=mean)
bf=focal(b==3, matrix(1, nrow=3, ncol=3), fun=mean)
cf=focal(c==3, matrix(1, nrow=3, ncol=3), fun=mean)
df=focal(d==3, matrix(1, nrow=3, ncol=3), fun=mean)
ef=focal(e==3, matrix(1, nrow=3, ncol=3), fun=mean)
ff=focal(f==3, matrix(1, nrow=3, ncol=3), fun=mean)

##change rater to point A matrix with three columns: x, y, and v (value)
af2=rasterToPoints(af)
bf2=rasterToPoints(bf)
cf2=rasterToPoints(cf)
df2=rasterToPoints(df)
ef2=rasterToPoints(ef)
ff2=rasterToPoints(ff)

##tibble A data frame is simply a matrix, but can have columns with different types
tibbleaf2=as_tibble(as.data.frame(af2))
tibblebf2=as_tibble(as.data.frame(bf2))
tibblecf2=as_tibble(as.data.frame(cf2))
tibbledf2=as_tibble(as.data.frame(df2))
tibbleef2=as_tibble(as.data.frame(ef2))
tibbleff2=as_tibble(as.data.frame(ff2))

##histogram $:specefic
hist(tibbleaf2$layer)
hist(tibblebf2$layer)
hist(tibblecf2$layer)
hist(tibbledf2$layer)
hist(tibbleef2$layer)
hist(tibbleff2$layer)

##table $:specific
table(tibbleaf2$layer)
table(tibblebf2$layer)
table(tibblecf2$layer)
table(tibbledf2$layer)
table(tibbleef2$layer)
table(tibbleff2$layer)

##table without 0 value
table(tibbleaf2$layer[tibbleaf2$layer>0]) 
table(tibblebf2$layer[tibblebf2$layer>0])
table(tibblecf2$layer[tibblecf2$layer>0])
table(tibbledf2$layer[tibbledf2$layer>0])
table(tibbleef2$layer[tibbleef2$layer>0])
table(tibbleff2$layer[tibbleff2$layer>0])