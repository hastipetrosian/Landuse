# landuse 2018
## library(geosample)  ## 
library(sf)
library(rgeos)
library(readxl)
library(raster)
library(tidyverse) ## includes purrr, readr, dplyr, ...
library(fasterize)  ## may need to install this
library(rasterVis)
library(foreign)
library(cowplot)
library(lulcc)
library(sp)

do_shape_files <- FALSE
source("functions.R")

## all shape files(vectors):s
shapefiles <- list.files(pattern="*.shp$",recursive=TRUE)

## dd_list <- lapply(v2shp, read_sf)
## reading all of the files into a list
dd_list <- map(shapefiles, read_sf)

## extract the 2014 number from
## . represents the current element in the list
## key.pos and reset are necessary so we can plot all of the maps
## together (see ?plot.sf)each file name
year_vec <- parse_number(shapefiles)
names(dd_list) <- year_vec
plot(dd_list[["2014"]])
  
## set up a 2x3 grid of plots
## (2 rows, 3 columns)
op <- par(mfrow=c(2,3))  

## draw all of the vector maps
map(dd_list,~ plot(.["descrip"],key.pos=NULL,reset=FALSE))

## dd_list[[1]]["descrip"]
all_descrip <- map(dd_list, ~ sort(.["descrip"]$descrip))
sort(unique(unlist(all_descrip)))

## only reading the landuse rasters
rasterfiles <- list.files(pattern="*.tif$",recursive=TRUE)
years <- parse_number(rasterfiles)
years <- years[years>1900] ## leave out DEM file

## rr_list
##reading all of the files into a list
rr_list <- map(years, get_categorical_raster, list_cats=TRUE)  
names(rr_list) <- years

##dem
dem <- raster("dem/Extract_dem11.tif")


## draw all of the raster maps
## (2 rows, 3 columns)
plots <- map(rr_list,levelplot,colorkey=FALSE)
plot_grid(plotlist=plots)

##climate
clim_data <-  read_excel("climate/climate_data.xlsx", col_names=TRUE)

## ObsLulcRasterStack(rr_list)  ## doesn't know what to do
rs <- ObsLulcRasterStack(rr_list,
                   pattern="[0-9]+", ## use all numbers
                   ## this only works if we only have the
                   ##  land-use rasters; if we have climate,
                   ##  DEM, etc. then we have to give just
                   ##  the vector of years
t=as.numeric(names(rr_list)))

##crosstab
crosstab(stack(rr_list[[1]],rr_list[[2]]))
crosstab(stack(rs[[1]],rs[[2]]))
crossTabulate(rs,times=c(1987,2014))

##slope and aspect
## FIXME: this is a repeat from above.  We should clean up!
slope <- terrain(dem, opt="slope", unit="radians", neighbors=8)
levelplot(slope)
aspect <- terrain(dem, opt="aspect", unit="radians", neighbors=8)
levelplot(aspect)

##focal with matrix 3*3 with sum (default)
## BMB: numeric operations on categorical rasters don't usually make sense
## ERROR
## prop <- focal(a,matrix(1/9,nrow=3,ncol=3))
##focal with matrix 3*3 with modal (BMB: not mode!)
## using 1/9 as weights instead of 1 as weights gives weird answers
## (it divides all of the numbers, which correspond to land-use types,
## by 9)
## I don't think  modal is what we actually want ...
## prop2 <- focal(a,matrix(1,nrow=3,ncol=3), fun=modal)
## BMB: 
##data frame of 1987 raster layer (recived error)
## ERROR
## xx <- as_tibble(as.data.frame(rasterToPoints(prop2)))
## BMB: we've lost the categorical labels again ...
## head(xx)
## table(xx$layer)
##str(xx$layer)
## make it back into a categorical variable
##xx$layer <- factor(xx$layer,
##                   levels=as.numeric(landuse_cats), ## numeric values
##                   ## use labels from the land use categories
##                   labels=levels(landuse_cats)
##)
## str(xx$layer)
## table(xx$layer)

## xx_lost <- (full_join(xx,change1,by=c("x","y"))
##             ## keep only points where there is change 
##             ## %>% filter(layer.y %in% c(1,2))
##             %>% filter(layer.y %in% c(2,3))
## )
## xx_lost


## BMB::as.data.frame.matrix is not what we want;
## we need the data in long format (one row per pixel)
## the NaN values are there because they represent the
## 'undefined' corners of the map

## don't do this! for illustration only; might as well leave it as a raster
## xx2 <- as.data.frame.matrix(prop2)
## image(as.matrix(xx2)) ## rotated, ugly
 
before_after <- function(x,y) {2*as.numeric(x=="erg")+as.numeric(y=="erg")}
before_after(c("erg","other"),c("other","erg"))
ergba=function(x,y){as.numeric(x=="erg")+as.numeric(y=="erg")}


## BMB: PLEASE CLEAN THIS UP
# a=1987,b=1197,c=2003,d=2008,e=2014,f=2018
a=rr_list[[1]]
b=rr_list[[2]]
c=rr_list[[3]]
d=rr_list[[4]]
e=rr_list[[5]]
f=rr_list[[6]]
##raster to point of all maps
rr_points= map(rr_list, ~ as_tibble(rasterToPoints(.))) 

##change
## this includes all but the last land use map
## this includes all but the last land use map  (2018)
## the first element of rr_before is the 1987 map
## the first element of rr_after is the 1997 map (i.e. the second map)
rr_before=rr_list[1:5] 

## this includes all but the firstland use map(1987)
## this includes all but the first land use map
rr_after= rr_list[2:6]

#Change in the erg, every two consecutive years
change=function(x,y,code=3) {
    2*as.numeric(x==code)+as.numeric(y==code)
}
abchange=overlay(a,b,fun=change)
bcchange=overlay(b,c,fun=change)
cdchange=overlay(c,d,fun=change)         
dechange=overlay(d,e,fun=change)
efchange=overlay(e,f,fun=change)

#map2=Map over multiple inputs simultaneously.
## the map2() function runs a command on the elements of two lists
# .=all of the data with same length ~ =formula
##H-P:I have received a error?
## BMB: this works for me (when running beginning to end)
rr_changes=map2(rr_before, rr_after, ~ overlay(.x,.y,fun=change)) 


#PLOT_GRID
plot(efchange)
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
levelplot(rr_changes[[1]])
levelplot(rr_changes[[2]])
levelplot(rr_changes[[3]])
levelplot(rr_changes[[4]])
levelplot(rr_changes[[5]])

##all plots are correct but plot grid result is rotated
plot_grid(plot_ab,plot_cd,ncol=1)
plot_grid(plot_ab,plot_cd,plot_bc,plot_de,plot_ef) 

#focal mean,focal=0(no dune neighbour) 1(all the cells are dune)
af=focal(a==3, matrix(1, nrow=3, ncol=3), fun=mean)
bf=focal(b==3, matrix(1, nrow=3, ncol=3), fun=mean)
cf=focal(c==3, matrix(1, nrow=3, ncol=3), fun=mean)
df=focal(d==3, matrix(1, nrow=3, ncol=3), fun=mean)
ef=focal(e==3, matrix(1, nrow=3, ncol=3), fun=mean)
ff=focal(f==3, matrix(1, nrow=3, ncol=3), fun=mean)
##H-P:I recived a error?
## BMB: R is case sensitive, Map is different from map
rr_focal=map(rr_list, ~ focal(.==3, matrix(1, nrow=3, ncol=3), fun=mean))

##point
##change raster to point A matrix with three columns: x, y, and v (value)
## convert from raster to points
##map=Apply a function to each element of a vectoraf2=rasterToPoints(af)
abchange2=rasterToPoints(abchange)
bcchange2=rasterToPoints(bcchange)
cdchange2=rasterToPoints(cdchange)
dechange2=rasterToPoints(dechange)
efchange2=rasterToPoints(efchange)

af2=rasterToPoints(af)
bf2=rasterToPoints(bf)
cf2=rasterToPoints(cf)
df2=rasterToPoints(df)
ef2=rasterToPoints(ef)
ff2=rasterToPoints(ff)

##H-P:I have received a error?
## map() is a way to run the same operation on all of the elements
## of a list at the same time
## BMB: I didn't get an error. What error did you get?
rr_changepoints <- map(rr_changes, ~as_tibble(rasterToPoints(.)))

##tibble
##make tbl=tibble A data frame is simply a matrix, but can have columns with different types
tibbleabchange2=as_tibble(as.data.frame(abchange2))
tibblebcchange2=as_tibble(as.data.frame(bcchange2))
tibblecdchange2=as_tibble(as.data.frame(cdchange2))
tibbledechange2=as_tibble(as.data.frame(dechange2))
tibbleefchange2=as_tibble(as.data.frame(efchange2))

tibbleaf2=as_tibble(as.data.frame(af2))
tibblebf2=as_tibble(as.data.frame(bf2))
tibblecf2=as_tibble(as.data.frame(cf2))
tibbledf2=as_tibble(as.data.frame(df2))
tibbleef2=as_tibble(as.data.frame(ef2))
tibbleff2=as_tibble(as.data.frame(ff2))

## BMB: moved this here, otherwise it won't run
##tables of slope and aspect
## BMB: these need to be tibbles or data frames to work with full_join() later
## unfortunately this has to be done in exactly this order

conv_tbl <- function(x) tibble(as.data.frame(rasterToPoints(x)))
slope2=conv_tbl(slope)
aspect2=conv_tbl(aspect)
tabas2=table(aspect2)
tabslo2=table(slope2)

##histogram $:specefic
hist(tibbleaf2$layer)
hist(tibblebf2$layer)
hist(tibblecf2$layer)
hist(tibbledf2$layer)
hist(tibbleef2$layer)
hist(tibbleff2$layer)

##table $:specific=make table
table(tibbleaf2$layer) 

##table $:specific
table(tibblebf2$layer)
table(tibblecf2$layer)
table(tibbledf2$layer)
table(tibbleef2$layer)
table(tibbleff2$layer)


##table without 0 value = at least one dune neighbours
table(tibbleaf2$layer[tibbleaf2$layer>0]) 
table(tibblebf2$layer[tibblebf2$layer>0])
table(tibblecf2$layer[tibblecf2$layer>0])
table(tibbledf2$layer[tibbledf2$layer>0])
table(tibbleef2$layer[tibbleef2$layer>0])
table(tibbleff2$layer[tibbleff2$layer>0])

## figure out how to do everything you want (i.e. combine all the different
## pieces: change, topography, neighbourhood, climate?) with a single map
## e.g. the 1987 - 1997 change
## full join aspect2,slope2,possible with tbl format not table format
##1987
comaspectslope=full_join(aspect2,slope2,by=c("x","y"))
compaf2sloas=full_join(comaspectslope,tibbleaf2,by=c("x","y"))
compall=full_join(compaf2sloas, tibbleabchange2, by=c("x","y"))


## str is "STRucture"
str(slope2)

##class
class(slope2) ## NOT a matrix

##lenght
length(rr_points)  ## all of our landuse maps, as tibbles (only 6)
length(rr_changepoints) ## all of our change maps, as tibbles (only 5)

## rr_list is a list of raster objects (one for each land-use map)
comb_terrain <- full_join(aspect2,slope2,by=c("x","y"))
rr_points2 <- map(rr_points,
           ## ~ (tilde) says "interpret the rest of this line as a command;
           ## . (dot) will be where we substitute one of the items from the
           ##   list
           ~ full_join(., comb_terrain, by=c("x","y")))

## what happens if we try to combine rr_changes with rr_points?
## map2(rr_points, rr_changepoints, ~full_join(.x, .y, by=c("x","y")))

## leave out last map because we don't have changes for it
rr_points3 <- rr_points2[-length(rr_points2)]
rr_comb2 <- map2(rr_points3, rr_changepoints, ~full_join(.x, .y, by=c("x","y")))

## search for "purrr package map"
