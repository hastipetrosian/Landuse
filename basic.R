
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

## reading all of the files into a list
dd_list <- map(shapefiles, read_sf)

## set up a 3x3 grid of plots
## (3 rows, 3 columns)
op <- par(mfrow=c(3,3))  

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
names(rr_list)

##dem
dem <- raster("dem/Extract_dem11.tif")


## draw all of the raster maps
## (2 rows, 3 columns)
plots <- map(rr_list,levelplot,colorkey=FALSE)
plot_grid(plotlist=plots)

##climate
clim_data <-  read_excel("climate/climate_data.xlsx", col_names=TRUE)

##crosstab
crosstab(stack(rr_list[[1]],rr_list[[2]]))

##slope and aspect
slope <- terrain(dem, opt="slope", unit="radians", neighbors=8)
levelplot(slope)
aspect <- terrain(dem, opt="aspect", unit="radians", neighbors=8)
levelplot(aspect)

##change during time in erg cells 
##before-after function
before_after <- function(x,y) {2*as.numeric(x=="erg")+as.numeric(y=="erg")}
##function with two mode(x==erg, y=other, not + together=2 before be erg)
##(x=other=0+1=1 after be erg)
before_after(c("erg","other"),c("other","erg"))

## converts from raster -> matrix -> data frame -> tibble
conv_tbl <- function(x,newname=NULL) {
    x2 <- tibble(as.data.frame(rasterToPoints(x)))
    if (!is.null(newname)) {
        names(x2) <- c("x","y",newname)
    }
    return(x2)
}
##use function data frame to all raster data
##H-p:I got an error
##Error during wrapup: 'names' attribute [3] must be the same length as the vector [1]
rr_tbl <- map(rr_list, conv_tbl, newname="landuse")

##change
rr_before=rr_list[1:5] 
rr_after= rr_list[2:6]

#function Change in the erg, every two consecutive years
change=function(x,y,code=3) {
    2*as.numeric(x==code)+as.numeric(y==code)
}

rr_changes=map2(rr_before, rr_after, ~ overlay(.x,.y,fun=change))

#plot of changes

changeplots <- map(rr_changes, levelplot,
                   margin=FALSE)
#PLOT_GRID:all plots together
plot_grid(plotlist=changeplots)

## converts each of the before and after change rasters into a tibble
## RENAME HERE
##H-p:I got an error
##Error in attr(x, "names") <- as.character(value) : 
##'names' attribute [3] must be the same length as the vector [1]
rr_change_tbl <- map(rr_changes, conv_tbl, newname="change")

#neighburs,focal mean,focal=0(no dune neighbour) 1(all the cells are dune)
rr_focal=map(rr_list,
             ~ focal(.==3, matrix(1, nrow=3, ncol=3), fun=mean))


## converts each of the neighbers rasters into a tibble
## RENAME HERE
##H-p:I got an error
## Error in attr(x, "names") <- as.character(value) : 
##names' attribute [3] must be the same length as the vector [1]
rr_focal_tbl <-  map(rr_focal, conv_tbl, newname="prop_dune_nbrs")

##converts slope and aspect rasters into a tibble
slope2=conv_tbl(slope)
aspect2=conv_tbl(aspect)


##histogram $:specefic
par(mfrow=c(2,3)) ## 2 rows x 3 cols
##H-P:I got an error
##Error in hist.default(.$layer) : 'x' must be numeric In addition: Warning message:
##Unknown or uninitialised column: 'layer'. 
map(rr_focal_tbl,~ hist(.$layer))

## tables without zeros
map(rr_focal_tbl, ~ table(.$layer[.$layer>0]))
    

##H-p:I got an error?
##Error: `by` can't contain join column `x`, `y` which is missing from LHS
comaspectslope=full_join(aspect2,slope2,by=c("x","y"))
compaf2sloas=full_join(comaspectslope,rr_focal_tbl[["1987"]],by=c("x","y"))

## x and y values are not matching up!
compall=full_join(compaf2sloas, rr_change_tbl[["1987"]], by=c("x","y"))
names(compall)
##H-P:for matching map there are two ways:
##first=introduce a vector map (name is border) and crop rasters with it
##I got an error
border=read_sf("border/border.shp")
crop(a,border)
#Error in (function (classes, fdef, mtable)  : 
            unable to find an inherited method for function ‘crop’ for signature ‘"list"’
##second=change extent of rasters map with dem extent
##I got a same error
extent=extent(dem)            
setExtent(a,extent,keepres = TRUE)
#Error in (function (classes, fdef, mtable)  : 
                        unable to find an inherited method for function ‘xres’ for signature ‘"list"’
                      >             

##lenght
length(rr_tbl)  ## all of our landuse maps, as tibbles (only 6)
length(rr_change_tbl) ## all of our change maps, as tibbles (only 5)

## rename the single column in each landuse tibble
rr_tbl <- map(rr_tbl, ~setNames(.,c("x","y","landuse")))
## rr_list is a list of raster objects (one for each land-use map)
comb_terrain <- full_join(aspect2,slope2,by=c("x","y"))
## original land use values combined with slope and aspect
rr_points2 <- map(rr_tbl,
           ## ~ (tilde) says "interpret the rest of this line as a command;
           ## . (dot) will be where we substitute one of the items from the
           ##   list
           ~ full_join(., comb_terrain, by=c("x","y")))

## combine with focal map and rename
rr_points3 <- map2(rr_points2, rr_focal_tbl,
                   ~ full_join(.x, .y, by=c("x","y")))

names(rr_points3[[1]])
## what happens if we try to combine rr_changes with rr_points?
## map2(rr_points, rr_changepoints, ~full_join(.x, .y, by=c("x","y")))
length(rr_points3) ## 6 landscapes
length(rr_change_tbl) ## 5 land-use change maps

## leave out last map because we don't have changes for it
rr_points4 <- rr_points3[-length(rr_points3)] ## drop the last landscape
rr_comb2 <- map2(rr_points4, rr_change_tbl, ~full_join(.x, .y, by=c("x","y")))

names(rr_comb2[[1]])
## search for "purrr package map"
