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
library(broom)
library(ggplot2)

##change during time in erg cells 

##function with two mode(x==erg, y=other, not + together=2 before be erg)
## Change in erg over two consecutive maps
## code=3 corresponds to erg
## x=before, y=after
## not-erg=0, erg=1
## 0: not-erg before and after (no gain)
## 1: not-erg before, erg after (gain)
## 2: erg before, not-erg after (loss)
## 3: erg before and after (no loss)
change <- function(x,y,code=3) {
    2*as.numeric(x==code)+as.numeric(y==code)
}


## converts from raster -> point matrix -> data frame -> tibble
conv_tbl <- function(x,newname=NULL) {
    x2 <- tibble(as.data.frame(rasterToPoints(x)))
    ## fix column names (why??)
    names(x2)[1:2] <- c("x","y")
    if (!is.null(newname)) {
        names(x2) <- c("x","y",newname)
    }
    return(x2)
}

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

##list the descrip of maps withoiut any repeat(unique) 
sort(unique(unlist(all_descrip)))

## only reading the landuse rasters
rasterfiles <- list.files(pattern="*.tif$",recursive=TRUE)

## reading landuse raster file,leave out DEM file
years <- parse_number(rasterfiles)
years <- years[years>1900] ## leave out DEM file

## rr_list
##reading all of the raster files into a list with classess
rr_list <- map(years, get_categorical_raster, list_cats=TRUE)

## set the raster names equal to the years
names(rr_list) <- years 

##dem
dem <- raster("dem/Extract_dem11.tif")

## https://gis.stackexchange.com/questions/154276/reprojecting-raster-from-lat-lon-to-utm-in-r
## dem_reproj <- projectRaster(dem, crs=crs(rr_list[[1]]))
##change project system of dem to landuse raster 1987
demR <- projectRaster(dem, rr_list[[1]])

## for finding max and min x and y
extent_dem <- extent(dem)

## cropping/extent adjustment; this isn't necessary/doesn't do what we want
## rr_crop_list <- map(rr_list, ~setExtent(., extent_dem))
## dem_crop <- crop(dem,extent(rr_list[[1]]))
## border <- read_sf("border/border.shp")
## rr_crop_list2 <- map(rr_list, ~crop(., border))
## extent(border)
## extent(rr_list[[1]])
## identical(extent(rr_crop_list[[1]]), extent(dem))
## head(sort(conv_tbl(rr_crop_list[[1]])$x),2)

## compute slope and aspect
slope <- terrain(demR, opt="slope", unit="radians", neighbors=8)
levelplot(slope)
aspect <- terrain(demR, opt="aspect", unit="radians", neighbors=8)
levelplot(aspect)

## there are more rows in the DEM than in the slope
## ? we can't compute slope on the edges
nrow(conv_tbl(demR))
nrow(conv_tbl(slope))

##converts slope and aspect rasters into a tibble
slope_tbl  <- conv_tbl(slope)
aspect_tbl <- conv_tbl(aspect)
dem_tbl <- conv_tbl(demR)

comb_terrain <- full_join(aspect_tbl,slope_tbl,by=c("x","y"))

## try to match up terrain (derived from DEM) with first 1987 landuse map; do the x and y match?
tmp <- conv_tbl(rr_list[["1987"]])
combaf2sloas=full_join(comb_terrain, tmp, by=c("x","y"))
nrow(combaf2sloas)
nrow(comb_terrain)
nrow(tmp)
## we have about the same number of rows in landuse and terrain and combination, but not exactly
##  ??? different numbers of NAs ???

## draw all of the raster maps
## (2 rows, 3 columns)
plots <- map(rr_list,levelplot,colorkey=FALSE)

##draw all plots in one sheet
plot_grid(plotlist=plots)

##climate
clim_data <-  read_excel("climate/climate_data.xlsx", col_names=TRUE)

##crosstab
crosstab(stack(rr_list[[1]],rr_list[[2]]))


##use function data frame to all raster data
rr_tbl <- map(rr_list, conv_tbl, newname="landuse")

##change(0=no dune,1=after dune,2=before dune,3=before and after dune)
rr_before=rr_list[1:5] 
rr_after= rr_list[2:6]

rr_changes=map2(rr_before, rr_after, ~ overlay(.x,.y,fun=change))

#plot of changes
changeplots <- map(rr_changes, levelplot, margin=FALSE)
#PLOT_GRID:all plots together
plot_grid(plotlist=changeplots)

## converts each of the before and after change rasters into a tibble
## RENAME HERE
##'names' attribute [3] must be the same length as the vector [1]
rr_change_tbl <- map(rr_changes, conv_tbl, newname="change")

#neighburs,focal mean,focal=0(no dune neighbour) 1(all the cells are dune)
rr_focal=map(rr_list,
             ~ focal(.==3, matrix(1, nrow=3, ncol=3), fun=mean))


## converts each of the neighbers rasters into a tibble
## RENAME HERE
rr_focal_tbl <-  map(rr_focal, conv_tbl, newname="prop_dune_nbrs")

##histogram
par(mfrow=c(2,3)) ## 2 rows x 3 cols
map(rr_focal_tbl,~ hist(.$prop_dune_nbrs))

## tables without zeros
map(rr_focal_tbl, ~ table(.$prop_dune_nbrs[.$prop_dune_nbrs>0]))
    
##length
length(rr_tbl)  ## all of our landuse maps, as tibbles (only 6)
length(rr_change_tbl) ## all of our change maps, as tibbles (only 5)

## rename the single column in each original landuse tibble
rr_tbl <- map(rr_tbl, ~setNames(.,c("x","y","landuse")))

## 1-original land use values combined with slope and aspect(comb_terrain)
rr_points2 <- map(rr_tbl,
           ## ~ (tilde) says "interpret the rest of this line as a command;
           ## . (dot) will be where we substitute one of the items from the
           ##   list
           ~ full_join(., comb_terrain, by=c("x","y")))

## 2-combine with neighburs value- focal map and rename
rr_points3 <- map2(rr_points2, rr_focal_tbl,
                   ~ full_join(.x, .y, by=c("x","y")))


## 6 landscapes
length(rr_points3) 
## 5 land-use change maps
length(rr_change_tbl)

##rr_points4= what happens if we try to combine rr_changes-tbl with rr_points3?
## drop the last landscape
## leave out last map because we don't have changes for it
rr_points4 <- rr_points3[-length(rr_points3)] 
rr_points5 <- map2(rr_points4, rr_change_tbl, ~full_join(.x, .y, by=c("x","y")))


dd <- (rr_points5[["1987"]]
    ## only want points that were erg before
    ## only values that have aspect data
    %>% drop_na(aspect)
    ## don't need these columns any more
    %>% select(-c(x,y))
)

## ANALYSIS 1: analyzing

dd_loss <- (dd
    ## if change==3 we had erg both before
    ##  and after, so this is 0 for 'no change'
    ## otherwise 1 for 'lost erg'
    %>% filter(change %in% c(2,3))
    %>% mutate(change=ifelse(change==3,0,1))
)

dd_gain <- (dd
    ## if change==3 we had erg both before
    ##  and after, so this is 0 for 'no change'
    ## otherwise 1 for 'lost erg'
    %>% filter(change %in% c(0,1))
)

## you could add proportion of nearby vegetation

table(dd$landuse)
logist1 <- glm(change~ slope+aspect+prop_dune_nbrs, data = dd_loss, family = "binomial")
summary(logist1)


## look at nnet::multinom function to fit multinomial response model

##H-P:For the first staff if I have to done it based on the previous code (for example for buildup area) I have to do follow steps:

rr_focalbuild=map(rr_list,~ focal(.==12, matrix(1, nrow=3, ncol=3), fun=mean))
rr_focal_tblbuild <- map(rr_focalbuild, conv_tbl, newname="prop_build_nbrs")
rr_points33 <- map2(rr_points3, rr_focal_tblbuild, ~ full_join(.x, .y, by=c("x","y")))

##in this way when I used.==12, just consider buildup area without regarding to ergs, but I think I should to find a way that consider neighbors values(vegetation, buildup area, erg) just around erg cells(3 separate classes just around ergs.
