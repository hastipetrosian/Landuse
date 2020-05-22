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
library(ggplot2); theme_set(theme_bw())
library(dotwhisker)

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
conv_tbl <- function(x,newname=NULL,rescale=NA) {
    x2 <- tibble(as.data.frame(rasterToPoints(x)))
    ## fix column names (why??)
    names(x2)[1:2] <- c("x","y")
    if (!is.na(rescale)) {
        x2[,3] <- x2[,3]/rescale
    }
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

##neighburs,focal mean,focal=0(no dune neighbour) 1(all the cells are dune)


w <- matrix(1, nrow=3, ncol=3)
w[2,2] <- 0   ## center cell doesn't count!
rr_focal <- map(rr_list, ~ focal(.==3, w, sum))

levelplot(rr_focal[[1]])


## converts each of the neighbers rasters into a tibble
## and divide sum of nbrs by 8 to get proportion
rr_focal_tbl <-  map(rr_focal, conv_tbl, newname="prop_erg_nbrs", rescale=8)

## FIXME: do the same thing for prop_build_nbrs

tt <- table(rr_focal_tbl[[1]]$prop_erg_nbrs)
##histogram
par(mfrow=c(2,3)) ## 2 rows x 3 cols
map(rr_focal_tbl,~ hist(.$prop_erg_nbrs))

## tables without zeros
map(rr_focal_tbl, ~ table(.$prop_erg_nbrs[.$prop_erg_nbrs>0]))
    
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

##buildup area
rr_focalbuild=map(rr_list,~ focal(.==12, w, fun=sum))
rr_focal_tblbuild <- map(rr_focalbuild, conv_tbl, newname="prop_build_nbrs", rescale=8)
## we don't need the last one (because there's no change to compare it to)
rr_focal_tblbuild1 <- rr_focal_tblbuild[-length(rr_focal_tblbuild)]

##vegetation
rr_focalveg=map(rr_list,~ focal(.==10, w, fun=sum))
rr_focal_tblveg <- map(rr_focalveg, conv_tbl, newname="prop_veg_nbrs", rescale=8)
rr_focal_tblveg1=rr_focal_tblveg[-length(rr_focal_tblveg)]

##river bed vegetation
rr_focalriveg=map(rr_list,~ focal(.==7, w, fun=sum))
rr_focal_tblriveg <- map(rr_focalriveg, conv_tbl, newname="prop_riveg_nbrs", rescale=8)
rr_focal_tblriveg1=rr_focal_tblriveg[-length(rr_focal_tblriveg)]

##settlement
rr_focalset=map(rr_list,~ focal(.==9, w, fun=sum))
rr_focal_tblset <- map(rr_focalset, conv_tbl, newname="prop_settle_nbrs", rescale=8)
rr_focal_tblset1=rr_focal_tblset[-length(rr_focal_tblset)]

##agriculture
rr_focalagri=map(rr_list,~ focal(.==1, w, fun=sum))
rr_focal_tblagri <- map(rr_focalagri, conv_tbl, newname="prop_agri_nbrs", rescale=8)
rr_focal_tblagri1=rr_focal_tblagri[-length(rr_focal_tblagri)]

## table(xx$prop_build_nbrs)
## xx <- rr_focal_tblbuild1[["2014"]]
rr_points6 <- map2(rr_points5, rr_focal_tblbuild1, ~ full_join(.x, .y, by=c("x","y")))
rr_points7 <- map2(rr_points6, rr_focal_tblveg1, ~ full_join(.x,.y, by=c("x","y")))
rr_points8 <- map2(rr_points7, rr_focal_tblriveg1, ~ full_join(.x,.y, by=c("x","y")))
rr_points9 <- map2(rr_points8, rr_focal_tblset1, ~ full_join(.x,.y, by=c("x","y")))
rr_points10 <- map2(rr_points9, rr_focal_tblagri1, ~ full_join(.x,.y, by=c("x","y")))

## running everything for one set of changes

run_logist_regression <- function(dd=rr_points10[["2014"]],
                                  scale=FALSE) {
    dd <- (dd
        ## only want points that were erg before
        ## only values that have aspect data
        %>% drop_na(aspect)
        ## don't need these columns any more
        %>% select(-c(x,y))
    )

    ## ANALYSIS 1: analyzing
    ##just consider the cells that are now erg-without2=before
    dd_loss <- (dd
        ## if change==3 we had erg both before
        ##  and after, so this is 0 for 'no change'
        ## otherwise 1 for 'lost erg'
        %>% filter(change %in% c(2,3))
        %>% mutate(change=ifelse(change==3,0,1))
    )
    ## table(dd_loss$prop_build_nbrs)
    ## table(dd_loss$prop_settle_nbrs)
    ## table(dd$landuse)
    
    ##logistic

    print(table(dd_loss$change))
    ## logist1 <- glm(change~ slope+aspect+prop_erg_nbrs+prop_veg_nbrs+prop_build_nbrs+prop_riveg_nbrs+prop_settle_nbrs+prop_agri_nbrs, data = dd_loss, family = "binomial")
    ## . = everything

    predvars <- dd_loss[,setdiff(names(dd_loss),c("landuse","change"))]
    if (scale) {
        sdvec <- 2*sapply(predvars,sd,na.rm=TRUE)
        predvars <- scale(predvars,
                          center=TRUE,
                          scale=sdvec)
        ## all-zero or constant columns will mess things up
        okvars <- colSums(!is.na(predvars))
        predvars <- predvars[,okvars>0]
    }
    dd_loss <- data.frame(change=dd_loss$change, predvars)
    logist1 <- glm(change~ . , data = dd_loss, family = "binomial")
    return(logist1)
    
}

logist1 <- run_logist_regression()
summary(logist1)
logist1S <- run_logist_regression(scale=TRUE)


## leave the first set of changes out
## since we only lose 4/18K pixels
logist_list <- map(rr_points10[-1], run_logist_regression) ## do all fits at once

## draw the plots
plot1 <- dwplot(logist_list)
plot1 + scale_x_continuous(limits=c(NA,10))
## zoom in
plot1 + scale_x_continuous(limits=c(-3,3)) + geom_vline(lty=2,xintercept=0)

## scaled by 2SD by default
dwplot(logist1) + geom_vline(lty=2,xintercept=0)
## or we can turn that off
dwplot(logist1, by_2sd=FALSE)

tidy(logist1,conf.int=TRUE)
## map_dfr() runs the function on each item in the list
##  and combines the results into a data frame
param_tab <- purrr::map_dfr(logist_list,tidy,.id="year",
                            conf.int=TRUE) %>% arrange(term)
View(param_tab)
## look at nnet::multinom function to fit multinomial response model


###
library(raster)
library(purrr)
library(cowplot)
years <- c(1987,1997,2003,2008,2014,2018)

##change name of included map
fn <- sprintf("Average_temperature/%dAT.tif",years)
pr<- sprintf("precipitation/%dpr.tif",years)

AT_list <- map(fn, raster)
##H-P:Error in .local(.Object, ...) : 
##Error in .rasterObjectFromFile(x, band = band, objecttype = "RasterLayer",  : 
##Cannot create a RasterLayer object from this file. (file does not exist)
PR_list <- map(pr, raster)

pr_ATplots <- map(AT_list, levelplot, margin=FALSE)
PRplots <- map(pr_list,~levelplot, margin=FALSE)
#PLOT_GRID:all plots together
plot_grid(plotlist=ATplots)
plot_grid(plotlist=PRplots)

## experimenting with temperature

x <- 1:365
y <- 26 + 10*cos(2*pi*x/365)  ## average temperatures from 16 to 36 C
plot(x,y)
mean(y) ## 26
## standard error
sd(y)/sqrt(365)
## [1] 0.37
