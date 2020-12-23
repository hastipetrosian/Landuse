## increase memory
if (.Platform$OS.type=="windows") memory.limit(1000000)

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
library(future)
library(furrr)
library(bbmle)
library(DHARMa)
library(ResourceSelection)
library(arm)
library(brglm2)
library(SparseM)
library(Hmisc)
library(rms)

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


do_shape_files <- FALSE
##all logistic formula is here
source("functions.R")

## get climate data
load("climate.RData") ## get climate data
load("winddir_out.rda") ## get winddir matrix ('comb')

##Load rr_point14 first
if (file.exists("rr_points14.RData")) {
  load("rr_points14.RData")
} else {
  
  ## all shape files(vectors):s
  shapefiles <- list.files(pattern="*.shp$",recursive=TRUE)
  
  ## reading all of the files into a list
  dd_list <- map(shapefiles, read_sf)
  
  ## set up a 3x3 grid of plots
  ## (3 rows, 3 columns)
  op <- par(mfrow=c(1,1))  
  
  ## draw all of the vector maps
  map(dd_list,~ plot(.["descrip"],key.pos=NULL,reset=FALSE))
  
  ## draw all of the raster maps
  
  all_descrip <- map(dd_list, ~ sort(.["descrip"]$descrip))
  
  ##list the descrip of maps withoiut any repeat(unique) 
  sort(unique(unlist(all_descrip)))
  
  ## only reading the landuse rasters
  rasterfiles <- list.files(pattern="*.tif$",recursive=TRUE)
  
  ## reading landuse raster file,leave out DEM file
  years <- parse_number(rasterfiles)
  years <- years[years>1900] ## leave out DEM file
  years2 <- unique(years)
  
  
  ## rr_list
  ##reading all of the raster files into a list with classess
  cat("get all rasters\n")
  rr_list <- map(years2 , get_categorical_raster, list_cats=TRUE)
  
  ## set the raster names equal to the years
  names(rr_list) <- years2 
  
  ##dem
  cat("get DEM\n")
  dem <- raster("dem/Extract_dem11.tif")
  
  ## https://gis.stackexchange.com/questions/154276/reprojecting-raster-from-lat-lon-to-utm-in-r
  ## dem_reproj <- projectRaster(dem, crs=crs(rr_list[[1]]))
  ##change project system of dem to landuse raster 1987
  crs(rr_list[[1]])
  ## lapply(rr_list, crs) ## check coord systems for all land-use maps
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
  cat("calc slope and aspect\n")
  
  slope <- terrain(demR, opt="slope", unit="radians", neighbors=8)
  levelplot(slope)
  aspect <- terrain(demR, opt="aspect", unit="radians", neighbors=8)
  levelplot(aspect)
  
  ##converts slope and aspect rasters into a tibble
  cat("convert to tibbles\n")
  
  slope_tbl  <- conv_tbl(slope)
  aspect_tbl <- conv_tbl(aspect)
  dem_tbl <- conv_tbl(demR)
  
  cat("join terrain\n")
  
  comb_terrain <- full_join(aspect_tbl,slope_tbl,by=c("x","y"))
  
  ## try to match up terrain (derived from DEM) with first 1987 landuse map; do the x and y match?
  ## tmp <- conv_tbl(rr_list[["1987"]])
  ## combaf2sloas=full_join(comb_terrain, tmp, by=c("x","y"))
  ## nrow(combaf2sloas)
  ## nrow(comb_terrain)
  ## nrow(tmp)
  ## we have about the same number of rows in landuse and terrain and combination, but not exactly
  ##  ??? different numbers of NAs ???
  
  ## (2 rows, 3 columns)
  plots <- map(rr_list,levelplot,colorkey=FALSE)
  
  ##draw all plots in one sheet (SKIP)
  ## plot_grid(plotlist=plots)
  
  ##climate
  cat("get climate data\n")
  clim_data <-  read_excel("climate/climate_data.xlsx", col_names=TRUE)
  
  ##crosstab
  ## crosstab(stack(rr_list[[1]],rr_list[[2]]))
  
  cat("convert all to tibble\n")
  ##use function data frame to all raster data
  rr_tbl <- map(rr_list, conv_tbl, newname="landuse")
  
  ##change(0=no dune,1=after dune,2=before dune,3=before and after dune)
  rr_before=rr_list[1:5] 
  rr_after= rr_list[2:6]
  
  cat("compute overlay\n")
  rr_changes=map2(rr_before, rr_after, ~ overlay(.x,.y,fun=change))
  
  ##plot of changes
  changeplots <- map(rr_changes, levelplot, margin=FALSE)
  ##PLOT_GRID:all plots together
  #plot_grid(plotlist=changeplots)
  
  ## converts each of the before and after change rasters into a tibble
  cat("convert changes to tbl\n")
  rr_change_tbl <- map(rr_changes, conv_tbl, newname="change")
  
  ##neighburs,focal mean,focal=0(no dune neighbour) 1(all the cells are dune)
  
  cat("compute focal\n")
  w <- matrix(1, nrow=3, ncol=3)
  w[2,2] <- 0   ## center cell doesn't count!
  rr_focal <- map(rr_list, ~ focal(.==3, comb, sum))
  
  ## levelplot(rr_focal[[1]])
  
  
  ## save.image("basic_save.rda")  ## save everything so we can reload it
  ## stop()
  ## load("basic_save.rda")
  ## converts each of the neighbers rasters into a tibble
  ## and divide sum of nbrs by 8 to get proportion
  cat("compute prop erg nbrs\n")

  rr_focal_tbl <-  map(rr_focal, conv_tbl, newname="prop_erg_nbrs", rescale=8)

  ## tt <- table(rr_focal_tbl[[1]]$prop_erg_nbrs)
  ## histogram
  ## par(mfrow=c(2,3)) ## 2 rows x 3 cols
  ## map(rr_focal_tbl,~ hist(.$prop_erg_nbrs))
  
  ## tables without zeros
  ## map(rr_focal_tbl, ~ table(.$prop_erg_nbrs[.$prop_erg_nbrs>0]))
  
  ##length
  ## length(rr_tbl)  ## all of our landuse maps, as tibbles (only 6)
  ## length(rr_change_tbl) ## all of our change maps, as tibbles (only 5)
  
  cat("set names\n")
  ## rename the single column in each original landuse tibble
  rr_tbl <- map(rr_tbl, ~setNames(.,c("x","y","landuse")))
  
  cat("calculating rr_points2\n")
  ## 1-original land use values combined with slope and aspect(comb_terrain)
  rr_points2 <- map(rr_tbl,
                    ## ~ (tilde) says "interpret the rest of this line as a command;
                    ## . (dot) will be where we substitute one of the items from the
                    ##   list
                    ~ full_join(., comb_terrain, by=c("x","y")))
  
  cat("calculating rr_points3\n")
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
  cat("calculating rr_points4, 5\n")
  
  rr_points4 <- rr_points3[-length(rr_points3)] 
  rr_points5 <- map2(rr_points4, rr_change_tbl, ~full_join(.x, .y, by=c("x","y")))
  
  cat("calculating land use rasters\n")
  ##buildup area
  rr_focalbuild=map(rr_list,~ focal(.==12, comb, fun=sum))
  rr_focal_tblbuild <- map(rr_focalbuild, conv_tbl, newname="prop_build_nbrs", rescale=8)
  ## we don't need the last one (because there's no change to compare it to)
  rr_focal_tblbuild1 <- rr_focal_tblbuild[-length(rr_focal_tblbuild)]
  
  ##vegetation
  rr_focalveg=map(rr_list,~ focal(.==10, comb, fun=sum))
  rr_focal_tblveg <- map(rr_focalveg, conv_tbl, newname="prop_veg_nbrs", rescale=8)
  rr_focal_tblveg1=rr_focal_tblveg[-length(rr_focal_tblveg)]
  
  ##river bed vegetation
  rr_focalriveg=map(rr_list,~ focal(.==7, comb, fun=sum))
  rr_focal_tblriveg <- map(rr_focalriveg, conv_tbl, newname="prop_riveg_nbrs", rescale=8)
  rr_focal_tblriveg1=rr_focal_tblriveg[-length(rr_focal_tblriveg)]
  
  ##settlement
  rr_focalset=map(rr_list,~ focal(.==9, comb, fun=sum))
  rr_focal_tblset <- map(rr_focalset, conv_tbl, newname="prop_settle_nbrs", rescale=8)
  rr_focal_tblset1=rr_focal_tblset[-length(rr_focal_tblset)]
  
  ##agriculture
  rr_focalagri=map(rr_list,~ focal(.==1, comb, fun=sum))
  rr_focal_tblagri <- map(rr_focalagri, conv_tbl, newname="prop_agri_nbrs", rescale=8)
  rr_focal_tblagri1=rr_focal_tblagri[-length(rr_focal_tblagri)]
  
  ##bareland
  rr_focalbare=map(rr_list,~ focal(.==2, comb, fun=sum))
  rr_focal_tblbare <- map(rr_focalbare, conv_tbl, newname="prop_bare_nbrs", rescale=8)
  rr_focal_tblbare1=rr_focal_tblbare[-length(rr_focal_tblbare)]
  
  ## table(xx$prop_build_nbrs)
  ## xx <- rr_focal_tblbuild1[["2014"]]
  cat("joining all rasters\n")
  rr_points6 <- map2(rr_points5, rr_focal_tblbuild1, ~ full_join(.x, .y, by=c("x","y")))
  rr_points7 <- map2(rr_points6, rr_focal_tblveg1, ~ full_join(.x,.y, by=c("x","y")))
  rr_points8 <- map2(rr_points7, rr_focal_tblriveg1, ~ full_join(.x,.y, by=c("x","y")))
  rr_points9 <- map2(rr_points8, rr_focal_tblset1, ~ full_join(.x,.y, by=c("x","y")))
  rr_points10 <- map2(rr_points9, rr_focal_tblagri1, ~ full_join(.x,.y, by=c("x","y")))
  
  cat("joining climate data\n")
  ## combine climate data (loaded them in from climate.RData)
  rr_points11 <- map2(rr_points10, pr_change_tbl, ~ full_join(.x,.y, by=c("x","y")))
  rr_points12 <- map2(rr_points11, at_change_tbl, ~ full_join(.x,.y, by=c("x","y")))
  rr_points13 <- map2(rr_points12, ws_change_tbl, ~ full_join(.x,.y, by=c("x","y")))
  rr_points14 <- map2(rr_points13, rr_focal_tblbare1, ~ full_join(.x,.y, by=c("x","y")))
  
  save("rr_points14",file="rr_points14.RData")
}
