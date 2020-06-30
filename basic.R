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

## draw all of the raster mapsall_descrip <- map(dd_list, ~ sort(.["descrip"]$descrip))

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
rr_list <- map(years2 , get_categorical_raster, list_cats=TRUE)

## set the raster names equal to the years
names(rr_list) <- years2 

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


load("climate.RData") ## get climate data

## table(xx$prop_build_nbrs)
## xx <- rr_focal_tblbuild1[["2014"]]
rr_points6 <- map2(rr_points5, rr_focal_tblbuild1, ~ full_join(.x, .y, by=c("x","y")))
rr_points7 <- map2(rr_points6, rr_focal_tblveg1, ~ full_join(.x,.y, by=c("x","y")))
rr_points8 <- map2(rr_points7, rr_focal_tblriveg1, ~ full_join(.x,.y, by=c("x","y")))
rr_points9 <- map2(rr_points8, rr_focal_tblset1, ~ full_join(.x,.y, by=c("x","y")))
rr_points10 <- map2(rr_points9, rr_focal_tblagri1, ~ full_join(.x,.y, by=c("x","y")))

## combine climate data (loaded them in from climate.RData)
rr_points11 <- map2(rr_points10, pr_change_tbl, ~ full_join(.x,.y, by=c("x","y")))
rr_points12 <- map2(rr_points11, at_change_tbl, ~ full_join(.x,.y, by=c("x","y")))
rr_points13 <- map2(rr_points12, ws_change_tbl, ~ full_join(.x,.y, by=c("x","y")))

## running everything for one set of changes

run_logist_regression <- function(dd=rr_points13[["2014"]],
                                  scale=FALSE,
                                  poly_xy_degree=NA,
                                  direction="gain") {
    dd <- (dd
        ## only want points that were erg before
        ## only values that have aspect data
        %>% drop_na(slope)
        ## don't need x,y columns any more
        ## (we may not want to use them in the logistic regression, and the formula
        ##  response ~ . includes *everything* in the data set as part of the model
        %>% mutate_at(c("x","y"), ~ . / 1000 ) ## CHECK: are these now kms?
    )

    ## ANALYSIS 1: analyzing
    ##just consider the cells that are now erg-without2=before
    if (direction=="loss") {
        dd_change <- (dd
            ## if change==3 we had erg both before
            ##  and after, so this is 0 for 'no change'
            ## otherwise 1 for 'lost erg'
            %>% filter(change %in% c(2,3))
            %>% mutate(change=ifelse(change==3,0,1))
        )
    } else {  ## gain
        dd_change <- (dd
            ## if change==3 we had erg both before
            ##  and after, so this is 0 for 'no change'
            ## otherwise 1 for 'lost erg'
            %>% filter(change %in% c(0,1))
            %>% mutate(change=ifelse(change==0,0,1))
        )
    }
    ## table(dd_change$prop_build_nbrs)
    ## table(dd_change$prop_settle_nbrs)
    ## table(dd$landuse)
    
    ##logistic
    print(table(dd_change$change))
    ## logist1 <- glm(change~ slope+aspect+prop_erg_nbrs+prop_veg_nbrs+prop_build_nbrs+prop_riveg_nbrs+prop_settle_nbrs+prop_agri_nbrs, data = dd_change, family = "binomial")
    ## . = everything
    
    predvars <- dd_change[,setdiff(names(dd_change),c("landuse","change"))]
    if (scale) {
        sdvec <- 2*sapply(predvars,sd,na.rm=TRUE)
        predvars <- scale(predvars,
                          center=TRUE,
                          scale=sdvec)
        ## all-zero or constant columns will mess things up
        okvars <- colSums(!is.na(predvars))
        predvars <- predvars[,okvars>0]
    }
    dd_change <- data.frame(change=dd_change$change, predvars)
    if (is.na(poly_xy_degree)) {
        form <- change ~ . - x - y
    } else {
        ## poly_xy_degree = 1  ->  linear model in x and y
        ## poly_xy_degree = 2  ->  quadratic model in x and y
        form <- change ~ . - x - y + poly(x,y,degree=poly_xy_degree)
    }
    logist1 <- glm(form , data = dd_change, family = "binomial")
    return(logist1)
}
 
##loss
logistlost <- run_logist_regression(direction="loss")

##logist1=logistgain
## default is gain
##without direction
logistgain <- run_logist_regression()  

## glm.fit: fitted probabilities numerically 0 or 1 occurred
##  means we (probably) have *complete separation* (one variable completely explains everything)
##  does mean that the standard errors come out ridiculously large
##  simple way to get confidence intervals estimate +/- 1.96 std errors
##  BUT that is not as good as 'profile confidence intervals', which is what
##   confint() computes, and when we have complete separation (and therefore the
##   SEs are ridiculous) it doesn't work right at all.
##   So we have to do confint(), and wait for it to finish ...
summary(logistgain)

## running with scale=TRUE takes a little longer
##scale=normalized data
##logist1=logistgain
logistgainS <- run_logist_regression(scale=TRUE)

##my R doesnt have enogh memory I have used below codes but I m not sure is it true or not
memory.limit(500000)

## leave the first set of changes out
## since we only lose 4/18K pixels
logist_list <- map(rr_points13, run_logist_regression) ## do all fits at once

## this will be unscaled; we could also add scale=TRUE to get the scaled version
## this should compute tidy() for each logistic regression, including confidence intervals (slow!)
## tidy_quad_list <- map(logist_quad_list, tidy, conf.int=TRUE)
## but SEE BELOW: map_dfr() instead of map(); future_map_dfr() instead of map()

## draw the plots
##dwplot is a function for quickly and easily generating plots of regression models 
plot1 <- dwplot(logist_list)

##change scale
##limit:modify the axis limits 
##geoms add reference lines (sometimes called rules) to a plot, either horizontal, vertical(v), or diagonal
plot1 + scale_x_continuous(limits=c(NA,10))+ geom_vline(lty=2,xintercept=0)
## zoom in
plot1 + scale_x_continuous(limits=c(-3,3)) + geom_vline(lty=2,xintercept=0)

## extract parameters
## extract table data by tidy
##logist1=logistgain
tidy(logistgain)  ## estimate, SE, Z-statistic, p-value
tidy(logistlost)

##The 1-degree polynomial is a simple linear logistic regression
##Linear and quadratic trend surface model mean responce surface is linear or quadratic
logistgain_linear <- run_logist_regression(poly_xy_degree=1)
##coef is a generic function which extracts model coefficients 
coef(logistgain_linear)
summary(logistgain_linear)

##A quadratic function is a second degree polynomial function. this model turns a linear regression model into a curve when there is a non-linear relationships
##quadratic model (squared model)polynomial predictorused if required by theory or simply to allow for curvature in empirical models.
##quadratic gain (2014)
logistgain_quadratic <- run_logist_regression(poly_xy_degree=2)
summary(logistgain_quadratic)
tidy(logistgain_quadratic)

##all map logistic quadaratic (gain, scale=false)
logist_quad_list <- map(rr_points13, run_logist_regression, poly_xy_degree=2)
tidy_quad_list <-map_dfr(logist_quad_list, tidy)
save("logist_quad_list", "tidy_quad_list", file="saved_logist_fits.RData")

## compare models
install.packages("bbmle")
library(bbmle)
AICtab(logistgain,logistgain_linear,logistgain_quadratic)
dwplot(logistgain,logistgain_linear,logistgain_quadratic)

##quadratic loss(for 2014)
logistloss_quadratic <- run_logist_regression(poly_xy_degree=2, direction="loss")
summary(logistloss_quadratic)
tidy(logistloss_quadratic)

##quadratic list,Scale=False
##Lost
logist_quad_list_lost <- map(rr_points13, run_logist_regression, poly_xy_degree=2, direction = "loss")
tidy_quad_list_lost <-map(logist_quad_list_lost, tidy)
save("logist_quad_list_lost", "tidy_quad_list_lost", file="saved_logist_fits2.RData")

##quadratic list,Scale=TRUE
##saved files seperately

run_logist_regression2 <- function(dd=rr_points13[["2014"]],
                                  scale=TRUE,
                                  poly_xy_degree=NA,
                                  direction="gain") {
    dd <- (dd
           ## only want points that were erg before
           ## only values that have aspect data
           %>% drop_na(slope)
           ## don't need x,y columns any more
           ## (we may not want to use them in the logistic regression, and the formula
           ##  response ~ . includes *everything* in the data set as part of the model
           %>% mutate_at(c("x","y"), ~ . / 1000 ) ## CHECK: are these now kms?
    )
    
    ## ANALYSIS 1: analyzing
    ##just consider the cells that are now erg-without2=before
    if (direction=="loss") {
        dd_change <- (dd
                      ## if change==3 we had erg both before
                      ##  and after, so this is 0 for 'no change'
                      ## otherwise 1 for 'lost erg'
                      %>% filter(change %in% c(2,3))
                      %>% mutate(change=ifelse(change==3,0,1))
        )
    } else {  ## gain
        dd_change <- (dd
                      ## if change==3 we had erg both before
                      ##  and after, so this is 0 for 'no change'
                      ## otherwise 1 for 'lost erg'
                      %>% filter(change %in% c(0,1))
                      %>% mutate(change=ifelse(change==0,0,1))
        )
    }
    ## table(dd_change$prop_build_nbrs)
    ## table(dd_change$prop_settle_nbrs)
    ## table(dd$landuse)
    
    ##logistic
    
    print(table(dd_change$change))
    ## logist1 <- glm(change~ slope+aspect+prop_erg_nbrs+prop_veg_nbrs+prop_build_nbrs+prop_riveg_nbrs+prop_settle_nbrs+prop_agri_nbrs, data = dd_change, family = "binomial")
    ## . = everything
    
    predvars <- dd_change[,setdiff(names(dd_change),c("landuse","change"))]
    if (scale) {
        sdvec <- 2*sapply(predvars,sd,na.rm=TRUE)
        predvars <- scale(predvars,
                          center=TRUE,
                          scale=sdvec)
        ## all-zero or constant columns will mess things up
        okvars <- colSums(!is.na(predvars))
        predvars <- predvars[,okvars>0]
    }
    dd_change <- data.frame(change=dd_change$change, predvars)
    if (is.na(poly_xy_degree)) {
        form <- change ~ . - x - y
    } else {
        ## poly_xy_degree = 1  ->  linear model in x and y
        ## poly_xy_degree = 2  ->  quadratic model in x and y
        form <- change ~ . - x - y + poly(x,y,degree=poly_xy_degree)
    }
    logist1 <- glm(form , data = dd_change, family = "binomial")
    return(logist1)
}

##gain (2014)
logistgain_quadraticS=run_logist_regression2(poly_xy_degree=2)
summary(logistgain_quadraticS)
tidy(logistgain_quadraticS)

#logistic quadratic for all maps (scale=true)
logist_quad_listS <- map(rr_points13, ~run_logist_regression2(., poly_xy_degree=2))
tidy_quad_listS <-map_dfr(logist_quad_listS, tidy, conf.int=TRUE, .id="year")
## LARGE: 600M or so
##save seperatly
save("logist_quad_listS",file="saved_logist_fitsS.RData")
save("tidy_quad_listS",  file="saved_tidy_fitsS.RData")

##Loss (2014)
logistloss_quadraticS=run_logist_regression2(poly_xy_degree=2,direction="loss")
summary(logistloss_quadraticS)
tidy(logistloss_quadraticS)

##Loss all the maps (scale=true)
##map makes list
##map2_dfr make tbl

logist_quad_list_lostS <- map(rr_points13, run_logist_regression2,poly_xy_degree=2,direction="loss")
##H-P:map_dfr doesnt make a tbl file, make a list file and I have recevied below error:
##Error in approx(sp$y, sp$x, xout = cutoff) : 
##need at least two non-NA values to interpolate

tidy_quad_list_lostS <-map_dfr(logist_quad_list_lostS, tidy, conf.int=TRUE, .id="year")
save("logist_quad_list_lostS",file="saved_logist_lost_fitsS.RData")
save("tidy_quad_list_lostS",  file="saved_tidy_lost_fitsS.RData")

##plots
print(ggplot(tidy_quad_listS, aes(x=estimate, y=term, xmin=conf.low, xmax=conf.high, colour=year))
      ## + geom_errorbar()
      + geom_pointrange(position=position_dodgev(height=0.25)))
      

##H-P:because tidy_quad_list_lostS is not tbl so ggplot is nt works
print(ggplot(tidy_quad_list_lostS, aes(x=estimate, y=term, xmin=conf.low, xmax=conf.high, colour=year))
      ## + geom_errorbar()
      + geom_pointrange(position=position_dodgev(height=0.25)))

##resual check:we have to make a model that DHARMa supported it:
S1=simulate(logistgain_quadraticS, nsim=100)
##make matrix
S2=do.call(cbind, S1)
##make a DHARMa fitted model
##H-P:I recevied an error(I think most error occured because of same problem but I dont find how I can solve it)
##Error number of observations < 3 ... this rarely makes sense
##the lenght of  observedResponse = rr_points13$change is zero, but I could not find how it is possible
library(DHARMa)
S3=createDHARMa(simulatedResponse = S2, 
                observedResponse = rr_points13$change,
                fittedPredictedResponse = predict(logistgain_quadraticS),
                integerResponse = TRUE)

S4=plotSimulatedResiduals(S3)

##Hosmer-Lemeshow Test:validity
##H-P: Error: variable lengths differ
library(ResourceSelection)
hoslem.test(rr_points13$change, fitted(logistgain_quadraticS), g=10)
##Test different g:
for (i in 5:15){print(hoslem.test(rr_points13$change, fitted(logistgain_quadraticS), g=i)$p.value)}

##H-P:Error:lengths of p or logit and y do not agree, the lenght of Num_gai_quadS is zero but I could not find how it is possible
##Acuracy
library(rms)  
library(mlmRev)
Num_gai_quadS=as.numeric(rr_points13$change)
val.prob (y=rr_points13, logit=predict(logistgain_quadraticS))
length(Num_gai_quadS)
 


## using ff for compress files
install.packages("ff")
library(ff)
##Gain -scale=FALSE
ff_logist_quad_list <- ff(map(rr_points13, run_logist_regression, poly_xy_degree=2))

##the ff_logist_quad_list is 1872 in comparision to logist_quad_list that is 1294728160
library(pryr)
object.size(ff_logist_quad_list)
object.size(logist_quad_list)

##Lost -scale=FALSE
ff_logist_quad_list_lost <- ff(map(rr_points13, run_logist_regression, poly_xy_degree=2, direction = "loss"))
##Gain -scale=TRUE
ff_logist_quad_listS <- ff(map(rr_points13, run_logist_regression2, poly_xy_degree=2))
##Lost-scale=TRUE
ff_logist_quad_list_lostS <- ff(map(rr_points13, run_logist_regression2,poly_xy_degree=2,direction="loss"))



##furr
param_tabqudratic <- furrr::future_map_dfr(logist_quad_list,tidy,.id="year", conf.int=TRUE) %>% arrange(term)
View(param_tabqudratic)
param_tablossqudratic <- furrr::future_map_dfr(logist_quad_list_lost,tidy,.id="year", conf.int=TRUE) %>% arrange(term)
View(param_tablossqudratic)

## scaled by 2SD by default
dwplot(logistgain) + geom_vline(lty=2,xintercept=0)
## or we can turn that off
dwplot(logistgain, by_2sd=FALSE)

## takes a while because of the confidence interval calculation
system.time(tidy(tt1 <- logistgain,conf.int=TRUE))
print(tt1)

## if you run out of memory, maybe try a smaller number of workers (2 or only 1)
## run jobs on 3 cores at once
plan(multiprocess(workers=3))
options(future.globals.maxSize=Inf)
plan(sequential)  ## turn off multi-processing, just one job at a time

## map_dfr() runs the function on each item in the list
##  and combines the results into a data frame
## this is going to take about 10-12 minutes to run

logist_list <- map(rr_points13, run_logist_regression)
fn <- "param_tab_basic.RData"
if (!file.exists(fn)) {
    param_tab <- furrr::future_map_dfr(logist_list,tidy,.id="year", conf.int=TRUE,
                                       .progress=TRUE) %>% arrange(term)
    save("param_tab",file=fn)
} else {
    load(fn)
}


## Remember the tidying will be slow; set up an if() statement to check if the results have
##   already been saved in a file; if not, run the command and save the results; if they have,
##   then load the results (as above with the 'load' command, use a sensible file name that
##   we will recognize/understand later
param_tabgainqudraticS <- furrr::future_map_dfr(logistgain_quadratic_listS,tidy,.id="year", conf.int=TRUE) %>% arrange(term)
View(param_tabgainqudraticS)
param_tablossqudraticS <- furrr::future_map_dfr(logistloss_quadratic_listS,tidy,.id="year", conf.int=TRUE) %>% arrange(term)
View(param_tablossqudraticS)


## for val.prob
library(rms)  
library(mlmRev)
Contraception$age <- scale(Contraception$age, scale=2*sd(Contraception$age))
m1 <- glm(use ~ livch*urban*age, family=binomial, data=Contraception)
cS <- split(Contraception, Contraception$livch)
names(cS) <- levels(Contraception$livch)
cfits <- map(cS, glm, formula= use ~ urban*age, family=binomial)
tidy_cfits <- map_dfr(cfits, tidy, conf.int=TRUE, .id="livch")
library(ggstance)
print(ggplot(tidy_cfits, aes(x=estimate, y=term, xmin=conf.low, xmax=conf.high, colour=livch))
      ## + geom_errorbar()
      + geom_pointrange(position=position_dodgev(height=0.25)))
      )

head(predict(m1)) ## log-odds
head(predict(m1,type="response")) ## probabilities
n_use <- as.numeric(Contraception$use)-1  ## convert to 0/1
val.prob(y=n_use, logit=predict(m1))


