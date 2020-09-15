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
    rr_list <- map(years2 , get_categorical_raster, list_cats=TRUE)

    ## set the raster names equal to the years
    names(rr_list) <- years2 

    ##dem
    dem <- raster("dem/Extract_dem11.tif")

    ## https://gis.stackexchange.com/questions/154276/reprojecting-raster-from-lat-lon-to-utm-in-r
    ## dem_reproj <- projectRaster(dem, crs=crs(rr_list[[1]]))
    ##change project system of dem to landuse raster 1987
    crs(rr_list[[1]])
    lapply(rr_list, crs) ## check coord systems for all land-use maps
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
    rr_focal <- map(rr_list, ~ focal(.==3, comb, sum))

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
    rr_points6 <- map2(rr_points5, rr_focal_tblbuild1, ~ full_join(.x, .y, by=c("x","y")))
    rr_points7 <- map2(rr_points6, rr_focal_tblveg1, ~ full_join(.x,.y, by=c("x","y")))
    rr_points8 <- map2(rr_points7, rr_focal_tblriveg1, ~ full_join(.x,.y, by=c("x","y")))
    rr_points9 <- map2(rr_points8, rr_focal_tblset1, ~ full_join(.x,.y, by=c("x","y")))
    rr_points10 <- map2(rr_points9, rr_focal_tblagri1, ~ full_join(.x,.y, by=c("x","y")))
    
    ## combine climate data (loaded them in from climate.RData)
    rr_points11 <- map2(rr_points10, pr_change_tbl, ~ full_join(.x,.y, by=c("x","y")))
    rr_points12 <- map2(rr_points11, at_change_tbl, ~ full_join(.x,.y, by=c("x","y")))
    rr_points13 <- map2(rr_points12, ws_change_tbl, ~ full_join(.x,.y, by=c("x","y")))
    rr_points14 <- map2(rr_points13, rr_focal_tblbare1, ~ full_join(.x,.y, by=c("x","y")))

    save("rr_points14",file="rr_points14.RData")
}

if (FALSE) {
    str(rr_points13[["2014"]])
    str(rr_points12[["2014"]])
    str(rr_points12[["2014"]])
    ## default is gain
    ##without direction

    ## debugging
    summary(rr_points13[["2014"]][c("x","y")])

    xysum <- function(dd1,dd2) {
        c(length(intersect(dd1$x,dd2$x)),
          length(intersect(dd1$y,dd2$y)))
    }
    xysum(comb_terrain,rr_tbl[[1]])
    xysum(rr_tbl[[1]],rr_tbl[[2]])
    xysum(rr_tbl[["1987"]],rr_tbl[["2014"]])
    xysum(comb_terrain, rr_tbl[["2014"]])
    xysum(rr_tbl[["1987"]],rr_tbl[["2018"]])
    xysum(rr_tbl[["1987"]],rr_tbl[["2008"]])

    head(sort(unique(rr_tbl[["2014"]]$x)))
    head(sort(unique(rr_tbl[["2018"]]$x)))
    head(sort(unique(comb_terrain$x)))
    
    rr_list[["1987"]]
    rr_list[["2014"]]

    summary(comb_terrain[c("x","y")])
 ##       x                y          
 ## Min.   :592356   Min.   :2827279  
 ## 1st Qu.:608506   1st Qu.:2834879  
 ## Median :616206   Median :2840379  
 ## Mean   :616041   Mean   :2840359  
 ## 3rd Qu.:623856   3rd Qu.:2845529  
 ## Max.   :636706   Max.   :2857829  
    summary(rr_tbl[[1]][c("x","y")])

    dd <- rr_points13[["2014"]]
    table(dd$change, useNA="always")
    get_logist_data(dd, scale=FALSE, direction="gain")

    nrow(dd %>% drop_na(slope))
}

## we do have lots of 0 and 1 values (which are
## not-erg -> not-erg  and not-erg -> erg)


##loss
logistlost <- run_logist_regression(direction="loss")
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

## list of all files after using run_logist_regression
## do all fits at once

logist_list <- map(rr_points14, run_logist_regression) 


## draw the plots
##dwplot is a function for quickly and easily generating plots of regression models
if (FALSE) {
    plot1 <- dwplot(logist_list)

    ##change scale
    ##limit:modify the axis limits 
    ##geoms add reference lines (sometimes called rules) to a plot, either horizontal, vertical(v), or diagonal
    plot1 + scale_x_continuous(limits=c(NA,10))+ geom_vline(lty=2,xintercept=0)
    ## zoom in
    plot1 + scale_x_continuous(limits=c(-3,3)) + geom_vline(lty=2,xintercept=0)
}


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
##without direction
logistgain_quadratic <- run_logist_regression(poly_xy_degree=2)
summary(logistgain_quadratic)
tidy(logistgain_quadratic)


## but SEE BELOW: map_dfr() instead of map(); future_map_dfr() instead of map()
##all map logistic quadratic (gain, scale=false)
logist_quad_list <- map(rr_points13, run_logist_regression, poly_xy_degree=2)
tidy_quad_list <-map_dfr(logist_quad_list, tidy)
save("logist_quad_list", "tidy_quad_list", file="saved_logist_fits.RData")

## compare models
AICtab(logistgain,logistgain_linear,logistgain_quadratic)
try(dwplot(logistgain,logistgain_linear,logistgain_quadratic))

##Scale=False
##quadratic loss(for 2014)
##with direction, SCALE=FALSE
logistloss_quadratic <- run_logist_regression(poly_xy_degree=2, direction="loss")
summary(logistloss_quadratic)
tidy(logistloss_quadratic)

##quadratic list lost,Scale=False
##Lost
logist_quad_list_lost <- map(rr_points13, run_logist_regression, poly_xy_degree=2, direction = "loss")
tidy_quad_list_lost <-map(logist_quad_list_lost, tidy)
save("logist_quad_list_lost", "tidy_quad_list_lost", file="saved_logist_fits2.RData")

##Scale=TRUE
##quadratic list,Scale=TRUE
##saved files separately
##we could also add scale=TRUE to get the scaled version
##gain (2014)
## running with scale=TRUE takes a little longer
##scale=normalized data

logistgain_quadraticS=run_logist_regression(poly_xy_degree=2, scale = TRUE)
summary(logistgain_quadraticS)
tidy(logistgain_quadraticS)

#logistic quadratic for all maps (scale=true)
logist_quad_listS <- map(rr_points14, ~run_logist_regression(., poly_xy_degree=2,scale = TRUE))
tidy_quad_listS <-map_dfr(logist_quad_listS, tidy, conf.int=TRUE, .id="year")
## LARGE: 600M or so
##save seperatly
save("logist_quad_listS",file="saved_logist_fitsS.RData")
save("tidy_quad_listS",  file="saved_tidy_fitsS.RData")

##Loss (2014)
##Scale=TRUE
logistloss_quadraticS=run_logist_regression(poly_xy_degree=2,direction="loss", scale = TRUE)
summary(logistloss_quadraticS)
tidy(logistloss_quadraticS)

##Lost all the maps (scale=true)
##map makes list
##map2_dfr make tbl
logist_quad_list_lostS <- map(rr_points13, run_logist_regression,poly_xy_degree=2,direction="loss", scale = TRUE)
save("logist_quad_list_lostS",file="saved_logist_lost_fitsS.RData")

##tidy of loss
##load loss scale quadratic 
L <- load("~/Dropbox/saved_logist_lost_fitsS.RData")
L <- load("saved_logist_lost_fitsS.RData")
print(L)
## skip the first year, no loss at all
## skip the second year, only 17 pixels lost  
logist_OK <- logist_quad_list_lostS[-1]
mm <- logist_OK[["2008"]]
##map dfr with safe tidy
tidy_quad_list_lostS <- map_dfr(
  logist_OK,
  safe_tidy, .id="year")
save("tidy_quad_list_lostS",  file="saved_tidy_lost_fitsS.RData")

##plots
library(rms)  
print(ggplot(tidy_quad_listS, aes(x=estimate, y=term, xmin=conf.low, xmax=conf.high, colour=year))
      ## + geom_errorbar()
      + geom_pointrange(position=position_dodgev(height=0.25)))

print(ggplot(tidy_quad_list_lostS, aes(x=estimate, y=term, xmin=conf.low, xmax=conf.high, colour=year))
      ## + geom_errorbar()
      + geom_pointrange(position= position_dodgev(height=0.25)))

##residual check:we have to make a model that DHARMa supported it:
S1=simulate(logistgain_quadraticS, nsim=100)
##make matrix
S2=do.call(cbind, S1)

##make a DHARMa fitted model
##H-P:I recevied an error(I think most error occured because of same problem but I dont find how I can solve it)
##Error number of observations < 3 ... this rarely makes sense
##the lenght of  observedResponse = rr_points13$change is zero, but I could not find how it is possible
S3=createDHARMa(simulatedResponse = S2, 
                observedResponse = Num_gai_quadS,
                fittedPredictedResponse = predict(logistgain_quadraticS),
                integerResponse = TRUE)

S4=plotSimulatedResiduals(S3)

##Acuracy
##Hosmer-Lemeshow Test:
##2014
##gain-Scale=TRUE
## x <- logistgain_quadraticS
load("saved_logist_fitsS.RData")
x <- logist_quad_listS[["2014"]]
tidy(x)

##return a data.frame with the variables needed to use formula
table(model.frame(x)$change)
##make numbers
Num_gai_quadS=as.numeric(model.frame(x)$change)
hoslem.test(Num_gai_quadS, fitted(x), g=2)


##relation ship between observation and predication variables
## A data frame is a table or a two-dimensional array-like structure in which each column contains values of one variable and each row contains one set of values from each column
##pred=Predicted values based on the model. 
##fitted=mean response value
##make a table with two colume prediction and observation
dd <- data.frame(pred=fitted(x),obs=as.numeric(model.frame(x)$change))

## %>%=will forward a value, or the result of an expression, into the next function call/expression.
## the mutate function is used to create a new variable from a data set
##breks(from=0, to=1)
##bin is based on the predicted values
dd_sum <- (dd
    %>% arrange(pred)
    %>% mutate(bin=cut(pred,breaks=seq(0,1,by=0.05)))
    %>% group_by(bin)
    %>% summarise(n=n(),
                  pred=mean(pred),
                  lwr=prop.test(sum(obs),n())$conf.int[1],
                  upr=prop.test(sum(obs),n())$conf.int[2],
                  sumobs=sum(obs),
                  sumpred=pred*n(),
                  obs=mean(obs)
                  )
)

plot(x)

##~=tidle right hands of tidle depends on left hand of tidle
glm(obs~1, data=dd)

##midpt=average of bin colume
##aes=inputs are quoted to be evaluated in the context of the data. 
##geom_pointrange=defined by an upper and lower value for the line
##geom_abline=The abline geom adds a line with specified slope and intercept to the plot.
##midpt=mid bin=predicted values and obs==mean(obs)
dd_sum$midpt <- seq(0.025,0.975,by=0.05)
(ggplot(dd_sum,aes(midpt,obs,ymin=lwr,ymax=upr))
    + geom_pointrange()
    + geom_abline(intercept=0,slope=1,colour="red"))


x <- logist_quad_listS[["2014"]]
x2 <- logist_quad_listS[["2008"]]


plot_preds(x,"prop_settle_nbrs")
plot_preds(x,"prop_erg_nbrs")
plot_preds(x,"slope")
plot_preds(x,"aspect")
plot_preds(x,"prop_veg_nbrs")
plot_preds(x,"prop_riveg_nbrs")
plot_preds(x,"prop_agri_nbrs")
plot_preds(x,"prop_build_nbrs")
plot_preds(x,"averagetemchange")
plot_preds(x,"windchange")
plot_preds(x,"precipchange")

## Predict vs residual
##Fitted values are also called predicted values.
plot(predict(x),residuals(x))

##
library(arm)
binnedplot(fitted(x), 
           residuals(x, type = "response"), 
           nclass = NULL, 
           xlab = "Expected Values", 
           ylab = "Average residual", 
           main = "Binned residual plot", 
           cex.pts = 0.8, 
           col.pts = 1, 
           col.int = "gray")


##validity
##2014
##gain-Scale=TRUE
rms::val.prob (y=Num_gai_quadS, logit=predict(logistgain_quadraticS))

## extra terms
## does this help??
##aspect
##not good accuracy
logist_quad_listS_extra <- map(rr_points13,
                                ~run_logist_regression(., poly_xy_degree=2,scale = TRUE, extra_terms="(aspect^2)"))

extra1 <- logist_quad_listS_extra[["2014"]]
table(model.frame(extra1)$change)
Num_gai_quadS=as.numeric(model.frame(extra1)$change)
hoslem.test(Num_gai_quadS, fitted(extra1), g=10)

##plot
binnedplot(fitted(extra1), 
             residuals(extra1, type = "response"), 
             nclass = NULL, 
             xlab = "Expected Values", 
             ylab = "Average residual", 
             main = "Binned residual plot-extra1", 
             cex.pts = 0.8, 
             col.pts = 1, 
             col.int = "gray")
##settlement
##not good accuracy
logist_quad_listS_extra2 <- map(rr_points13,
                               ~run_logist_regression(., poly_xy_degree=2,scale = TRUE, extra_terms="(prop_settle_nbrs^2)"))

extract2 <- logist_quad_listS_extra2[["2014"]]
##plot
binnedplot(fitted(extract2), 
             residuals(extract2, type = "response"), 
             nclass = NULL, 
             xlab = "Expected Values", 
             ylab = "Average residual", 
             main = "Binned residual plot-extra2", 
             cex.pts = 0.8, 
             col.pts = 1, 
             col.int = "gray")

table(model.frame(extract2)$change)
Num_gai_quadS=as.numeric(model.frame(extract2)$change)
hoslem.test(Num_gai_quadS, fitted(extract2), g=10)

##mtry: Number of variables randomly sampled as candidates at each split
##spatial autocorelation,Morans I Index
##Convert data to spatial points dataframe
## https://cran.r-project.org/web/packages/spdep/vignettes/nb.pdf
glm <- run_logist_regression(rr_points14[["2003"]])
data <- rr_points14[["1987"]]
datpoint <- SpatialPointsDataFrame(cbind(data$x, data$y), data)

library(spdep)
## test with smaller data set
testdat <- filter(rr_points14[["2014"]],
                  x<604000 & y >284000 &  y < 2846000)

## get only the points that are being used in the analysis
## (e.g. for "gain" regression, only points that start as non-erg)
## and make sure to drop NA values, so that the length matches the
## length of the logistic fit res## POSSIBLE solution to spatial autocorrelation: aggregate the data set to
##  a larger scale (i.e. go back to the rasters and compute fraction erg)iduals

testglm <- run_logist_regression(testdat)
## now run the Moran test
## Construct weights matrix in weights list form using the 10 nearest neighbors
lstw  <- nb2listw(knn2nb(knearneigh(testpoint, k = 10)))
## moran.test(residuals(testglm), lstw)

##Histogram
testdat2 <- na.omit(get_logist_data(testdat, scale=TRUE, direction="gain"))
testpoint <- SpatialPointsDataFrame(cbind(testdat2$x, testdat2$y), testdat2)
res0 <- residuals(testglm,type="response")
hist(res0)
hist(predict(testglm))
hist(predict(testglm,type="response"))

## scale residuals from 0 to 1 
res <- scale(res0, center=min(res0), scale=diff(range(res0)))
range(res)
cr <- colorRamp(c("blue", "red"))
plot(testpoint$x,testpoint$y,col=rgb(cr(res)/255),
     pch=16) ##,pch=".",cex=3)

##randomforest
library(randomForest)
library(caTools)

## get 2014 data, drop NA values
a <- na.omit(rr_points14[["2008"]])
## how big is it?
nrow(a) 

## too big: let's just select the western tip of the study area
a2 <- filter(a,x<604000 & y >284000 &  y < 2846000)
## much smaller
nrow(a2) 

##generate a sequence of random numbers – it ensures that you get the same result if you start with that same seed each time you run the same process. 
set.seed(123)
sample <- sample.split(ss$change, SplitRatio = 0.75)

##subset=take random samples from a dataset
train <- subset(ss, sample == TRUE)
test <- subset(ss, sample == FALSE)

## REGRESSION: trying to predict {0,1,2,3} [change values]
## this doesn't make sense because
## 0: not-erg before and after (no gain)
## 1: not-erg before, erg after (gain)
## 2: erg before, not-erg after (loss)
## 3: erg before and after (no loss)
##
## regression might make sense if we treated gain {0,1} and loss {2,3}
## separately. BUT: classification probably still makes more sense, because
## a regression framework assumes equal change  in the output variable for
## the same amount of change in the input variable
## suppose we have a place that's very unlikely to gain erg (i.e. the prediction
## is close to 0), and we increase some input value that makes gain more likely
## (e.g. we increase the amount of nearby erg, or upwind erg, or ...)
## then we expect a big change in the output variable (from say 0.01 to 0.5)
## but if we have a place that's *likely* to gain erg (the prediction is already
## close to 1), then it doesn't make sense to increase the prediction by the
## same amount, because a prediction >1 doesn't really make  sense
## ... this is why we usually use logistic regression rather than linear
## regression for binary responses
## which is a long-winded way of saying we should probably do classification
## instead

if (file.exists("rf.RData")) {
    load("rf.RData")
} else {
    rf <- randomForest(formula= factor(change) ~ . - x - y ,
                       data = train, do.trace=1,
                       type="classification", proximity=TRUE)
    save("rf",file="rf.RData")
}
plot(rf)
plot(rf$predicted)
pred <- predict(rf, newdata=test)

library(caret)
ff <- function(x) factor(x,levels=0:3,labels=c("no gain","gain","loss","no loss"))
conf500= caret::confusionMatrix(ff(pred),ff(test$change))
save("conf500",  file="saved_conf-500.RData")

### fit a random forest model (using ranger)
library(ranger)
rf_fit <- train(as.factor(change2) ~ ., 
                data = ss, 
                method = "ranger")
rf_fit2 <-ranger(change2 ~ ., data = train, num.trees = 500, mtry = 6, importance = "impurity", min.node.size = 3, replace = TRUE, num.threads = 3)
save("rf_fit",  file="saved_rf_fit.RData")
rf_pred <- predict(rf_fit, test)
confusionMatrix(rf_pred, as.factor(test$change2))
## importance of each predictor
importance(rf)

# number of trees with lowest MSE.MSE=mean square errors: sum of squared residuals divided by n
MSE=rf$mse
which.min(rf$mse)

#cross validation
library(rfUtilities)
rf.crossValidation

## rf$votes is the number of votes for each category
##  for each data point, calculate the proportion of
##  trees that got the right answer
mm <- match(train$change,0:3) ## which column matches?
## this gives us the column number we want in each row
correct_prop <- rf$votes[cbind(seq(nrow(rf$votes)), mm)]
any(is.na(correct_prop)) ## no missing values
any(is.na(cr(correct_prop))) ## no missing values
rr <- rgb(cr(correct_prop)/255) ## no missing values
any(is.na(rr))

testpoint <- SpatialPointsDataFrame(cbind(train$x, train$y), train)
plot(testpoint$x,testpoint$y,col=rgb(cr(correct_prop)/255),
     pch=16) ##,pch=".",cex=3)
points(test$x,test$y,pch=".")

head(rf$votes)
head(a2$change)
npts <- nrow(rf$votes)


# RMSE (Root Mean Square Error)of this optimal random forest
sqrt(rf$mse[which.min(rf$mse)])
testpoint <- SpatialPointsDataFrame(cbind(a2$x, a2$y), a2)
plot(testpoint$x,testpoint$y,col=rgb(cr(res)/255),
     pch=16) ##,pch=".",cex=3)



##mtry: Number of variables randomly sampled as candidates at each split
##spatial autocorelation,Morans I Index
##Convert data to spatial points dataframe
## https://cran.r-project.org/web/packages/spdep/vignettes/nb.pdf
glm <- run_logist_regression(rr_points14[["2003"]])
data <- rr_points14[["1987"]]
datpoint <- SpatialPointsDataFrame(cbind(data$x, data$y), data)

library(spdep)
## test with smaller data set
plot(y~x,rr_points14[["2014"]],pch=".")
testdat <- filter(rr_points14[["2014"]],
                  x<604000 & y >284000 &  y < 2846000)
plot(y~x,testdat,pch=".")

## get only the points that are being used in the analysis
## (e.g. for "gain" regression, only points that start as non-erg)
## and make sure to drop NA values, so that the length matches the
## length of the logistic fit res## POSSIBLE solution to spatial autocorrelation: aggregate the data set to
##  a larger scale (i.e. go back to the rasters and compute fraction erg)iduals

testglm <- run_logist_regression(testdat)
## now run the Moran test
## Construct weights matrix in weights list form using the 10 nearest neighbors
lstw  <- nb2listw(knn2nb(knearneigh(testpoint, k = 10)))
## moran.test(residuals(testglm), lstw)

##Histogram
testdat2 <- na.omit(get_logist_data(testdat, scale=TRUE, direction="gain"))
plot(y~x,testdat,pch=".")
points(testdat2$x,testdat2$y,col=2)
testpoint <- SpatialPointsDataFrame(cbind(testdat2$x, testdat2$y), testdat2)
res0 <- residuals(testglm,type="response")
hist(res0)
hist(predict(testglm))
hist(predict(testglm,type="response"))

## scale residuals from 0 to 1 
res <- scale(res0, center=min(res0), scale=diff(range(res0)))
range(res)
cr <- colorRamp(c("blue", "red"))
plot(testpoint$x,testpoint$y,col=rgb(cr(res)/255),
     pch=16) ##,pch=".",cex=3)

plot(testpoint$x,testpoint$y)

plot(testpoint$x,testpoint$y,col=rgb(cr(res)/255),
     pch=16) ##,pch=".",cex=3)

## RMSE (Root Mean Square Error)of this optimal random forest
sqrt(rf$mse[which.min(rf$mse)])
testpoint <- SpatialPointsDataFrame(cbind(a2$x, a2$y), a2)
plot(testpoint$x,testpoint$y,col=rgb(cr(MSE)/255),
     pch=16) ##,pch=".",cex=3)

##upsample & downsample
library(ROSE)
table(a2$change)
a2$change <- factor(a2$change, levels=0:3,
                         labels= c("no gain","gain","loss","no loss"))


a2$change2 <- factor(ifelse(a2$change %in% c("no gain", "no loss"),
                                 "no change", "change"))

prop.table(x=table(a2$change))
prop.table(x=table(a2$change2))
ss <- ovun.sample(change2~., data = a2, method = "under")$data
ss2 <- ovun.sample(change2~., data = testdat, method = "under")$data

## time series classification ???
## install.packages("OSTSC")
library("OSTSC")
testdatlabel=testdat$y
testdatsample=testdat$x
testdat2=OSTSC(testdatsample,testdatlabel,parallel = FALSE)

## install.packages("groupdata2")
library(groupdata2)
ss3 <- downsample(testdat,cat_col = "change2")
testdatf=data.frame(testdat)
downsample(testdatf,cat_col = "change")


