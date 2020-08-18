## winddir.R
## ideas about analyzing directional effects
library(tidyverse)
library(raster)
library(circular)
## library(rgl)
library(Matrix)
source("functions.R")

## only reading the landuse rasters
rasterfiles <- list.files(pattern="*.tif$",recursive=TRUE)

## reading landuse raster file,leave out DEM file
years <- parse_number(rasterfiles)
years <- years[years>1900] ## leave out DEM file
years2 <- unique(years)

## rr_list
##reading all of the raster files into a list with classess
rr_list <- map(years2 , get_categorical_raster, list_cats=TRUE)

plot(rr_list[[1]])

## set up the machinery for calculating proportion of a particular
##  land use type that is upwind or downwind of a specified location

## focal():  we go to each pixel
##  using a weight matrix w, we calculate the sum of (w_i*x_i) for every pixel around the focal
## pixel.  If x is 1 for erg and 0 for non-erg, and if w is 0 for the focal cell and 1 for all of the
## other cells, then we're counting the number of erg pixels
## if we use mean() instead, then we're calculating the proportion of the neighbourhood
##
## now we're going change w so that we're weighting different cells in the neighbourhood differently
## the eventual goal is to count *upwind* pixels more heavily than downwind pixels
## i.e. we want to calculate a score that is "fraction of upwind region occupied by erg"

## definitions
n <- 25  ## window size: helps if n is odd
## 'range': larger value means we have longer-range influence
r <- 35 
kappa <- 20  ## concentration parameter: larger means more focused

##example winddirection=-45
winddir <- pi/4     ## wind direction
winddir_angle <- 45

##data=1
w <- matrix(1, nrow=n, ncol=n)

##ceiling=midle number
## test that center (focal) cell doesn't count!
##make a matrix just with 
mid <- ceiling(n/2)
w[mid,mid] <- 0 

##make an image
image(1:n,1:n,w)

## cartesian coordinates
x <- row(w)-mid
y <- col(w)-mid
image(x)
image(y)

d <- sqrt(x^2+y^2) ## distance from the focal cell
image(d)

##e^(-d/r)
r=20
d2 <- exp(-d/r)
image(d2)
persp(d2)
d2[mid,mid] <- 0 ## don't count the focal pixel
## rgl::persp3d(d2,col="gray")

##  now think about direction
##function returns the radian arctangent between the x-axis and the vector from the origin to 
##atan2=arc tangent
## converts from x and y to radian 
dir <- atan2(y,x) 
image(dir)  ## WATCH OUT FOR ORIENTATION!

circular(winddir)  ## uses radians by default (multiples of pi)
circular(180,units="degrees")

## von Mises distribution (Wikipedia)
dir2 <- dvonmises(circular(dir),      ## direction matrix we calculated (converted to 'circular' object)
                  mu=circular(winddir),  ## primary direction: this should be direction of the wind
                                      ## right now it's south easth
                  kappa=kappa)            ## 'concentration' parameter: 0 = no directional bias
                                      ## large values = strong directional bias
image(dir2)
image(Matrix(dir2))
## rgl::persp3d(dir2,col="gray")
## multiply direction and distance effects: this is what we want to use for focal()
comb <- dir2*d2  
image(Matrix(comb))
persp(comb)
comb[mid,mid] <- 0 ## don't count focal cell
## rgl::persp3d(comb,col="gray")
image(comb)

##use comb in 1987
plot(rr_list[[1]])
is_erg <- rr_list[[1]]==3  ## 
plot(is_erg)
rr_focalex <- focal(is_erg, comb, fun=mean)
rr_focalex <- focal(rr_list[[1]]==3, comb, fun=sum)

plot(rr_focalex)

## once this is figured out ...
## do it for all of the raster maps (substitute for the old focal calculations, using map() to
##  apply to all rasters, then converting to a table, then joining all the tables together ...)


save("comb",file="winddir_out.rda")
