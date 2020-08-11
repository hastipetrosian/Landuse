## ideas about analyzing directional effects
library(tidyverse)
library(raster)
library(circular)
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

n <- 7  ## helps if this is odd
w <- matrix(1, nrow=n, ncol=n)
mid <- ceiling(n/2)
w[mid,mid] <- 0   ## center cell doesn't count!
image(1:n,1:n,w)
## cartesian coordinates
x <- row(w)-mid
y <- col(w)-mid
image(x)
image(y)
d <- sqrt(x^2+y^2)
image(d)
d2 <- exp(-d)
image(d2)
persp(d2)

dir <- atan2(y,x)
image(dir)
dir2 <- dvonmises(circular(dir),mu=circular(pi/2),kappa=3)
image(dir2)

comb <- dir2*d2
image(comb)
persp(comb)

plot(rr_list[[1]])
rr_focalex <- focal(rr_list[[1]]==3, comb, fun=sum)
plot(rr_focalex)



