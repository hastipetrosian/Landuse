library(raster)
library(sf)
source("functions.R")

rr <- get_categorical_raster(2003)
levelplot(rr)
rr2 <- rr  ## make a copy of rr
## maybe we can tell if it changed when we
## run focal() on the original raster rr

rr_after <- focal(rr==3,
                  matrix(1,nrow=3,ncol=3),
                  fun=mean)

## is the original raster still the same as
## the copy we made of it?
identical(rr,rr2)
