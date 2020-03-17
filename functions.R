## get land use categories as global variable; assume these will stay stable
## stored as global variable (not necessarily a good idea but ...)

year <- 1987
dbf_fn <- paste0(year,"R/",year,"raster.tif.vat.dbf")
dd <- foreign::read.dbf(dbf_fn)
landuse_cats <- dd$descrip

make_categorical <- function(r,cats=landuse_cats) {
    r <- ratify(r) ## define as categorical
    rat <- levels(r)[[1]]  ## extract ID table
    ## check that number of labels equals number of categories!
    stopifnot(length(cats)==length(rat$ID))
    ## FIXME:
    rat$landuse <- cats
    levels(r) <- rat
    return(r)
}

get_categorical_raster <- function(year,quiet=FALSE) {
    if (!quiet) cat(year,"\n")
    raster_fn <- paste0(year,"R/",year,"raster.tif")
    r <- raster(raster_fn)
    return(make_categorical(r))
}

## code for reading excel files and converting Farsi numbers 
contains_slash <- function(x) {
  any(grepl("/",x))
}
make_number <- function(x) {
  as.numeric(stringr::str_replace(x,"/","."))
}
