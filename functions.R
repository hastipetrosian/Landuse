get_categorical_raster <- function(year,quiet=FALSE) {
    if (!quiet) cat(year,"\n")
    raster_fn <- paste0(year,"R/",year,"raster.tif")
    r <- raster(raster_fn)
    r <- ratify(r)
    rat <- levels(r)[[1]]  ## extract ID table
    dbf_fn <- paste0(year,"R/",year,"raster.tif.vat.dbf")
    dd <- foreign::read.dbf(dbf_fn)
    landuse_cats <- dd$descrip
    ## check that number of labels equals number of categories!
    stopifnot(length(landuse_cats)==length(rat$ID))
    rat$landuse <- landuse_cats
    levels(r) <- rat
    return(r)
}

## code for reading excel files and converting Farsi numbers 
contains_slash <- function(x) {
  any(grepl("/",x))
}
make_number <- function(x) {
  as.numeric(stringr::str_replace(x,"/","."))
}
