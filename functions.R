## get land use categories as global variable; assume these will stay stable
## stored as global variable (not necessarily a good idea but ...)

## use last year (this has all of the categories)
year <- 2018
dbf_fn <- paste0(year,"R/",year,"raster.tif.vat.dbf")
dd <- foreign::read.dbf(dbf_fn)
landuse_cats <- factor(dd$descrip,levels=dd$descrip)

get_rat <- function(x) levels(x)[[1]]

## take a numeric raster and make it categorical
make_categorical <- function(r,cats=landuse_cats,rat=NULL) {
    r <- ratify(r) ## define as categorical
    if (is.null(rat)) {
        rat <- get_rat(r)  ## extract ID table
        ## check that number of labels equals number of categories!
        stopifnot(length(cats)==length(rat$ID))
        rat$landuse <- cats
    }
    levels(r) <- rat
    return(r)
}

## read raster land-use files for all years
get_categorical_raster <- function(year,quiet=FALSE,
                                   list_cats=FALSE) {
    if (!quiet) cat(year,"\n")
    raster_fn <- paste0(year,"R/",year,"raster.tif")
    r <- raster(raster_fn)
    dbf_fn <- paste0(year,"R/",year,"raster.tif.vat.dbf")
    dd <- foreign::read.dbf(dbf_fn)
    if (list_cats) {
        cat(paste(as.character(dd$descrip),collapse="; "),"\n")
    }
    return(make_categorical(r,dd$descrip))
}

## code for reading excel files and converting Farsi numbers 
contains_slash <- function(x) {
  any(grepl("/",x))
}
make_number <- function(x) {
  as.numeric(stringr::str_replace(x,"/","."))
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

safe_tidy <- function(x) {
    x <- mm
    tt <- try(tidy(x,conf.int=TRUE))  ## try(): don't stop even if there's an error
    if (inherits(tt,"try-error")) {  ## if it failed
        ## fall back to Wald estimates
        tt <- tidy(x)  ## tidy without confidence intervals
        tt$conf.low  <- tt$estimate-1.96*tt$std.error
        tt$conf.high <- tt$estimate+1.96*tt$std.error
    }
    return(tt)
}
        
    
