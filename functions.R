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
        
    
## running everything for one set of changes

get_logist_data <- function(dd=rr_points13[["2014"]],
                            scale=FALSE,
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
    return(dd_change)
}

run_logist_regression <- function(dd=rr_points13[["2014"]],
                                  scale=FALSE,
                                  poly_xy_degree=NA,
                                  direction="gain",
                                  extra_terms=NA) {
    dd_change <- get_logist_data(dd, scale, direction)
    form_str <- ". - x - y"
    if (!is.na(poly_xy_degree)) {
        ## poly_xy_degree = 1  ->  linear model in x and y
        ## poly_xy_degree = 2  ->  quadratic model in x and y
        form_str <- c(form_str, "poly(x,y,degree=poly_xy_degree)")
    }
    if (!is.na(extra_terms)) {
        form_str <- c(form_str,extra_terms)
    }
    form <- reformulate(form_str, response="change")
    logist1 <- glm(form , data = dd_change, family = "binomial")
    return(logist1)
}

## find a way to plot *average* predicted and observed values for different ranges of parameters
plot_preds <- function(model, focal_var="prop_settle_nbrs",n_bins=20) {
    mf <- model.frame(model)
    mf$pred <- fitted(model)
    fv <- mf[[focal_var]]
    fv_bins <- seq(min(fv)-0.001,max(fv)+0.001, length.out=n_bins)
    mf$fv_cat <- cut(fv, breaks=fv_bins)  ## categories
    dd_sum <- (mf
        %>% arrange(fv_cat)
        %>% group_by(fv_cat)
        %>% summarise(n=n(),
                      pred=mean(pred),
                      lwr=prop.test(sum(change),n())$conf.int[1],
                  upr=prop.test(sum(change),n())$conf.int[2],
                  sumobs=sum(change),
                  sumpred=pred*n(),
                  obs=mean(change)
                  )
    )
    ##    dd_sum$midpts <- (fv_bins[-1] + fv_bins[-n_bins])/2  ## midpoints of the categories
    ggplot(dd_sum,aes(fv_cat,obs,ymin=lwr,ymax=upr)) +
        geom_linerange() +
        geom_point(aes(size=n),alpha=0.5) +
        geom_line(aes(y=pred),group=1,colour="red") +
        ggtitle(focal_var)
    
}
