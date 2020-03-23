## TO DO

- Draw raster maps of land cover change?
- Figure out how to compute neighbourhood values in R
- Think about the appropriate scale on which to make predictions
- Would like to use something like the following predictors for probability of conversion to/from dune: 
     - terrain: altitude, slope, aspect (all computed from DEM)
     - fraction of (at least) dune vs non-dune neighboring cells within a range of scales
  
- consider renaming climate variables to something easier to use in a model (no spaces, shorter names)


- think about aggregation
   - if we do everything on the original (30 m) scale then we don't have to make decisions and we don't lose any information, but there might not be much useful information at this scale and the computation will be a lot more awkward (large data sets, slow computation ...) [let's go ahead and *not* aggregate for now]
   
   
- The next thing to do is to compute proportions of land-use types near
a given cell; you should be able to compute this with the `focal()`
function. The function should be something like `function(x) { mean(x=="erg")}` (`x=="erg"` is going to be TRUE or FALSE, which will converted to 1 or 0, so mean of this number will be the proportion of erg)

 It looks like as.data.frame() will extract variables from a raster to a
data frame.

  as_tibble() will convert to a slightly more convenient format.

  In the end we're aiming to convert everything to a format something
like this: a data frame or tibble that contains one row for every pixel
in every year (maybe? only pixels that were erg sometime during the
range of the data?) containing


  year erg_before  erg_after slope aspect elevation nbr_prop_bare
nbr_prop_mangrove nbr_prop_riverbed ...

  where we have a column for the proportion of every other land type
(for simplicity we could start out with only prop_erg ?)
