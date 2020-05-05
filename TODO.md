## TO DO

- clean up `basic.R` ! 
    - try to make sure that variable names make sense 
    - delete anything we're not going to use (you can keep a copy of everything anywhere you want as long as I don't have to look at it)
    - either understand everything or put in comments with ???? (include something like "## HP" with your questions so that we can see them)
    
- Draw raster maps of land cover change? (you can do this with the new code; try to make a single plot with `plot_grid` that shows the before/after plots
- Figure out how to compute neighbourhood values in R (i.e. with `propfun <- function(x) mean(x==3)` and `focal()`)
- Use `full_join()` to combine columns of aspect, slope, proportion of neighbourhood that is dune, gain/loss score
- fix the DEM coordinates
- consider whether to use a 3x3 or larger neighbourhood when computing focal/prop dune nbrs
- Use `drop_na` to get rid of rows with NA values and `filter` to restrict this data frame to include only pixels that were dune beforehand
- run a logistic regression with aspect, slope, proportion as predictors and gain or loss of dune as the response
- do this for every pair of years


- Would like to use something like the following predictors for probability of conversion to/from dune: 
     - terrain: altitude, slope, aspect (all computed from DEM)
     - fraction of (at least) dune vs non-dune neighboring cells within a range of scales
  
- consider renaming climate variables to something easier to use in a model (no spaces, shorter names)

- figure out how to count the number of NA values in a raster and confirm that the difference between the DEM and the land use maps when converted from raster to points to tibble is exactly explained by the number of NAs in each object

## postpone

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

## 2020 April 28

* add columns for proportions of other landscape types (as described above), add these to the analysis
* look at the outputs of the logistic regressions and tell me what they mean; be prepared to explain every number
* find wind data and think about spatial and temporal scales we would need to use it sensibly
* OPTIONAL: think about multinomial models (nnet::multinomial)
* OPTIONAL: think about deriving directional neighbourhood information

## 2020 May 5

* read Schielzeth's paper, think about scaling and log-odds and stuff; the `dotwhisker` package might be useful
* go through with improved neighbourhood calculations, calculate prop of other land-use types
* ?? add wind data to repository, calculate *average* wind direction over the entire intervals between maps
* calculate neighbourhood indices based on downwind/upwind directions
