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

## 2020 May 19

- finish precipitation
- make sure x,y coordinates for climate rasters line up with DEM/landuse values
- calculate standard errors as well as annual means for temperature at each station; provide these values in a CSV file (mean and SE for each year)
- figure out how to calculate change in temp and precip between time points (follow examples for land use change), integrate these into the logistic regression

- kriging in R: https://rpubs.com/nabilabd/118172
- https://mgimond.github.io/Spatial/interpolation-in-r.html


## 2020 May 26

- still a little concerned about the kriging maps, and the fact that they're derived from only 3 points. Can we find any other information on spatial patterns of wind/precip/etc. in this area to cross-check these maps, at least qualitatively?
- try linear and quadratic trend surface models and see if this makes a big difference.  Do these fight with the climatic effects?

## 2020 June 2

* read about 'complete separation' (logistic regression) [this is a problem that happens sometimes when doing logistic regression]

* include variable "change in built up area" as a predictor; how exactly are you going to compute that variable?  BE CAREFUL not to make circular predictions, i.e. to put something in the model that's information from the later period that is strongly correlated with the value of the focal pixel.  For example, if you computed the fraction of the *entire* neighborhood (including the focal pixel) that was built up, then it would always be larger if the focal pixel went from erg to built-up area ...

suppose a 3x3 neighbourhood is all erg in 2014. If the 8 cells around the focal pixel are built-up area in 2018, it's probably inevitable that the focal pixel is also built-up (i.e. erg loss). But that's not actually circular. 

Do it and let's see!

* try linear and quadratic trend surface models and see if this makes a big difference.  Do these fight with the climatic effects?

Morrissey, Michael B. ; Ruxton, G. D. “Multiple Regression Is Not Multiple Regressions: The Meaning of Multiple Regression and the Non-Problem of Collinearity.” Philosophy, Theory, and Practice in Biology 10 (2018). http://dx.doi.org/10.3998/ptpbio.16039257.0010.003.

Akaike Information Criterion (AIC)
one way of choosing a model; it's equal to -2*(log likelihood) + 2*(number of parameters)
*penalized* goodness-of-fit measure

lower AIC is better
dAIC = "delta AIC" or difference from the best AIC

<2 units difference is "close to equivalent"
>10 units difference is "extremely different"

you should *definitely* use the quadratic model

Something else to work on:

* The confidence interval calculation is slow for logistic regressions with big data sets
* The data sets are much bigger now that we're looking at gain instead of loss, because
'gain' uses all the pixels that were *not* erg in the previous yeat
* So the CI interval calculation is slow
* So ... we want to run the confidence interval calculations once and save the answers

* Run tidy(., conf.int=TRUE) on quadratic models for every pair of years (if there's enough data),
both scaled (scale=TRUE in run_logist_regression()) and unscaled (scale=FALSE)

you can use map() to do this  (read the end of basic.R)

save the results as two lists (one a list of scaled tidy things, one a list of unscaled
tidy things)

push the results to the repository






---
"linear model" means the response is a linear function of the *predictor* variables,
but we can create multiple predictor variables from a single *input* variable.
Input variables are variables in your data set.

For example: let's say latitude is an input variable

NPP = a + b*latitude    "NPP is a linear function of latitude"
NPP = a + b*latitude+c*latitude^2  "NPP is a quadratic (nonlinear) function of latitude"
but this is still a LINEAR MODEL in terms of the parameters (a,b,c).  Suppose we make a new
variable called latitude_squared

NPP = a + b*latitude + c*latitude_squared

this is a linear combination of the two variables
mathematically, we could look at the derivative of the expression with respect to the parameters

d(NPP)/da = 1
d^2(NPP)/(da^2) = 0

d(NPP)/dc = latitude_squared
d^2(NPP)/(dc^2) = 0

NONLINEAR statistical model if I decided to fit

NPP = a*latitude^b

then I can't do this in a linear regression or a logistic regression ...

## 16 June 2020

* run all the slow stuff in 'sequential' mode to avoid running out of memory; save all the tidied parameter tables, make sure to push them to the repository **if they're not too big** (most of the parameter tables should not be very large, <100KB; don't save the model fits themselves to the repository)

Combining the `ff` package (which makes an index of a file on disk so that R can retrieve chunks at a time) and the `biglm` package (which also includes a `bigglm()` function) which can use the indexed file to fit the regression a chunk at a time
* http://www.bnosac.be/index.php/blog/10-bigglm-on-your-big-data-set-in-open-source-r-it-just-works-similar-as-in-sas
* https://faculty.washington.edu/tlumley/tutorials/user-biglm.pdf
* https://bookdown.org/egarpor/PM-UC3M/glm-bigdata.html

## 23 June 2020

* save logistic regression fits in Google drive folder
* save tidy logistic output (after re-running to add `conf.int=TRUE`) in GitHub
* create coefficient plots (do this with the scaled models only, separately for gain and loss)
* model validation
    * graphical vs quantitative tests: ROC curve, Hosmer-Lemeshow test
	* model **validity** (is my model "correct"? are the assumptions satisfied? am I missing important patterns that might invalidate the results?) vs. model **accuracy** (is the model working *well*, i.e., how accurately can I predict things?) (ROC, AUC)
    * `DHARMa` package: `vignette("DHARMa", package="DHARMa")`; check 'binomial' section
    * model **accuracy**: `rms::val.prob()`

## 30 June 2020

* **complete separation**

## 7 July 2020

* for 2014 (and maybe? for one or two other years), examine the predictor variables (with plot_preds()) to try to see which ones are causing trouble
* try adding a polynomial term in these variables (for example, add a quadratic term of proportion of nbr pixels settled)
```
rr <- run_logist_regression(poly_xy_degree=2,extra_terms="I(prop_settle_nbrs^2)")
```
* read a little bit about random forests and think about how you would run one on these data ...

## 14 July 2020

* try the `extra_terms` fits (now that they should be working); can you make the logistic fits for 2014 (for example) better by adding some quadratic terms of the 'worst' predictors?
* in other models/studies of land use change, what **scale** are people looking at? e.g. are they looking 1-ha blocks/pixels/etc., or 100-ha blocks, or ... ? What counts as a data point? How do they account for **spatial autocorrelation** (i.e. all of the pixels in a block simultaneously gain or lose erg ... this probably does not happen independently!)  How big are our pixels? (you could search for "land use change spatial autocorrelation" or "land use change scale")
* try out random forests with a smaller sample of the data so that you can see how it works without running out of memory
   * for example, 

* **BMB**: look at practical options for doing logistic fits with spatial correlations
* **BMB**: think about remote solutions (Ellie ? Austr ecocloud?)

## notes

* `library("sos"); findFn("autocorrelation")`
* `ngspatial::autologistic`, `spatialEco::logistic.regression`; `CARBayes::S.glm`

## 21 July 

* compute Moran test on residuals, possibly by converting residuals back to a raster layer??

* generate figures: compare original residuals plot with 'best effort' i.e. (1) weird residuals vs fitted plot; (2) weird residuals vs covariate plot; (3) and (4) are those same plots but with the regression with the added quadratic term ...

* figure out how to run random forest appropriately for spatial data (i.e. accounting properly for spatial autocorrelation) https://peerj.com/preprints/26693v1/GeoMLA_README_thengl.pdf

* BMB: keep thinking about practical autocorrelation fixes
* BMB: contact Ellie about ecocloud viability - other solutions?

## 28 July

* draw spatial plots of residuals _and_ predicted values, think about what this means in terms of where we predicted gain was probable and it didn't happen vs where we predicted
* use `type="response"` in `residuals()` to get 'raw' residuals (predict-obs), `type="response"` in `predict()` to get probabilities rather than log-odds (if you want)
* convert residuals from logistic regression back to a raster to make plotting etc. easier ...
* maybe too much but ... think about 1-ha pixels or even larger ... this is one way of dealing with spatial autocorrelation ...

* (BMB) understand why DHARMa plots fail: examples

```{r}
L <- load("saved_logist_fitsS.RData")
x <- logist_quad_listS[["2014"]]
## undebug(simulateResiduals)
ss <- DHARMa::simulateResiduals(x)
## Error in approxfun(vals, cumsum(tabulate(match(x, vals)))/(n + 1), method = "linear",  : 
##   need at least two non-NA values to interpolate

## simpler
set.seed(101)
n <- 2e5
dd <- data.frame(x=rnorm(n, mean=-5,sd=5))
dd$y <- rbinom(n,size=1,prob=plogis(dd$x))
m <- glm(y~x,data=dd,family=binomial)
ss <- DHARMa::simulateResiduals(m)
```

* support request from ecocloud: is this still supported? slow ...


## August 4

* figure out x-coordinate for 2014 land use raster: why are the x coordinates mismatched with the DEM and with the rest of the land use rasters?
* reconcile our use of "downwind" and "upwind" so that we are speaking the same language ...

* (BMB) think about incorporating interaction between wind direction and land use in the model ...

## August 11

* play around with the wind direction stuff; decide how big you want to make your focal window (bigger=slower; loses resolution)
* figure out the appropriate wind direction to encode

URL: https://github.com/hastipetrosian/Landuse.git

## August 18

* BMB: think about computational resources!
* HP: zoom in on the region that seems to be giving odd predictions
* use `predict(model, type="terms")`: this will give a matrix with columns for each predictor variable and how much it contributes to the predicted outcome at the point

```
set.seed(101)
dd <- data.frame(z=rbinom(10,size=1,prob=0.5),y=rnorm(10),x=rnorm(10))
m <- glm(z~y+x, data=dd,family=binomial)
## compare effects of individual terms
## total = intercept + sum of all terms
## prob = convert total (which is on the log-odds scale) to probability
cbind(predict(m,type="terms"), total=predict(m), prob=predict(m,type="response"))
```

## September 1

- random forest stuff:
   - we want to analyze gain and loss separately??
   - probably want do classification ...
   - the error rates are very different for common and rare categories ...
   - balancing the data set?? when is unbalanced data actually a problem?
       * (1) when your data is so big that you can't handle it, and most of your data is boring because it's the overrepresented group : ?? *downsample* the overrepresented group (i.e., throw out most of the no-change data points) This is throwing away data, so it's not really worthwhile unless you can't do the computations you want with the full data set
	   * (2) if the way you're fitting is trying to minimize classification error but weights misclassification of common and rare categories the same. Read about this: is there a way to tell the RF program what it should be trying to optimize?  (People sometimes *upsample* the rare categories) What "loss function" is it using? (The loss function is the measurement of error that we're trying to minimize ... [unweighted classification error, AUC, negative log-likelihood, etc.]
   
Learn about block cross-validation for spatial problems: is there a way to do random forests with appropriate spatial resampling?

* Roberts, David R., Volker Bahn, Simone Ciuti, Mark S. Boyce, Jane Elith, Gurutzeta Guillera-Arroita, Severin Hauenstein, et al. “Cross-Validation Strategies for Data with Temporal, Spatial, Hierarchical, or Phylogenetic Structure.” Ecography, December 1, 2016, n/a-n/a. https://doi.org/10.1111/ecog.02881.

```
library(sos)  ## install this first
findFn("block validation spatial")
```
`blockCV` package 
   
**BMB**: figure out why predictions of training sets have gaps??
   
## 8 September 

- https://rspatial.org/raster/analysis/3-spauto.html
- spdep package: `sp.correlogram` 
- compute a spatial autocorrelation function for the residuals of the logistic regression; how far away do we have to go before residuals are (close enough to) independent?
- find out about coastal management zone sub-regions; is there a good *ecologically relevant* way to split the data into 5-10 spatial subsets?
- do spatial cross-validation (with whatever kind of spatial blocking you decide is best) to estimate out-of-sample error [fit the model to k-1 blocks (training set), estimate error in the left-out block (testing set)]: use AUC? 

[NOT: Nagelkerke R^2? (`performance:r2_nagelkerke`)]
[NOT: equally-weighted classification error]

- concerns about up/downsampling:
    - like everything else, up- and down-sampling usually assumes spatial independence???
	- is it really necessary? (not for logistic regression)
    - can we achieve the same goal by setting the `classwt` parameter?
- read more about using random forest for spatial prediction problems?  How do we deal with spatial autocorrelation in this context??	

https://msu.edu/~ashton/classes/866/notes/lect19/index.htm
https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0229509
https://github.com/thengl/GeoMLA

**BMB**: figure out why predictions of training sets have gaps??

## 23 September

- reject H0 of the Hosmer-L test, or if we think when we plot the residuals on the y axis and either the fitted values, or any of the predictors, on the x axis, that we see anything other than a constant flat line, that means that there are patterns in the data that we're not capturing. If not (i.e. we reject or DHARMa::plot(simulateResiduals(model)) shows trends), then we do want to think about a nonlinear model.

* one way is to add quadratic terms to the logistic fit
* another way is to use a random forest (nonlinear)
* another way is to use a GAM (generalized additive model), which fits a smooth curve instead of a straight line or a quadratic curve

* mgcv::gam(response ~ s(predictor1) + s(predictor2) + ...)

  where s() stands for "smooth term"
 
* clean up random forest script, include explanations of how we know it's overfitting
* learn about Rmarkdown (lots on the web)

## 30 September

* random forest:
   * out-of-bag error (OOB)
   * variable importance
   * try running for the whole spatial set ...
* classification and regression tree (CART) models: classification is for categorical outcomes (like ours)
* `cart` package (also `partykit` [recursive partitioning])



## 7 October

Morrissey, Michael B. ; Ruxton, and Graeme D. Ruxton. “Multiple Regression Is Not Multiple Regressions: The Meaning of Multiple Regression and the Non-Problem of Collinearity.” Philosophy, Theory, and Practice in Biology 10 (2018). http://dx.doi.org/10.3998/ptpbio.16039257.0010.003.

* figure out spatial blocking -> figure out how to compute cross-validation error appropriately across spatial blocks (Roberts et al 2016 paper)
* would like to figure out error metrics that don't get too screwed up by imbalance (ROC/AUC; deviance) -> whichever we pick, we need to compute for spatial cross-validation blocks for whatever methods we use to model (logistic regression, RF, etc. ...)
* long run/big picture:
    * create the 'best' model or models we can (random forest, logistic regression, lasso/penalized regression, ...)
	* make sure that we are incorporating and/or accounting for spatial correlation appropriately
	* discuss results!

## 14 October

- can we understand small differences in AUC/ROC curve between results on different systems, even when `set.seed(101)` is done on both to make sure that we have the same random-number stream?  (There can often be small computational differences between operating systems, versions of R, versions of packages ...)
- are we overfitting if we have a large AUC?
    - if we are calculating AUC on the *training* set, then this is probably overfitting
	- if we are calculating AUC on the *testing* set, **and the observations in the training set are independent of the observations in the training set**, then we're not overfitting
	- but ... when I have a SPATIAL data set, this probably isn't true any more; I have to pick my testing sets in a spatially structured way (like with spatial blocks)

papers about spatial blocking etc:

---
Wenger, Seth J., and Julian D. Olden. “Assessing Transferability of Ecological Models: An Underappreciated Aspect of Statistical Validation.” Methods in Ecology and Evolution 3, no. 2 (April 2012): 260–67. https://doi.org/10.1111/j.2041-210X.2011.00170.x.

Harris, David J. “Generating Realistic Assemblages with a Joint Species Distribution Model.” Methods in Ecology and Evolution 6, no. 4 (April 1, 2015): 465–73. https://doi.org/10.1111/2041-210X.12332 . (Figure 2 shows spatial test vs train)

Roberts, David R., Volker Bahn, Simone Ciuti, Mark S. Boyce, Jane Elith, Gurutzeta Guillera-Arroita, Severin Hauenstein, et al. “Cross-Validation Strategies for Data with Temporal, Spatial, Hierarchical, or Phylogenetic Structure.” Ecography, December 1, 2016, n/a-n/a. https://doi.org/10.1111/ecog.02881.

---

Are existing papers that analyze landscape change making this mistake?

* spatial blocking like this is important at two stages:
     * if using cross-validation for model development, then the blocks should be *spatial* blocks (each fold needs to be spatially separated)
	 * if using a test set to quantify model performance (e.g. AUC), then the test set needs to be spatially separated from the training set

* visualize folds

## 21 October 2020

* "buffering" approach: is there a way to do this with a smaller number of folds, where the testing set in each case has a reasonable number of points in it (say, 50)? (Compare with the figure from the Harris 'mistnet' paper (2015). The reason to do it this way is so that we only have to do 10-fold (rather than 1500-fold! cross-validation)
* buffering is a good way to do it. Block CV might be good enough ...
* we want reasonably independent train and test sets, and we want a reasonable number of folds (5-20), however we get this done is fine with me ...

* we can average AUC across folds

## 28 October 2020

* make sure that names (change/no change, gain/no gain), 0/1 are consistent; make sure that in predb3, no gain and gain are correctly assigned
* compute spatial autocorrelation function of residuals from random forest? (don't know if random forest gives residuals, but it should be possible to get predicted *probabilities* (rather than 0/1 predictions); then the residuals are just (change (0/1) - predicted_prob), e.g. if pred_prob=0.9 and change=1, then the residual is 0.1; residuals will range from -1 to 1

* recompute random forests etc. making sure that the buffering object is being computed consistently

predict() might give 0/1 by default, there might be an option to ask it to give you probabilities instead

let's say that for observation 571 we get a _predicted_ probability from the fitted RF model of 0.9. Then if there actually was a gain of dune (i.e. change=1) then the residual is 0.1

could plot the residuals in space (x,y)

compute spatial autocorrelation function of residuals. That's what will tell us how we ought to be buffering ...
