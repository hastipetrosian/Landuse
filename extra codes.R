##EXAMPLE
## for val.prob
library(rms)  
library(mlmRev)
Contraception$age <- scale(Contraception$age, scale=2*sd(Contraception$age))
m1 <- glm(use ~ livch*urban*age, family=binomial, data=Contraception)
cS <- split(Contraception, Contraception$livch)
names(cS) <- levels(Contraception$livch)
cfits <- map(cS, glm, formula= use ~ urban*age, family=binomial)
tidy_cfits <- map_dfr(cfits, tidy, conf.int=TRUE, .id="livch")
library(ggstance)
print(ggplot(tidy_cfits, aes(x=estimate, y=term, xmin=conf.low, xmax=conf.high, colour=livch))
      ## + geom_errorbar()
      + geom_pointrange(position=position_dodgev(height=0.25))
)

head(predict(m1)) ## log-odds
head(predict(m1,type="response")) ## probabilities
n_use <- as.numeric(Contraception$use)-1  ## convert to 0/1
val.prob(y=n_use, logit=predict(m1))


##Extra Codes for DHARMa
for (y in names(logist_OK)) {
  print(y)
  tt <- safe_tidy(logist_quad_list_lostS[[y]])
}
table(model.frame(mm)$change)
sapply(model.frame(mm),sd)
length(coef(mm))
names(tidy(mm))

### load 'gain', scaled, quadratic
L <- load("saved_logist_fitsS.RData")
print(L)
names(logist_quad_listS)
x <- logist_quad_listS[["2014"]]
ss <- try(simulateResiduals(x))
## Error in approxfun(vals, cumsum(tabulate(match(x, vals)))/(n + 1), method = "linear",  : 
##   need at least two non-NA values to interpolate

S1 <- simulate(x, nsim=100)
S2 <- do.call(cbind, S1)

dd <- get_logist_data(rr_points13[["2014"]],
                      scale=TRUE,
                      direction="gain")
dd <- na.omit(dd)
nrow(dd)
S3=createDHARMa(simulatedResponse = S2, 
                observedResponse = Num_gai_quadS,
                fittedPredictedResponse = predict(logistgain_quadraticS),
                integerResponse = TRUE)
## same problem

set.seed(101)
dd <- data.frame(z=rbinom(10,size=1,prob=0.5),y=rnorm(10),x=rnorm(10))
m <- glm(z~y+x, data=dd,family=binomial)
## compare effects of individual terms
## total = intercept + sum of all terms
## prob = convert total (which is on the log-odds scale) to probability
cbind(predict(testglm,type="terms"), total=predict(testglm), prob=predict(testglm,type="response"))
## using ff for compress files
install.packages("ff")
library(ff)
##Gain -scale=FALSE
ff_logist_quad_list <- ff(map(rr_points13, run_logist_regression, poly_xy_degree=2))

##the ff_logist_quad_list is 1872 in comparision to logist_quad_list that is 1294728160
library(pryr)
object.size(ff_logist_quad_list)
object.size(logist_quad_list)

##Lost -scale=FALSE
ff_logist_quad_list_lost <- ff(map(rr_points13, run_logist_regression, poly_xy_degree=2, direction = "loss"))
##Gain -scale=TRUE
ff_logist_quad_listS <- ff(map(rr_points13, run_logist_regression, poly_xy_degree=2))
##Lost-scale=TRUE
ff_logist_quad_list_lostS <- ff(map(rr_points13, run_logist_regression,poly_xy_degree=2,direction="loss"))

