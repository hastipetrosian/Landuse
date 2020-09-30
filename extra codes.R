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
##randomforest
library(randomForest)
library(caTools)

## get 2014 data, drop NA values
a <- na.omit(rr_points14[["2008"]])
## how big is it?
nrow(a) 

## too big: let's just select the western tip of the study area
a2 <- filter(a,x<604000 & y >284000 &  y < 2846000)
## much smaller
nrow(a2) 

##generate a sequence of random numbers – it ensures that you get the same result if you start with that same seed each time you run the same process. 
set.seed(123)
sample <- sample.split(ss2$change2, SplitRatio = 0.75)

##subset=take random samples from a dataset
train <- subset(ss2, sample == TRUE)
test <- subset(ss2, sample == FALSE)

## REGRESSION: trying to predict {0,1,2,3} [change values]
## this doesn't make sense because
## 0: not-erg before and after (no gain)
## 1: not-erg before, erg after (gain)
## 2: erg before, not-erg after (loss)
## 3: erg before and after (no loss)
##
## regression might make sense if we treated gain {0,1} and loss {2,3}
## separately. BUT: classification probably still makes more sense, because
## a regression framework assumes equal change  in the output variable for
## the same amount of change in the input variable
## suppose we have a place that's very unlikely to gain erg (i.e. the prediction
## is close to 0), and we increase some input value that makes gain more likely
## (e.g. we increase the amount of nearby erg, or upwind erg, or ...)
## then we expect a big change in the output variable (from say 0.01 to 0.5)
## but if we have a place that's *likely* to gain erg (the prediction is already
## close to 1), then it doesn't make sense to increase the prediction by the
## same amount, because a prediction >1 doesn't really make  sense
## ... this is why we usually use logistic regression rather than linear
## regression for binary responses
## which is a long-winded way of saying we should probably do classification
## instead

if (file.exists("rf.RData")) {
  load("rf.RData")
} else {
  rf <- randomForest(formula= factor(change2) ~ . - x - y ,
                     data = train, n.trees=250,interaction.depth=7, do.trace=1,
                     type="classification", proximity=TRUE)
  save("rf",file="rf.RData")
}

rf_tune=tuneRF(formula= factor(change2) ~ . - x - y ,
               data = train, n.trees=c(0,75,200,500),interaction.depth=c(5,6,7,8), do.trace=1,
               type="classification", proximity=TRUE)
plot(rf)
plot(rf$predicted)
#predicting the class for the test data set
pred <- predict(rf, newdata=test,type="class")
table(pred,test$change2)
library(caret)
ff <- function(x) factor(x,levels=0:3,labels=c("no gain","gain","loss","no loss"))
conf500= caret::confusionMatrix(ff(pred),ff(test$change2))
save("conf500",  file="saved_conf-500.RData")

### fit a random forest model (using ranger)
library(ranger)
rf_fit <- train(as.factor(change2) ~ ., 
                data = ss, 
                method = "ranger")
rf_fit2 <- ranger(change2 ~ ., data = train, num.trees = 500, mtry = 6, importance = "impurity", min.node.size = 3, replace = TRUE, num.threads = 3)
save("rf_fit",  file="saved_rf_fit.RData")
rf_pred <- predict(rf_fit, dat_upsample)
confusionMatrix(rf_pred, as.factor(dat_upsample$change2))
