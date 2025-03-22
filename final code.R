gss<-read.csv("/Users/cengxin/Desktop/stats assignment/GSS_HappyData.csv",
              header = TRUE)
str(gss)

#######################################
########## CORRELATION MATRIX##########
#######################################

cor(gss)

#######################################
###### deal with missing data#######
#######################################

#install.packages("mice")#
library(mice)
vars<- c("AGE", "SEX", "RACE", "EDUC", "HEALTH", "MARITAL", 
         "HOMPOP", "PersIncomeAdj", "SOCBAR", "SOCFREND", 
         "SOCOMMUN", "SOCREL", "TVHOURS")
md.pattern(gss[vars])
imp <- mice(gss[vars], m=5, method='pmm', maxit=10, seed=123)
plot(imp)
gss2 <- complete(imp, 1)
str(gss2)
sum(is.na(gss2))

#######################################
##########transform data###############
#######################################


##y is vhappy
gss$VHAPPY<-as.factor(gss$VHAPPY) #vhappy is "very happy",1 is very happy, 0 is not very happy

## x--basic information (individual,home,work/income, social activities)
###age,sex,race, educ,health,marital,hompop,persincomeadj...

gss2$SEX<-factor(gss2$SEX) #1male 2 female
gss2$SEX
gss2$RACE<-factor(gss2$RACE) #1white 2 black 3 other
gss2$HEALTH<-factor(gss2$HEALTH)#1 excellent

summary(gss2$MARITAL) #1-5married widowed  divorced seperated never married
summary(gss2$HOMPOP)

gss3<-cbind(gss$VHAPPY,gss2)
View(gss3)
hist(gss3$AGE)
write.csv(gss3, "~/Desktop/gss3.csv") 

###################################################
###descriptive statistics and viualization###
###################################################
gss3<-read.csv("/Users/cengxin/Desktop/stats assignment/gss3.csv",
              header = TRUE)

######descriptive data######
gss3$VHAPPY<-as.integer(gss3$gss.VHAPPY)
library(dplyr)
gss3$SOCBAR <- recode(gss3$SOCBAR, `1` = 7, `2` = 6, `3` = 5, `5` = 3, `6` = 2, `7` = 1, .default = 4)
gss3$SOCFREND <- recode(gss3$SOCFREND, `1` = 7, `2` = 6, `3` = 5, `5` = 3, `6` = 2, `7` = 1, .default = 4)
gss3$SOCOMMUN  <- recode(gss3$SOCOMMUN, `1` = 7, `2` = 6, `3` = 5, `5` = 3, `6` = 2, `7` = 1, .default = 4)
gss3$SOCREL <- recode(gss3$SOCREL, `1` = 7, `2` = 6, `3` = 5, `5` = 3, `6` = 2, `7` = 1, .default = 4)


aggregate(gss.VHAPPY ~ SOCBAR, function(y) c(mean = mean(y), sd = sd(y)), data = gss3)
aggregate(gss.VHAPPY ~ SOCFREND, function(y) c(mean = mean(y), sd = sd(y)), data = gss3)
aggregate(gss.VHAPPY ~ SOCOMMUN, function(y) c(mean = mean(y), sd = sd(y)), data = gss3)
aggregate(gss.VHAPPY ~ SOCREL, function(y) c(mean = mean(y), sd = sd(y)), data = gss3)
aggregate(gss.VHAPPY ~ TVHOURS, function(y) c(mean = mean(y), sd = sd(y)), data = gss3)
######visualization######
par(mfrow = c(2, 2))
gss3$SOCBAR<-as.factor(gss3$SOCBAR)
gss3$SOCFREND<-as.factor(gss3$SOCFREND)
gss3$SOCOMMUN<-as.factor(gss3$SOCOMMUN)
gss3$SOCREL<-as.factor(gss3$SOCREL)
plot(gss3$SOCBAR, main = "Frequency of Socializing at Bar")
plot(gss3$SOCFREND, main = "Frequency of Socializing with Friends")
plot(gss3$SOCOMMUN, main = "Frequency of Socializing with Neighbors")
plot(gss3$SOCREL, main = "Frequency of Socializing with Relatives")



par(mfrow = c(1,1))
gss3$VHAPPY<-as.factor(gss3$gss.VHAPPY)
gss3$SEX<-as.factor(gss3$SEX) #1male 2 female
gss3$RACE<-as.factor(gss3$RACE) #1white 2 black 3 other
gss3$HEALTH<-as.factor(gss3$HEALTH)#1 excellent
gss3$MARITAL<-as.factor(gss3$MARITAL)

varsx<- c("AGE", "SEX",  "MARITAL", 
         "PersIncomeAdj", "SOCBAR", "SOCFREND", 
          "SOCOMMUN", "SOCREL", "TVHOURS")
for (variable in varsx) {
  plot(gss3$VHAPPY~gss3[[variable]],, main = paste("VHAPPY vs", variable),
       xlab = variable, ylab = "VHAPPY")
}
####data transformation####
gss3$VHAPPY<-as.factor(gss3$gss.VHAPPY)
gss3$MARITAL<-as.factor(gss3$MARITAL)
str(gss3)

par(mfrow = c(2, 2))
plot(gss3$SOCBAR, main = "Frequency of Socializing at Bar")
plot(gss3$SOCFREND, main = "Frequency of Socializing with Friends")
plot(gss3$SOCOMMUN, main = "Frequency of Socializing with Neighbors")
plot(gss3$SOCREL, main = "Frequency of Socializing with Relatives")

library(dplyr)
gss3$SOCBAR <- recode(gss3$SOCBAR, `1` = 7, `2` = 6, `3` = 5, `5` = 3, `6` = 2, `7` = 1, .default = 4)
gss3$SOCFREND <- recode(gss3$SOCFREND, `1` = 7, `2` = 6, `3` = 5, `5` = 3, `6` = 2, `7` = 1, .default = 4)
gss3$SOCOMMUN  <- recode(gss3$SOCFREND, `1` = 7, `2` = 6, `3` = 5, `5` = 3, `6` = 2, `7` = 1, .default = 4)
gss3$SOCREL <- recode(gss3$SOCFREND, `1` = 7, `2` = 6, `3` = 5, `5` = 3, `6` = 2, `7` = 1, .default = 4)

View(gss3)
summary (gss3)
str(gss3)




####### hypothesis test to see if it dependent among different variables########

hist(gss3$AGE)
gss3$age_group<-NA
gss3$age_group[gss$AGE<=30]<-1
gss3$age_group[gss$AGE>30&gss$AGE<=50]<-2
gss3$age_group[gss$AGE>50&gss$AGE<=80]<-3
gss3$age_group[gss$AGE>80]<-4
gss3$age_group<-as.factor(gss3$age_group)
chisq.test(gss3$age_group,gss3$SOCBAR)
chisq.test(gss3$age_group,gss3$SOCFREND)
chisq.test(gss3$age_group,gss3$SOCOMMUN)
chisq.test(gss3$age_group,gss3$SOCREL)
chisq.test(gss3$MARITAL,gss3$SOCBAR)
chisq.test(gss3$MARITAL,gss3$SOCFREND)
chisq.test(gss3$MARITAL,gss3$SOCOMMUN)
chisq.test(gss3$MARITAL,gss3$SOCREL)
chisq.test(gss3$SOCFREND,gss3$SOCBAR)
chisq.test(gss3$SOCFREND,gss3$SOCREL)
gss3$log.TVHOURS<-log(gss3$TVHOURS)
hist(gss3$log.TVHOURS)
anova(lm(gss3$TVHOURS~gss3$SOCFREND))
anova(lm(gss3$TVHOURS~gss3$SOCOMMUN))
anova(lm(gss3$TVHOURS~gss3$SOCFREND))
anova(lm(gss3$TVHOURS~gss3$SOCFREND))

######CrossTable##########
table( gss3$MARITAL,gss3$SOCBAR,gss3$SOCFREND,gss3$SOCOMMUN,gss3$SOCREL,gss3$TVHOURS)

#####glm-but wrong#####
#LM3 <- glm(VHAPPY ~  AGE+SEX+EDUC+HEALTH+MARITAL+PersIncomeAdj+SOCFREND+
             #SOCOMMUN, family = binomial , data=gss3)
#summary(LM3)
#vif(LM3)

###################################################
###lasso logistic regression###
###################################################


#install.packages("glmnet")
library(glmnet)

#################

## Step 1. Select the variables to be included in the set of candidate predictors

## Step 2. Scale continuous predictors
gss3$VHAPPY<-as.integer(gss3$gss.VHAPPY)
gss3$SOCREL <-as.integer(gss3$SOCREL)
gss3$SOCOMMUN  <-as.integer(gss3$SOCOMMUN )
gss3$SOCBAR <-as.integer(gss3$SOCBAR)
gss3$SOCFREND  <-as.integer(gss3$SOCFREND )


str(gss3)
gss3$scaled.PersIncomeAdj = scale(gss3$PersIncomeAdj)
gss3$scaled.TVHOURS = scale(gss3$TVHOURS)
gss3$scaled.SOCBAR= scale(gss3$SOCBAR)
gss3$scaled.SOCFREND = scale(gss3$SOCFREND )
gss3$scaled.SOCOMMUN = scale(gss3$SOCOMMUN )
gss3$scaled.SOCREL = scale(gss3$SOCREL )
gss3$scaled.AGE = scale(gss3$AGE)

#x<- model.matrix(~SEX+MARITAL+SOCBAR+SOCFREND+
                   #SOCOMMUN+SOCREL+ RACE, data = gss3)[, -1]
## make a dataset with only the Dependent and Predictor Variables
str(gss3)
predictors <-c("SEX", "MARITAL", "scaled.SOCBAR", "scaled.SOCFREND", "scaled.SOCOMMUN", 
               "scaled.SOCREL", 
               "scaled.PersIncomeAdj",  "scaled.TVHOURS","scaled.AGE")

predictors <-c("SEX", "MARITAL", "SOCBAR", "SOCFREND", "SOCOMMUN", 
               "SOCREL", 
               "PersIncomeAdj",  "TVHOURS","AGE")
gssdata<-gss3[,c("VHAPPY", predictors)]
View(gssdata)

## Step 3. Split the data into training and testing sets
train.size <- 0.7   ## use 70% of the data for training

library(caret)
library(glmnet)
train.index <- createDataPartition(as.factor(gss3$VHAPPY), p= train.size, list=F)   # createDataPartition function is in the caret package

gssdata.train <- gssdata[train.index, ]
gssdata.test <- gssdata[-train.index, ]


## Step 4. Create a weight variable to correct for imbalance in the response variable

# weights for the training set
p.VHAPPY <- sum(gssdata.train$VHAPPY)/length(gssdata.train$VHAPPY)
weights <- rep(NA, times=length(gssdata.train$VHAPPY))
weights[gssdata.train$VHAPPY == 0] <- p.VHAPPY 
weights[gssdata.train$VHAPPY == 1] <- 1-p.VHAPPY 


## Step 5. Regression procedure for training, identify lambda and fit the model using cross-validation

## MODEL 1:  No interaction terms

cv.lasso <- cv.glmnet(y = as.matrix(gssdata.train$VHAPPY), x = as.matrix(gssdata.train[, -gssdata.train$VHAPPY]), #负号是除去了剩下的y变量
                      
                      family="binomial", na.action = NULL, weights = weights, type.measure = "auc")
coef(cv.lasso)
table_coef<-table(as.vector(coef(cv.lasso)))
odds_ratios <- exp(coef(cv.lasso, cv.lasso$lambda.min))
odds_ratios 

coefficients <- as.matrix(coef(cv.lasso, s = cv.lasso$lambda.min))

odds_ratios <- exp(coefficients)

coef_odds <- data.frame(
  Variable = rownames(coefficients),
  Coefficient = coefficients[, 1],
  Odds_Ratio = odds_ratios[, 1]
)
coef_odds 
write.csv(coef_odds, "~/Desktop/coef_odds.csv") 
###roc curve####
library(pROC)
par(mfrow = c(1,1))
probs <- predict(cv.lasso, as.matrix(gssdata.test[, -which(names(gssdata.test) == "VHAPPY")]), 
                 s = cv.lasso$lambda.min, type = "response")
roc_obj <- roc(gssdata.test$VHAPPY, probs, smoothed = TRUE,
               ci = TRUE, ci.alpha = 0.9, stratified = FALSE,
               plot = TRUE, auc.polygon = TRUE, max.auc.polygon = TRUE, grid = TRUE, 
               print.auc = TRUE, show.thres = TRUE, main = "AUC Lasso Regression: No Interactions")
sens.ci <- ci.se(roc_obj)
plot(sens.ci, type = "shape", col = "lightblue")
plot(sens.ci, type = "bars")

## MODEL 2:  With interaction terms

  #### Set the model formula with interaction variables ('+0' here because glmnet adds an intercept by default)
  interaction_formula <- as.formula(VHAPPY ~ . + .*. + 0) 
  
  # combine the model formula and the data to create the design matrix for the regression 
  X.train <- model.matrix(interaction_formula, gssdata.train)
  X.test <- model.matrix(interaction_formula, gssdata.test)  # later we will need to have these same interactions in place for the test dataset
  # colnames(X.train)  ## This will provide you a list of all the terms that will be included in the model
  
  
  cv.lasso.withInt <- cv.glmnet(y = as.matrix(gssdata.train$VHAPPY), x = as.matrix(X.train), 
                                family="binomial", na.action = NULL, weights = weights, type.measure = "auc")
  odds_ratios <- exp(coef( cv.lasso.withInt,  cv.lasso.withInt$lambda.min))
  odds_ratios 
  
  summary(cv.lasso.withInt )
  
  
  coefficients2 <- as.matrix(coef(cv.lasso.withInt, s = cv.lasso.withInt$lambda.min))
  
  odds_ratios2 <- exp(coefficients2)
  
  coef_odds2 <- data.frame(
    Variable = rownames(coefficients2),
    Coefficient = coefficients2[, 1],
    Odds_Ratio = odds_ratios2[, 1]
  )
  coef_odds2
  write.csv(coef_odds, "~/Desktop/coef_odds.csv") 
  
  ##roc curve####
  library(pROC)
  par(mfrow = c(1,1))

  probs2 <- predict(cv.lasso.withInt, as.matrix(X.test), s=cv.lasso.withInt$lambda.min, type = "response")
  roc_obj <- roc(gssdata.test$VHAPPY, probs2, smoothed = TRUE,
                 ci = TRUE, ci.alpha = 0.9, stratified = FALSE,
                 plot = TRUE, auc.polygon = TRUE, max.auc.polygon = TRUE, grid = TRUE, 
                 print.auc = TRUE, show.thres = TRUE, main = "AUC Lasso Regression: with Interactions")
  
  # Adding confidence intervals to the plot
  sens.ci <- ci.se(roc_obj)
  plot(sens.ci, type="shape", col="red")
  plot(sens.ci, type="bars")
  



