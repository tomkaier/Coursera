mtcars
dat <- mtcars

# Course 7 - Regression models
# Week 3
# Q1 
lm(mpg ~ as.factor(cyl) + wt, data = dat)
levels(as.factor(dat$cyl))

# Q2 
lm(mpg ~ as.factor(cyl) + wt, data = dat)
lm(mpg ~ as.factor(cyl), data = dat)

# Q3
require(lmtest)
lm1 <- lm(mpg ~ as.factor(cyl) + wt, data = dat)
lm2 <- lm(mpg ~ as.factor(cyl)*wt, data = dat)
lm2
summary(lm1)
summary(lm2)
fit_non_interaction <- lm(mpg ~ cyl + wt, mtcars)
fit_interaction <- lm(mpg ~ cyl + wt + cyl:wt, mtcars)
summary(fit_interaction)
lrtest(fit_interaction, fit_non_interaction)

# Q4
lm(mpg ~ I(wt * 0.5) + factor(cyl), data = mtcars)

# Q5
x <- c(0.586, 0.166, -0.042, -0.614, 11.72)
y <- c(0.549, -0.026, -0.127, -0.751, 1.344)
fit <- lm(y ~ x)
max(hatvalues(fit))

# Q6
x <- c(0.586, 0.166, -0.042, -0.614, 11.72)
y <- c(0.549, -0.026, -0.127, -0.751, 1.344)
fit <- lm(y ~ x)
influence.measures(fit)$infmat[5, 'dfb.x']

# Module 7
library(MASS)
library(dplyr)
?shuttle
shuttle
df <- shuttle
shuttle2 <- mutate(shuttle,auto=1*(use=="auto"))

logreg1 <- glm(auto~factor(wind)-1,family = binomial(link="logit"),data=shuttle2)
summary(logreg1)$coef
Coeffs <- summary(logreg1)$coef
LogoddRatioHeadTail <- Coeffs[1,1]-Coeffs[2,1]
oddRatioHeadTail <- exp(LogoddRatioHeadTail)
oddRatioHeadTail

## Q2 
logreg2 <- glm(auto~factor(wind)+factor(magn)-1,family = binomial(link="logit"),data=shuttle2)
summary(logreg2)$coef
Coeffs <- summary(logreg2)$coef
LogoddRatioHeadTail <- Coeffs[1,1]-Coeffs[2,1]
oddRatioHeadTail <- exp(LogoddRatioHeadTail)
oddRatioHeadTail

# Q4
poisreg4 <- glm(count~factor(spray)-1,family = poisson(link="log"),data=InsectSprays)
poisregcoefs <- summary(poisreg4)$coef
poisregcoefs
rateSprayASprayB<- exp(poisregcoefs[1,1]-poisregcoefs[2,1])
rateSprayASprayB

# Module 8 - Week 2 Quiz
library(AppliedPredictiveModeling)
data(AlzheimerDisease)
data(concrete)
library(caret)
set.seed(1000)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]

library(Hmisc)
cols <- colnames(training)
subCols <- cols[-length(cols)] #all but CompressiveStrength
plotCols = 2
par(mfrow = c(ceil(length(subCols)/plotCols), plotCols))
res <- sapply(subCols, function(colName){
  cut <- cut2(training[,colName])
  lab <- paste0("index: col=",colName)
  plot(training$CompressiveStrength, pch=19, col=cut, xlab=lab, ylab="CompressiveStrength")
})

# Q4
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]
IL_Colnames = grep("^IL", colnames(training), value=TRUE,ignore.case=TRUE)
pcaMod <- preProcess(training[,IL_Colnames], method="pca", thresh=0.9)
pcaMod

# Q5
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433); data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

createSet <- function(ds){
  IL_Colnames = grep("^IL", colnames(ds), value=TRUE,ignore.case=TRUE)
  ds[,IL_Colnames]
}
trainingIL <- createSet(training)
testingIL <- createSet(testing)
model_no_pca <- train(training$diagnosis ~ ., trainingIL, method="glm")
predictIL_no_pca <- predict(model_no_pca,testingIL)
result_no_pca <- confusionMatrix(testing$diagnosis, predictIL_no_pca)
result_no_pca$overall["Accuracy"]
pcaObj <- preProcess(trainingIL, method="pca", thresh=0.8)
trainingIL_pca <- predict(pcaObj, trainingIL)
testingIL_pca <- predict(pcaObj, testingIL)
model_pca <- train(training$diagnosis ~ ., trainingIL_pca, method="glm")
predictIL_pca <- predict(model_pca,testingIL_pca)
result_pca <- confusionMatrix(testing$diagnosis, predictIL_pca)
result_pca$overall["Accuracy"]

# Module 8 - week 3
library(AppliedPredictiveModeling)
data(segmentationOriginal)
library(caret)
set.seed(125)

inTrain = createDataPartition(y = segmentationOriginal$Case, p = .5, list=FALSE)
training = segmentationOriginal[ inTrain,]
testing = segmentationOriginal[-inTrain,]

modFit <- train(Class ~ ., method='rpart', data = training)
modFit$finalModel

testA <- segmentationOriginal[0,]
testA[1,c("TotalIntenCh2", "FiberWidthCh1", "PerimStatusCh1")] <- c(23000, 10, 2)
predict(modFit, testA, type="prob")

# b. TotalIntench2 = 50,000; FiberWidthCh1 = 10;VarIntenCh4 = 100
testB <- segmentationOriginal[0,]
testB[1,c("TotalIntenCh2", "FiberWidthCh1", "VarIntenCh4")] <- c(50000, 10, 100)
predict(modFit, testB, type="prob")

# Question 3
library(pgmm)
data(olive)
olive = olive[,-1]
newdata = as.data.frame(t(colMeans(olive)))

set.seed(125)
modCART2 <- train(Area ~ ., method='rpart', data = olive)
modCART2

predict(modCART2, newdata)

# Question 4
library(ElemStatLearn)
data(SAheart)
set.seed(8484)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]
missClass = function(values,prediction){sum(((prediction > 0.5)*1) != values)/length(values)}

set.seed(13234)
fit <- train(chd ~ age + alcohol + obesity + tobacco + typea + ldl, data=trainSA, method="glm", family="binomial")

predictTrainSA <- predict(fit)
missClass(trainSA$chd,predictTrainSA)

predictTestSA <- predict(fit, testSA)
missClass(testSA$chd,predictTestSA)

# Question 5
library(ElemStatLearn)
data(vowel.train)
data(vowel.test)
set.seed(33833)
vowel.train$y <- as.factor(vowel.train$y)
vowel.test$y <- as.factor(vowel.test$y)

modRF <- train(y ~ ., data=vowel.train, method="rf")
res <- predict(modRF,vowel.test)
varImp(modRF)

# Week 4
library(ElemStatLearn)
data(vowel.train)
data(vowel.test)
vowel.train$y <- as.factor(vowel.train$y)
vowel.test$y <- as.factor(vowel.test$y)
set.seed(33833)

modFit1 <- train(y~., method="rf", data=vowel.train)
modFit2 <- train(y~., method="gbm", data=vowel.train)

pred1 <- predict(modFit1, vowel.test)
pred2 <- predict(modFit2, vowel.test)
confusionMatrix(pred1, vowel.test$y)$overall[1]
confusionMatrix(pred2, vowel.test$y)$overall[1]

pred <- data.frame(pred1, pred2, y=vowel.test$y, agree=pred1 == pred2)
head(pred)
accuracy <- sum(pred1[pred$agree] == pred$y[pred$agree]) / sum(pred$agree)
accuracy # Agreement Accuracy: 0.6569579

# Question 2
library(caret)
library(gbm)
set.seed(3433)
library(AppliedPredictiveModeling)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]
set.seed(62433)

modRF <- train(diagnosis ~., data=training, method="rf")
modgbm <- train(diagnosis ~., data=training, method="gbm", verbose=FALSE)
modlda <- train(diagnosis ~., data=training, method="lda")
predRF <- predict(modRF, testing)
predgbm <- predict(modgbm, testing)
predlda <- predict(modlda, testing)
predDF <- data.frame(predRF, predgbm, predlda, diagnosis = testing$diagnosis)
combModFit <- train(diagnosis ~., method="rf", data=predDF)
combPred <- predict(combModFit, predDF)
# Testing error
sqrt(sum((predRF-testing$diagnosis)^2))

# Accuracies
confusionMatrix(predRF, testing$diagnosis)$overall[1]
confusionMatrix(predgbm, testing$diagnosis)$overall[1]
confusionMatrix(predlda, testing$diagnosis)$overall[1]
confusionMatrix(combPred, testing$diagnosis)$overall[1]

# Question 3
set.seed(3523)
library(AppliedPredictiveModeling)
library(elasticnet)
data(concrete)

inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]

set.seed(233)
modlasso <- train(CompressiveStrength ~., data=training, method="lasso")
modlasso
plot.enet(modlasso$finalModel, xvar="penalty", use.color=T)

# Question 4
library(lubridate) # For year() function below
library(forecast)
dat = read.csv("./data/gaData.csv")
training = dat[year(dat$date) < 2012,]
testing = dat[(year(dat$date)) > 2011,]
tstrain = ts(training$visitsTumblr)

fit <- bats(tstrain)
fit
pred <- forecast(fit, level=95, h=dim(testing)[1])
names(data.frame(pred))
predComb <- cbind(testing, data.frame(pred))
names(testing)
names(predComb)
predComb$in95 <- (predComb$Lo.95 < predComb$visitsTumblr) & 
  (predComb$visitsTumblr < predComb$Hi.95)
# How many of the testing points is the true value within the 
# 95% prediction interval bounds?
prop.table(table(predComb$in95))[2] # 0.9617021

# Question 5
set.seed(3523)
library(AppliedPredictiveModeling)
library(e1071)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]
set.seed(325)
# fit <- svm(CompressiveStrength ~., data=training)
# OR another way
fit <- train(CompressiveStrength ~., data=training, method="svmRadial")
pred <- predict(fit, testing)
acc <- accuracy(pred, testing$CompressiveStrength)
acc
acc[2] # RMSE 6.715009

#--------------------------------
# Shiny
library(shiny)
