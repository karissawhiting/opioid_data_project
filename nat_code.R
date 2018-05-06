library(tidyverse)
library(caret)
library(MASS)
library(tree)
library(randomForest)
library(rpart)
library(gbm)
library(glmnet)
library(e1071)

op <- read_csv(file = "./data/clean_data.csv")

op <- op %>%
  dplyr::select(-X1, -State, -Abrev, -MH_Spending, -Op_Prescribers1, -Homeless, -Alldrug_OD) %>%
  mutate(Med_Exp= as.factor(Med_Exp)) %>%
  mutate(Homeless_t = as.numeric(Homeless_t))

map(op, ~sum(is.na(.x)))


# cross val lm --------------------
set.seed(1)
cvSplits <- createFolds(op$Overdose, 
                        k = 6, 
                        returnTrain = TRUE)
# returnTrain = TRUE: return indices which are held-out (training data)
str(cvSplits)


K <- 6
mseK1 <- rep(NA, K)

for(k in 1:K)
{
  trRows <- cvSplits[[k]]
  
  fit_tr1 <- lm(Overdose~., data = op[trRows,])
  mseK1[k] <- mean((predict(fit_tr1, op[-trRows,])-op$Overdose[-trRows])^2)
}
# K-fold MSE
sqrt(mean(mseK1))
11.7^2


#Karissa's cv code for comparison:
# Try caret cross validation
ctrl<-trainControl(method = "cv", number = 6) # I think this can be changes to leave one out somehow. probably via the "method" argument of this function.
lmCVFit<-train(Overdose ~ ., data = op, method = "lm", trControl = ctrl, metric="RMSE")
summary(lmCVFit)
featurePlot(op[,-c(10,1)], y = op$Overdose)

# Same as above but trying manual cross validation to make sure results are similar to the caret results. They seems to be similar
op<-op[sample(nrow(op)),]
folds <- cut(seq(1,nrow(op)),breaks=6,labels=FALSE)

for(i in 1:6){
  #Segement your data by fold using the which() function 
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- op[testIndexes, ]
  trainData <- op[-testIndexes, ]
  mod <- lm(Overdose ~ ., trainData)
  pred <- predict(mod, testData)
  print(sqrt(mean((pred - testData$Overdose)^2)))
}

pred <- predict(lmCVFit, op)
sqrt(mean((pred - op$Overdose)^2)) #RMSE is about 5

# Plot predicted values versus actual values
plot(pred, op$Overdose)

# Both methods (caret and manual) gave a CV error around 10
# residual plots shows non linear is likely. should add squared term or try spline/gam?

------------------------------------------------------------------------


# tree pruning by selecting alpha via cross val
library(ISLR)
library(rpart)
set.seed(1)

rpart.op <- rpart(Overdose ~., op)
rpart.op
printcp(rpart.op) 
plotcp(rpart.op)

rpart.op2 <- rpart(Overdose ~., op, 
                   control = rpart.control(cp = .021))

printcp(rpart.op2)
library(party)
library(partykit)
plot(as.party(rpart.op2))

#to get training error via rpart from cp, multiply the relative error by the root node error

#using tree package to get training error
library(tree)
tree.op = tree(Overdose ~., op)
plot(tree.op)
text(tree.op, pretty = 0)

prune.op = prune(tree.op, best=3) #best tree has 3 terminal nodes
plot(prune.op)
text(prune.op, pretty = 0)


#cross val tree -----------------------------------------------
set.seed(1)
cvSplits <- createFolds(op$Overdose, 
                        k = 6, 
                        returnTrain = TRUE)
K <- 6
mseK2a <- rep(NA, K)

for(k in 1:K)
{
  trRows <- cvSplits[[k]]
  fit_tr2a = tree(Overdose ~., data = op[trRows,])
  mseK2a[k] <- mean((predict(fit_tr2a, op[-trRows,])-op$Overdose[-trRows])^2)
}
# K-fold MSE
sqrt(mean(mseK2a))




#cross val tree (pruned) --------------------------------------

set.seed(1)
cvSplits <- createFolds(op$Overdose, 
                        k = 6, 
                        returnTrain = TRUE)
K <- 6
mseK2 <- rep(NA, K)

for(k in 1:K)
{
  trRows <- cvSplits[[k]]
  tree.op = tree(Overdose ~., data = op[trRows,])
  fit_tr2 <- prune(tree.op, best=3)
  mseK2[k] <- mean((predict(fit_tr2, op[-trRows,])-op$Overdose[-trRows])^2)
}
# K-fold RMSE
sqrt(mean(mseK2))
8.9^2

#cross val bagging -------------------------------------------------

library(randomForest)
set.seed(1)

cvSplits <- createFolds(op$Overdose, 
                        k = 6, 
                        returnTrain = TRUE)
K <- 6
mseK3 <- rep(NA, K)

for(k in 1:K)
{
  trRows <- cvSplits[[k]]
  fit_tr3 <- randomForest(Overdose ~ ., data=op[trRows,], mtry=23, importance =TRUE)
  mseK3[k] <- mean((predict(fit_tr3, op[-trRows,])-op$Overdose[-trRows])^2)
}
# K-fold RMSE
sqrt(mean(mseK3))


plot(mseK3,op$Overdose)

impp = importance(fit_tr3)
varImpPlot(fit_tr3)
rownames(impp)[order(imp[, 1], decreasing=TRUE)]


#rf = randomForest(Overdose ~ ., data=op, mtry=23, importance =TRUE)
#pred = mean((predict(rf, op)-op$Overdose)^2)
#sqrt(mean(pred))


#cross val randomforest -------------------------------------------

library(randomForest)
set.seed(1)

cvSplits <- createFolds(op$Overdose, 
                        k = 6, 
                        returnTrain = TRUE)
K <- 6
mseK4 <- rep(NA, K)

for(k in 1:K)
{
  trRows <- cvSplits[[k]]
  fit_tr4 <- randomForest(Overdose ~ ., data=op[trRows,], mtry=7, importance =TRUE)
  mseK4[k] <- mean((predict(fit_tr4, op[-trRows,])-op$Overdose[-trRows])^2)
}
# K-fold MSE
sqrt(mean(mseK4))
fit_tr4$mtry
imp=importance(fit_tr4)
varImpPlot(fit_tr4)

rownames(imp)[order(imp[, 1], decreasing=TRUE)]



#cross val boosting via caret----------------------------------------
library(gbm)
library(caret)
set.seed(1)
pows <- seq(-10, -0.2, by = 0.1)
lambdas <- 10^pows
test.err <- rep(NA, length(lambdas))

ctrl<-trainControl(method = "cv",
                   number = 6)

tuneGrid<-  expand.grid(interaction.depth = c(3,5), 
                        n.trees = c(500, 750),
                        shrinkage = c(.01, .1),
                        n.minobsinnode = c(2, 8))

boost.mod <-train(Overdose ~ ., data = op, 
                  method = "gbm", 
                  tuneGrid = tuneGrid,
                  trControl = ctrl, metric="RMSE")

boost.mod$bestTune 
boost.mod$finalModel
boost.mod$results$RMSE[which.min(boost.mod$results$RMSE)]

plot(boost.mod)

#pred = predict(boost.mod, op, n.trees=750)

#View (cbind.data.frame(pred, op$Overdose))
