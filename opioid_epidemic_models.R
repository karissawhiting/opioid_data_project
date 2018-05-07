library(tidyverse)
library(caret)
library(MASS)
library(tree)
library(randomForest)
library(rpart)
library(gbm)
library(glmnet)
library(e1071)
o <- read_csv(file = "./data/clean_data.csv")

# Summary Stats --------------------

o <- o %>%
  dplyr::select(-X1, -State, -Abrev, -MH_Spending, -Op_Prescribers1, -Homeless, -Alldrug_OD) %>%
  mutate(Med_Exp= as.factor(Med_Exp)) %>%
  mutate(Homeless_t = as.numeric(Homeless_t))

map(o, ~sum(is.na(.x)))

cor(o2)
o2 <- o[,-10]
cor(o2)

o2 <- na.omit(o2)
vif(lm(Overdose ~ ., o2))

summary(o)
summary(o$Overdose)
hist(o$Overdose, breaks = 10)
View(cor(o[,-10])) #Opiood claims and # presecriber are correlated

# Linear Mod --------------------

set.seed(1)

# caret cross validation
ctrl<-trainControl(method = "cv", number = 6) # I think this can be changes to leave one out somehow. probably via the "method" argument of this function.
lmCVFit<-train(Overdose ~ ., data = o, method = "lm", trControl = ctrl, metric="MSE")
summary(lmCVFit)
featurePlot(o[,-c(10,1)], y = o$Overdose)

# Same as above but trying manual cross validation to make sure results are similar to the caret results. They seems to be similar
o<-o[sample(nrow(o)),]
folds <- cut(seq(1,nrow(o)),breaks=6,labels=FALSE)

for(i in 1:6){
  #Segement your data by fold using the which() function 
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- o[testIndexes, ]
  trainData <- o[-testIndexes, ]
  mod <- lm(Overdose ~ ., trainData)
  pred <- predict(mod, testData)
  print(sqrt(mean((pred - testData$Overdose)^2)))
}

pred <- predict(lmCVFit, o)
sqrt(mean((pred - o$Overdose)^2)) #RMSE is about 5

# Plot predicted values versus actual values
plot(pred, o$Overdose)

# Both methods (caret and manual) gave a CV error around 10
# residual plots shows non linear is likely. should add squared term or try spline/gam?

# Lasso Mod --------------------

#glmnet way
x<- model.matrix(Overdose ~ ., data = o)[,-1]
cv <- cv.glmnet(x, o$Overdose)
plot(cv)
bestlam=cv$lambda.min

lass<- glmnet(x, o$Overdose, alpha = 1, lambda = bestlam)
plot(lass)
lasso.coef=predict(lass,type="coefficients", s=bestlam)

lasso.pred <- predict(lass, s = bestlam, newx = x)
sqrt(mean((lasso.pred-o$Overdose)^2))


#caret way
ctrl<-trainControl(method = "cv",
                   number = 6)

tuneGrid=expand.grid(alpha=1, lambda=seq(0, 10, by = 0.5))
lasso.mod <-train(Overdose ~ ., data = o, 
                  method = "glmnet", 
                  tuneGrid = tuneGrid,
                  trControl = ctrl, metric="RMSE")
lasso.mod$bestTune 
lasso.mod$finalModel

#get coefficients of final model and see which were selected out
coef(lasso.mod$finalModel, lasso.mod$bestTune$lambda)

#get c.v. error - aroung 7
lasso.mod$results$RMSE[which.min(lasso.mod$results$RMSE)]


# Basic Tree Mod -------------------- 
set.seed(1)

tree.op <- tree(Overdose ~ ., data = o)
summary(tree.op)

plot(tree.op)
text(tree.op, pretty = 0)

set.seed(10)
vec <- c()
for(i in 1:6){
  #Segement your data by fold using the which() function 
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- o[testIndexes, ]
  trainData <- o[-testIndexes, ]
  mod <- tree(Overdose ~ ., data = o)
  pred <- predict(mod, testData)
  print(sqrt(mean((pred - testData$Overdose)^2)))
  vec[i]<- sqrt(mean((pred - testData$Overdose)^2))
}
mean(vec)

ctrl<-trainControl(method = "cv", number = 6) # I think this can be changes to leave one out somehow. probably via the "method" argument of this function.
lmCVFit<-train(Overdose ~ ., data = o, method = "rpart", trControl = ctrl, metric="RMSE")
summary(lmCVFit)


# Pruned Tree Mod --------------------
# tree pruning by selecting alpha via cross val
set.seed(1)

rpart.op <- rpart(Overdose ~., op)
rpart.op
printcp(rpart.op) 
plotcp(rpart.op)

rpart.op2 <- rpart(Overdose ~., op, 
                   control = rpart.control(cp = .021))

printcp(rpart.op2)

#to get training error via rpart from cp, multiply the relative error by the root node error
#using tree package to get training error

tree.op = tree(Overdose ~., op)
plot(tree.op)
text(tree.op, pretty = 0)

prune.op = prune(tree.op, best=3) #best tree has 3 terminal nodes
plot(prune.op)
text(prune.op, pretty = 0)

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

# Boosted Tree Mod --------------------
set.seed(1)
ctrl<-trainControl(method = "cv",
                   number = 6)

tuneGrid<-  expand.grid(interaction.depth = c(3,5), 
                        n.trees = c(300, 500, 750),
                        shrinkage = c(.001,.01, .1),
                        n.minobsinnode = c(2, 5, 8))

boost.mod <-train(Overdose ~ ., data = o, 
                  method = "gbm", 
                  tuneGrid = tuneGrid,
                  trControl = ctrl, metric="RMSE")
boost.mod$bestTune 
boost.mod$finalModel

boost.mod$results$RMSE[which.min(boost.mod$results$RMSE)] #6.09

barplot(varImp(boost.mod))
plot(boost.mod)


# RF Tree Mod --------------------
set.seed(1)
rfmod = randomForest(Overdose ~. ,o, importance = TRUE)
rfmod
summary(rfmod)

pred = predict(rfmod, newdata = o)
importance(rfmod)

sqrt(mean((pred - o$Overdose)^2)) #RMSE is about 3 best!
plot(pred, o$Overdose)

tuneGrid<-  expand.grid(interaction.depth = c(3,5, 7), 
                        n.trees = c(300, 500, 750),
                        shrinkage = c(.001,.01, .1),
                        n.minobsinnode = c(2, 5, 8))

tuneGrid<-  expand.grid(mtry = c(7,12,20))

ctrl<-trainControl(method = "cv", number = 6, verbose = TRUE) # I think this can be changes to leave one out somehow. probably via the "method" argument of this function.
lmCVFit<-train(Overdose ~ ., data = o, method = "rf",
               tuneGrid = tuneGrid,
               trControl = ctrl, metric="RMSE")
summary(lmCVFit)

lmCVFit$finalModel


# Bagged Tree Mod --------------------
set.seed(1)
bag.mod = randomForest(Overdose ~. ,o, mtry = 23, importance = TRUE)
bag.mod
importance(bag.mod)

tree.pred = predict(bag.mod, newdata = test)
table(tree.pred, test$Purchase)

pred = predict(bag.mod, newdata = o)
sqrt(mean((pred - o$Overdose)^2)) #RMSE is about still good !
plot(pred, o$Overdose)

ctrl<-trainControl(method = "cv", number = 6) # I think this can be changes to leave one out somehow. probably via the "method" argument of this function.
lmCVFit<-train(Overdose ~ ., data = o, method = "treebag", trControl = ctrl, metric="RMSE")
summary(lmCVFit)



