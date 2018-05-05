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

o <- o %>%
  dplyr::select(-X1, -State, -Abrev, -MH_Spending, -Op_Prescribers1, -Homeless, -Alldrug_OD) %>%
  mutate(Med_Exp= as.factor(Med_Exp)) %>%
  mutate(Homeless_t = as.numeric(Homeless_t))

map(o, ~sum(is.na(.x)))

# Summary Stats --------------------

summary(o)

summary(o$Overdose)
hist(o$Overdose, breaks = 10)

cor(o[,-10])

#Opiood claims and # presecriber are correlated

# Linear Mod --------------------

# Try caret cross validation
ctrl<-trainControl(method = "cv", number = 6) # I think this can be changes to leave one out somehow. probably via the "method" argument of this function.
lmCVFit<-train(Overdose ~ ., data = o, method = "lm", trControl = ctrl, metric="RMSE")
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

tree.op <- tree(Overdose ~ ., data = o)
summary(tree.op)

plot(tree.op)
text(tree.op, pretty = 0)

# Boosted Tree Mod --------------------

set.seed(1)

pows <- seq(-10, -0.2, by = 0.5)
lambdas <- 10^pows
train.err <- rep(NA, length(lambdas))

##for (i in 1:length(lambdas)) {
 # boost <- gbm(Overdose ~ ., data = train, distribution = "gaussian", n.trees = 100,
#               shrinkage = lambdas[i])
#  
#  pred.train <- predict(boost, train, n.trees = 1000)
#  train.err[i] <- mean((pred.train - train$Salary)^2)
#}

#Train v test set doesnt work too small
boost <- gbm(Overdose ~ ., data = o, distribution = "gaussian", n.trees = 100)
summary(boost)

pred <- predict(boost, o)
sqrt(mean((pred - o$Overdose)^2)) #RMSE is about 5
plot(pred, o$Overdose)

# caret way 
ctrl<-trainControl(method = "cv",
                   number = 6)

tuneGrid<-  expand.grid(interaction.depth = c(3,5), 
                        n.trees = c(100, 500),
                        shrinkage = c(.01, .1),
                        n.minobsinnode = c(2, 8))

boost.mod <-train(Overdose ~ ., data = o, 
                  method = "gbm", 
                  tuneGrid = tuneGrid,
                  trControl = ctrl, metric="RMSE")
boost.mod$bestTune 
boost.mod$finalModel

boost.mod$results$RMSE[which.min(boost.mod$results$RMSE)] #6.09

# RF Tree Mod --------------------
set.seed(1)
rfmod = randomForest(Overdose ~. ,o, importance = TRUE)
rfmod
summary(rfmod)

pred = predict(rfmod, newdata = o)
importance(rfmod)

sqrt(mean((pred - o$Overdose)^2)) #RMSE is about 3 best!
plot(pred, o$Overdose)


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



# SVM Mod --------------------
tune.out <- tune(svm, Overdose ~ ., data= o, 
                 kernel="linear",
                 ranges=list(cost=c(0.1,1,10, 100),
                             gamma=c(0.0001,0.001,0.01,0.1,0.5) ))

summary(tune.out)

pred <- predict(tune.out$best.model, 
                         newdata = o)

sqrt(mean((pred - o$Overdose)^2)) #6, worst performance
