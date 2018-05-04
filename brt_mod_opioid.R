library(tidyverse)
library(caret)
library(MASS)
library(tree)
library(randomForest)
library(rpart)
library(gbm)
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

#Opiood claims and # presecribers related
# Linear Mod --------------------

lin.mod <- lm(Overdose ~ ., data = o)
summary(lin.mod)

plot(lin.mod)
#test error
pred <- predict(lin.mod, o)
sqrt(mean((pred - o$Overdose)^2)) #RMSE is about 5
plot(pred, o$Overdose)

featurePlot(o[,-c(10,1)], y = o$Overdose)

#LASSO

#shows non linear is likely. should add term 
# Basic Tree Mod --------------------

tree.op <- tree(Overdose ~ ., data = o)
summary(tree.op)

plot(tree.op)
text(tree.op, pretty = 0)

important(tree.op)

lin.mod <- lm(Overdose ~ ., data = o)

summary(lin.mod)



# Boosted Tree Mod --------------------

set.seed(1)
train_ind <- sample(1:nrow(o), 34)
train <- o[train_ind,]
test <- o[-train_ind,]

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


# RF Tree Mod --------------------

set.seed(1)
rfmod = randomForest(Overdose ~. ,o, importance = TRUE)
rfmod

pred = predict(rfmod, newdata = o)
sqrt(mean((pred - o$Overdose)^2)) #RMSE is about 3 best!
plot(pred, o$Overdose)


# Bagged Tree Mod --------------------
set.seed(1)
bag.mod = randomForest(Overdose ~. ,o, mtry = 17, importance = TRUE)
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
