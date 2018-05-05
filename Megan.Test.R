### Data Science ii Final project ###
library(tidyverse)
library(caret)
library(MASS)
library(tree)
library(car)

getwd()
setwd("/Users/megfiasco/Desktop/SCHOOL/opioid_data_project/data")


o <- read.csv(file = "clean_data.csv")


o <- o %>%
    dplyr::select(-X, -State, -Abrev, -MH_Spending, -Op_Prescribers1, -Homeless, -Alldrug_OD) %>%
    mutate(Med_Exp= as.factor(Med_Exp))

o.new<- o
o.new[o.new == "NR"] <- NA

map(o.new, ~sum(is.na(.x)))

summary(o.new)

## Association exploration

White.mod <- lm(Overdose ~ White, data = o.new)
summary(White.mod)

Op_Prescribers1_t.mod <- lm(Overdose ~ Op_Prescribers1_t, data = o.new)
summary(Op_Prescribers1_t.mod) ##

Op_Prescribers2.mod <- lm(Overdose ~ Op_Prescribers2, data = o.new)
summary(Op_Prescribers2.mod) ###

Op_Claims.mod <- lm(Overdose ~ Op_Claims, data = o.new)
summary(Op_Claims.mod) # borderline

ExtRel_Claims.mod <- lm(Overdose ~ ExtRel_Claims, data = o.new)
summary(ExtRel_Claims.mod)

Poverty.mod <- lm(Overdose ~ Poverty, data = o.new)
summary(Poverty.mod)

Metro.mod <- lm(Overdose ~ Metro, data = o.new)
summary(Metro.mod)

Homeless_t.mod <- lm(Overdose ~ Homeless_t, data = o.new)
summary(Homeless_t.mod)

Spending.mod <- lm(Overdose ~ Spending, data = o.new)
summary(Spending.mod) ###

RxFilled.mod <- lm(Overdose ~ RxFilled, data = o.new)
summary(RxFilled.mod) #

MH_Spending1<-as.numeric(as.character(o.new$MH_Spending))

Med_Exp.mod <- lm(Overdose ~ Med_Exp, data = o.new)
summary(Med_Exp.mod) #

PoorMH.mod <- lm(Overdose ~ PoorMH, data = o.new)
summary(PoorMH.mod)

Depression.mod <- lm(Overdose ~ Depression, data = o.new)
summary(Depression.mod) ##

Alcohol.mod <- lm(Overdose ~ Alcohol, data = o.new)
summary(Alcohol.mod) ##

Txt_Teens.mod <- lm(Overdose ~ Txt_Teens, data = o.new)
summary(Txt_Teens.mod)

Txt_Adults.mod <- lm(Overdose ~ Txt_Adults, data = o.new)
summary(Txt_Adults.mod) ###

Adults.mod <- lm(Overdose ~ Adults, data = o.new)
summary(Adults.mod) 

Medicaid.mod <- lm(Overdose ~ Medicaid, data = o.new)
summary(Medicaid.mod) 

Unemployment.mod <- lm(Overdose ~ Unemployment, data = o.new)
summary(Unemployment.mod) 

Education.mod <- lm(Overdose ~ Education, data = o.new)
summary(Education.mod) 

Income.mod <- lm(Overdose ~ as.numeric(Income), data = o.new)
summary(Income.mod) 

Foreclosures.mod <- lm(Overdose ~ Foreclosures, data = o.new)
summary(Foreclosures.mod) 

Medicaid_Rx.mod <- lm(Overdose ~ Medicaid_Rx, data = o.new)
summary(Medicaid_Rx.mod) 

## multivariate model ##\
mv.mod0 <- lm(Overdose ~., data = o.new)
summary(mv.mod0)


mv.mod1 <- lm(Overdose ~ Op_Prescribers2 + 
                 RxFilled + Depression , data = o.new)
summary(mv.mod1)


#VIF
vif(lm(mv.mod1, data=o.new)) # explore Op_Prescribers2 & Op_Claims   ?

mv.mod2 <- lm(Overdose ~  + Op_Prescribers2 +
                RxFilled + Med_Exp + Depression + Txt_Adults
              + Op_Prescribers1_t + Medicaid + Medicaid_Rx, data = o.new)
summary(mv.mod2)

vif(lm(mv.mod2, data=o.new)) 

mv.mod3 <- lm(Overdose ~ Op_Prescribers2 + 
                  RxFilled + Depression +
                    Op_Prescribers2 * Op_Claims, data = o.new)
summary(mv.mod3)


# not enough evidence for multicollinearity??

mv.mod4 <- lm(Overdose ~ Op_Prescribers2 + 
                 RxFilled + Depression, data = o.new)
summary(mv.mod4)



###tree methods###

### k fold validation ###
train_control<- trainControl(method="cv", number=10, savePredictions = TRUE)
model<- train(Overdose~., data=o.new, trControl=train_control, method="rpart")
model$pred


set.seed(1)
dat <- data.frame(label=round(runif(100,0,5)),v1=rnorm(100),v2=rnorm(100))
tc <- trainControl("cv",10,savePred=T)
fit <- train(Overdose~.,data=o.new,method="glm",trControl=tc)
fit

?which()

#Randomly shuffle the data
o.new<-yourdata[o.new(nrow(o.new)),]
#Create 10 equally size folds
folds <- cut(seq(1,nrow(o.new)),breaks=10,labels=FALSE)
#Perform 10 fold cross validation
for(i in 1:10){
    #Segement your data by fold using the which() function 
    testIndexes <- which(folds==i,arr.ind=TRUE)
    testData <- o.new[testIndexes, ]
    trainData <- o.new[-testIndexes, ]
    #Use the test and train data partitions however you desire...
}



# basic tree
set.seed(1)
tree.o <- tree(Overdose ~ ., data = o.new)
summary(tree.o)
plot(tree.o)
text(tree.o, pretty = 0)


set.seed(1)
pows <- seq(-10, -0.2, by = 0.5)
lambdas <- 10^pows
train.err <- rep(NA, length(lambdas))


# caret
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


# RF Tree Mod 
set.seed(1)
rfmod = randomForest(Overdose ~. ,o, importance = TRUE)
rfmod
summary(rfmod)

pred = predict(rfmod, newdata = o)
importance(rfmod)

sqrt(mean((pred - o$Overdose)^2))
plot(pred, o$Overdose) #3 




