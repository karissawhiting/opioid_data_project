library(tidyverse)
library(caret)
library(MASS)
library(tree)
o <- read_csv(file = "./data/clean_data.csv")

o <- o %>%
  dplyr::select(-X1, -State, -Abrev, -MH_Spending, -Op_Prescribers1, -Homeless, -Alldrug_OD) %>%
  mutate(Med_Exp= as.factor(Med_Exp))

map(o, ~sum(is.na(.x)))

tree.op <- tree(Overdose ~ ., data = o)
summary(tree.op)

plot(tree.op)
text(tree.op, pretty = 0)

important(tree.op)


lin.mod <- lm(Overdose ~ ., data = o)

summary(lin.mod)
