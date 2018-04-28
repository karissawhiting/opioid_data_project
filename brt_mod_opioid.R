library(tidyverse)
library(caret)
library(MASS)
library(tree)
o <- read_csv(file = "./data/clean_data.csv")

o <- o %>%
  dplyr::select(-X1, -State, -Abrev, -MH_Spending) %>%
  mutate(Med_Exp= as.factor(Med_Exp))

map(o, ~sum(is.na(.x)))

tree.op <- tree(Overdose ~ ., data = o)
summary(tree.op)
plot(tree.op)
text