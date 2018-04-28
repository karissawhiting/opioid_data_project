library(tidyverse)
library(caret)
library(MASS)

o <- read_csv("./data/opioid.csv")
names(o)

o2 <- o %>%
  mutate(Op_Prescribers1_t = Op_Prescribers1*100000,
         Homeless_t = Homeless*10 ) %>%
  dplyr::select(-Black, -Hispanic)

write.csv(o2, "./data/clean_data.csv")


