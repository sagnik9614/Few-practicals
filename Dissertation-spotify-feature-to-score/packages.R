packages<-c("tidyverse", "lemon", "gridExtra", "caret", "glmnet","reshape2")
inst<-packages %in% installed.packages()
if(length(packages[!inst])>0) install.packages(packages[!inst])

lapply(packages, require, character.only = T)

rm(inst, packages)

