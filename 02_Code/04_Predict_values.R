### Load libraries----------------------------------------------------------
suppressPackageStartupMessages(library(caret))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(pROC))
suppressPackageStartupMessages(library(DescTools))


### Cargar objetos requeridos
#Set wd
setwd("C:/Users/veren/github/ML_Project_Predict_Employee_Performance")


#Load objects
load("03_Results/GE_objects.RData")



###Generate random sample observation--------------------------------------------
test_raw <- read_csv("01_Data/test_raw.csv")
test_raw <- test_raw[, -1]

examp <- sample(1:nrow(test_raw), 1)


print(paste0("examplary observation equals row number ", examp))


examp <- test_raw[examp,]


### Pre-Process sample observation-----------------------------------------
#Save target separately
target_examp <- examp$performance
examp <- examp[, -1]


#Realize one-hot-encoding for categorical variables
examp <- as.data.frame(predict(dmy, examp))

examp


#Winsorizing (robust method)
examp <- wins_rob(examp, centers = centers_train, scales = scales_train)

examp


#Re-integrate target with examp
examp <- data.frame(performance = target_examp, examp)


#Pre-process sample observation
examp <- as.data.frame(predict(prep, examp))


examp



###Predict performance---------------------------------------------------------
#Predict value
set.seed(100)
(pred_examp <- as.character(caret::predict.train(ml_nb, newdata = examp)))


#Save prediction
pred_examp <- data.frame("performance_pred" = pred_examp, examp)


write.csv(pred_examp, "04_Predictions/pred.csv")