### Load libraries----------------------------------------------------------
suppressPackageStartupMessages(library(MASS))
suppressPackageStartupMessages(library(caret))
suppressPackageStartupMessages(library(corrplot))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(MLmetrics))
suppressPackageStartupMessages(library(pROC))
suppressPackageStartupMessages(library(RANN))
suppressPackageStartupMessages(library(DMwR))
suppressPackageStartupMessages(library(WVPlots))
suppressPackageStartupMessages(library(DescTools))
suppressPackageStartupMessages(library(robustHD))
suppressPackageStartupMessages(library(PerformanceAnalytics))
suppressPackageStartupMessages(library(car))
suppressPackageStartupMessages(library(skimr))
suppressPackageStartupMessages(library(e1071))



#Opciones generales
options(scipen = 999,
        readr.num_columns = 0)



### Definition of functions and other useful objects------------------------------------------
#Function to calculate MAD (Median Absolute Deviation) with NA
mad_na <- function(x) {
  if (is.numeric(x)) {
    z <- median(abs(x-median(x, na.rm = T)), na.rm = T)*1.4826
    mad <- ifelse(z == 0, .1, z)
  } else {
    mad <- x
  }
  return(mad)
}


#Function to transform a matrix into robust z-scores
z_rob <- function(x, center = TRUE, scale = TRUE) { 
  Med <- median(x, na.rm = T)
  Mad <- mad_na(x)
  z_train <- (x-Med)/Mad
  z_test <- (x-center)/scale
  if (scale == TRUE & center == TRUE) {
    return(z_train)
  } else {
    return(z_test)
  }
}


# #Function to replace extreme values (z > |3.29|) with NA
# outliersZ <- function(x, values = FALSE) {  
#   Z <- scale(x)
#   x[abs(Z) > 3.29] <- NA 
#   if (values == TRUE) {
#     return(round(Z, 3))
#   } else {
#     return(round(x, 3))
#   }
# }


#Function to winsorise extreme values (z > |2.57|)
wins <- function(x, cut = 2.57) { 
  x <- as.data.frame(x)
  z <- x %>% 
    mutate_if(is.numeric, scale(.))
  z[z > cut] <- cut
  z[z < -cut] <- -cut
  x <- (z*sd(x, na.rm = T)) + mean(x, na.rm = T)
  return(x)
}


#Robust function to winsorise extreme values (z > |2.57|)
wins_rob <- function(x, cut = 2.57, centers, scales, transform = FALSE) { 
  z_matrix <- matrix(rep(NA, nrow(x)*ncol(x)), ncol = ncol(x))
  x_matrix <- z_matrix
  for (i in 1:ncol(x)) {
    z <- scale(x[,i], center = centers[i], scale = scales[i])
    z_wins <- case_when(
      z > cut ~ cut,
      z < -cut ~ -cut,
      TRUE ~ z
    )
    z_matrix[,i] <- z_wins
    x_wins <- z_wins*scales[i] + centers[i]
    x_matrix[,i] <- x_wins
  }
  if (transform == TRUE) {
    z_matrix <- as.data.frame(z_matrix)
    names(z_matrix) = names(x)
    return(z_matrix)
  } else {
    x_matrix <- as.data.frame(x_matrix)
    names(x_matrix) = names(x)
    return(x_matrix)
  }
}


#Function to detect multivariate extreme values with Mahalanobis' Distance (MD)
md <- function(x, showout = FALSE) {
  x <- as.data.frame(x)
  nums <- unlist(lapply(x, is.numeric)) 
  xmd <- x %>% 
    mutate(MD = mahalanobis(x[nums],
                            colMeans(x[nums], na.rm = TRUE),
                            cov(x[nums], use = "complete.obs")))
  crit <- qchisq(.999, (ncol(x[nums])))
  x <- xmd[xmd$MD <= crit | is.na(xmd$MD), -ncol(xmd)]
  y <- xmd[xmd$MD > crit & !is.na(xmd$MD), -ncol(xmd)]
  if (showout == FALSE) {
    return(x)
  } else {
    return(y)
  }
}


#Function to create a mosaic plot using ggplot2
ggMMplot <- function(data, x, y, statDigits = 1, residDigits = 1, pDigits = 3, ...){
  xvar <- deparse(substitute(x))
  yvar <- deparse(substitute(y))
  mydata <- data[c(xvar, yvar)]
  mytable <- table(mydata)
  
  widths <- c(0, cumsum(apply(mytable, 1, sum)))
  heights <- apply(mytable, 1, function(x){c(0, cumsum(x/sum(x)))})
  
  alldata <- data.frame()
  allnames <- data.frame()
  for(i in 1:nrow(mytable)){
    for(j in 1:ncol(mytable)){
      alldata <- rbind(alldata, c(widths[i], widths[i+1], heights[j, i], heights[j+1, i]))
    }
  }
  colnames(alldata) <- c("xmin", "xmax", "ymin", "ymax")
  
  alldata[[xvar]] <- rep(dimnames(mytable)[[1]],rep(ncol(mytable), nrow(mytable)))
  alldata[[yvar]] <- rep(dimnames(mytable)[[2]],nrow(mytable))
  
  chisq <- chisq.test(mytable)
  df <- chisq$parameter
  pval <- chisq$p.value
  chisqval <- chisq$statistic
  # stdResids <- chisq$stdres
  alldata$xcent <- (alldata$xmin + alldata$xmax)/2
  alldata$ycent <- (alldata$ymin + alldata$ymax)/2
  alldata$stdres <- round(as.vector(t(chisq$stdres)), residDigits)
  # print(chisq$stdres)
  # print(alldata)
  
  titleTxt1 <- paste0("Mosaic plot of ",
                      yvar,
                      " against ",
                      xvar,
                      ", ")
  titleTxt2 <- paste0("chisq(",
                      df,
                      ") = ",
                      round(chisqval, statDigits),
                      ", p = ",
                      format.pval(pval, digits = pDigits))
  titleTxt <- paste0(titleTxt1, titleTxt2)
  subTitleTxt <- "Cell labels are standardised residuals"
  
  ggplot(data  = alldata, 
         aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax)) + 
    geom_rect(color="black", aes_string(fill=yvar)) +
    geom_text(aes(x = xcent, y = ycent, label = stdres)) +
    xlab(paste0("Count of '", 
                xvar,
                "', total = ",
                max(alldata$xmax))) + # tweaked by CE
    ylab(paste0("Proportion of '", 
                yvar,
                "' per level of '",
                xvar,
                "'")) +
    ggtitle(titleTxt,
            subtitle = subTitleTxt) +
    theme_bw() +
    theme(plot.title = element_text(hjust = .5),
          plot.subtitle = element_text(hjust = .5))
}



### Load database-------------------------------------------
setwd("C:/Users/veren/github/ML_Project_Predict_Employee_Performance")


#Save database
perf <- read.csv("01_Data/performance.csv")


### Data Screening and cleaning--------------------------------------------------
summary(perf)


#Put target variable first and convert into ordered factor
perf <- perf %>%
  dplyr::select(performance, quality:empowerment) %>% 
  mutate(performance = ordered(performance, levels = c("C", "B", "A")))

summary(perf$performance)


#Round all numerics to two decimals
perf <- perf %>% 
  mutate_if(is.numeric, ~round(., 2))



### EDA-------------------------------------------------------------------------
##Summary
View(skimr::skim(perf))   #histograms show that majority of numeric variables is not normally distributed; better use robust methods.

glimpse(perf)


##Histograms of variables and correlations
#Diagram with all variables
PerformanceAnalytics::chart.Correlation(mutate(perf, performance = as.integer(performance)))


##Missing values
#Are there missing values?
anyNA(perf)


#Percentage of missing values in each variable
data.frame(round(colMeans(is.na(perf)*100),2))


##Extreme values
#Boxplots of competencies
boxplot(perf[1:6], col = "red", main = "Boxplot of competencies")


#Boxplots of potencial measures
boxplot(perf[7:9], col = "red", main = "Boxplot of potencial measures")


#z-values
perf[-1] %>% 
  mutate_all(~scale(.)) %>% 
  summary()


##Correlational Analysis
#Correlations between predictors
round(cor(mutate(perf, performance = as.integer(performance)),
          use = "complete.obs"), 2)


corrplot::corrplot.mixed(corr = cor(mutate(perf, performance = as.integer(performance)),
                                           use = "complete.obs"), upper = "shade")



#Check for multicollinearity: Variance Inflation Factors (VIF) should be < 10
car::vif(lm(as.integer(performance) ~ ., data = perf))


##Predictors grouped by target variable
#Grouped boxplots
caret::featurePlot(x = perf[, -1], 
                   y = perf$performance, 
                   plot = "box",
                   strip = strip.custom(par.strip.text = list(cex = .7)),
                   scales = list(x = list(relation = "free"), 
                                 y = list(relation = "free")),
                   auto.key = list(columns = 3))


#Grouped density plots
caret::featurePlot(x = perf[, -1], 
                   y = perf$performance, 
                   plot = "density",
                   strip = strip.custom(par.strip.text = list(cex = .7)),
                   scales = list(x = list(relation = "free"), 
                                 y = list(relation = "free")),
                   auto.key = list(columns = 3))


##Importance of variables as predictors (generic method)
#Linear filter
caret::filterVarImp(x = perf[, -1], 
                    y = perf$performance,
                    nonpara = F) %>% 
  rownames_to_column() %>% 
  arrange(desc(A))



### Separate into train and test------------------------------------------------------
set.seed(100)
split <- caret::createDataPartition(perf$performance, p = .8, list = F)


#Create train_raw
train_raw <- perf[split, ]


#Create test_raw
test_raw <- perf[-split, ]


#Save test raw
write.csv(test_raw, "01_Data/test_raw.csv")



### Data pre-processing-----------------------------------------------------
# #Steps for train:
# 1. Separate data into target and predictors
# 2. One-Hot-Encoding for categorical predictors (not necessary here)
# 3. Robust winsorizing of predictors (with robust z-values + MAD)
#    to adjust extreme univariate values (threshold alpha = .01)
# 4. Re-integrate pre-processed predictors with target variable
# 5. Use Mahalanobis Distance to inspect (and if necessary, delete) extreme multivariate values, at alpha = .001
# 6. Use preProcess = c("zv", "nzv", "corr", "center", "scale", "knnImpute")
# 
# #Steps for test:
# 1. Separate data into target and predictors
# 2. One-Hot-Encoding for categorical predictors (not necessary here)
# 3. Re-integrate pre-processed predictors with target variable
# 4. Use preProcess (parameters of train)


##Train
#Descriptive stats
View(skimr::skim(train_raw))


#Save target separately
target_train <- train_raw$performance
train <- train_raw[, -1]


#Process one-hot-encoding for categorical variables
dmy <- dummyVars(~ ., data = train, levelsOnly = T, fullRank = T)
train <- as.data.frame(predict(dmy, train))


summary(train)


#Save parameters of train
(centers_train <- apply(train, 2, function(x) median(x, na.rm = T)))
(scales_train <- apply(train, 2, mad_na))


#Winsorizing (robust method with median and mad)
train <- wins_rob(train, centers = centers_train, scales = scales_train)

summary(train)


#Re-integrate data with target
train <- data.frame(performance = target_train, train)


#Mahalanobis Distance (delete multivariate extreme values)
md(train, showout = T)  #just show multivariate outliers; there are none

train <- md(train)


#Pre-process train
prep <- caret::preProcess(train, method = c("zv", "nzv", "corr",
                                            "center", "scale",
                                            "knnImpute"))
train <- as.data.frame(predict(prep, train))


summary(train)
anyNA(train)


##test
#Descriptive stats
View(skimr::skim(test_raw))


#Save target separately
target_test <- test_raw$performance
test <- test_raw[, -1]


#Process one-hot-encoding for categorical variables
test <- as.data.frame(predict(dmy, test))

summary(test)


#Winsorizing (robust method with same median and mad parameters of train)
test <- wins_rob(test, centers = centers_train, scales = scales_train)

summary(test)


#Re-integrate data with target
test <- data.frame(performance = target_test, test)


#Pre-process test (with the same parameters as train)
test <- as.data.frame(predict(prep, test))


summary(test)
anyNA(test)


##Remove redundant objects
rm(list = "target_train", "target_test",
   "train_raw", "test_raw")



### Define cross-validation methods-------------------------------------------------
set.seed(100)


##Control methods for Recursive Feature Elimination
#Linear Discriminant Analysis
rfe_lda <- rfeControl(functions = ldaFuncs,
                      method = "repeatedcv",
                      repeats = 10,
                      verbose = FALSE)


#Random Forest
rfe_rf <- rfeControl(functions = rfFuncs,
                     method = "repeatedcv",
                     repeats = 10,
                     verbose = FALSE)


#Naive Bayes
rfe_nb <- rfeControl(functions = nbFuncs,
                     method = "repeatedcv",
                     repeats = 10,
                     verbose = FALSE)


#Others
rfe_gen <- rfeControl(functions = caretFuncs,
                      method = "repeatedcv",
                      repeats = 5,
                      verbose = FALSE)


##Control method for k-fold cross-validation
ctrl <- caret::trainControl(method = "repeatedcv", repeats = 10,
                            selectionFunction = "best",
                            summaryFunction = multiClassSummary,
                            savePredictions = "final", classProbs = T,
                            allowParallel = T,
                            sampling = "smote")



### Crear modelos ML: Naive Bayes--------------------------------
##Predictor Selection through RFE
set.seed(100)


#Process RFE
ml_nb_rfe <- caret::rfe(performance ~ .,
                        data = train,
                        sizes = c(1:9),
                        metric = "Kappa",
                        rfeControl = rfe_nb)

ml_nb_rfe


#Review predictors importance
caret::varImp(ml_nb_rfe, scale = F)
plot(ml_nb_rfe, type = c("g", "o"))


##Define model
set.seed(100)
ml_nb_gen <- caret::train(performance ~ problem_solving + organisation
                          + service_orientation + quality + innovation,
                          data = train,
                          method = "nb", tuneLength = 10,
                          trControl = ctrl,
                          metric = "Kappa")


##Optimise model
#Review optimised parameters
ml_nb_gen
plot(ml_nb_gen)


#Look up info for Naive Bayes model
modelLookup("nb")


#Define tuneGrid
grid_nb <- expand.grid(fL = 1,
                       usekernel = F,
                       adjust = 0)



#Define final model
set.seed(100)
ml_nb <- caret::train(performance ~ problem_solving + organisation
                      + service_orientation + quality + innovation,
                      data = train,
                      method = "nb", tuneGrid = grid_nb,
                      trControl = ctrl,
                      metric = "Kappa")


ml_nb


#Summary of final model
caret::getTrainPerf(ml_nb)
ml_nb$finalModel


#Predictor importance
(var_nb <- caret::varImp(ml_nb, scale = F))
plot(var_nb)



##Out of sample prediction (with test)
#Predict test targets
set.seed(100)
(pred_nb <- as.ordered(caret::predict.train(ml_nb, newdata = test)))


#ROC curve
(roc_nb <- pROC::multiclass.roc(test$performance, pred_nb))


#Confusion Matrix
caret::confusionMatrix(reference = test$performance,
                       data = pred_nb, 
                       mode = "everything", positive = "A")


##Analyse prediction problems
test %>% 
  dplyr::mutate(performance = as.character(performance),
         performance_pred_nb = as.character(pred_nb),
         correct = case_when(
           performance_pred_nb == performance ~ TRUE,
           TRUE ~ FALSE)) %>% 
  dplyr::filter(correct == FALSE) %>% 
  dplyr::select(performance_pred_nb, performance:correct) %>% 
  View()



### Create ML Model: Linear Discriminant Analysis--------------------------------
## Selection of predictors
set.seed(100)


#Process RFE
ml_lda_rfe <- rfe(performance ~ .,
                  data = train,
                  sizes = c(1:9),
                  metric = "Kappa",
                  rfeControl = rfe_lda)

ml_lda_rfe


#Review importance of predictors
caret::varImp(ml_lda_rfe, scale = F)
plot(ml_lda_rfe, type = c("g", "o"))


##Entrenar modelo
set.seed(100)
ml_lda_gen <- caret::train(performance ~ problem_solving + organisation + service_orientation + quality + innovation + determination,
                           data = train,
                           method = "lda", tuneLength = 10,
                           trControl = ctrl,
                           metric = "Kappa",
                           importance = T)


ml_lda_gen
plot(ml_lda_gen)


##Optimise model
#Look up info for LDA model
modelLookup("lda")   #no tuning parameters


ml_lda <- ml_lda_gen


#Resumen del modelo final
getTrainPerf(ml_lda)
ml_lda$finalModel


#Importance of predictors
(var_lda <- varImp(ml_lda, scale = F))
plot(var_lda)


##Out of sample validity (for test)
#Predict values of test
set.seed(100)
(pred_lda <- as.ordered(caret::predict.train(ml_lda, newdata = test)))


#ROC Curve
(roc_lda <- pROC::multiclass.roc(test$performance, pred_lda))


#Confusion Matrix
caret::confusionMatrix(reference = test$performance,
                       data = pred_lda, 
                       mode = "everything", positive = "A")



### Create model: Random Forest--------------------------------
## Selection of predictors
set.seed(100)


#Process RFE
ml_forest_rfe <- rfe(performance ~ .,
                     data = train,
                     sizes = c(1:9),
                     metric = "Kappa",
                     rfeControl = rfe_rf)

ml_forest_rfe


#Review importance of predictors
caret::varImp(ml_forest_rfe, scale = F)
plot(ml_forest_rfe, type = c("g", "o"))


##Definir modelo
set.seed(100)
ml_forest_gen <- caret::train(performance ~ problem_solving+organisation+
                              quality+innovation+service_orientation+determination,
                              data = train,
                              method = "rf", tuneLength = 10,
                              trControl = ctrl,
                              metric = "Kappa")


##Optimise model
#View optimised parameters
ml_forest_gen
plot(ml_forest_gen)


#Look up info for Random Forest model
modelLookup("rf")


#Define tuneGrid
grid_forest <- expand.grid(mtry = 2)


#Definir modelo final
set.seed(100)
ml_forest <- caret::train(performance ~ problem_solving+organisation+
                          quality+innovation+service_orientation+determination,
                          data = train,
                          method = "rf", tuneGrid = grid_forest,
                          trControl = ctrl)


ml_forest


#Summary of final model
caret::getTrainPerf(ml_forest)
ml_forest$finalModel


#Importance of variables
(var_forest <- caret::varImp(ml_forest, scale = F))
plot(var_forest)


##Out of Sample validation
#Predict values of test
set.seed(100)
(pred_forest <- as.ordered(caret::predict.train(ml_forest, newdata = test)))


#ROC Curve
(roc_forest <- pROC::multiclass.roc(test$performance, pred_forest))


#Confusion Matrix
caret::confusionMatrix(reference = test$performance,
                       data = pred_forest, 
                       mode = "everything", positive = "A")



### Create model: Non-Linear Support Vector Machines--------------------------------
## Selection of predictors
set.seed(100)


#Process RFE
ml_gen_rfe <- rfe(performance ~ .,
                  data = train,
                  sizes = c(1:9),
                  metric = "Kappa",
                  rfeControl = rfe_gen)

ml_gen_rfe


#Review importance of predictors
caret::varImp(ml_gen_rfe, scale = F)
plot(ml_gen_rfe, type = c("g", "o"))


##Define model
#Train model
set.seed(100)
ml_svmnl_gen <- caret::train(performance ~ organisation+curiosity+problem_solving+
                             analysis+determination+empowerment+quality+
                             innovation+service_orientation,
                             data = train,
                             method = "svmRadial", tuneLength = 10,
                             trControl = ctrl,
                             importance = T,
                             metric = "Kappa")


##Optimise model
#Review optimised parameters
ml_svmnl_gen
plot(ml_svmnl_gen)


#Look up info for model svmboost
modelLookup("svmRadial")


#Define tuneGrid
grid_svmnl <- expand.grid(C = .5,
                          sigma = .09128954)


#Define model
set.seed(100)
ml_svmnl <- caret::train(performance ~ organisation+curiosity+problem_solving+
                         analysis+determination+empowerment+quality+
                         innovation+service_orientation,
                         data = train,
                         method = "svmRadial", tuneGrid = grid_svmnl,
                         trControl = ctrl,
                         importance = T)


ml_svmnl


#Summary of final model
getTrainPerf(ml_svmnl)
ml_svmnl$finalModel


#Importance of variables
(var_svmnl <- caret::varImp(ml_svmnl, scale = F))
plot(var_svmnl)


##Out of Sample validation
#Predict values of test
set.seed(100)
(pred_svmnl <- as.ordered(caret::predict.train(ml_svmnl, newdata = test)))


#ROC Curve
(roc_svmnl <- pROC::multiclass.roc(test$performance, pred_svmnl))


#Confusion Matrix
caret::confusionMatrix(reference = test$performance,
                       data = pred_svmnl, 
                       mode = "everything", positive = "A")




### Compare models---------------------------------------------------------------
##Define models to be compared
comp <- caret::resamples(list(LDA = ml_lda,
                              NaiveBayes = ml_nb,
                              RandomForest = ml_forest,
                              NLSVM = ml_svmnl))


##Compare models
#Summary
summary(comp)


#Boxplots
lattice::bwplot(comp, scales = list(x = list(relation = "free"), y = list(relation = "free")))


#Correlations between models
round(caret::modelCor(comp),2)

#LDA - NB: r = .72
#LDA - RF: r = .52
#LDA - SVMNL: r = .52
#NB - RF: r = .54
#NB - SVMNL: r = .57
#RF - SVMNL: r = 0.50



### Save important objects------------------------------------
#Save final model
save(ml_nb, prep, dmy, centers_train, scales_train, wins_rob,
     file = "03_Results/GE_objects.RData")


