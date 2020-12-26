# Prediction of Employee Performance
ML Project: Supervised Classification of Future Employee Performance in a Recruitment Context.

In this ML project, I work on predicting future employee performance
through competency measures collected during the standardised
interviewing process.

This project is a simplified version of a former work assignment,
AND ALL DATA AS MUCH AS THE CONTEXT IN WHICH THIS PROJECT IS EMBEDDED
HERE ARE COMPLETELY SYNTHETIC. However, the dataset is based on a
similar covariance matrix of the predictors and target variable of the
original dataset to closely match the actual work assignment.

The sample has been taken from 225 employees target variable is a
categorical, ordered factor variable where employees in comparable
customer service positions (all from the same Peruvian health sector
company) are given a "D" (low performer; this letter does not actually
exist in the sample dataset), "C" (average performer), "B" (fully
meets expectations) and "A" (exceeds expectations). Employee performance
was measured 6 months upon starting the role at the company. Ratings
of performance were administered by the employees' line managers
through a standardised, multi-faceted performance appraisal method,
then aggregated into a single outcome measure.

The predictors are measures obtained through standarised interviews
in the context of candidate selection during the recruiting process.
Some of them are measures of current competency levels (response
scale 1-5), some others are indicators of future potencial (response
scale 0-1).

Given the target - an ordered, thus inherently categorical variable
- the ML problem at hand was a typical supervised multi-class
classification. As the dataset is much simplified from the original
(just 9 predictor variables in total), the task seems straight-
forward, but doesn't come without potential pitfalls:

- the dataset is very small for ML (just 225 cases in total), which
  I decided to address by using a sophisticated technique of over-
  and undersampling called SMOTE. SMOTE is a mechanism that synthesises
  new observations of underrepresented target categories using KNN.
  SMOTE also serves to mitigate datasets with imbalanced target
  variables (see below).
- the target is quite imbalanced, meaning that the three existing
  performance categories (A, B and C) don't show the same frequencies.
  In the case of dichotomic variables, I usually deal with imbalances
  by adjusting the threshold values, thus modifying precision and
  recall metrics. In this multi-class classification, however, I
  chose Kappa as the appropiate metric to select tuning parameters
  during cross-validation to address imbalance. Kappa is a good metric
  for both multi-class problems and imbalanced datasets as it gives an
  idea of how well the classifier does in comparison to a baseline,
  "random-guesses-model", taking into consideration the actual
  frequencies of each class.
- the predictor variables are (partially) highly skewed which might
  be caused by a range restriction problem. Range restriction occurs
  in a situation where the sample is not representative of an entire
  population, and in fact is a common issue in People Analytics.
  In this specific case, only the best candidates were offered a
  job which means that the bigger part of the population would obtain
  lower values on the predictors - the sample shows low variance on
  most of the predictors. Range restriction therefore may lead to
  skewed correlations. It can partly be estimated by statistical
  procedures, but is hard to fully account for when actual repre-
  sentative data is missing as we have to estimate the real variance
  (parameter) of the predictor variable(s) in the population of all
  candidates - however, these data points are not normally saved or
  easily accesible due to data protection.
- additionally, skewed predictors are calling for robust methods in
  both pre-processing and selection of robust algorithms. As far as
  pre-processing goes, I decided to winsorise the predictor variables
  in the training data set using robust measures, namely medians and
  MADs (median absolute deviation) to normalise a high number of
  outliers without letting them affect the central tendency measures.

In different notebooks, I showcase the end-to-end process from 1)
data screening, cleaning & EDA, 2) pre-processing the train and test
data, 3) modelling, tuning and k-fold cross-validation of four
different classifier algorithms (Naive Bayes, Linear Discriminant
Analysis, Random Forest, and Non-Linear Support Vector Machines),
and 4) comparison of out-of-sample validity and performance metrics
using the test dataset for each model.
