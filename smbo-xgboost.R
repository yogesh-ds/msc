########################################################################################################################################
#SMBO Parameter tuning - xgboost
########################################################################################################################################

library(mlr)
#> Loading required package: ParamHelpers
library(mlbench)

set.seed(180715)
cd$flag<-as.factor(cd$flag)
tr = createDummyFeatures(cd, target = "flag")
tsk = makeClassifTask(data = tr, target = "flag")
ho = makeResampleInstance("Holdout", tsk)
tsk.train = subsetTask(tsk, ho$train.inds[[1]])
tsk.test = subsetTask(tsk, ho$test.inds[[1]])

lrn = makeLearner("classif.xgboost", nrounds=10)

cv = makeResampleDesc("CV", iters=5)
res = resample(lrn, tsk.train, cv, acc)

# Tune hyperparameters
ps = makeParamSet(makeNumericParam("eta", 0, 1),
                  makeNumericParam("lambda", 0, 200),
                  makeIntegerParam("max_depth", 1, 20)
)
tc = makeTuneControlMBO(budget = 100)
tr = tuneParams(lrn, tsk.train, cv5, acc, ps, tc)

lrn = setHyperPars(lrn, par.vals = tr$x)

# Evaluate performance
mdl = train(lrn, tsk.train)
prd = predict(mdl, tsk.test)

# Final model
#mdl = train(lrn, tsk)



test_conf <- caret::confusionMatrix(data=prd$data$response, reference=prd$data$truth)
test_conf

#Accuracy : 0.8416          
#Sensitivity : 0.8750
#Specificity : 0.8000

library(ROCR)
library(Comp2ROC)


#test_cutoff_churn <- ifelse(predicted_response=="Yes",1,0)
#test_actual_churn <- ifelse(actual_response=="Yes",1,0)

test_cutoff_churn <- as.numeric(prd$data$response)
test_actual_churn <- as.numeric(prd$data$truth)

modeltests(test_cutoff_churn , test_actual_churn)

# KS Statistics Score: 0.68
# Area Under ROC Curve: 0.84 
# GINI: 0.68

#F1 Score
round(F1_Score(test_actual_churn, test_cutoff_churn),digits = 2)
#0.86

# Lift & Gain Chart 
actual_response <- factor(prd$data$truth)
test_cutoff_churn<-factor(prd$data$response)
Churn_decile = lift(actual_response, test_cutoff_churn, groups = 10)
Churn_decile
#     5    10         2      36  80      1.6
