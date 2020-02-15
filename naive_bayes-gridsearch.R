library(caTools)
library(caret)
library(naivebayes)

train <- cd[indices, ]
test <- cd[!indices, ]

train$flag<-as.factor(train$flag)
test$flag<-as.factor(test$flag)

nb <- naive_bayes(flag ~ ., train)
summary(nb)
predict(nb, test[,-1], type = "class")


ControlParamteres <- trainControl(method = "cv",
                                  number = 5,
                                  laplace = 0,
                                  usekernel=TRUE,
                                  usepoisson = TRUE,
                                  search = "grid"
)

parametersGrid <-  expand.grid(eta = 0.1, 
                               colsample_bytree=c(0.5,0.7),
                               max_depth=c(50,100),
                               nrounds=100,
                               gamma=1,
                               min_child_weight=2,subsample = c(0.5, 0.6))

train$flag <- as.factor(ifelse(train$flag == 1, "Yes", "No"))
test$flag <- as.factor(ifelse(test$flag == 1, "Yes", "No"))

library(doParallel)
library(foreach)
### Register parallel backend
cl <- makeCluster(detectCores())
registerDoParallel(cl)
getDoParWorkers()




modelxgboost <- caret::train(flag~., 
                      data = train,
                      method = "naive_bayes",
                      trControl = ControlParamteres,
                      tuneGrid=parametersGrid)
stopCluster(cl)
modelxgboost

prediction<-stats::predict(modelxgboost,test[,-1],positive="Yes")

test_conf <- caret::confusionMatrix(data=as.factor(prediction), reference=as.factor(test$flag),positive="Yes")
test_conf
#Accuracy : 0.7912
#Sensitivity : 0.7143
#Specificity : 0.8571

library(ROCR)
library(Comp2ROC)

test_cutoff_churn <- ifelse(prediction=="Yes",1,0)
test_actual_churn <- ifelse(test$flag=="Yes",1,0)

modeltests(test_cutoff_churn , test_actual_churn)

# KS Statistics Score: 0.57 
# Area Under ROC Curve: 0.79 
# GINI: 0.57


#F1 Score
round(F1_Score(test_actual_churn, test_cutoff_churn),digits = 2)
#0.82

# Lift & Gain Chart 
actual_response <- factor(test_actual_churn)
test_cutoff_churn<-factor(test_cutoff_churn)
Churn_decile = lift(actual_response, test_cutoff_churn, groups = 10)
Churn_decile
#6     9         3      34  81.0    1.35
