##############################################################################################################################################################
##lightgbm
##############################################################################################################################################################
library(lightgbm)
#library(Matrix)
library(mltools)
library(data.table)

spmtrain <- sparsify(as.data.table(train[,-1]))
spmtest <- sparsify(as.data.table(test[,-1]))

bst <- lightgbm(
  data = spmtrain
  , label = train$flag
  , num_leaves = 9L
  , learning_rate = 1.0
  , nrounds = 3L
  , objective = "binary"
  , verbose = 2L
)

#imp<-lgb.importance(bst, percentage = TRUE)

pred <- predict(bst, spmtest)
err <- mean(as.numeric(pred > 0.5) != test$flag)
print(paste("test-error=", err))


prediction <- as.numeric(pred > 0.5)
print(head(prediction))

test_conf <- caret::confusionMatrix(data=as.factor(prediction), reference=as.factor(test$flag))
test_conf
# Accuracy : 0.8242
# Sensitivity : 0.8571          
# Specificity : 0.7857


#---------------------------------------------------------    
# Let's find out the optimal probalility cutoff 
#---------------------------------------------------------    

# Creating cutoff values from 0.01 to 0.99 for plotting and initiallizing a matrix of 1000 X 4.
test_pred<-pred
cutoff_range = seq(from=.01,to=1,by=0.01)
conf_matrix_values = matrix(0,100,4)

for(i in 1:100)
{
  conf_matrix_values[i,] = calculate_confusion_matrix(cutoff_range[i])
} 
conf_matrix_values[conf_matrix_values[,2]>=0.79 & conf_matrix_values[,3]>=0.79 & conf_matrix_values[,4]>=0.79,]
#We'll choose cutoff that gives better sensitivity so that we can predict
#heart disease cases more accurately.
#Cutoff of 0.5 gives 79% Sensitivity, 86% Specificity & 82% Accuracy

#---------------------------------------------------------    
# plot of cutoff values vs confusion matrix values 
plotcutoff(cutoff_range,conf_matrix_values)
########################################################################
predicted_response <- factor(ifelse(test_pred >=0.5, "Yes", "No"))
actual_response <- factor(ifelse((test$flag==1), "Yes", "No"))
test_conf <- caret::confusionMatrix(data=predicted_response, reference=actual_response, positive = "Yes")
test_conf

#Accuracy : 0.8242          
#Sensitivity : 0.7857
#Specificity : 0.8571
#Kappa : 0.6451

library(ROCR)
library(Comp2ROC)


test_cutoff_churn <- ifelse(predicted_response=="Yes",1,0)
test_actual_churn <- ifelse(actual_response=="Yes",1,0)
modeltests(test_cutoff_churn , test_actual_churn)

# KS Statistics Score: 0.64
# Area Under ROC Curve: 0.82 
# GINI: 0.64

#F1 Score
round(F1_Score(actual_response, predicted_response),digits = 2)
#0.84

# Lift & Gain Chart 
actual_response <- factor(test$flag)
test_cutoff_churn<-factor(test_cutoff_churn)
Churn_decile = lift(actual_response, test_cutoff_churn, groups = 10)
Churn_decile
#6     9         2      35  83.3    1.39
