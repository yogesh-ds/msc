########################################################################################################################
##catboost
########################################################################################################################
library(catboost)
train_pool <- catboost.load_pool(data = train[,-1],label=train$flag)


#test_pool <- catboost.load_pool(data = test,label=test$flag)
test_pool <- catboost.load_pool(data = test[,-1])

# model<-catboost.train(train_pool,  NULL,
#                       params = list(loss_function = 'Logloss',
#                                     iterations = 100, metric_period=10))

model<-catboost.train(train_pool,  NULL,
                      params = list(loss_function = 'Logloss',
                                    iterations = 100, metric_period=10,depth=3,learning_rate=0.06))


pred <- catboost.predict(model, test_pool,prediction_type="Probability")
print(pred)

prediction <- as.numeric(pred > 0.5)
print(head(prediction))

err <- mean(as.numeric(pred > 0.5) != test$flag)
print(paste("test-error=", err))
#test-error= 0.175824175824176


test_conf <- caret::confusionMatrix(data=as.factor(prediction), reference=as.factor(test$flag))
test_conf
# Accuracy : 0.8
# Sensitivity : 0.9          
# Specificity : 0.7


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
conf_matrix_values[conf_matrix_values[,2]>0.76 & conf_matrix_values[,3]>=0.76 & conf_matrix_values[,4]>=0.76,]
#We'll choose cutoff that gives better sensitivity so that we can predict
#heart disease cases more accurately.
#Cutoff of 0.38 gives 80% Sensitivity, 79% Specificity & 79% Accuracy

#---------------------------------------------------------    
# plot of cutoff values vs confusion matrix values 
plotcutoff(cutoff_range,conf_matrix_values)
########################################################################
predicted_response <- factor(ifelse(test_pred >=0.38, "Yes", "No"))
actual_response <- factor(ifelse((test$flag==1), "Yes", "No"))
test_conf <- caret::confusionMatrix(data=predicted_response, reference=actual_response, positive = "Yes")
test_conf

#Accuracy : 0.79         
#Sensitivity : 0.79  
#Specificity : 0.8

library(ROCR)
library(Comp2ROC)


test_cutoff_churn <- ifelse(predicted_response=="Yes",1,0)
test_actual_churn <- ifelse(actual_response=="Yes",1,0)
modeltests(test_cutoff_churn , test_actual_churn)

# KS Statistics Score: 0.58 
# Area Under ROC Curve: 0.79 
# GINI: 0.58

#F1 Score
round(F1_Score(actual_response, predicted_response),digits = 2)
#0.8

# Lift & Gain Chart 
actual_response <- factor(test$flag)
test_cutoff_churn<-factor(test_cutoff_churn)
Churn_decile = lift(actual_response, test_cutoff_churn, groups = 10)
Churn_decile
#      6     9         1      34  81.0    1.35