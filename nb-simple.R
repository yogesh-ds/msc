library(e1071)
starttime=Sys.time()
Naive_Bayes_Model=naiveBayes(Diagnosis_Heart_Disease ~., data=train_tbl)
endtime=Sys.time()
endtime-starttime

#What does the model say? Print the model summary
Naive_Bayes_Model
NB_Predictions=predict(Naive_Bayes_Model,test_tbl[,-14])
table(NB_Predictions,test_tbl$Diagnosis_Heart_Disease)



test_conf <- caret::confusionMatrix(data=as.factor(NB_Predictions), reference=as.factor(test_tbl$Diagnosis_Heart_Disease))
test_conf
# Accuracy : 0.8814 
# Sensitivity : 0.8750          
# Specificity : 0.8889


library(ROCR)
library(Comp2ROC)




modeltests(as.numeric(NB_Predictions) , as.numeric(test_tbl$Diagnosis_Heart_Disease))

# KS Statistics Score: 0.76 
# Area Under ROC Curve: 0.88 
# GINI: 0.76


#F1 Score
round(F1_Score(test_tbl$Diagnosis_Heart_Disease, NB_Predictions),digits = 2)
#0.89

# Lift & Gain Chart 
actual_response <- factor(test_tbl$Diagnosis_Heart_Disease)
test_cutoff_churn<-factor(NB_Predictions)
Churn_decile = lift(actual_response, test_cutoff_churn, groups = 10)
Churn_decile
# 5     6         3      24  88.9    1.78

library(mlr)

#Create a classification task for learning on Titanic Dataset and specify the target feature
task = makeClassifTask(data = train_tbl, target = "Diagnosis_Heart_Disease")
#Initialize the Naive Bayes classifier
selected_model = makeLearner("classif.naiveBayes")
#Train the model
NB_mlr = train(selected_model, task)
NB_mlr$learner.model        
#Predict on the dataset without passing the target feature
predictions_mlr = as.data.frame(predict(NB_mlr, newdata = test_tbl[,-14]))

##Confusion matrix to check accuracy

table(predictions_mlr[,1],test_tbl$Diagnosis_Heart_Disease)


test_conf <- caret::confusionMatrix(data=as.factor(predictions_mlr[,1]), reference=as.factor(test_tbl$Diagnosis_Heart_Disease))
test_conf
#Accuracy : 0.8814
#Sensitivity : 0.8750
#Specificity : 0.8889