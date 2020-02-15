###########################################################################################################################################################
#Heart disease prediction
###########################################################################################################################################################
# 00-Business Understanding
# 01-Prep/Initiation
# 02-Data Prep/Understanding/EDA 
# 03-Model Buliding & Evaluation
# 04-Model Deployment & Conclusions/Reccomendation
###########################################################################################################################################################

#-----------------------------------------------------------------------------------------
# 00-Business Understanding
#-----------------------------------------------------------------------------------------
##Will Write Business Understanding notes here
#-----------------------------------------------------------------------------------------
# 01-Prep/Initiation
#-----------------------------------------------------------------------------------------
#Set working directory and install all required packages below before you run the code.
#Loading required library
library(ggplot2)
library(caTools)
library(MASS)
library(car)
library(dplyr)
library(corrplot)
library(DMwR)
library(ROCR)
library(randomForest)
library(ROSE)
library(woeBinning)
library(scorecard)
library(stringr)
library(Information)
library(caret)
library(MLmetrics)
library(magrittr)
library(mlr)
library(mlrMBO)

# #Functions
# 
# #Function to plot heart disease rate for each variable
plot_heart_disease <- function(cat_var, var_name,ds){
  aggr <- aggregate(flag~cat_var, ds, mean)
  count <- data.frame(table(cat_var))
  count <- count[,-1]
  agg_response <- cbind(aggr, count)

  colnames(agg_response) <- c(var_name, "flag","No. of patients")
  agg_response[, 2] <- format(round(agg_response[, 2], 2))
  ggplot(agg_response, aes(agg_response[, 1], count, label = as.factor(flag))) + geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + geom_text(size = 3, vjust = -0.5) + xlab(var_name)

}
# 

#function for mutating on columns

mutatecol<- function(colname,partition)
{
  clause<- paste(" ",colname ," = case_when( ")
  for (i in 1:length(partition))  
  { 
    if (i==1)
    { clause<-paste(clause , " ( ",colname ," <= ", partition[i]," )~ ",i," , ")
    
    } else if (i>1 
               # & i!=length(partition)
    )
    {
      clause<-paste(clause, " ( ", colname ," > ", partition[i-1]," & ",colname ," <= ",partition[i]," ) ~ ",i," , ")
    }
  }
  clause<-paste(clause , " ( ", colname ," > ", partition[length(partition)]," )~ ",length(partition)+1," ,T~",length(partition)+2," ) ")
  # clause<-paste( " lazyeval::interp( ", clause , ")") 
  print(clause)  
  #   Main_object_copy<-mutate(Main_object_copy,trim(clause))
  # print(head(Main_object_copy[,continuous_varaible[1]],1))
  #   return(Main_object_copy)
  #Main_object_copy<<-Main_object_copy
  # Main_object_copy<<-Main_object_copy
}


#Function to calculate KS Statistics Score,Area Under ROC Curve and GINI & Plot ROC Curve
modeltests <- function(test_cutoff_churn , test_actual_churn) {
  
  #KS Test on testing  data
  pred_object_test<- ROCR::prediction(test_cutoff_churn, test_actual_churn)
  performance_measures_test<- ROCR::performance(pred_object_test, "tpr", "fpr")
  ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
    (attr(performance_measures_test, "x.values")[[1]])
  
  Area_ROCR <- ROCR::performance(pred_object_test, measure = "auc")
  Area_ROCR <- Area_ROCR@y.values[[1]]
  Area_ROCR
  
  pred_default <- data.frame(fpr=unlist(performance_measures_test@x.values), tpr=unlist(performance_measures_test@y.values))
  gini<-(Area_ROCR*2)-1
  cat("KS Statistics Score:",round(max(ks_table_test),digits = 2),"\nArea Under ROC Curve:",round(Area_ROCR,digits = 2),"\nGINI:",round(gini,digits = 2))
  
  
  ggplot(pred_default ,aes(x=fpr, y=tpr)) +
    geom_line(colour="BLUE") +
    geom_line(data=data.frame(), aes(x=c(0,1), y=c(0,1)), colour="black") +
    labs(x="% False Positive", y="% True Positive",title="ROC Curve") +
    theme(axis.text.x=element_text(hjust=1))+ annotate("text", x=0.4, y=0.00, hjust=0, vjust=0, size=5,
                                                       label=paste("AUC =", round(Area_ROCR, 3))) 
  
}



# #Function to calculate confusion matrix for models with change in cutoff
calculate_confusion_matrix <- function(cutoff)
{
  predicted_response <- factor(ifelse(test_pred >=cutoff, "Yes", "No"))
  actual_response <- factor(ifelse((test$flag==1), "Yes", "No"))
  test_conf <- caret::confusionMatrix(data=predicted_response, reference=actual_response, positive = "Yes")
  acc <- round(test_conf$overall[1],2)
  sens <- round(test_conf$byClass[1],2)
  spec <- round(test_conf$byClass[2],2)
  out <- t(as.matrix(c(cutoff,sens, spec, acc)))
  colnames(out) <- c("cutoff","sensitivity", "specificity", "accuracy")
  return(out)
}
# 
# #Function to calculate confusion matrix for RF models with change in cutoff
# RF_confusion_matrix <- function(cutoff) 
# {
#   predicted_response <- as.factor(ifelse(testPred[, 2] >= cutoff, "Yes", "No"))
#   actual_response <- rf_test_data$flag
#   conf <- caret::confusionMatrix(data=predicted_response, reference=actual_response, positive = "Yes")
#   acc <- conf$overall[1]
#   sens <- conf$byClass[1]
#   spec <- conf$byClass[2]
#   out <- t(as.matrix(c(cutoff,sens, spec, acc))) 
#   colnames(out) <- c("cutoff","sensitivity", "specificity", "accuracy")
#   return(out)
# }
# 
# 

#Function to calculate & plot lift-gain
lift <- function(labels , predicted_prob,groups=10) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups)))
  
  #Plot gain chart
  pred <- ROCR::prediction(predicted_prob, labels)
  gain <- ROCR::performance(pred, "tpr", "rpp")
  
  plot(x=c(0, 1), y=c(0, 1), type="l", col="red", lwd=2,ylab="Rate of Actual heart disease", xlab="Rate of Predicted heart disease")
  title(main="Cumulative Gain Chart")
  lines(x=unlist(slot(gain, 'x.values')), y=unlist(slot(gain, 'y.values')), col="green", lwd=3)
  
  return(gaintable)
  
}
# 
# 
# #Function for plotting cutoff values vs confusion matrix values 
plotcutoff <- function(cutoff_range , conf_matrix_values){
  plot(cutoff_range, conf_matrix_values[,2],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
  axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
  axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
  lines(cutoff_range,conf_matrix_values[,3],col="darkgreen",lwd=2)
  lines(cutoff_range,conf_matrix_values[,4],col=4,lwd=2)
  box()
  legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))
}




#-----------------------------------------------------------------------------------------
# 02-Data Prep/Understanding/EDA 
#-----------------------------------------------------------------------------------------
# Load data. 
#Ensure you set correct working directory using setwd
setwd("C:/Personal/01_Cloud_Data/OneDrive/09_MSc/heart/data/cleveland")
cleaveland_data<- read.csv("processed.cleveland.data",stringsAsFactors = F,header = FALSE)
cleaveland_names<- c("age","sex","cp","trestbps","chol","fbs","restecg","thalach","exang","oldpeak","slope","ca","thal","flag")
names(cleaveland_data)<-cleaveland_names




str(cleaveland_data) #303 obs. of  14 variables

sapply(cleaveland_data, function(x) sum(is.na(x)))

length(which(is.na(cleaveland_data$flag)==T))*100/nrow(cleaveland_data)


length(which(cleaveland_data$flag>='1'))*100/nrow(cleaveland_data)
#46% subjects 
length(which(cleaveland_data$flag=='0'))*100/nrow(cleaveland_data)
#54% subjects 

#Save a copy of original data before further cleanup
cleaveland_data_original<-cleaveland_data


cleaveland_data$flag[which(cleaveland_data$flag > 0)] <-1
#45.87459 subjects

sapply(cleaveland_data, function(x) length(which(x=="")))


str(cleaveland_data)
summary(cleaveland_data)


#data has thal and ca as characters.We'll convert to numeric.invalid chars in both will be replaced by na 
cleaveland_data$thal<-as.numeric(cleaveland_data$thal)
cleaveland_data$ca<-as.numeric(cleaveland_data$ca)

#2 NA need imputation 
cleaveland_data$thal[is.na(cleaveland_data$thal)]  <-median(cleaveland_data$thal,na.rm = T)

#4 NA values for CA need imputation.Impute with median since number of na is less
cleaveland_data$ca[is.na(cleaveland_data$ca)]  <-median(cleaveland_data$ca,na.rm = T)


##############################################################################
# Information value and WOE evaluation
#Create info table to see monotonic trends in vars
Infoval <- create_infotables(cleaveland_data,y="flag")
#Infoval$Tables contains tables of woe values and Infoval$Summary contains IV summary

str(Infoval$Tables) #List of data frames which contain bins as 1st variable  and WoE as 4th variable

plot_infotables(Infoval, "thal")#WoE is -ve for thal>=6
plot_infotables(Infoval, "cp")#WoE is +ve for cp>3
plot_infotables(Infoval, "ca")#WoE is +ve for ca>1
plot_infotables(Infoval, "oldpeak")#WoE is +ve for oldpeak>=0.8


plot_infotables(Infoval, "thalach")#WoE is +ve for thalach <145 - shows neative correlation
plot_infotables(Infoval, "exang")#WoE is +ve for exang>0
plot_infotables(Infoval, "slope")#WoE is +ve for slope>1
plot_infotables(Infoval, "age")#WoE is +ve for age>55
plot_infotables(Infoval, "sex")#WoE is +ve for 1 for sex=1/M
plot_infotables(Infoval, "trestbps")#trestbps does not show consistency/monotonic trend
plot_infotables(Infoval, "restecg")#WoE is +ve for restecg>0
plot_infotables(Infoval, "chol")#WoE is +ve for chol>254 - although there're some outliers
plot_infotables(Infoval, "fbs")#WoE is +ve for chol>254 - although there're some outliers
#We do not get monotonic trends for all variables
#WoE analysis alongwith univariate/bivariate analysis gives us important vars from already selected vars.


#################################################EDA#####################################################################
#We'll perform Univariate & Mutivariate analysis and treat outliers-if any

#Age-----
summary(cleaveland_data$age)
#54 is mean age and 56 is meadian and mean is close to median.
quantile(cleaveland_data$age,seq(0,1,0.01))
length(which(cleaveland_data$age<40))
#15 records have age less than 40-small number of young patients
#min age is 29

ggplot(cleaveland_data,aes(x=age,fill=as.factor(flag)))+geom_histogram(stat="count")+ 
  scale_x_discrete(name ="Age", 
                   limits=seq(10,70,3))
#From 27-56 number of heart disease seem to be higher 

table(cleaveland_data[which(cleaveland_data$flag==1),"age"])
# 35 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 
# 2  1  1  2  1  1  3  3  2  3  2  3  2  3  3  4  2  6  5  6 10 12  9  9  7  7  6  4  4  3  6  2  1  3 
# 77 
# 1 
#57-60 age range shows higher heart disease and highest number of heart disease at 58
plot_heart_disease(cleaveland_data$age,"age",cleaveland_data)
#55-63 shows high rate of heart disease.
#At age 61, Heart disease rate is highest
#Age seems to be important factor for heart disease

#based on the hostogram and quaertile we are divingd the data

cleaveland_data$agebins<-cleaveland_data$age
# partition<-c(20,40,60,80)
# mutatecol("agebins",partition)
# cleaveland_data %<>% mutate(mutatecol("agebins",partition))
  
cleaveland_data %<>% mutate(  agebins  = case_when(   (  age  >=  20 & age  < 40   )~  1  ,    (  age  >=  40 & age  < 60 )  ~  2  ,   (  age  <=  60 & age  < 80 )  ~   3  ,   (  age  <=  80 & age  < 100 )  ~   4  ,T~ 5 ))

plot_heart_disease(cleaveland_data$agebins,"agebins",cleaveland_data)
#3rd bin shows highest %age of heart disease followed by 4th

ggplot(cleaveland_data,aes(x=agebins,fill=as.factor(flag)))+geom_histogram(stat="count")
#2nd and 4th bin seem to have more impact since the count is high

##Sex----------------------------------------------------------------
summary(as.factor(cleaveland_data$sex))
# 0   1 
# 97 206 

table(cleaveland_data[which(cleaveland_data$flag==1),"sex"])
# 0   1 
# 25 114 

#More male subjects get heart disease.

ggplot(cleaveland_data,aes(x=sex,fill=as.factor(flag)))+geom_bar(stat="count") 
#males count wise are highest proportion
ggplot(cleaveland_data,aes(x=sex,fill=as.factor(flag)))+geom_bar(stat="count",position = 'fill') 
# male have higher ratio of heart disease
plot_heart_disease(cleaveland_data$sex,"sex",cleaveland_data)
#With 0.55 ratio, male have higher heart disease


str(cleaveland_data)

#cp------------------------------------
unique(cleaveland_data$cp)
#[1] 1 4 3 2
summary(cleaveland_data$cp)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.000   3.000   3.000   3.158   4.000   4.000 

ggplot(cleaveland_data,aes(x=cp,fill=as.factor(flag)))+geom_bar(stat="count") 
#subjects with cp=4 have higher heart disease followed by subjects with cp=3

ggplot(cleaveland_data,aes(x=cp,fill=as.factor(flag)))+geom_bar(stat="count",position = 'fill') 

plot_heart_disease(cleaveland_data$cp,"cp",cleaveland_data)
#Highest 0.73 rate of heart disease for cp=4 followed by cp=3 at 0.21
#CP seems to be important attribute
table(cleaveland_data[which(cleaveland_data$flag==1),"cp"])
# 1   2   3   4 
# 7   9  18 105 

#Higher number of heart disease for those with cp=4 followed by CP=3

##trestbps------------------------------------
summary(cleaveland_data$trestbps)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 94.0   120.0   130.0   131.7   140.0   200.0 
quantile(cleaveland_data$trestbps,seq(0,1,0.01))


ggplot(cleaveland_data,aes(x=trestbps,fill=as.factor(flag)))+geom_histogram(stat="count")+ 
  scale_x_discrete(name ="trestbps", 
                   limits=seq(-10,60,5))

ggplot(cleaveland_data,aes(x=trestbps,fill=as.factor(flag)))+geom_bar(stat="count",position = 'fill') 
table(cleaveland_data[which(cleaveland_data$flag==1),"trestbps"])

plot_heart_disease(cleaveland_data$trestbps,"trestbps",cleaveland_data)

#let's bin the variable
cleaveland_data$trestbpsbins<-cleaveland_data$trestbps

cleaveland_data %<>% mutate(  trestbpsbins  = case_when(   (  trestbps  >=  75 & trestbps  < 100   )~  1  ,    (  trestbps  >=  100 & trestbps  < 125 )  ~  2  ,   (  trestbps  <=  125 & trestbps  < 150 )  ~   3  ,   (  trestbps  <=  150 & trestbps  < 175 )  ~   4  ,(  trestbps  <=  175 & trestbps  < 200 )  ~   5  ,T~ 6 ))

plot_heart_disease(cleaveland_data$trestbpsbins,"trestbpsbins",cleaveland_data)

#trestbps does not seems to provide any specific conclusive insights based on the eda/plots
#although it is clearly seen that trestbps >175 has high heart disease ratio 

ggplot(cleaveland_data,aes(x=trestbpsbins,fill=as.factor(flag)))+geom_histogram(stat="count")
ggplot(cleaveland_data,aes(x=trestbpsbins,fill=as.factor(flag)))+geom_bar(stat="count",position = 'fill') 


#chol--------------------------------------------------------------------
summary(cleaveland_data$chol)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 126.0   211.0   241.0   246.7   275.0   564.0 

ggplot(cleaveland_data,aes(x=chol,fill=as.factor(flag)))+geom_bar(stat="count") 

table(cleaveland_data[which(cleaveland_data$flag==1),"chol"])
#No specific distribution pattern- we many need to bin this variable

ggplot(cleaveland_data,aes(x=chol,fill=as.factor(flag)))+geom_bar(stat="count",position = 'fill') 

plot_heart_disease(as.factor(cleaveland_data$chol),"chol",cleaveland_data)

#let's bin the variable
cleaveland_data$cholbins<-cleaveland_data$chol

cleaveland_data %<>% mutate(  cholbins  = case_when(   (  chol  >=  110 & chol  < 200   )~  1  ,    (  chol  >=  200 & chol  < 300 )  ~  2  ,   (  chol  <=  300 & chol  < 400 )  ~   3  ,   (  chol  <=  400 & chol  < 500 )  ~   4  ,(  chol  <=  500 & chol  < 600 )  ~   5  ,T~ 6 ))

plot_heart_disease(cleaveland_data$cholbins,"cholbins",cleaveland_data)

#Higher cholesterol is associated with heart disease.

ggplot(cleaveland_data,aes(x=cholbins,fill=as.factor(flag)))+geom_bar(stat="count") 
ggplot(cleaveland_data,aes(x=cholbins,fill=as.factor(flag)))+geom_bar(stat="count",position = 'fill') 

plot_heart_disease(as.factor(cleaveland_data$cholbins),"cholbins",cleaveland_data)


###fbs---------------------------------------------
summary(as.factor(cleaveland_data$fbs))
# 0   1 
# 258  45
ggplot(cleaveland_data,aes(x=fbs,fill=as.factor(flag)))+geom_bar(stat="count") 
#Number of subjects with fbs=0 are high as well as they have high number of heart disease occurances.

ggplot(cleaveland_data,aes(x=fbs,fill=as.factor(flag)))+geom_bar(stat="count",position = 'fill') 
#subjects with fbs=1 seem to have slightly higher ratio of heart disease however 

table(cleaveland_data[which(cleaveland_data$flag==1),"fbs"])
# 0   1 
# 117  22 

plot_heart_disease(cleaveland_data$fbs,"fbs",cleaveland_data)
#subjects with fbs=1 seem to have slightly higher ratio of heart disease however 
#This may not be significant variable

##restecg-------------- 
summary(as.factor(cleaveland_data$restecg))
# 0   1   2 
# 151   4 148 

ggplot(cleaveland_data,aes(x=restecg,fill=as.factor(flag)))+geom_bar(stat="count") 
#restecg=2 covers maximum number of the heart disease followed by restecg=1

ggplot(cleaveland_data,aes(x=restecg,fill=as.factor(flag)))+geom_bar(stat="count",position = 'fill') 
#higher heart disease ratio in restecg=1 followed by restecg=2
#However as seen from numbers, restecg=2 has higher numbers as well

table(cleaveland_data[which(cleaveland_data$flag==1),"restecg"])
# 0  1  2 
# 56  3 80

plot_heart_disease(cleaveland_data$restecg,"restecg",cleaveland_data)
#restecg=2 has high heart disease ratio of 0.54 followed by restecg=1.
#restecg>0 can be good predictor 


##thalach------------
summary(cleaveland_data$thalach)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 71.0   133.5   153.0   149.6   166.0   202.0

quantile(cleaveland_data$thalach,seq(0,1,0.01))
#Data seems to be  distributed

ggplot(cleaveland_data,aes(x=thalach,fill=as.factor(flag)))+geom_histogram(stat="count")+ 
  scale_x_discrete(name ="thalach", 
                   limits=seq(1,130,5))

ggplot(cleaveland_data,aes(x=thalach,fill=as.factor(flag)))+geom_bar(stat="count",position = 'fill') 
#fluctuating heart disease ratio across values however, lower thalach values indicate higher proportions of heart disease

plot_heart_disease(cleaveland_data$thalach,"thalach",cleaveland_data)
#lower thalach values indicate higher proportions of heart disease

table(cleaveland_data[which(cleaveland_data$flag==1),"thalach"])
#lower thalach values indicate higher proportions of heart disease
#Binning may better explain the pattern
#let's bin the variable
cleaveland_data$thalachbins<-cleaveland_data$thalach

cleaveland_data %<>% mutate(  thalachbins  = case_when(   (  thalach  >=  70 & thalach  < 100   )~  1  ,    (  thalach  >=  100 & thalach  < 125 )  ~  2  ,   (  thalach  <=  125 & thalach  < 150 )  ~   3  ,   (  thalach  <=  150 & thalach  < 175 )  ~   4  ,(  thalach  <=  175 & thalach  < 200 )  ~   5  ,T~ 6 ))

plot_heart_disease(cleaveland_data$thalachbins,"thalachbins",cleaveland_data)

ggplot(cleaveland_data,aes(x=thalachbins,fill=as.factor(flag)))+geom_histogram(stat="count")

ggplot(cleaveland_data,aes(x=thalachbins,fill=as.factor(flag)))+geom_bar(stat="count",position = 'fill') 


#lower thalach values indicate higher proportions of heart disease-Same pattern is retained
#thalach has negative correlation with heart disease


##exang---------------------
summary(as.factor(cleaveland_data$exang))
# 0   1 
# 204  99 

ggplot(cleaveland_data,aes(x=exang,fill=as.factor(flag)))+geom_histogram(stat="count")+ 
  scale_x_discrete(name ="exang", 
                   limits=seq(0,135,5))

ggplot(cleaveland_data,aes(x=exang,fill=as.factor(flag)))+geom_bar(stat="count",position = 'fill') + 
  scale_x_discrete(name ="exang", 
                   limits=seq(0,135,5))
#exang=1 has more numbers as well as ratio of heart disease


table(cleaveland_data[which(cleaveland_data$flag==1),"exang"])
# 0  1 
# 63 76 

plot_heart_disease(cleaveland_data$exang,"exang",cleaveland_data)
#0.77 ratio for exang=1 is more than double that of exang=0

##oldpeak-ST depression induced by exercise relative to rest ----------------
summary(cleaveland_data$oldpeak)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.00    0.00    0.80    1.04    1.60    6.20


ggplot(cleaveland_data,aes(x=oldpeak,fill=as.factor(flag)))+geom_bar(stat="count") 
#Does not give clear picture based on plot.Need to bin and check

table(cleaveland_data[which(cleaveland_data$flag==0),"oldpeak"])
#data seem to be distributed-may be weak predictor

ggplot(cleaveland_data,aes(x=oldpeak,fill=as.factor(flag)))+geom_bar(stat="count",position = 'fill') 
#ratio shows growth with increase in oldpeak-although not clearly seen in all cases

plot_heart_disease(cleaveland_data$oldpeak,"oldpeak",cleaveland_data)
#ratio shows growth with increase in oldpeak.Need to confirm with binning.

#let's bin the variable
cleaveland_data$oldpeakbins<-cleaveland_data$oldpeak

cleaveland_data %<>% mutate(  oldpeakbins  = case_when(   (  oldpeak  >=  0 & oldpeak  < 1   )~  1  ,    (  oldpeak  >=  1 & oldpeak  < 2 )  ~  2  ,   (  oldpeak  <=  2 & oldpeak  < 3 )  ~   3  ,   (  oldpeak  <=  3 & oldpeak  < 4 )  ~   4  ,(  oldpeak  <=  4 & oldpeak  < 5 )  ~   5  ,T~ 6 ))

plot_heart_disease(cleaveland_data$oldpeakbins,"oldpeakbins",cleaveland_data)

ggplot(cleaveland_data,aes(x=oldpeakbins,fill=as.factor(flag)))+geom_bar(stat="count") 
ggplot(cleaveland_data,aes(x=oldpeakbins,fill=as.factor(flag)))+geom_bar(stat="count",position = 'fill') 
#Binning shows growth in heart disease with increase in oldpeak although ratio growth shows decline at 6th bin


#slope
##slope ----------------
summary(as.factor(cleaveland_data$slope))
# 1   2   3 
# 142 140  21 

ggplot(cleaveland_data,aes(x=slope,fill=as.factor(flag)))+geom_bar(stat="count") 
#slope>1 shows higher counts/ratio of heart disease

table(cleaveland_data[which(cleaveland_data$flag==0),"slope"])
# 1   2   3 
# 106  49   9 


ggplot(cleaveland_data,aes(x=slope,fill=as.factor(flag)))+geom_bar(stat="count",position = 'fill') 
#higher ratio for slope>1

plot_heart_disease(cleaveland_data$slope,"slope",cleaveland_data)
#slope>1 has higher heart disease ratio

#ca-number of major vessels (0-3) colored by flourosopy-------------------------

summary(as.factor(cleaveland_data$ca))
# 0   1   2   3 
# 180  65  38  20 

ggplot(cleaveland_data,aes(x=ca,fill=as.factor(flag)))+geom_bar(stat="count") 
#2 or more vessel blocks  shows higher ratio of heart disease

table(cleaveland_data[which(cleaveland_data$flag==1),"ca"])
# 0  1  2  3 
# 47 44 31 17 

ggplot(cleaveland_data,aes(x=ca,fill=as.factor(flag)))+geom_bar(stat="count",position = 'fill') 
#higher ratio for ca>1

plot_heart_disease(cleaveland_data$ca,"ca",cleaveland_data)
#ca>1 has higher heart disease ratio


#thal
summary(as.factor(cleaveland_data$thal))
# 3   6   7 
# 168  18 117 

ggplot(cleaveland_data,aes(x=thal,fill=as.factor(flag)))+geom_bar(stat="count") 
#thal>6 shows higher counts/ratio of heart disease

table(cleaveland_data[which(cleaveland_data$flag==1),"thal"])
#thal=7 has highest heart disease patients


ggplot(cleaveland_data,aes(x=thal,fill=as.factor(flag)))+geom_bar(stat="count",position = 'fill') 
#higher ratio for thal>6

plot_heart_disease(cleaveland_data$thal,"thal",cleaveland_data)
#thal>6 has higher heart disease ratio


sum(cleaveland_data[which(cleaveland_data$thal>=6), c("flag")])*100/length(cleaveland_data[which(cleaveland_data$thal>=6), c("flag")])
#74.8%

###########################################################Corelation#################################################################
#We'll keep only binned vars for model creation
cleaveland_data$age<-NULL
cleaveland_data$trestbps<-NULL
cleaveland_data$chol<-NULL
cleaveland_data$thalach<-NULL
cleaveland_data$oldpeak<-NULL


#Check correlation of flag with numeric variables
cleaveland_data_numeric<-na.omit(cleaveland_data[sapply(cleaveland_data, is.numeric)])
cortable<-cor(cleaveland_data_numeric)
View(cortable)
#Thalach has negative correlation
#Thal,ca,exang,oldpeak,cp,slope, age,sex have positive correlation
corrplot(cortable, method="circle")
#The same inferences about important variables are reflected in plot

######################################################################################################################################
#Data Prep for modelling

#make flag 1st column
cd<-cleaveland_data[,c(which(colnames(cleaveland_data)=="flag"),which(colnames(cleaveland_data)!="flag"))]

cd$flag<-as.numeric(cd$flag)

set.seed(9000)
indices <- sample.split(cd$flag, SplitRatio = 0.70)
train <- cd[indices, ]
test <- cd[!indices, ]
