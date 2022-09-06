# ************************************************
#  MANM354 MACHINE LEARNING & VISUALISATION
#  1) Overview of the typical customer profile within Telco Dataset
#  2) Which customers are likey to churn and 
#  3) What are the potential chances of their retention
#
#
# Vartan Zahorodnykov
# The Surrey Business School
# University of Surrey
# GUILDFORD
# Surrey GU2 7XH
#
# 20 June 2019
#
# v1.00
#upload the library

rm(list=ls()) # clears all objects in "global environment"
cat("\014") # clears console area


lovelyLibraries<-c("gsubfn","Hmisc","scatterplot3d","corrplot","MASS","swirl","tidyverse",
                   "funModeling","funModeling","rsample","recipes","Hmisc", "ggplot2","ggpubr","formattable","pROC","C50","caret")

pacman::p_load(char=lovelyLibraries,install = TRUE,character.only = TRUE)

#install.packages("gsubfn")
#install.packages("Hmisc")

#library(gsubfn)
#library(pacman)
#library(scatterplot3d)
#library(corrplot) #defines the colour range
#library(MASS) #contains preselected statistical data, only a couple of functions are used
#library(swirl)
#library(tidyverse)
#library(funModeling)
#library(rsample)
#library(recipes)
#library(Hmisc)
#swirl()
### Data Visualisation

source("Debugged_Telco_Prep_Functions.R")

myModelFormula<-function(training_data,fieldNameOutput){
  inputs<-paste(names(training_data)[which(names(training_data)!=fieldNameOutput)],collapse = "+")
  output<-paste(fieldNameOutput,"~")
  formular=as.formula(paste(output,inputs))
  return(formular)
} #endof myModelFormula()

Telco<- NreadDataset("Telco-Customer-Churn.csv") #read the dataset and assign it to the name

### Data examination
summary(Telco) # Summary of statistics of each field and & Missing Values detection
#df_status(Telco)
# 1
Graph1 <- table(Telco$Churn) # The dataset is relatively unbalanced with more than 5140 customers being a non-churn customer against 1860 Churn customers
barplot(Graph1, main = "Customer Churn ", xlab = "Churn", ylab = "Count", col = rainbow(2))

# 2
ggplot(Telco, aes(gender, fill = Churn)) + # Both males and females have the same likelihood to churn
  geom_bar() +
  labs(title = "Churn rate in respect to Gender", 
       x = "Gender", 
       y = "Count")

# 3
ggplot(Telco, aes(SeniorCitizen, fill = Churn)) + # Greater number of customers who are not Senior Citizens are likely to churn
  geom_bar() +
  labs(title = "Churn rate in respect to Senior Citizens", 
       x = "Senior Citizena", 
       y = "Count")

# 4 
ggplot(Telco, aes(Partner, fill = Churn)) + # Customers involved in the relationship are less likely to churn 
  geom_bar() +
  labs(title = "Churn rate in respect to Marital Status", 
       x = "Partner Marital Status?", 
       y = "Count")

# 5 
ggplot(Telco, aes(Dependents, fill = Churn)) + # Customers with dependent less likely to churn
  geom_bar() +
  labs(title = "Churn rate in respect to Dependant Person", 
       x = "Dependant person to a customer", 
       y = "Count")

# 6 
ggplot(Telco, aes(PhoneService, fill = Churn)) + # 90% of Customers have phone service therefore churn rate is higher
  geom_bar() +
  labs(title = "Churn rate in respect to Phone Service", 
       x = "Phone Service", 
       y = "Count")

# 7
ggplot(Telco, aes(MultipleLines, fill = Churn)) + # Churn rate is relatively the same regardless of the Number of Lines
  geom_bar() +
  labs(title = "Churn rate in respect to Multiple Lines", 
       x = "Multiple Lines", 
       y = "Count")

# 8
ggplot(Telco, aes(InternetService, fill = Churn)) + # Fiber Optic Customers are more likely to churn
  geom_bar() +
  labs(title = "Churn rate in respect to Internet Service", 
       x = "Internet Service", 
       y = "Count")

# 9
ggplot(Telco, aes(OnlineBackup, fill = Churn)) + # Customers without online backup are more likely to churn
  geom_bar() +
  labs(title = "Churn rate in respect to Online Backup", 
       x = "Online Back up", 
       y = "Count")

#10 
ggplot(Telco, aes(OnlineSecurity, fill = Churn)) + # Customers without security are more likely to churn
  geom_bar() +
  labs(title = "Churn rate in respect to Online Security", 
       x = "Online Security", 
       y = "Count")

#11 
ggplot(Telco, aes(TechSupport, fill = Churn)) + # Customers without tech support are more likely to churn
  geom_bar() +
  labs(title = "Churn rate in respect to Tech Support", 
       x = "Tech Support", 
       y = "Count")

#12
ggplot(Telco, aes(DeviceProtection, fill = Churn)) + # Customers without the device protection are more likely to churn
  geom_bar() +
  labs(title = "Churn rate in respect to Device Protection", 
       x = "Device Protection", 
       y = "Count")

#13
ggplot(Telco, aes(StreamingMovies, fill = Churn)) + # Customers who don't stream movies are more likely to churn
  geom_bar() +
  labs(title = "Churn rate in respect to Streaming Movies use", 
       x = "Streaming Movies use", 
       y = "Count")

#14
ggplot(Telco, aes(StreamingTV, fill = Churn)) + #Customers who don't stream TV are as likely to churn as customers who do
  geom_bar() +
  labs(title = "Churn rate in respect to Streaming TV use", 
       x = "Stream TV use", 
       y = "Count")

#15
ggplot(Telco, aes(Contract, fill = Churn)) + # Customers with short monthly contracts are much more likely to churn
  geom_bar() +
  labs(title = "Churn rate in respect to Contract Duration", 
       x = "Contract Duration", 
       y = "Count")

#16
ggplot(Telco, aes(PaperlessBilling, fill = Churn)) + #Customers with paperless billing use are much more likely to churn
  geom_bar() +
  labs(title = "Churn rate in respect to Paperless Billing use", 
       x = "Paperless Billing use", 
       y = "Count")

#17
ggplot(Telco, aes(PaymentMethod, fill = Churn)) + # Customers who use electronic check as a Payment method are much more likely to churn 
  geom_bar() +
  labs(title = "Churn rate in respect to the chosen Payment Method", 
       x = "Chosen Payment method", 
       y = "Count")
# Variables: OnlineSecurity, OnlineBackup, DeviceProtection, TechSupport, StreamingTV and StreamingMovies - Customers without these services tend to leave


field_types<-NPREPROCESSING_initialFieldType(Telco) # step 2 determine if fields are numeric/simb

field_types1<- NPREPROCESSING_discreetNumeric(Telco, field_types, cutoff = 4) # 3 determine if numeric fields are ordinal or dicreet

print(field_types)
print(field_types1)
#Assign numerical fields TotalCharges, MonthlyCharges, tenure and symbolic Churn 
#to separate values 
TotalCharges<- Telco$TotalCharges
MonthlyCharges<-Telco$MonthlyCharges
Tenure<-Telco$tenure
Churn<-Telco$Churn

#Visualise Churn rate in respect to TotalCharges, MonthlyCharges and Tenure # (Rdocumentation.org, 2019)
boxplot(TotalCharges ~ Churn, main = "Churn rate in respect to Total Charges", ylab = "Total Charges", xlab = "Churn rate", col=rainbow(2)) #
boxplot(MonthlyCharges ~ Churn, main = "Churn rate in respect to Monthly Charges", ylab = "Monthly charges in USD", xlab = "Churn rate", col=rainbow(2))
boxplot(Tenure ~ Churn, main = "Churn rate in respect to Tenure Duration", ylab = "Service use in months", xlab = "Churn rate", col=rainbow(2))

# Tenure - Customers usually churn after 10 months of service use
# TotalCharges - Customers who churn pay quite little ovarall with a median of around 900 USD
# Customers who churn have high Monthly charges with median of around 80 USD



ordinals<-Telco[,which(field_types1=="ORDINAL")] # create the separate dataframe for ordinal attributes # (Ryman-Tubb, 2019)
#View(ordinals)

discreet<- Telco[,which(field_types1== "DISCREET")] # create the separate dataframe for discreet attributes

### Show a dependancy and high correlation between Montly Charges and Total Charges # (R.tutor, 2019)
ggscatter(Telco, x = "MonthlyCharges", y = "TotalCharges", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "MonthlyCharges in USD", ylab = "TotalCharges in $", main="Pearson Correlation")

#remove Total charges, tenure, etc.
Telco1<-Telco[-20][-19][-6][-1]

               
zscaled<- scale(ordinals, center = TRUE, scale = TRUE) # z-scaled it
#View(zscaled)

myRescale0to1<-function(x){
  minv<-min(x)
  maxv<- max(x)
  return((x-minv)/(maxv-minv))} # additional function to create myRescale # (Ryman-Tubb, 2019)

Telco_scaled1<- NPREPROCESSING_categorical(Telco1, field_types1) #normalised categorical data into 0 and 1 each type on field_types1 # (Ryman-Tubb, 2019)
View(Telco_scaled1)

Tenure_scaled<- myRescale0to1(Telco$tenure) #scale manually Tenure from 0 to 1
MonthlyCharges_scaled<- myRescale0to1(Telco$MonthlyCharges) #scale manually MonthlyCHarges from 0 to 1
TotalCharges_scaled<-myRescale0to1(Telco$TotalCharges) #Scale Manually totalCharges from 0 to 1


Tenure_rounded<-round(Tenure_scaled, digits = 1) #round the Tenure to 1 dec point
MonthlyCharges_rounded<-round(MonthlyCharges_scaled, digits= 1) #round the MonthlyCharges to 1 dec point
TotalCharges_rounded<-round(TotalCharges_scaled, digits = 1) #round TotalCharges to 1 dec point


Telco_All_Scaled<-cbind(Telco_scaled1, Tenure_rounded, MonthlyCharges_rounded) #combine fields scaled in Telco_scaled1 
# and orridals scaled manually (Tenure, MontlyCharges-TotalCharges are redundant variable N/A)
View(Telco_All_Scaled)

### Correlation application
# combinedML<- NPREPROCESSING_redundantFields(Telco_All_Scaled, cutoff=0.95)
# 21

# combinedML<- NPREPROCESSING_redundantFields(Telco_All_Scaled, cutoff=0.5)
#27

Telco_Uncorr<- NPREPROCESSING_redundantFields(Telco_All_Scaled, cutoff=0.6) #determine which field correlate to each other 
# and remove the highly correlated fields as redundant beyond the correlation level of 0.6 # (Ryman-Tubb, 2019)
#24 fields are being removed here
print(paste0("Fields=", ncol(Telco_Uncorr))) #see the current number of fields

# corrplot::cor.mtest(Telco_All_Scal add something else into the correlation



### Split the dataset into 70:30                   
Telco_Uncorr<- Telco_Uncorr[order(runif(nrow(Telco_Uncorr))),] #randomise the dataset file rows order from lowest to highest between 0 and 1 in rows # (Ryman-Tubb, 2019)

training_records<- round(nrow((Telco_Uncorr)))*(70/100) # rounds to either 0 and 1 and gives only 70 of values

training_data <- Telco_Uncorr[1: training_records,] # put training records in tabular form # (Ryman-Tubb, 2019)
testing_data = Telco_Uncorr[-(1:training_records),] # create test records in tabular form


formular<-myModelFormula(training_data, fieldNameOutput = "Churn") #highlight the status given by myModelFormula # (Ryman-Tubb, 2019)
# sets up the parameters fr the LDA linear model, where Churn is an output field, and all other variables input regression variables


#7 To check the status of the field included


LDAmodel<- MASS:: lda(formular, data = training_data) #warns about collinear variables, # (Ryman-Tubb, 2019)
# produces Linear Discriminatn Analysis model
summary(LDAmodel)

results_test<- myEvaluateClassifier(LDAmodel, testing_data, threshold = 0.7) #evaluate LDAmodel # (Ryman-Tubb, 2019)
#Accuracy=77%, Pgood=77%, pbad=77, FPR=44%, TPR=90%
print(results_test)

Classifier_table<-data.frame(Value=t(t(results_test))) #present results_test evaluation in a table form, # (Ryman-Tubb, 2019)
View(Classifier_table)


  x<-seq(0, 1, by = 0.1) #assign x to the sequence of different threshold numbers between 0 and 1 
  
  cfpr<-vector() #assign cfpr to the vector function characteristic 
  ctpr<-vector()
  
  #Apply varience of threshhold (o.1) within each level # (Ryman-Tubb, 2019)
  for(threshold in x){
    results<-myEvaluateClassifier(LDAmodel,testing_data,threshold)
    cfpr<-c(cfpr,results$FPR)
    ctpr<-c(ctpr,results$TPR)}
  
 
  plot(x,cfpr,
       type="l",
       col="blue",
       xlab="Threshold",
       ylab="Error Rate",
       main="Threshold Perfomance LDA Model") #create the plot which will show how accurate your built model on the testing dataset result comparison

lines(x,ctpr,type="l",col="red",lwd=3,lty=2) #Error rate (ERR) of LDA model within the different levels of threshhold # (Ryman-Tubb, 2019)

legend("bottomright",c("FPR","TPR"),col=c("blue","red"),lty=1:2,lwd=2) # descriptions of lines applied to the previous line of code

### ROC chart applied
ROC_LDAmodel<-pROC::roc(testing_data$Churn,results_test$probabilityClass,
        plot=TRUE,auc=TRUE, auc.polygon=TRUE,
        percent=TRUE, grid=TRUE,print.auc=TRUE,
        main="ROC LDA Model") #set up parameters of ROC chart for LDAmodel threshhold evaluation (Rdocumentation, 2019)

ROC_table<-pROC::coords(ROC_LDAmodel, x="best",best.method="closest.topleft",
                 ret=c("threshold", "specificity",
                       "sensitivity","accuracy",
                       "tn", "tp", "fn", "fp",
                       "npv","ppv")) #present ROC findings in the table form # (Ryman-Tubb, 2019)

view(ROC_table)

fpr<-round(100.0-ROC_table["specificity"],digits=2L) ## (Ryman-Tubb, 2019)

#Add crosshairs to the graph
abline(h=ROC_table["sensitivity"],col="blue",lty=3,lwd=2) #add horizontal # (Ryman-Tubb, 2019)
abline(v=ROC_table["specificity"],col="blue",lty=3,lwd=2) #add vertical line to highlight the optimal level

#Add text to the ROC # (Ryman-Tubb, 2019)
text(x=ROC_table["specificity"],y=ROC_table["sensitivity"], adj =
       c(-0.2,2),cex=1,
     col="blue",
     paste("Threshold: ",round(ROC_table["threshold"],digits=4L),
           " TPR: ",round(ROC_table["sensitivity"],digits=2L),
           "% FPR: ",fpr,"%",sep=""))
#Use the "best" threshold to evaluate classifier i.e. Best Threshold level equals to 71% 

Best_ThreshholdResults<-myEvaluateClassifier(LDAmodel,testing_data,ROC_table["threshold"]) #see the classifier results for the Best Threshold level
#View(Best_ThreshholdResults) # (Ryman-Tubb, 2019)

Classifier_table2<-data.frame(Value=t(t(Best_ThreshholdResults))) #present results_test evaluation in a table form
View(Classifier_table2)



####

Decision_Tree <-rpart::rpart(Churn ~., data=training_data, method = "class")
summary(Decision_Tree)
rpart.plot::rpart.plot(Decision_Tree, extra = 106) # [Guru99.com, 2019]

# Confusion Matrix applied on training_data
Decision_Tree_Predict_train <- predict(Decision_Tree, newdata = training_data, type = "class") #[Guru99.com, 2019]
Decision_tree_confusionM<- NcalcConfusion(Decision_Tree_Predict_train, training_data$Churn)
# Accuracy = 78.6%, Sensitivity = 94.5%, Specificity = 34.9%

# Decision Tree applied on testing_data
Decision_Tree_Test <-rpart::rpart(Churn ~., data=testing_data, method = "class") #[Guru99.com, 2019]
rpart.plot::rpart.plot(Decision_Tree_Test, extra = 110)

# Confusion Matrix applied on testing_data
Decision_Tree_Predict_test <- predict(Decision_Tree_Test, newdata = testing_data, type = "class") #[Guru99.com, 2019]
NcalcConfusion(Decision_Tree_Predict_test, testing_data$Churn)
# Accuracy = 80%, pgood = 94, pbad = 42 Sensitivity = 81.2%, Specificity = 25.9%
sum(testing_data$tenu)

# ROC curve applied in the testing data
Roc_Tree_Test<-predict(Decision_Tree_Test, data = testing_data, type = "prob") #[Guru99.com, 2019]
Roc_Tree_Test_plot <- roc(testing_data$Churn, Roc_Tree_Test[,2], plot=TRUE, print.auc=TRUE)
# AUC = 82.4%


# To conclude
# Accuracy = 79%, AUC = 80.7% - Overall this model is worse than Logistic Regression














