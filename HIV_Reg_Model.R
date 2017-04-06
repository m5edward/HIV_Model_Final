#Mark Edwards
#CKME 136 Capstone Project
#HIV Logistic Regression

#Install Packages and Libraries

install.packages("ROCR")
install.packages("caret")
install.packages("e1071")

library(ROCR)
library(caret)
library(readr)
library(e1071)

#The Function
Regression_Model <- function()
  
{
  
  #Loading Dataset File
  
  DataSet <- data.frame(HIV_Data)
  
  
  #Naming Viral Load and CD4
  
  names(HIV_Data)[names(HIV_Data)=="VL-t0"] <- "VL.t0"
  names(HIV_Data)[names(HIV_Data)=="CD4-t0"] <- "CD4.t0"
  
  
  #Create Formula
  
  Formula_logic <- 'Resp~VL.t0+CD4.t0'
  print(formula <- as.formula(Formula_logic))
  
  
  #Create training and testing data sets
  
  training_model_data <- sample(nrow(DataSet),floor(nrow(DataSet)*0.8))
  training <- DataSet[training_model_data,]
  testing <- DataSet[-training_model_data,]
  
  
  #Create Regression Model
  
  Reg <- glm(formula, data=training, family = binomial(link="logit"))
  print(Reg)
  
  summary(Reg)
  
  
  #Anova Test
  
  anova(Reg, test="Chisq")
  
  
  #Create Prediction Model
  
  testing$accuracy <- predict(Reg,type="response",newdata=testing)
  Estimate <- prediction(testing$accuracy,testing$Resp)
  CMat <- confusionMatrix(as.integer(testing$accuracy > 0.5),testing$Resp)
  prf <- performance(Estimate, measure = "tpr", x.measure = "fpr")
  
 
  #Create ROC Plot
  
  plot(prf)
  
  
  #Plotting slope vs. VL.t0
  
  ggplot( testing, aes(x=VL.t0, y=accuracy)) +
    geom_point() +
    geom_smooth( aes(y = accuracy),  method="glm") 
  
  
  #Plotting slope vs. CD4.t0
  
  ggplot( testing, aes(x=CD4.t0, y=accuracy)) +
    geom_point() +
    geom_smooth( aes(y = accuracy),  method="glm") 
  
  
  #Display Results
  
  print(CMat$table)
  
}

Regression_Model()
