#Mark Edwards
#CKME 136 Capstone Project
#HIV LM Logistic Regression

#Install Packages and Libraries

install.packages("ROCR")
install.packages("caret")
install.packages("e1071")
install.packages("dplyr")

library(ROCR)
library(caret)
library(readr)
library(e1071)
library(dplyr)

  
  #Loading Dataset File
  
  HIV_Data <- read_csv("C:/Users/Mark/Desktop/School/CKME 136/HIV Project/HIV Data.csv")
  View(HIV_Data)
  DataSet <- data.frame(HIV_Data)
  
  #Create training and testing data sets
  
  DataSet$id <- 1:nrow(DataSet)
  training_lm <- DataSet %>% dplyr::sample_frac(.8)
  testing_lm  <- dplyr::anti_join(DataSet, training_lm, by = 'id')
  
  #Applying values to names for Viral Load and CD4
  names(HIV_Data)[names(HIV_Data)=="VL-t0"] <- "VL.t0"
  names(HIV_Data)[names(HIV_Data)=="CD4-t0"] <- "CD4.t0"
  
  #Create Formula and Regression Model
  model_test_lm <- lm(formula= Resp~VL.t0+CD4.t0, data=training_lm)
  
  print(model_test_lm)
  summary(model_test_lm)

  
  #Anova Test
  anova(model_test_lm, test="Chisq")

  
  #Create Prediction Model
  testing_lm$accuracy <- predict(model_test_lm,type="response",newdata=testing_lm)
  Estimate_lm <- prediction(testing_lm$accuracy,testing_lm$Resp)
  CMat_lm <- confusionMatrix(as.integer(testing_lm$accuracy > 0.5),testing_lm$Resp)
  prf_lm <- performance(Estimate_lm, measure = "tpr", x.measure = "fpr")
  
  #Create ROC Plot
  summary(prf_lm)
  plot(prf_lm)
  
  #Plotting slope vs. VL.t0
  ggplot( testing_lm, aes(x=VL.t0, y=accuracy)) +
    geom_point() +
    geom_smooth( aes(y = accuracy),  method="lm") 
  
  #Plotting slope vs. CD4.t0
  ggplot( testing_lm, aes(x=CD4.t0, y=accuracy)) +
    geom_point() +
    geom_smooth( aes(y = accuracy),  method="lm") 
  
  #Display Results
  print(CMat_lm$table)
  

