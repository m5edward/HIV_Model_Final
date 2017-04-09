#Mark Edwards
#CKME 136 Capstone Project
#HIV GLM Logistic Regression

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
  training_glm <- DataSet %>% dplyr::sample_frac(.8)
  testing_glm  <- dplyr::anti_join(DataSet, training_glm, by = 'id')
  
  #Applying values to names for Viral Load and CD4
  names(HIV_Data)[names(HIV_Data)=="VL-t0"] <- "VL.t0"
  names(HIV_Data)[names(HIV_Data)=="CD4-t0"] <- "CD4.t0"
  
  #Create Formula and Regression Model
  model_test_glm <- glm(formula= Resp~VL.t0+CD4.t0, data=training_glm, family=binomial(link="logit"))
  
  print(model_test_glm)
  summary(model_test_glm)

  
  #Anova Test
  anova(model_test_glm, test="Chisq")

  
  #Create Prediction Model
  testing_glm$accuracy <- predict(model_test_glm,type="response",newdata=testing_glm)
  Estimate_glm <- prediction(testing_glm$accuracy,testing_glm$Resp)
  CMat_glm <- confusionMatrix(as.integer(testing_glm$accuracy > 0.5),testing_glm$Resp)
  prf_glm <- performance(Estimate_glm, measure = "tpr", x.measure = "fpr")
  
  #Create ROC Plot
  summary(prf_glm)
  plot(prf_glm)
  
  #Plotting slope vs. VL.t0
  ggplot( testing_glm, aes(x=VL.t0, y=accuracy)) +
    geom_point() +
    geom_smooth( aes(y = accuracy),  method="glm") 
  
  #Plotting slope vs. CD4.t0
  ggplot( testing_glm, aes(x=CD4.t0, y=accuracy)) +
    geom_point() +
    geom_smooth( aes(y = accuracy),  method="glm") 
  
  #Display Results
  print(CMat_glm$table)
  

