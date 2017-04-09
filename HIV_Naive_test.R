#Mark Edwards
#CKME 136 Capstone Project
#HIV Naive Bayes and Decision Tree

#Install Packages and Libraries

install.packages("ROCR")
install.packages("caret")
install.packages("e1071")
install.packages("dplyr")
install.packages("pls")
install.packages("rpart")

library(ROCR)
library(caret)
library(readr)
library(e1071)
library(dplyr)
library(rpart) 

  #Loading Dataset File
  HIV_Data <- read_csv("C:/Users/Mark/Desktop/School/CKME 136/HIV Project/HIV Data.csv")
  View(HIV_Data)
  DataSet <- data.frame(HIV_Data)
  
  
  #Create training and testing data sets
  DataSet$id <- 1:nrow(DataSet)
  training_nb <- DataSet %>% dplyr::sample_frac(.8)
  testing_nb  <- dplyr::anti_join(DataSet, training_nb, by = 'id')
  
  
  #Applying values to names for Viral Load and CD4
  names(HIV_Data)[names(HIV_Data)=="VL-t0"] <- "VL.t0"
  names(HIV_Data)[names(HIV_Data)=="CD4-t0"] <- "CD4.t0"


  #Naive Bayes Model
  Naive_model <- naiveBayes( formula= Resp~VL.t0+CD4.t0, data = training_nb)
  class(Naive_model)
  summary(Naive_model)
  print(Naive_model)
  
  #naive_pred_test2 <- predict(Naive_model,type="class", newdata =testing_nb)
  
  naive_pred_test <- predict(Naive_model, testing_nb,type="raw")
  print(naive_pred_test)
  summary(naive_pred_test)
  plot(naive_pred_test)
  print(naive_pred_test)
  
  #take the raw probabilities and turn them in to a categorization
  training_nb$accuracy <- apply(naive_pred_test, 1, function(x) {   if(x[1] > 0.5) {return(0)} else {return(1)} })
  testing_nb$accuracy <- apply(naive_pred_test, 1, function(x) {   if(x[1] > 0.5) {return(0)} else {return(1)} })
  plot(testing_nb$accuracy)
  
  
  
  #Plotting VL.t0 vs. CD4.t0
  ggplot( testing_nb, aes(x=CD4.t0, y=VL.t0)) +
    geom_point(aes(color = factor(accuracy)), position = "jitter",size=3) 
  
  #Plotting VL.t0 vs. CD4.t0
  ggplot( testing_nb, aes(x=CD4.t0, y=testing_nb$accuracy)) +
    geom_point(aes(color = factor(accuracy)), position = "jitter",size=3)
  
  #Display Results
  CMat_nb <- confusionMatrix(as.integer(testing_nb$accuracy > 0.5),testing_nb$Resp)
  print(CMat_nb$table)
  
  anova(Naive_model, test="Chisq")
  
  testing_nb$accuracy <- predict(Naive_model,type="class",newdata=testing_nb)
  Estimate_nb <- prediction(testing_nb$accuracy,testing_nb$Resp)
  prf_nb <- performance(Estimate_nb, measure = "tpr", x.measure = "fpr")
  plot(prf_nb)
  summary()
  

 
  
  
  

  
  
  
  
  
  
  #Decision Tree
  #dt <- rpart(formula= Resp~VL.t0+CD4.t0, data=training_nb, method = "class") 
  dt_model <- rpart(formula= Resp~VL.t0+CD4.t0,
               method="class", data=training_glm)
  
  table(predictedY,training_glm$accuracy)

  plot(dt_model)
  text(dt_model)
  summary(predictedY)
  
  ggplot( testing_nb, aes(x=VL.t0, y=accuracy)) +
    geom_point() +
    geom_smooth( aes(y = accuracy),  method="glm")
  
  
  #Anova Test
  anova(model_test, test="Chisq")
  anova(Naive_model, test="Chisq")
  
  #Create Prediction Model
  testing_glm$accuracy <- predict(dt_model,type="class",newdata=testing_glm)
  Estimate_dt <- prediction(dt_model,testing_glm$Resp,testing_glm$accuracy)
  CMat_dt <- confusionMatrix(as.integer(testing_glm$accuracy > 0.5),testing_glm$Resp)
  prf_glm <- performance(Estimate_glm, measure = "tpr", x.measure = "fpr")
  
  print(CMat_dt$table)
  
  prf_glm <- performance(Estimate_glm, measure = "tpr", x.measure = "fpr")
  
  #Create ROC Plot
  summary(prf_glm)
  plot(prf_glm)
  
  #Plotting slope vs. VL.t0
  ggplot( testing_glm, aes(x=VL.t0, y=accuracy)) +
    geom_point() +
    geom_smooth( aes(y = accuracy),  method="glm") 
  

  


