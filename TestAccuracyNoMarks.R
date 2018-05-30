TestAccuracyNoMarks <- function(model,test_data)
{

  #test data set
  all_test_f  <- test_data[,c(1,2,5,6)]
  test_target_f <- test_data[,7]
  
#One hot encoding to_categorical converts class vectors or integers to binary class
  # matrix
  testlabels <- to_categorical(test_target_f)
  
  #Evaluate the model with the test data
  model1 <- model %>%
    evaluate(all_test_f,testlabels)
  
  #Prediction and confusion matrix - test data
  prob <- model %>%
    predict_proba(all_test_f)
  pred <- model %>%
    predict_classes(all_test_f)
  
  #Confusion matrix
  table1 <- table(Predicted = pred, Actual = test_target_f)
  
  return(model1)
  
}

