TestAccuracyMarks <- function(model, test_data)
{
  #test data set
  all_test  <- test_data[, 1:6]
  test_target <- test_data[, 7]
  
  #One hot encoding to_categorical converts class vectors or integers to binary class
  # matrix
  testlabels <- to_categorical(test_target)
  
  #Evaluate the model with the test data
  model1 <- model %>%
    evaluate(all_test, testlabels)
  
  #Prediction and confusion matrix - test data
  prob <- model %>%
    predict_proba(all_test)
  pred <- model %>%
    predict_classes(all_test)
  
  #Confusion matrix
  table1 <- table(Predicted = pred, Actual = test_target)
  
  return(model1)
  
}
