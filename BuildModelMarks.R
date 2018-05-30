BuildModelMarks <- function(all_data)
{
  #training data set
  all_train <- all_data[, 1:6]
  train_target <- all_data[, 7]
  
  #One hot encoding to_categorical converts class vectors or integers to binary class
  # matrix
  trainlabels <- to_categorical(train_target)
  
  #Create a sequential model layer_dense for a densely connected units =8
  # means that there are 8 nodes/neurons in the hidden layer
  # activation = relu -> Rectified Linear Units
  model <- keras_model_sequential()
  model %>%
    layer_dense(units = 50,
                activation = 'relu',
                input_shape = c(6)) %>%
    layer_dense(units = 2, activation = 'softmax')
  
  #summary(model)
  
  #Compile the model to configure the learning process
  # Since we have 2 categories we use binary_crossentropy -> If we have more than 2 then
  # use categorical_crossentropy
  model %>%
    compile(loss = 'binary_crossentropy',
            optimizer = 'sgd' ,
            metrics = 'accuracy')
  
  #Fit model Multilayer perceptron Neural network for multiclass softmax classification
  # batch_size is the number of samples you can use per gradient, default = 32
  #change and see what impact it has on the accuracy of the model
  #
  
  history <- model %>%
    fit(
      all_train,
      trainlabels,
      epochs = 100,
      batch_size = 64,
      validation_split = 0.1,
      verbose = 0
    )
  PlotG(history)
  return(model)
  
}
