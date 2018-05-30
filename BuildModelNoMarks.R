BuildModelNoMarks <- function(all_data_f)
{
  #Model removing marks and keep other dimensions
  #training data and testing same as above , but removing the average marks and
  #total marks column
  
  #training data set
  all_train_f <- all_data_f[,c(1,2,5,6)]
  train_target_f <- all_data_f[,7]
  
  #One hot encoding to_categorical converts class vectors or integers to binary class
  # matrix
  trainlabels_f <-to_categorical(train_target_f)
 
  #Create a sequential model layer_dense for a densely connected units =8
  # means that there are 8 nodes/neurons in the hidden layer
  # activation = relu -> Rectified Linear Units
  model_f <- keras_model_sequential()
  model_f %>% 
    layer_dense(units = 50,activation = 'relu', input_shape = c(4)) %>%
    layer_dense(units = 2, activation = 'softmax')
  
  #summary(model_f)
  
  
  #Compile the model to configure the learning process
  # Since we have 2 categories we use binary_crossentropy -> If we have more than 2 then 
  # use categorical_crossentropy
  model_f %>%
    compile(loss = 'binary_crossentropy',
            optimizer = 'sgd' ,
            metrics = 'accuracy')
  
  #Fit model Multilayer perceptron Neural network for multiclass softmax classification
  # batch_size is the number of samples you can use per gradient, default = 32 
  #change and see what impact it has on the accuracy of the model
  #
  
   model_f %>%
    fit(all_train_f,
        trainlabels_f,
        epochs = 100,
        batch_size = 64,
        validation_split=0.1)

  return(model_f)
  
}


