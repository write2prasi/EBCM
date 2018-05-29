library(corrplot)
library(keras)
library(dplyr)


buildData <- function(data)
{
  #data <- REI
  data <- select(data, status, everything())
  data <- select(data,-(user_id:competency_code))
  # plot(data$average_score,
  #      data$total_time_spent,
  #      pch=21, bg=c("red","blue","green", "orange", "black", "pink", "white", "maroon")[unclass(data$competency_code)],
  #      xlab="Score",
  #      ylab="Time")
  #
  # cor(data$average_score, data$total_time_spent)
  # M <- cor(data[,1:4])
  # corrplot(M, method="circle")
  
  data[, 1] <- as.numeric(data[, 1]) - 1
  data <- as.matrix(data)
  dimnames(data) <- NULL
  
  data_original <- data
  data <- normalize(data[, 1:7])
  
  ind <- sample(2,
                nrow(data),
                replace = TRUE,
                prob = c(0.9, 0.1))
  data.training <- data[ind == 1, 2:7]
  data.test <- data[ind == 2, 2:7]
  data.trainingtarget <- data_original[ind == 1, 1]
  data.testtarget <- data_original[ind == 2, 1]
  
  data.trainLabels <- to_categorical(data.trainingtarget)
  data.testLabels <- to_categorical(data.testtarget)
  
  modelANN(data.test, data.testtarget, data.testLabels)
}



modelANN <- function(data.test,
                     data.testtarget,
                     data.testLabels)
{
  model <- keras_model_sequential()
  model %>%
    layer_dense(units = 50,
                activation = 'relu',
                input_shape = c(6)) %>%
    #layer_dense(units = 20, activation = 'relu') %>%
    layer_dense(units = 2, activation = 'softmax')
  
  # model %>%
  #   layer_dense(units = 20, activation = 'relu', input_shape = c(8)) %>%
  #   layer_dense(units = 10, activation = 'relu') %>%
  #   layer_dense(units = 2, activation = 'softmax')
  
  
  # summary(model)
  # get_config(model)
  # get_layer(model, index = 1)
  #
  # model$layers
  # model$inputs
  # model$outputs
  adam <- optimizer_adam(lr = 0.001)
  model %>% compile(loss = 'binary_crossentropy',
                    optimizer = 'adam',
                    metrics = 'accuracy')
  
  history <- model %>% fit(
    data.training,
    data.trainLabels,
    epochs = 136,
    batch_size = 16,
    validation_split = 0.1,
    verbose = 1
  )
  plotG(history)
  
  
  classes <-
    model %>% predict_classes(data.test, batch_size = 16, verbose = 1)
  table(data.testtarget, classes)
  score <- model %>% evaluate(data.test,
                              data.testLabels,
                              batch_size = 16,
                              verbose = 1)
  print(score)
  
}


plotG <- function(history)
{
  plot(history)
  plot(
    history$metrics$loss,
    main = "Model Loss for Training Data",
    xlab = "epoch",
    ylab = "loss",
    col = "blue",
    type = "l"
  )
  lines(history$metrics$val_loss, col = "green")
  legend(
    "topright",
    c("train", "test"),
    col = c("blue", "green"),
    lty = c(1, 1)
  )
  
  plot(
    history$metrics$val_loss,
    main = "Model Loss for Testing Data",
    xlab = "epoch",
    ylab = "val_loss",
    col = "green",
    type = "l"
  )
  lines(history$metrics$loss, col = "blue")
  legend(
    "topright",
    c("train", "test"),
    col = c("blue", "green"),
    lty = c(1, 1)
  )
  
  
  plot(
    history$metrics$acc,
    main = "Model Accuracy with Training Data",
    xlab = "epoch",
    ylab = "accuracy",
    col = "blue",
    type = "l"
  )
  lines(history$metrics$val_acc, col = "green")
  legend(
    "bottomright",
    c("train", "test"),
    col = c("blue", "green"),
    lty = c(1, 1)
  )
  
  plot(
    history$metrics$val_acc,
    main = "Model Accuracy with Testing Data",
    xlab = "epoch",
    ylab = "accuracy",
    col = "green",
    type = "l"
  )
  lines(history$metrics$acc, col = "blue")
  legend(
    "bottomright",
    c("train", "test"),
    col = c("blue", "green"),
    lty = c(1, 1)
  )
  
}


# Initialise Varible ---------------------------------------------------------------

path <- "./Data/"

cfileCED <- read.csv(paste(path, "completedCED.csv", sep = ""))
pfileCED <- read.csv(paste(path, "progressCED.csv", sep = ""))
afileCED <- read.csv(paste(path, "allcollectionCED.csv", sep = ""))
ufileCED <- read.csv(paste(path, "usercollectionCED.csv", sep = ""))

cfileEEA <- read.csv(paste(path, "completedEEA.csv", sep = ""))
pfileEEA <- read.csv(paste(path, "progressEEA.csv", sep = ""))
afileEEA <- read.csv(paste(path, "allcollectionEEA.csv", sep = ""))
ufileEEA <- read.csv(paste(path, "usercollectionEEA.csv", sep = ""))

cfileEEC <- read.csv(paste(path, "completedEEC.csv", sep = ""))
pfileEEC <- read.csv(paste(path, "progressEEC.csv", sep = ""))
afileEEC <- read.csv(paste(path, "allcollectionEEC.csv", sep = ""))
ufileEEC <- read.csv(paste(path, "usercollectionEEC.csv", sep = ""))

cfileMD <- read.csv(paste(path, "completedMD.csv", sep = ""))
pfileMD <- read.csv(paste(path, "progressMD.csv", sep = ""))
afileMD <- read.csv(paste(path, "allcollectionMD.csv", sep = ""))
ufileMD <- read.csv(paste(path, "usercollectionMD.csv", sep = ""))

cfileFLE <- read.csv(paste(path, "completedFLE.csv", sep = ""))
pfileFLE <- read.csv(paste(path, "progressFLE.csv", sep = ""))
afileFLE <- read.csv(paste(path, "allcollectionFLE.csv", sep = ""))
ufileFLE <- read.csv(paste(path, "usercollectionFLE.csv", sep = ""))

cfileNQ <- read.csv(paste(path, "completedNQ.csv", sep = ""))
pfileNQ <- read.csv(paste(path, "progressNQ.csv", sep = ""))
afileNQ <- read.csv(paste(path, "allcollectionNQ.csv", sep = ""))
ufileNQ <- read.csv(paste(path, "usercollectionNQ.csv", sep = ""))

cfileOAT <- read.csv(paste(path, "completedOAT.csv", sep = ""))
pfileOAT <- read.csv(paste(path, "progressOAT.csv", sep = ""))
afileOAT <- read.csv(paste(path, "allcollectionOAT.csv", sep = ""))
ufileOAT <- read.csv(paste(path, "usercollectionOAT.csv", sep = ""))

cfileREI <- read.csv(paste(path, "completedREI.csv", sep = ""))
pfileREI <- read.csv(paste(path, "progressREI.csv", sep = ""))
afileREI <- read.csv(paste(path, "allcollectionREI.csv", sep = ""))
ufileREI <- read.csv(paste(path, "usercollectionREI.csv", sep = ""))

cfileRL <- read.csv(paste(path, "completedRL.csv", sep = ""))
pfileRL <- read.csv(paste(path, "progressRL.csv", sep = ""))
afileRL <- read.csv(paste(path, "allcollectionRL.csv", sep = ""))
ufileRL <- read.csv(paste(path, "usercollectionRL.csv", sep = ""))

CED <- merge(x = cfileCED, y = pfileCED, all = TRUE)
EEA <- merge(x = cfileEEA, y = pfileEEA, all = TRUE)
EEC <- merge(x = cfileEEC, y = pfileEEC, all = TRUE)
MD <- merge(x = cfileMD, y = pfileMD, all = TRUE)
FLE <- merge(x = cfileFLE, y = pfileFLE, all = TRUE)
NQ <- merge(x = cfileNQ, y = pfileNQ, all = TRUE)
OAT <- merge(x = cfileOAT, y = pfileOAT, all = TRUE)
REI <- merge(x = cfileREI, y = pfileREI, all = TRUE)
RL <- merge(x = cfileRL, y = pfileRL, all = TRUE)

data <- CED
buildData(data)
