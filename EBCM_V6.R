library(dplyr)
library(keras)
library(tensorflow)
library(caret)
library(data.table)
library(stringr)
library(splitstackshape)

rm(list=ls(all=TRUE))

builtdata <- function(path, cfile, pfile, afile, ufile){

	completed <- fread(file.path(path, cfile))
	progress <- fread(file.path(path, pfile))
	allcollection <- fread(file.path(path, afile))
	usercollection <- fread(file.path(path, ufile))
	
  # Reshape data ---------
  completed$status <- as.factor(completed$status)
  progress$status <- as.factor(progress$status)
  
  # merge both progress and completed
  all <- rbind(completed,progress)
  
  #Working on allcollection column to get individual competency code
  a<-cSplit(allcollection,"competency_code",",")
  
  newcollection <-data.frame(character(),character())
  if(ncol(a)>2)
  {
  	for(i in 1:nrow(a))
  	{
  		size <- ncol(a)
  		for(j in 2:size)
  		{
  			if(!is.na(a[i,j, with=FALSE]))
  			{
  				newrow <- data.frame(a[i,1 , with=FALSE], a[i,j , with=FALSE])
  				colnames(newrow) <- c("user_id", "competency_code")
  				newcollection <-rbind(newcollection,newrow)
  			}
  		}
  	}
  }
  if(ncol(a)==2)
  {
  	newcollection<-data.frame(allcollection)
  }
  
  newcollection$competency_code <- as.character(newcollection$competency_code)
  
  #To join the user_col and all_col dataframes first remove the brackets and double
  #quotes in all_data
  
  
  for(i in 1:nrow(newcollection))
  {
  	newcollection$competency_code[i] <-str_replace_all(newcollection$competency_code[i],'[""]','')
  }
  
  for(i in 1:nrow(newcollection))
  {
  	newcollection$competency_code[i] <-str_replace_all(newcollection$competency_code[i],'[\\[]','')
  }
  
  for(i in 1:nrow(newcollection))
  {
  	newcollection$competency_code[i] <-str_replace_all(newcollection$competency_code[i],'[\\]]','')
  }
  
  # Count the number of collections for each user, for each competency
  # Find the total number of collections for each competency
  
  user_col <- usercollection %>%
  group_by(user_id,competency_code) %>%
  summarise(count=n())
  
  all_col<- newcollection %>%
  group_by(competency_code) %>%
  summarise(count=n())
  
  #changing the column names of all_col and user_col
  
  colnames(all_col) <- c("competency_code", "total_count")
  colnames(user_col) <- c("user_id","competency_code", "ind_count")
  
  #Convert all_data into dataframe type
  all_col<-data.frame(all_col)
  
  
  #Merge user_col and all_col
  val <- inner_join(user_col,all_col)
  val<- data.frame(val)
  
  #Creating a new column that stores the percentage of resources consumed
  #by a learner per competency
  val$resource_count<-val$competency_code
  
  for(i in 1:nrow(val))
  {
  	val$resource_count[i]<-val$ind_count[i]/val$total_count[i]
  }
  
  #Keeping only resource_count in val
  val1 <- val[,c(1,2,5)]
  
  #Merge all user information
  all_data<-inner_join(all,val1)
  
  #Keep necessary columns
  all_data_final <-  all_data[,c(4,5,7,8,9,10,3)]
  
  #change data to matrix
  all_data_f<- as.matrix(all_data_final)
  
  #Normalise data
  all_data_f[,1]<-normalize(all_data_final[,1])
  all_data_f[,2]<-normalize(all_data_final[,2])
  all_data_f[,3]<-normalize(all_data_final[,3])
  all_data_f[,4]<-normalize(all_data_final[,4])
  all_data_f[,5]<-normalize(all_data_final[,5])
  #all_data_f[,6]<-normalize(all_data_final[,6])
  all_data_f[,7]<-all_data_final[,7]
  all_data_f[,6]<-all_data_final[,6]
  
  #data is present in all_data_f remove the column names
  dimnames(all_data_f)<-NULL
  
  #change the predicting variable into 0,1 etc
  all_data_f[,7]<-as.numeric(all_data_f[,7]) -1
  
  return(all_data_f)
  
}

#**********************************************************************************



BuildAndTestModelMarks <- function(all_data_f,test_data)
{
  #training data set
  all_train <- all_data_f[,1:6]
  #test data set
  all_test  <- test_data[,1:6]
  
  train_target <- all_data_f[,7]
  test_target <- test_data[,7]
  
  #One hot encoding to_categorical converts class vectors or integers to binary class
  # matrix
  trainlabels <-to_categorical(train_target)
  testlabels <- to_categorical(test_target)
  
  #Create a sequential model layer_dense for a densely connected units =8
  # means that there are 8 nodes/neurons in the hidden layer
  # activation = relu -> Rectified Linear Units
  model <- keras_model_sequential()
  model %>% 
  layer_dense(units = 50,activation = 'relu', input_shape = c(6)) %>%
  layer_dense(units = 2, activation = 'softmax')
  
  #summary(model)
  
  #Compile the model to configure the learning process
  # Since we have 2 categories we use binary_crossentropy -> If we have more than 2 then 
  # use categorical_crossentropy
  model %>%
  compile(loss = 'binary_crossentropy',
  	optimizer = 'adam' ,
  	metrics = 'accuracy')
  
  #Fit model Multilayer perceptron Neural network for multiclass softmax classification
  # batch_size is the number of samples you can use per gradient, default = 32 
  #change and see what impact it has on the accuracy of the model
  #
  
  history <- model %>%
  fit(all_train,
  	trainlabels,
  	epochs = 100,
  	batch_size = 32,
  	validation_split=0.1)
  
  #plot(history)
  
  #Evaluate the model with the test data
  model1 <- model %>%
  evaluate(all_test,testlabels)
  
  #Prediction and confusion matrix - test data
  prob <- model %>%
  predict_proba(all_test)
  pred <- model %>%
  predict_classes(all_test)
  
  #Confusion matrix
  table1 <- table(Predicted = pred, Actual = test_target)
  
  return(model1)
  
}

BuildAndTestModelNoMarks <- function(all_data_f,test_data)
{
  #Model removing marks and keep other dimensions
  #training data and testing same as above , but removing the average marks and
  #total marks column
  
  #training data set
  all_train_f <- all_data_f[,c(1,2,5,6)]
  #test data set
  all_test_f  <- test_data[,c(1,2,5,6)]
  
  train_target_f <- all_data_f[,7]
  test_target_f <- test_data[,7]
  
  #One hot encoding to_categorical converts class vectors or integers to binary class
  # matrix
  trainlabels_f <-to_categorical(train_target_f)
  testlabels_f <- to_categorical(test_target_f)
  
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
  	optimizer = 'adam' ,
  	metrics = 'accuracy')
  
  #Fit model Multilayer perceptron Neural network for multiclass softmax classification
  # batch_size is the number of samples you can use per gradient, default = 32 
  #change and see what impact it has on the accuracy of the model
  #
  
  history_f <- model_f %>%
  fit(all_train_f,
  	trainlabels_f,
  	epochs = 100,
  	batch_size = 32,
  	validation_split=0.1)
  
  #plot(history_f)
  
  #Evaluate the model with the test data
  model1_f <- model_f %>%
  evaluate(all_test_f,testlabels_f)
  
  #Prediction and confusion matrix - test data
  prob_f <- model_f %>%
  predict_proba(all_test_f)
  pred_f <- model_f %>%
  predict_classes(all_test_f)
  
  #Confusion matrix
  table1_f <- table(Predicted = pred_f, Actual = test_target_f)
  
  return(model1_f)
  
}

#************************************************************************************

OneBuildAndTestModelMarks <- function(all_data_f,ind)
{
  #training data set
  all_train <- all_data_f[ind==1,1:6]
  #test data set
  all_test  <- all_data_f[ind==2,1:6]
  
  train_target <- all_data_f[ind==1,7]
  test_target <- all_data_f[ind==2,7]
  
  #One hot encoding to_categorical converts class vectors or integers to binary class
  # matrix
  trainlabels <-to_categorical(train_target)
  testlabels <- to_categorical(test_target)
  
  #Create a sequential model layer_dense for a densely connected units =8
  # means that there are 8 nodes/neurons in the hidden layer
  # activation = relu -> Rectified Linear Units
  model <- keras_model_sequential()
  model %>% 
  layer_dense(units = 50,activation = 'relu', input_shape = c(6)) %>%
  layer_dense(units = 4, activation = 'softmax')
  
  #summary(model)
  
  #Compile the model to configure the learning process
  # Since we have 2 categories we use binary_crossentropy -> If we have more than 2 then 
  # use categorical_crossentropy
  model %>%
  compile(loss = 'binary_crossentropy',
  	optimizer = 'adam' ,
  	metrics = 'accuracy')
  
  #Fit model Multilayer perceptron Neural network for multiclass softmax classification
  # batch_size is the number of samples you can use per gradient, default = 32 
  #change and see what impact it has on the accuracy of the model
  #
  
  history <- model %>%
  fit(all_train,
  	trainlabels,
  	epochs = 100,
  	batch_size = 32,
  	validation_split=0.1)
  
  #plot(history)
  
  #Evaluate the model with the test data
  model1 <- model %>%
  evaluate(all_test,testlabels)
  
  #Prediction and confusion matrix - test data
  prob <- model %>%
  predict_proba(all_test)
  pred <- model %>%
  predict_classes(all_test)
  
  #Confusion matrix
  table1 <- table(Predicted = pred, Actual = test_target)
  
  return(model1)
  
}

OneBuildAndTestModelNoMarks <- function(all_data_f,ind)
{
  #Model removing marks and keep other dimensions
  #training data and testing same as above , but removing the average marks and
  #total marks column
  
  #training data set
  all_train_f <- all_data_f[ind==1,c(1,2,5,6)]
  #test data set
  all_test_f  <- all_data_f[ind==2,c(1,2,5,6)]
  
  train_target_f <- all_data_f[ind==1,7]
  test_target_f <- all_data_f[ind==2,7]
  
  #One hot encoding to_categorical converts class vectors or integers to binary class
  # matrix
  trainlabels_f <-to_categorical(train_target_f)
  testlabels_f <- to_categorical(test_target_f)
  
  #Create a sequential model layer_dense for a densely connected units =8
  # means that there are 8 nodes/neurons in the hidden layer
  # activation = relu -> Rectified Linear Units
  model_f <- keras_model_sequential()
  model_f %>% 
  layer_dense(units = 50,activation = 'relu', input_shape = c(4)) %>%
  layer_dense(units = 4, activation = 'softmax')
  
  #summary(model_f)
  
  
  #Compile the model to configure the learning process
  # Since we have 2 categories we use binary_crossentropy -> If we have more than 2 then 
  # use categorical_crossentropy
  model_f %>%
  compile(loss = 'binary_crossentropy',
  	optimizer = 'adam' ,
  	metrics = 'accuracy')
  
  #Fit model Multilayer perceptron Neural network for multiclass softmax classification
  # batch_size is the number of samples you can use per gradient, default = 32 
  #change and see what impact it has on the accuracy of the model
  #
  
  history_f <- model_f %>%
  fit(all_train_f,
  	trainlabels_f,
  	epochs = 100,
  	batch_size = 32,
  	validation_split=0.1)
  
  #plot(history_f)
  
  #Evaluate the model with the test data
  model1_f <- model_f %>%
  evaluate(all_test_f,testlabels_f)
  
  #Prediction and confusion matrix - test data
  prob_f <- model_f %>%
  predict_proba(all_test_f)
  pred_f <- model_f %>%
  predict_classes(all_test_f)
  
  #Confusion matrix
  table1_f <- table(Predicted = pred_f, Actual = test_target_f)
  
  return(model1_f)
  
}


#Seed function to make data repeatable and shuffle the data

ShuffleData <- function(all_data_f)
{
	set.seed(12345)
	all_data_f <- all_data_f[sample(nrow(all_data_f)),]
	return(all_data_f)
}

#Seed function to make data repeatable and shuffle the data

SampleData <- function(all_data_f)
{
	set.seed(12345)
	ind <- sample(2,nrow(all_data_f),replace=T, prob = c(0.8,0.2))
	return(ind)
}


# Initialise Varible ---------------------------------------------------------------

path <- "/home/thrvkrm/Desktop/EBCM/EBCM/Data/"

cfileCED <- "completedCED.csv"
pfileCED <- "progressCED.csv"
afileCED <- "allcollectionCED.csv"
ufileCED <- "usercollectionCED.csv"

cfileEEA <- "completedEEA.csv"
pfileEEA <- "progressEEA.csv"
afileEEA <- "allcollectionEEA.csv"
ufileEEA <- "usercollectionEEA.csv"

cfileEEC <- "completedEEC.csv"
pfileEEC <- "progressEEC.csv"
afileEEC <- "allcollectionEEC.csv"
ufileEEC <- "usercollectionEEC.csv"

cfileMD <- "completedMD.csv"
pfileMD <- "progressMD.csv"
afileMD <- "allcollectionMD.csv"
ufileMD <- "usercollectionMD.csv"

cfileFLE <- "completedFLE.csv"
pfileFLE <- "progressFLE.csv"
afileFLE <- "allcollectionFLE.csv"
ufileFLE <- "usercollectionFLE.csv"

cfileNQ <- "completedNQ.csv"
pfileNQ <- "progressNQ.csv"
afileNQ <- "allcollectionNQ.csv"
ufileNQ <- "usercollectionNQ.csv"

cfileOAT <- "completedOAT.csv"
pfileOAT <- "progressOAT.csv"
afileOAT <- "allcollectionOAT.csv"
ufileOAT <- "usercollectionOAT.csv"

cfileREI <- "completedREI.csv"
pfileREI <- "progressREI.csv"
afileREI <- "allcollectionREI.csv"
ufileREI <- "usercollectionREI.csv"

cfileRL <- "completedRL.csv"
pfileRL <- "progressRL.csv"
afileRL <- "allcollectionRL.csv"
ufileRL <- "usercollectionRL.csv"

all_data_fCED <- builtdata(path, cfileCED, pfileCED, afileCED, ufileCED)
all_data_fEEA <- builtdata(path, cfileEEA, pfileEEA, afileEEA, ufileEEA)
all_data_fEEC <- builtdata(path, cfileEEC, pfileEEC, afileEEC, ufileEEC)
all_data_fMD <- builtdata(path, cfileMD, pfileMD, afileMD, ufileMD)
all_data_fFLE <- builtdata(path, cfileFLE, pfileFLE, afileFLE, ufileFLE)
all_data_fNQ <- builtdata(path, cfileNQ, pfileNQ, afileNQ, ufileNQ)
all_data_fOAT <- builtdata(path, cfileOAT, pfileOAT, afileOAT, ufileOAT)
all_data_fREI <- builtdata(path, cfileREI, pfileREI, afileREI, ufileREI)
all_data_fRL <- builtdata(path, cfileRL, pfileRL, afileRL, ufileRL)
all_data_fCED = ShuffleData(all_data_f = all_data_fCED)

ind_CED = SampleData(all_data_fCED)
table_CED=OneBuildAndTestModelMarks(all_data_fCED,ind_CED)
table1_CED=OneBuildAndTestModelNoMarks(all_data_fCED,ind_CED)


all_data_fEEA = ShuffleData(all_data_f = all_data_fEEA)
ind_EEA = SampleData(all_data_fEEA)
table_EEA=OneBuildAndTestModelMarks(all_data_fEEA,ind_EEA)
table1_EEA=OneBuildAndTestModelNoMarks(all_data_fEEA,ind_EEA)

all_data_fEEC = ShuffleData(all_data_f = all_data_fEEC)
ind_EEC = SampleData(all_data_fEEC)
table_EEC=OneBuildAndTestModelMarks(all_data_fEEC,ind_EEC)
table1_EEC=OneBuildAndTestModelNoMarks(all_data_fEEC,ind_EEC)

all_data_fMD = ShuffleData(all_data_f = all_data_fMD)
ind_MD = SampleData(all_data_fMD)
table_MD=OneBuildAndTestModelMarks(all_data_fMD,ind_MD)
table1_MD=OneBuildAndTestModelNoMarks(all_data_fMD,ind_MD)

all_data_fFLE = ShuffleData(all_data_f = all_data_fFLE)
ind_FLE = SampleData(all_data_fFLE)
table_FLE=OneBuildAndTestModelMarks(all_data_fFLE,ind_FLE)
table1_FLE=OneBuildAndTestModelNoMarks(all_data_fFLE,ind_FLE)

all_data_fNQ = ShuffleData(all_data_f = all_data_fNQ)
ind_NQ = SampleData(all_data_fNQ)
table_NQ=OneBuildAndTestModelMarks(all_data_fNQ,ind_NQ)
table1_NQ=OneBuildAndTestModelNoMarks(all_data_fNQ,ind_NQ)

all_data_fOAT = ShuffleData(all_data_f = all_data_fOAT)
ind_OAT = SampleData(all_data_fOAT)
table_OAT=OneBuildAndTestModelMarks(all_data_fOAT,ind_OAT)
table1_OAT=OneBuildAndTestModelNoMarks(all_data_fOAT,ind_OAT)

all_data_fREI = ShuffleData(all_data_f = all_data_fREI)
ind_REI = SampleData(all_data_fREI)
table_REI=OneBuildAndTestModelMarks(all_data_fREI,ind_REI)
table1_REI=OneBuildAndTestModelNoMarks(all_data_fREI,ind_REI)

all_data_fRL = ShuffleData(all_data_f = all_data_fRL)
ind_RL = SampleData(all_data_fRL)
table_RL=OneBuildAndTestModelMarks(all_data_fRL,ind_RL)
table1_RL=OneBuildAndTestModelNoMarks(all_data_fRL,ind_RL)


#Build Model and Test Model with other data set for CED

CEDtable1 <- BuildAndTestModelMarks(all_data_f = all_data_fCED , test_data = all_data_fEEA)
CEDtable1_f <- BuildAndTestModelNoMarks(all_data_f = all_data_fCED , test_data = all_data_fEEA)

CEDtable2 <- BuildAndTestModelMarks(all_data_f = all_data_fCED , test_data = all_data_fEEC)
CEDtable2_f <- BuildAndTestModelNoMarks(all_data_f = all_data_fCED , test_data = all_data_fEEC)

CEDtable3 <- BuildAndTestModelMarks(all_data_f = all_data_fCED , test_data = all_data_fMD)
CEDtable3_f <- BuildAndTestModelNoMarks(all_data_f = all_data_fCED , test_data = all_data_fMD)

CEDtable4 <- BuildAndTestModelMarks(all_data_f = all_data_fCED , test_data = all_data_fFLE)
CEDtable4_f <- BuildAndTestModelNoMarks(all_data_f = all_data_fCED , test_data = all_data_fFLE)

CEDtable6 <- BuildAndTestModelMarks(all_data_f = all_data_fCED , test_data = all_data_fNQ)
CEDtable6_f <- BuildAndTestModelNoMarks(all_data_f = all_data_fCED , test_data = all_data_fNQ)

CEDtable7 <- BuildAndTestModelMarks(all_data_f = all_data_fCED , test_data = all_data_fOAT)
CEDtable7_f <- BuildAndTestModelNoMarks(all_data_f = all_data_fCED , test_data = all_data_fOAT)

CEDtable8 <- BuildAndTestModelMarks(all_data_f = all_data_fCED , test_data = all_data_fREI)
CEDtable8_f <- BuildAndTestModelNoMarks(all_data_f = all_data_fCED , test_data = all_data_fREI)

CEDtable9 <- BuildAndTestModelMarks(all_data_f = all_data_fCED , test_data = all_data_fRL)
CEDtable9_f <- BuildAndTestModelNoMarks(all_data_f = all_data_fCED , test_data = all_data_fRL)

#Build Model and Test Model with other data set for EEA

all_data_fEEA = ShuffleData(all_data_f = all_data_fEEA)

EEAtable1 <- BuildAndTestModelMarks(all_data_f = all_data_fEEA , test_data = all_data_fCED)
EEAtable1_f <- BuildAndTestModelNoMarks(all_data_f = all_data_fEEA , test_data = all_data_fCED)

EEAtable2 <- BuildAndTestModelMarks(all_data_f = all_data_fEEA , test_data = all_data_fEEC)
EEAtable2_f <- BuildAndTestModelNoMarks(all_data_f = all_data_fEEA , test_data = all_data_fEEC)

EEAtable3 <- BuildAndTestModelMarks(all_data_f = all_data_fEEA , test_data = all_data_fMD)
EEAtable3_f <- BuildAndTestModelNoMarks(all_data_f = all_data_fEEA , test_data = all_data_fMD)

EEAtable4 <- BuildAndTestModelMarks(all_data_f = all_data_fEEA , test_data = all_data_fFLE)
EEAtable4_f <- BuildAndTestModelNoMarks(all_data_f = all_data_fEEA , test_data = all_data_fFLE)

EEAtable6 <- BuildAndTestModelMarks(all_data_f = all_data_fEEA , test_data = all_data_fNQ)
EEAtable6_f <- BuildAndTestModelNoMarks(all_data_f = all_data_fEEA , test_data = all_data_fNQ)

EEAtable7 <- BuildAndTestModelMarks(all_data_f = all_data_fEEA , test_data = all_data_fOAT)
EEAtable7_f <- BuildAndTestModelNoMarks(all_data_f = all_data_fEEA , test_data = all_data_fOAT)

EEAtable8 <- BuildAndTestModelMarks(all_data_f = all_data_fEEA , test_data = all_data_fREI)
EEAtable8_f <- BuildAndTestModelNoMarks(all_data_f = all_data_fEEA , test_data = all_data_fREI)

EEAtable9 <- BuildAndTestModelMarks(all_data_f = all_data_fEEA , test_data = all_data_fRL)
EEAtable9_f <- BuildAndTestModelNoMarks(all_data_f = all_data_fEEA , test_data = all_data_fRL)


#Build Model and Test Model with other data set for EEC

all_data_fEEC = ShuffleData(all_data_f = all_data_fEEC)
EECtable1 <- BuildAndTestModelMarks(all_data_f = all_data_fEEC , test_data = all_data_fCED)
EECtable1_f <- BuildAndTestModelNoMarks(all_data_f = all_data_fEEC , test_data = all_data_fCED)

EECtable2 <- BuildAndTestModelMarks(all_data_f = all_data_fEEC , test_data = all_data_fEEA)
EECtable2_f <- BuildAndTestModelNoMarks(all_data_f = all_data_fEEC , test_data = all_data_fEEA)

EECtable3 <- BuildAndTestModelMarks(all_data_f = all_data_fEEC , test_data = all_data_fMD)
EECtable3_f <- BuildAndTestModelNoMarks(all_data_f = all_data_fEEC , test_data = all_data_fMD)

EECtable4 <- BuildAndTestModelMarks(all_data_f = all_data_fEEC , test_data = all_data_fFLE)
EECtable4_f <- BuildAndTestModelNoMarks(all_data_f = all_data_fEEC , test_data = all_data_fFLE)

EECtable6 <- BuildAndTestModelMarks(all_data_f = all_data_fEEC , test_data = all_data_fNQ)
EECtable6_f <- BuildAndTestModelNoMarks(all_data_f = all_data_fEEC , test_data = all_data_fNQ)

EECtable7 <- BuildAndTestModelMarks(all_data_f = all_data_fEEC , test_data = all_data_fOAT)
EECtable7_f <- BuildAndTestModelNoMarks(all_data_f = all_data_fEEC , test_data = all_data_fOAT)

EECtable8 <- BuildAndTestModelMarks(all_data_f = all_data_fEEC , test_data = all_data_fREI)
EECtable8_f <- BuildAndTestModelNoMarks(all_data_f = all_data_fEEC , test_data = all_data_fREI)

EECtable9 <- BuildAndTestModelMarks(all_data_f = all_data_fEEC , test_data = all_data_fRL)
EECtable9_f <- BuildAndTestModelNoMarks(all_data_f = all_data_fEEC , test_data = all_data_fRL)


#Build Model and Test Model with other data set for MD

all_data_fMD = ShuffleData(all_data_f = all_data_fMD)
MDtable1 <- BuildAndTestModelMarks(all_data_f = all_data_fMD , test_data = all_data_fCED)
MDtable1_f <- BuildAndTestModelNoMarks(all_data_f = all_data_fMD , test_data = all_data_fCED)

MDtable2 <- BuildAndTestModelMarks(all_data_f = all_data_fMD , test_data = all_data_fEEA)
MDtable2_f <- BuildAndTestModelNoMarks(all_data_f = all_data_fMD , test_data = all_data_fEEA)

MDtable3 <- BuildAndTestModelMarks(all_data_f = all_data_fMD , test_data = all_data_fEEC)
MDtable3_f <- BuildAndTestModelNoMarks(all_data_f = all_data_fMD , test_data = all_data_fEEC)

MDtable4 <- BuildAndTestModelMarks(all_data_f = all_data_fMD , test_data = all_data_fFLE)
MDtable4_f <- BuildAndTestModelNoMarks(all_data_f = all_data_fMD , test_data = all_data_fFLE)

MDtable6 <- BuildAndTestModelMarks(all_data_f = all_data_fMD , test_data = all_data_fNQ)
MDtable6_f <- BuildAndTestModelNoMarks(all_data_f = all_data_fMD , test_data = all_data_fNQ)

MDtable7 <- BuildAndTestModelMarks(all_data_f = all_data_fMD , test_data = all_data_fOAT)
MDtable7_f <- BuildAndTestModelNoMarks(all_data_f = all_data_fMD , test_data = all_data_fOAT)

MDtable8 <- BuildAndTestModelMarks(all_data_f = all_data_fMD , test_data = all_data_fREI)
MDtable8_f <- BuildAndTestModelNoMarks(all_data_f = all_data_fMD , test_data = all_data_fREI)

MDtable9 <- BuildAndTestModelMarks(all_data_f = all_data_fMD , test_data = all_data_fRL)
MDtable9_f <- BuildAndTestModelNoMarks(all_data_f = all_data_fMD , test_data = all_data_fRL)

#Build Model and Test Model with other data set for FLE

all_data_fFLE = ShuffleData(all_data_f = all_data_fFLE)
FLEtable1 <- BuildAndTestModelMarks(all_data_f = all_data_fFLE , test_data = all_data_fCED)
FLEtable1_f <- BuildAndTestModelNoMarks(all_data_f = all_data_fFLE , test_data = all_data_fCED)

FLEtable2 <- BuildAndTestModelMarks(all_data_f = all_data_fFLE , test_data = all_data_fEEA)
FLEtable2_f <- BuildAndTestModelNoMarks(all_data_f = all_data_fFLE , test_data = all_data_fEEA)

FLEtable3 <- BuildAndTestModelMarks(all_data_f = all_data_fFLE , test_data = all_data_fEEC)
FLEtable3_f <- BuildAndTestModelNoMarks(all_data_f = all_data_fFLE , test_data = all_data_fEEC)

FLEtable4 <- BuildAndTestModelMarks(all_data_f = all_data_fFLE , test_data = all_data_fMD)
FLEtable4_f <- BuildAndTestModelNoMarks(all_data_f = all_data_fFLE , test_data = all_data_fMD)

FLEtable6 <- BuildAndTestModelMarks(all_data_f = all_data_fFLE , test_data = all_data_fNQ)
FLEtable6_f <- BuildAndTestModelNoMarks(all_data_f = all_data_fFLE , test_data = all_data_fNQ)

FLEtable7 <- BuildAndTestModelMarks(all_data_f = all_data_fFLE , test_data = all_data_fOAT)
FLEtable7_f <- BuildAndTestModelNoMarks(all_data_f = all_data_fFLE , test_data = all_data_fOAT)

FLEtable8 <- BuildAndTestModelMarks(all_data_f = all_data_fFLE , test_data = all_data_fREI)
FLEtable8_f <- BuildAndTestModelNoMarks(all_data_f = all_data_fFLE , test_data = all_data_fREI)

FLEtable9 <- BuildAndTestModelMarks(all_data_f = all_data_fFLE , test_data = all_data_fRL)
FLEtable9_f <- BuildAndTestModelNoMarks(all_data_f = all_data_fFLE , test_data = all_data_fRL)


#Build Model and Test Model with other data set for NQ

all_data_fNQ = ShuffleData(all_data_f = all_data_fNQ)
NQtable1 <- BuildAndTestModelMarks(all_data_f = all_data_fNQ , test_data = all_data_fCED)
NQtable1_f <- BuildAndTestModelNoMarks(all_data_f = all_data_fNQ , test_data = all_data_fCED)

NQtable2 <- BuildAndTestModelMarks(all_data_f = all_data_fNQ , test_data = all_data_fEEA)
NQtable2_f <- BuildAndTestModelNoMarks(all_data_f = all_data_fNQ , test_data = all_data_fEEA)

NQtable3 <- BuildAndTestModelMarks(all_data_f = all_data_fNQ , test_data = all_data_fEEC)
NQtable3_f <- BuildAndTestModelNoMarks(all_data_f = all_data_fNQ , test_data = all_data_fEEC)

NQtable4 <- BuildAndTestModelMarks(all_data_f = all_data_fNQ , test_data = all_data_fMD)
NQtable4_f <- BuildAndTestModelNoMarks(all_data_f = all_data_fNQ , test_data = all_data_fMD)

NQtable5 <- BuildAndTestModelMarks(all_data_f = all_data_fNQ , test_data = all_data_fFLE)
NQtable5_f <- BuildAndTestModelNoMarks(all_data_f = all_data_fNQ , test_data = all_data_fFLE)

NQtable7 <- BuildAndTestModelMarks(all_data_f = all_data_fNQ , test_data = all_data_fOAT)
NQtable7_f <- BuildAndTestModelNoMarks(all_data_f = all_data_fNQ , test_data = all_data_fOAT)

NQtable8 <- BuildAndTestModelMarks(all_data_f = all_data_fNQ , test_data = all_data_fREI)
NQtable8_f <- BuildAndTestModelNoMarks(all_data_f = all_data_fNQ , test_data = all_data_fREI)

NQtable9 <- BuildAndTestModelMarks(all_data_f = all_data_fNQ , test_data = all_data_fRL)
NQtable9_f <- BuildAndTestModelNoMarks(all_data_f = all_data_fNQ , test_data = all_data_fRL)

#Build Model and Test Model with other data set for OAT

all_data_fOAT = ShuffleData(all_data_f = all_data_fOAT)
OATtable1 <- BuildAndTestModelMarks(all_data_f = all_data_fOAT , test_data = all_data_fCED)
OATtable1_f <- BuildAndTestModelNoMarks(all_data_f = all_data_fOAT , test_data = all_data_fCED)

OATtable2 <- BuildAndTestModelMarks(all_data_f = all_data_fOAT , test_data = all_data_fEEA)
OATtable2_f <- BuildAndTestModelNoMarks(all_data_f = all_data_fOAT , test_data = all_data_fEEA)

OATtable3 <- BuildAndTestModelMarks(all_data_f = all_data_fOAT , test_data = all_data_fEEC)
OATtable3_f <- BuildAndTestModelNoMarks(all_data_f = all_data_fOAT , test_data = all_data_fEEC)

OATtable4 <- BuildAndTestModelMarks(all_data_f = all_data_fOAT , test_data = all_data_fMD)
OATtable4_f <- BuildAndTestModelNoMarks(all_data_f = all_data_fOAT , test_data = all_data_fMD)

OATtable5 <- BuildAndTestModelMarks(all_data_f = all_data_fOAT , test_data = all_data_fFLE)
OATtable5_f <- BuildAndTestModelNoMarks(all_data_f = all_data_fOAT , test_data = all_data_fFLE)

OATtable7 <- BuildAndTestModelMarks(all_data_f = all_data_fOAT , test_data = all_data_fNQ)
OATtable7_f <- BuildAndTestModelNoMarks(all_data_f = all_data_fOAT , test_data = all_data_fNQ)

OATtable8 <- BuildAndTestModelMarks(all_data_f = all_data_fOAT , test_data = all_data_fREI)
OATtable8_f <- BuildAndTestModelNoMarks(all_data_f = all_data_fOAT , test_data = all_data_fREI)

OATtable9 <- BuildAndTestModelMarks(all_data_f = all_data_fOAT , test_data = all_data_fRL)
OATtable9_f <- BuildAndTestModelNoMarks(all_data_f = all_data_fOAT , test_data = all_data_fRL)

#Build Model and Test Model with other data set for REI

all_data_fREI = ShuffleData(all_data_f = all_data_fREI)
REItable1 <- BuildAndTestModelMarks(all_data_f = all_data_fREI , test_data = all_data_fCED)
REItable1_f <- BuildAndTestModelNoMarks(all_data_f = all_data_fREI , test_data = all_data_fCED)

REItable2 <- BuildAndTestModelMarks(all_data_f = all_data_fREI , test_data = all_data_fEEA)
REItable2_f <- BuildAndTestModelNoMarks(all_data_f = all_data_fREI , test_data = all_data_fEEA)

REItable3 <- BuildAndTestModelMarks(all_data_f = all_data_fREI , test_data = all_data_fEEC)
REItable3_f <- BuildAndTestModelNoMarks(all_data_f = all_data_fREI , test_data = all_data_fEEC)

REItable4 <- BuildAndTestModelMarks(all_data_f = all_data_fREI , test_data = all_data_fMD)
REItable4_f <- BuildAndTestModelNoMarks(all_data_f = all_data_fREI , test_data = all_data_fMD)

REItable5 <- BuildAndTestModelMarks(all_data_f = all_data_fREI , test_data = all_data_fFLE)
REItable5_f <- BuildAndTestModelNoMarks(all_data_f = all_data_fREI , test_data = all_data_fFLE)

REItable7 <- BuildAndTestModelMarks(all_data_f = all_data_fREI , test_data = all_data_fNQ)
REItable7_f <- BuildAndTestModelNoMarks(all_data_f = all_data_fREI , test_data = all_data_fNQ)

REItable8 <- BuildAndTestModelMarks(all_data_f = all_data_fREI , test_data = all_data_fOAT)
REItable8_f <- BuildAndTestModelNoMarks(all_data_f = all_data_fREI , test_data = all_data_fOAT)

REItable9 <- BuildAndTestModelMarks(all_data_f = all_data_fREI , test_data = all_data_fRL)
REItable9_f <- BuildAndTestModelNoMarks(all_data_f = all_data_fREI , test_data = all_data_fRL)

#Build Model and Test Model with other data set for RL

all_data_fRL = ShuffleData(all_data_f = all_data_fRL)
RLtable1 <- BuildAndTestModelMarks(all_data_f = all_data_fRL , test_data = all_data_fCED)
RLtable1_f <- BuildAndTestModelNoMarks(all_data_f = all_data_fRL , test_data = all_data_fCED)

RLtable2 <- BuildAndTestModelMarks(all_data_f = all_data_fRL , test_data = all_data_fEEA)
RLtable2_f <- BuildAndTestModelNoMarks(all_data_f = all_data_fRL , test_data = all_data_fEEA)

RLtable3 <- BuildAndTestModelMarks(all_data_f = all_data_fRL , test_data = all_data_fEEC)
RLtable3_f <- BuildAndTestModelNoMarks(all_data_f = all_data_fRL , test_data = all_data_fEEC)

RLtable4 <- BuildAndTestModelMarks(all_data_f = all_data_fRL , test_data = all_data_fMD)
RLtable4_f <- BuildAndTestModelNoMarks(all_data_f = all_data_fRL , test_data = all_data_fMD)

RLtable5 <- BuildAndTestModelMarks(all_data_f = all_data_fRL , test_data = all_data_fFLE)
RLtable5_f <- BuildAndTestModelNoMarks(all_data_f = all_data_fRL , test_data = all_data_fFLE)

RLtable7 <- BuildAndTestModelMarks(all_data_f = all_data_fRL , test_data = all_data_fNQ)
RLtable7_f <- BuildAndTestModelNoMarks(all_data_f = all_data_fRL , test_data = all_data_fNQ)

RLtable8 <- BuildAndTestModelMarks(all_data_f = all_data_fRL , test_data = all_data_fOAT)
RLtable8_f <- BuildAndTestModelNoMarks(all_data_f = all_data_fRL , test_data = all_data_fOAT)

RLtable9 <- BuildAndTestModelMarks(all_data_f = all_data_fRL , test_data = all_data_fREI)
RLtable9_f <- BuildAndTestModelNoMarks(all_data_f = all_data_fRL , test_data = all_data_fREI)


