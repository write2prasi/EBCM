library(dplyr)
library(keras)
library(tensorflow)
library(caret)
library(data.table)
library(stringr)
library(splitstackshape)

rm(list=ls(all=TRUE))


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

pathname <- "/home/praseeda/Documents/research/gooru/evidence/code/V6_ModifiedCompleted/data/"

#Fetch all files in the folder mentioned in path
all_files<-list.files(path=pathname)

#Parse the file names and populate normalized data 

j<-1
ListDataSet <- list()
len <- length(all_files)/4

for(i in 1:len)
{
  code <- sub(".*allcollection *(.*?) *.csv.*","\\1",all_files[i])
  print(paste0("Code J= ",j))
  ListDataSet[[j]] <- code
  inputFiles <- grep(code, all_files, value=TRUE)
  if(!is.na(str_match(inputFiles[1],".*allcollection.*")) && 
     !is.na(str_match(inputFiles[2],".*completed.*")) && 
     !is.na(str_match(inputFiles[3],".*progress.*")) && 
     !is.na(str_match(inputFiles[4],".*usercollection.*")) )
  {
    j<-j+1
    print(paste0("Data J= ",j))      
    ListDataSet[[j]] <- BuildData(pathname, inputFiles)
    j<-j+1
  }
  else{
    break
  }
}
  
#Create place holders to store accuracy of models with and without marks

MarksModelAccuracyList <- list()
NoMarksModelAccuracyList <- list()

MarksModelAccuracyList <- PopulateMarksAccuracy(ListDataSet,MarksModelAccuracyList)
NoMarksModelAccuracyList <- PopulateNoMarksAccuracy(ListDataSet,NoMarksModelAccuracyList)

#Find the difference in the accuracy of both models
diff <- list()
for(i in 1:length(MarksModelAccuracyList))
  diff[i] <- MarksModelAccuracyList[[i]]$acc - NoMarksModelAccuracyList[[i]]$acc

mean(as.numeric(diff))

#push the results to a matrix to get the heat maps
num <- length(ListDataSet)/2
Mlist <- list()
Nlist <- list()
Dlist <- list()
len<-length(MarksModelAccuracyList)
for(i in 1:len)
{
  Dlist[i] <- as.numeric(diff[i])
  Mlist[i] <- as.numeric(MarksModelAccuracyList[[i]]$acc)
  Nlist[i] <- as.numeric(NoMarksModelAccuracyList[[i]]$acc)
}

Diffmatrix <- matrix(unlist(Dlist), ncol = num)
Marksmatrix <- matrix(unlist(Mlist),nrow = num)
NoMarksmatrix <- matrix(unlist(Nlist),nrow = num)

write.table(Diffmatrix, file = "/home/praseeda/Documents/research/gooru/evidence/code/V6_ModifiedCompleted/results/DiffAselu64.csv", sep = ",",row.names = FALSE)
write.table(Marksmatrix, file = "/home/praseeda/Documents/research/gooru/evidence/code/V6_ModifiedCompleted/results/MarksAselu64.csv", sep = ",",row.names = FALSE)
write.table(NoMarksmatrix, file = "/home/praseeda/Documents/research/gooru/evidence/code/V6_ModifiedCompleted/results/NoMarksAselu64.csv", sep = ",",row.names = FALSE)


