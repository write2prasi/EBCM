PopulateMarksAccuracy <- function(DataSet,MarksAccuracy)
{
  
  num<-length(DataSet)/2
  for(i in 1:num)
  {
    all_data_f <- DataSet[2*i]
    all_data1 <- data.frame(all_data_f)
    all_data1 <- ShuffleData(all_data_f = all_data1)
    all_data <- as.matrix(all_data1)
    Model <- BuildModelMarks(all_data = all_data)
    for(j in 1:num)
    {
      #j<-2
      if(i==j)
      {
        #Single Model building and testing
        # Populate the sample for testing
        print(paste0("Single Model i = ",i)) 
        ind <- SampleData(all_data)
        MarksAccuracy[[(i-1)*num+j]]<- OneBuildAndTestModelMarks(all_data,ind)
      }
      else
      {
        print(paste0("Model i = ",i)) 
        print(paste0("Test j = ",j))
        test_data_f <- DataSet[2*j]
        test_data1 <- data.frame(test_data_f)
        test_data <- as.matrix(test_data1)
        MarksAccuracy[[(i-1)*num+j]]<- TestAccuracyMarks(Model,test_data)
      }
    }
    
  }
  return(MarksAccuracy)
}

