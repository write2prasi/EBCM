PopulateNoMarksAccuracy <- function(DataSet, NoMarksAccuracy)
{
  #DataSet <- ListDataSet
  #NoMarksAccuracy <- NoMarksModelAccuracyList
  #i<-1
  #num<-2
  num <- length(DataSet) / 2
  for (i in 1:num)
  {
    all_data_f <- DataSet[2 * i]
    all_data1 <- data.frame(all_data_f)
    all_data1 <- ShuffleData(all_data_f = all_data1)
    all_data <- as.matrix(all_data1)
    Model <- BuildModelNoMarks(all_data = all_data)
    for (j in 1:num)
    {
      if (i == j)
      {
        #Single Model building and testing
        # Populate the sample for testing
        print(paste0("Single Model i = ", i))
        ind <- SampleData(all_data)
        NoMarksAccuracy[[(i - 1) * num + j]] <-
          OneBuildAndTestModelNoMarks(all_data, ind)
      }
      else
      {
        print(paste0("Model i = ", i))
        print(paste0("Test j = ", j))
        test_data_f <- DataSet[2 * j]
        test_data1 <- data.frame(test_data_f)
        test_data <- as.matrix(test_data1)
        NoMarksAccuracy[[(i - 1) * num + j]] <-
          TestAccuracyNoMarks(Model, test_data)
      }
    }
    
  }
  return(NoMarksAccuracy)
}
