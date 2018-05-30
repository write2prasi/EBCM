BuildData <- function(path, inputFileList) {
  allcollection <- fread(file.path(path, inputFileList[1]))
  completed <- fread(file.path(path, inputFileList[2]))
  progress <- fread(file.path(path, inputFileList[3]))
  usercollection <- fread(file.path(path, inputFileList[4]))
  
  # Reshape data ---------
  completed$status <- as.factor(completed$status)
  progress$status <- as.factor(progress$status)
  
  # merge both progress and completed
  all <- rbind(completed, progress)
  
  #Working on allcollection column to get individual competency code
  a <- cSplit(allcollection, "competency_code", ",")
  
  newcollection <- data.frame(character(), character())
  if (ncol(a) > 2)
  {
    for (i in 1:nrow(a))
    {
      size <- ncol(a)
      for (j in 2:size)
      {
        if (!is.na(a[i, j, with = FALSE]))
        {
          newrow <- data.frame(a[i, 1 , with = FALSE], a[i, j , with = FALSE])
          colnames(newrow) <- c("user_id", "competency_code")
          newcollection <- rbind(newcollection, newrow)
        }
      }
    }
  }
  if (ncol(a) == 2)
  {
    newcollection <- data.frame(allcollection)
  }
  
  newcollection$competency_code <-
    as.character(newcollection$competency_code)
  
  #To join the user_col and all_col dataframes first remove the brackets and double
  #quotes in all_data
  
  
  for (i in 1:nrow(newcollection))
  {
    newcollection$competency_code[i] <-
      str_replace_all(newcollection$competency_code[i], '[""]', '')
  }
  
  for (i in 1:nrow(newcollection))
  {
    newcollection$competency_code[i] <-
      str_replace_all(newcollection$competency_code[i], '[\\[]', '')
  }
  
  for (i in 1:nrow(newcollection))
  {
    newcollection$competency_code[i] <-
      str_replace_all(newcollection$competency_code[i], '[\\]]', '')
  }
  
  # Count the number of collections for each user, for each competency
  # Find the total number of collections for each competency
  
  user_col <- usercollection %>%
    group_by(user_id, competency_code) %>%
    summarise(count = n())
  
  all_col <- newcollection %>%
    group_by(competency_code) %>%
    summarise(count = n())
  
  #changing the column names of all_col and user_col
  
  colnames(all_col) <- c("competency_code", "total_count")
  colnames(user_col) <- c("user_id", "competency_code", "ind_count")
  
  #Convert all_data into dataframe type
  all_col <- data.frame(all_col)
  
  
  #Merge user_col and all_col
  val <- inner_join(user_col, all_col)
  val <- data.frame(val)
  
  #Creating a new column that stores the percentage of resources consumed
  #by a learner per competency
  val$resource_count <- val$competency_code
  
  for (i in 1:nrow(val))
  {
    val$resource_count[i] <- val$ind_count[i] / val$total_count[i]
  }
  
  #Keeping only resource_count in val
  val1 <- val[, c(1, 2, 5)]
  
  #Merge all user information
  all_data <- inner_join(all, val1)
  
  #Keep necessary columns
  all_data_final <-  all_data[, c(4, 5, 7, 8, 9, 10, 3)]
  
  #change data to matrix
  all_data_f <- as.matrix(all_data_final)
  
  #Normalise data
  all_data_f[, 1] <- normalize(all_data_final[, 1])
  all_data_f[, 2] <- normalize(all_data_final[, 2])
  all_data_f[, 3] <- normalize(all_data_final[, 3])
  all_data_f[, 4] <- normalize(all_data_final[, 4])
  all_data_f[, 5] <- normalize(all_data_final[, 5])
  #all_data_f[,6]<-normalize(all_data_final[,6])
  all_data_f[, 7] <- all_data_final[, 7]
  all_data_f[, 6] <- all_data_final[, 6]
  
  #data is present in all_data_f remove the column names
  dimnames(all_data_f) <- NULL
  
  #change the predicting variable into 0,1 etc
  all_data_f[, 7] <- as.numeric(all_data_f[, 7]) - 1
  
  return(all_data_f)
  
}
