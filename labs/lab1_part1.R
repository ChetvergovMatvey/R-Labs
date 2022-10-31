data_e1 <- read.csv('C:/Users/79130/Downloads/lab1_e1.csv')
sapply(data_e1, typeof)

head(data_e1)

fix_data <- function(data) 
{
  for (i in names(data)) 
  { if (length(grep('[a-z]', data[[i]], ignore.case=T))==0) 
  { 
    data[[i]] <- as.double(gsub(" ","", data[[i]], fixed =TRUE))
  }
  } 
  return(data) 
}

data_e1_result <- fix_data(data_e1) 
sapply(data_e1_result, typeof)

head(data_e1_result)


load('C:/Users/79130/Downloads/lab1_e2.Rdata')
str(all_data)

get_id <- function(data) 
{
  full_data <- do.call("rbind", data)
  
  mean_temp_data <- setNames(aggregate(temp ~ id, full_data, mean), c("id", "mean_temp"))
  
  id_count_data <- setNames(aggregate(temp ~ id, full_data, length), c("id", "count"))
  
  new_data <- merge(mean_temp_data, id_count_data, by = 'id')
  
  result <- subset(new_data, count == 7, select = c('id', 'mean_temp'))
  
  row.names(result) <- c(1:nrow(result))
  return (result)
}

data_e2 <- get_id(all_data)
print(data_e2)