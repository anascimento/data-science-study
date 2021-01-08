pollutantmean <- function(directory, pollutant, id = 1:332){
  
  directory <- paste(getwd(),"/",directory,"/",sep="")
  
  # Aux variables
  file_list <- list.files(directory)
  data <- NA
  #For each id passed as parameter:
  for (i in id) {
    #Read the file,
    file_dir <- paste(directory,file_list[i],sep="")
    file_data <- read.csv(file_dir)
    
    # accumulate the data
    # adding new rows
    data <- rbind(data,file_data)
  }
  # Calculate the mean and return it
  # to read just a collumn 
  mean(data[[pollutant]],na.rm = TRUE)
  
}


complete <- function(directory, id = 1:332){
  # Get full path of the specsdata folder
  directory <- paste(getwd(),"/","specdata","/",sep="")
  
  # Aux variables
  file_list <- list.files(directory)
  # use vector because the number of rows is unknown
  ids <- vector()
  nobs <- vector()
  
  #For each id passed as parameter:
  for (i in id) {
    # Read the file,
    file_dir <- paste(directory,file_list[i],sep="")
    file_data <- read.csv(file_dir)
    
    # acumulate ids and nobs values in the vectors    
    ids = c(ids,i)
    nobs = c(nobs,sum(complete.cases(file_data)))        
  }
  # Finally, Create the data frame using the vectors and return it
  data.frame(id = ids, nobs = nobs)
}



corr <- function(directory, threshold = 0) {
  # Get full path of the specsdata folder
  directory <- paste(getwd(),"/",directory,"/",sep="")    
  
  #Get observations and filter by threshold
  observations <- complete(directory)
  filtered_observations = subset(observations,observations$nobs > threshold)
  
  # Aux variables
  file_list <- list.files(directory)
  correlation <- vector()
  
  # For each id in filtered observations:
  for (i in filtered_observations$id) {
    # Read the file,
    file_dir <- paste(directory,file_list[i],sep="")
    file_data <- read.csv(file_dir)
    # remove NA,
    file_data <- subset(file_data,complete.cases(file_data))        
    # and calculate the cor and accumulate it in the corellation vector.
    correlation <- c(correlation,cor(file_data$nitrate,file_data$sulfate))    
  }
  #Finally, return the vector
  correlation
}