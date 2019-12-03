aggregateFiles <- function(directory, id)
{
  aggregatedData <- NULL
  for (i in id) 
  {
     filePath = paste(
        directory, 
        "\\", 
        formatC(i, flag = "0", width = 3), 
        ".csv", 
        sep="")
     
      rawData <- read.csv(file=filePath)
      
      if(is.null(aggregatedData))
      {
        aggregatedData <- rawData
      }
      else
      {
        aggregatedData <- rbind(aggregatedData, rawData)
      }
  }

  aggregatedData
}

pollutantmean <- function(directory, pullutant, id = 1:332)
{
  data = aggregateFiles(directory, id)[pullutant]
  cleanedData = data[!is.na(data),]
  mean(cleanedData)
}

complete <- function(directory, id = 1:332)
{
  data <- aggregateFiles(directory, id)
  comp <- data[complete.cases(data),]
  factorized <- factor(comp$ID)
  rawIds <-levels(factorized)
  idSummary <- summary(factorized)
  
  idVec <- vector()
  countVec <- vector()
  
  for (i in 1:length(rawIds))
  {
    idVec[i] <- rawIds[[i]]
    countVec[i] <- idSummary[[rawIds[[i]]]]
  }
  data.frame("id" = idVec, "nobs" = countVec)
}

importCompleteCases <- function(directory, threshold)
{
  aggregatedData = list()
  for(i in 1:332)
  {
    filePath = paste(
      directory, 
      "\\", 
      formatC(i, flag = "0", width = 3), 
      ".csv", 
      sep="")
    
    rawData <- read.csv(file=filePath)
    rawData <- rawData[complete.cases(rawData),]
    if(nrow(rawData) >= threshold)
    {
      aggregatedData[i] = rawData
    }
  }
  aggregatedData
}

corr <- function(directory, threshold = 0)
{
  corVec <- vector()
  for(i in 1:332)
  {
    filePath = paste(
      directory, 
      "\\", 
      formatC(i, flag = "0", width = 3), 
      ".csv", 
      sep="")
    
    rawData = read.csv(file=filePath)
    rawData = rawData[complete.cases(rawData),]
    if(nrow(rawData) >= threshold)
    {
      corVec = append(corVec, cor(rawData$sulfate, rawData$nitrate))
    }
  }
  corVec
}





