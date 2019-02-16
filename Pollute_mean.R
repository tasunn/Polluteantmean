pollutantmean<-function(directory,pollutant,id=1:332)
{
  ## define buffer variable to set path name of the directory using paste() 
  # address<-paste("D:/works/R-program for Johns hopkins/playground",directory,sep="/")
  address<-paste("D:/Tasun/Tasun's files/Program/Rstudio/Rstudio-HW",directory,sep="/")
  setwd(address) 
  ## define buffer variable for sum and count default =0
  SumMeanMyData<-0
  count<-0
  AllSumrow<-0
  AllSumMyData<-0
  ## use i as a buffer to recall each the value of id 
  for(i in id)
  {
    ## use str_pad function in stringr package to complete 3 digit of id
    id_<-str_pad(i, width=3, side="left", pad="0")
    ## define buffer variable to store the file name
    filename <-paste(id_,"csv",sep=".")
    
    if(pollutant=="nitrate")
    { 
      # #import csv data to Rstudio
      # MyData <- read.csv(file=filename, sep=",",colClasses=c("NULL", "NULL", NA))
      # #Calculate Mean
      # MeanMyData<-colMeans(MyData,na.rm=TRUE)
      # #Sum mean up
      # SumMeanMyData<-SumMeanMyData+MeanMyData
      # #counter count
      # count<-count+1
      
      #import csv data to Rstudio
      MyData <- read.csv(file=filename, sep=",",colClasses=c("NULL", "NULL", NA,"NULL"))
      #CleanData
      CompleateMyData<-na.omit(MyData)
      #Calculate row of file
      Sumrow<-nrow(CompleateMyData)
      if(Sumrow!=0)
      {
        #Sum row up
        AllSumrow<-AllSumrow+Sumrow
        #Calculate sum of file
        SumMyData<-sum(CompleateMyData,na.rm=TRUE)
        #Sum mean up
        AllSumMyData<-AllSumMyData+SumMyData
      }
      
    }
    else if(pollutant=="sulfate")
    {
      #import csv data to Rstudio
      MyData <- read.csv(file=filename, sep=",",colClasses=c("NULL", NA, "NULL","NULL"))
      #Calculate mean of file
      SumMyData<-sum(MyData,na.rm=TRUE)
      #Sum mean up
      AllSumMyData<-AllSumMyData+SumMyData
      #Calculate row of file
      CompleateMyData<-na.omit(MyData)
      Sumrow<-nrow(CompleateMyData)
      #Sum row up
      AllSumrow<-AllSumrow+Sumrow
    }
  }
  Totalmean<-AllSumMyData/AllSumrow
  message("Totalmean : ",Totalmean)
  
}

complete<-function(directory,id)
{
  ## define buffer variable to set path name of the directory using paste() 
  # address<-paste("D:/works/R-program for Johns hopkins/playground",directory,sep="/")
  address<-paste("D:/Tasun/Tasun's files/Program/Rstudio/Rstudio-HW",directory,sep="/")
  setwd(address)
  
  ## use i as a buffer to recall each the value of id 
  for(i in id)
  {
    ## use str_pad function in stringr package to complete 3 digit of id
    id_<-str_pad(i, width=3, side="left", pad="0")
    ## define buffer variable to store the file name
    filename <-paste(id_,"csv",sep=".")
    #import csv data to Rstudio
    MyData <- read.csv(file=filename, sep=",",colClasses=c(NA,NA, NA))
    #remove the NA value in the row to complete the table
    CompleateMyData<-MyData[complete.cases(MyData), ]
    #count number of rows
    nobs<-nrow(CompleateMyData)
    message("ID: ",i,"--> nobs : ",nobs)
  }
  
}

corr<-function(directory,threshold=0)
{
  ## define buffer variable to set path name of the directory using paste() 
  # address<-paste("D:/works/R-program for Johns hopkins/playground",directory,sep="/")
  address<-paste("D:/Tasun/Tasun's files/Program/Rstudio/Rstudio-HW",directory,sep="/")
  setwd(address)
  output<-0
  ## use i as a buffer to recall each the value of id 
  for(i in 1:332)
  {
    ## use str_pad function in stringr package to complete 3 digit of threshold
    id_<-str_pad(i, width=3, side="left", pad="0")
    ## define buffer variable to store the file name
    filename <-paste(id_,"csv",sep=".")
    #import csv data to Rstudio
    MyData <- read.csv(file=filename, sep=",",colClasses=c("NULL",NA, NA,"NULL"))
    #remove the NA value in the row to complete the table
    CompleateMyData<-MyData[complete.cases(MyData), ]
    #count number of rows
    nobs<-nrow(CompleateMyData)
    if(nobs>threshold)
    {
      Corr_compleateMyData<-cor(CompleateMyData)
      # message(Corr_compleateMyData)
      output<-Corr_compleateMyData[1,2]
      # output<-rbind(Corr_compleateMyData)
      
    }
    
  }
  return(output)
  
  
}