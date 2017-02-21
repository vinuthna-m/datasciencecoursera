#Function to return vector with correlation values for files in the directory 
#which have complete observations greater than or equal to the threshold value
corr <- function(directory, threshold = 0){
  x <- vector(mode = "numeric", length=0)
  temp <- complete(directory, 1:332)
  temp <- subset(temp, nobs>=threshold)
  vec <- temp$id
  for(f in vec){
    if(f<10){
      filename <- paste("00", f, ".csv", sep = "")
    }
    else if((f>=10)&&(f<100)){
      filename <- paste("0",f,".csv",sep="")
    }
    else{
      filename <- paste(f,".csv",sep = "")
    }
    df <- read.csv(paste(directory,filename,sep = "/"))
    df <- na.omit(df)
    val <- cor(df$sulfate,df$nitrate)
    x <- c(x,val)
  }
  #returning vector with correlation values for files with nobs greater than threshold
  x
}