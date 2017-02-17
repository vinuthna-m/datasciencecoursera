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
    append(x, cor(df$sulfate,df$nitrate),after = length(x))
  }
  x
}