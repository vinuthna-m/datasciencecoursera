complete <- function(directory, id = 1:332){
  rtrnFrame <- data.frame(id = numeric(0), nobs = numeric(0))
  for(f in id){
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
    nrows <- nrow(df)
    nobs <- c(f,nrows)
    rtrnFrame[nrow(rtrnFrame) + 1, ] <- nobs
  }
  rtrnFrame
}
