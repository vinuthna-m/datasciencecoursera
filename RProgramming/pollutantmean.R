pollutantmean <- function(directory, pollutant, id=1:332){
  if(id[1]<10){
    filename <- paste("00", id[1], ".csv", sep = "")
  }
  else if((id[1]>=10)&&(id[1]<100)){
    filename <- paste("0",id[1],".csv",sep="")
  }
  else{
    filename <- paste(id[1],".csv",sep = "")
  }
  new_id <- id[1]:id[length(id)]
  df <- read.csv(paste(directory,filename,sep = "/"))
  for(f in new_id){
    if(f<10){
      filename <- paste("00", f, ".csv", sep = "")
    }
    else if((f>=10)&&(f<100)){
      filename <- paste("0",f,".csv",sep="")
    }
    else{
      filename <- paste(f,".csv",sep = "")
    }
    df <- rbind(df, read.csv(paste(directory,filename,sep = "/")))
  }
  mean(df[,pollutant], na.rm = TRUE)
}

