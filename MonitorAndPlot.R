monitor=function(driver,trip,summarize=FALSE)
{
  path="C:/Users/Home/Desktop/drivers"
  trip_char=as.character(trip)
  driver_char=as.character(driver)
  file_name=paste(c(path,"/",driver_char,"/",trip_char,".csv"),collapse='')
  
  
  if(summarize==TRUE)
  {
    print(summary(fread(file_name)))
  }
  x=fread(file_name)
  return(x)
  
}



plotTrips=function(driver)
{
  setwd("C:/Users/Home/Desktop")
  source("monitor.R")
  library(ggplot2)
  
setwd("C:/Users/Home/Desktop/drivers")
d1=c()
for(i in 1:200){
d1=rbind(d1,cbind(i,monitor(driver,i,F)))
}
p=qplot(x,y,data=d1,colour=i,geom="point")
return(p)
setwd("C:/Users/Home/Desktop")
}
