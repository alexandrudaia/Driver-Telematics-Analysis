
library(data.table)
setwd("C:/Users/Home/Desktop")
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
speedDistribution <- function(trip)
{
  speed = sqrt(diff(trip[,x],1,1)^2 + diff(trip[,y],1,1)^2)
  return(speed)
} 

monotonyEnergy=function(trip)
{
speed=speedDistribution(trip)
# brings + or-  if  speed   is increasing or decreasing and  0  if speed is the same
diffspeed=diff(speed,1,1)
m=sign(diffspeed)
probabilities=(table(m))/length(m)
onicescuMonotonyEnergy=sum(probabilities^2)
return(onicescuMonotonyEnergy)

}
onicescuCorrelation=function(trip)
{
#diffs  for  x and  y
sx=sign(round(diff(trip[,x],1,1)))
sy=sign(round(diff(trip[,y],1,1)))
sx0=length(sx[sx==0])
sx1=length(sx[sx==1])
sxminus1=length(sx[sx==-1])
sy0=length(sy[sy==0])
sy1=length(sy[sy==1])
syminus1=length(sy[sy==-1])
#probabilities of   rounded diffs 
Psx=(c(sx0,sx1,sxminus1))/length(sx)
Psy=(c(sy0,sy1,syminus1))/length(sy)
#onicescu energy of rounded    diffs
IPsx=sum(Psx^2)
IPsy=sum(Psy^2)

 
#onicescu  correlation 

Rxy=(Psx%*%Psy)/sqrt(IPsx*IPsy)
return(Rxy)
}
coef=c()
for(i in 1:200)
{
trip=monitor(1,i,F)
t=onicescuCorrelation(trip)
coef=c(coef,t)

}
# to  be cotinued urgently , frechet distance  , bariblian 







 
