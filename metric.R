library(data.table)
#library(foreach)
#library(doParallel)
#cl<-makeCluster(detectCores())
#registerDoParallel(cl)
#I   get  only 2 random trips from  4  distint  regions  in the trips  in order to  create the boundary
#It  was correct  to use all of  the points  as boundary
# I will  sample  2 trips  from regions without replacement
region1=c(1:50)
region2=c(51:100)
region3=(101:150)
region4=(151:200)
setwd("C:/Users/Home/Desktop")
source('monitor.R')
barbilianBoundary=function(driver)
{
boundary=data.table()
epsilon=0.001
tripRegion1=sample(region1,2,replace=F)
tripRegion2=sample(region2,2,replace=F)
tripRegion3=sample(region3,2,replace=F)
tripRegion4=sample(region4,2,replace=F)
trip1=monitor(driver,tripRegion1[1],F)
trip2=monitor(driver,tripRegion1[2],F)
trip3=monitor(driver,tripRegion2[1],F)
trip4=monitor(driver,tripRegion2[2],F)
trip5=monitor(driver,tripRegion3[1],F)
trip6=monitor(driver,tripRegion3[2],F)
trip7=monitor(driver,tripRegion4[1],F)
trip8=monitor(driver,tripRegion4[2],F)
boundary=rbind(boundary,trip1[dim(trip1)[1],]+(epsilon*sign(trip1[dim(trip1)[1],])))
boundary=rbind(boundary,trip2[dim(trip2)[1],]+(epsilon*sign(trip2[dim(trip2)[1],])))
boundary=rbind(boundary,trip3[dim(trip3)[1],]+(epsilon*sign(trip3[dim(trip3)[1],])))
boundary=rbind(boundary,trip4[dim(trip4)[1],]+(epsilon*sign(trip4[dim(trip4)[1],])))
boundary=rbind(boundary,trip5[dim(trip5)[1],]+(epsilon*sign(trip5[dim(trip5)[1],])))
boundary=rbind(boundary,trip6[dim(trip6)[1],]+(epsilon*sign(trip6[dim(trip6)[1],])))
boundary=rbind(boundary,trip7[dim(trip7)[1],]+(epsilon*sign(trip7[dim(trip7)[1],])))
boundary=rbind(boundary,trip8[dim(trip8)[1],]+(epsilon*sign(trip8[dim(trip8)[1],])))
return(boundary)
}

barbilianMetric=function(driver,trip)
{
boundary=barbilianBoundary(driver)
trip=monitor(driver,trip)
metric=data.table()
for( i in 2:(dim(trip)[1]-1))
{
#get  consecutive    points x,y- not  
x=trip[i,]
y=trip[i+1,]

#max  for first  term 
term1=c()
for(j in 1:dim(boundary)[1])
{
term=log((dist(rbind(as.matrix(boundary[,j]-x),c(0,0))))/dist(rbind(as.matrix(boundary[,j]-y),c(0,0))))
term1=c(term1,term)
}
 
term2=c()
for(j in 1:dim(boundary)[1])
{
term=log((dist(rbind(as.matrix(boundary[,j]-y),c(0,0))))/dist(rbind(as.matrix(boundary[,j]-x),c(0,0))))
term2=c(term2,term)
}
metric=rbind(metric,data.table(max(term1)+max(term2)))
 
}
return(metric)
}





































