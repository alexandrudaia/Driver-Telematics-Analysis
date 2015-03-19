#this  function  returns   euclidian distanrce between  2  consecutive   points from  the  trip
library('gbm')
library('DMwR')
speedDistributionQuantiles <- function(trip)
{
  vitesse = 3.6*sqrt(diff(trip$x,8,1)^2 + diff(trip$y,8,1)^2)/20
  return(quantile(vitesse, seq(0.05,1, by = 0.017)))
}

complexFeature=function(trip)
{
  y=diff(trip$y,1,1)
  x=diff(trip$x,1,1)
  x= ifelse(x==0,0.1,x)
  slope=y/x
  speed = sqrt(diff(trip$x,1,1)^2 + diff(trip$y,1,1)^2)
  c=slope*speed
  return(sd(c))
}
#  feature acc  avg
accelerationAvg=function(trip)
{
  speed = sqrt(diff(trip$x,1,1)^2 + diff(trip$y,1,1)^2)
  acc=diff(speed,1,1)
  return(mean(acc))
}
#standard deviation of acceleration
accelerationSd=function(trip)
{
  speed = sqrt(diff(trip$x,1,1)^2 + diff(trip$y,1,1)^2)
  acc=diff(speed,1,1)
  return(sd(acc))
}
#maximum speed
speedMax=function(trip)
{
  
  speed = sqrt(diff(trip$x,1,1)^2 + diff(trip$y,1,1)^2)
  max=max(speed)
  return(max)
}
#returns the  average of  the speed  measured  from one point to another
#since  the time is  zero  the speed is equal  with the distance  so we   compute  the euclidian distance(l2 norm)
speedDistributionAvg <- function(trip)
{
  vitesse = 3.6*sqrt(diff(trip$x,1,1)^2 + diff(trip$y,1,1)^2)/20
  # return(quantile(vitesse, seq(0.05,1, by = 0.1)))
  return(mean(vitesse))
}
#mileage is   the sum of the lengths from one point to another
#this  feature it  is simple the   length of the trip
mileage=function(trip)
{
  mil=  sum(sqrt(diff(trip$x,1,1)^2 + diff(trip$y,1,1)^2))
  return(mil)
}
#we know  that  the  coordinates  are  gathered from a gps each second
#so we create a feature that  represents  the total time
#it  is simple the  the number of columns  in the csv  files dim(dataframe) returns on  the  first index the number of columns

triptime=function(trip)
{
  t=dim(trip)[1]
  return(t)
}
speedVariance=function(trip)
{
  
  vitesse = 3.6*sqrt(diff(trip$x,1,1)^2 + diff(trip$y,1,1)^2)/20
  v=var(vitesse)
  return(v)
}
panta=function(trip)
{  
  
  y=diff(trip$y,20,1)
  x=diff(trip$x,20,1)
  x= ifelse(x==0,0.1,x)
  slope=y/x
  
  return(quantile(slope, seq(0.05,1, by = 0.05)))
  
}


#this  function  returns   euclidian distanrce between  2  consecutive   points from  the  trip

speedDistribution <- function(trip)
{
  speed = sqrt(diff(trip$x,1,1)^2 + diff(trip$y,1,1)^2)
  return(speed)
} 

#here is onicescu energy(sum of  squared probabilities) for the speed
onicescuEnergySpeed=function(trip)
{
  #calls  the  speedDistribution function and  stores  the  speed's in a variable called  s
  s=speedDistribution(trip)
  #here are  computed  the probabilities of rounded  speeds
  #table() counts  how many elements are  from each class
  probabilities=table(round(s))/length(s)
  onicescu=sum(probabilities^2)
  #I loged it   so  the onicescu  energy looks  more  gaussian
  return(log10(onicescu))
  
}
#I compute  the slope  for  each point(y2-y1)/(x2-x1) diff  returns  differences of consecutive elements
panta=function(trip)
{  
  
  y=diff(trip$y,1,1)
  x=diff(trip$x,1,1)
  x= ifelse(x==0,0.1,x)#correction in case x  is o  we will have  -inf which we don;t wan't
  slope=y/x
  
  return(slope)
  
}
#computes onicescu energy for  the  array(vector) with  slopes(I compute slope in order to see  the curvature  of the trip)
onicescuEnergySlope=function(trip)
{
  s=panta(trip)
  probabilities=table(round(s))/length(s)
  onicescu=sum(probabilities^2)
  return(onicescu)
  
}
 
setwd("C:/Users/Home/Desktop")

drivers = list.files("drivers")



 

target = 1
names(target) = "target"
submission1 = NULL
for(driver in drivers)
{ t0=proc.time()
  setwd("C:/Users/Home/Desktop/sampling")
  refData=read.table(paste("sampleFordriver",driver,".csv"),header=F,sep=",",skip=1)
  refData=refData[,-1]
  
  refData=as.matrix(refData)
 
  setwd("C:/Users/Home/Desktop")
  
  print(driver)
  dirPath = paste0("drivers/", driver, '/')
  currentData = NULL
  for(i in 1:200)
  {
    trip = read.csv(paste0(dirPath, i, ".csv"))
    features = c(speedDistributionQuantiles(trip),onicescuEnergySpeed(trip), onicescuEnergySlope(trip),triptime(trip),speedVariance(trip),speedDistributionAvg(trip),(speedDistributionAvg (trip))^2,(speedDistributionAvg (trip))^3,mileage(trip),accelerationSd(trip),speedMax(trip),accelerationAvg(trip),complexFeature(trip),target)
    currentData = rbind(currentData, features)
  
  }
 
  rownames(currentData)=NULL
  
  index= seq(from=1 ,to=56)
  h=paste0('q',index)
  headers=c(h,"OnicescuSpeed","OnicescuSlope","triptime","speedVariance","speedDistributionAvg","speedDistributionpow2","speedDistributionAvgpow3","mileage","accelerationSd","speedMax","accelerationAvg","complexFeature","target")
  
  train1 = rbind(currentData, refData)
  train1 = as.data.frame(train1)
  names(train1)=headers
  train1$target=as.factor(train1$target)
  rownames(train1)=NULL
   #BECAUSE  WE HAVE RARE POPULATIONS
   train1=SMOTE(target ~ ., train1, perc.over = 100,perc.under=200)
 
 # preProc=preProcess(train1[,1:58],method="pca")
 # train1PC=predict(preProc,train1[,1:58])
 # g= gbm.fit(x=as.matrix(train1[,1:68]),y=as.matrix(train1[,69]),distribution="bernoulli",n.trees=300,interaction.depth=5,shrinkage=0.01,n.minobsinnode=10,verbose=T,var.monotone=c())
 
 # g = glm(target ~ ., data=train1, family = binomial("logit"))
 # g= foreach(ntree=300, .combine=combine, .packages='randomForest') %dopar%
 #randomForest(target~., data=train1, ntree=ntree,na.action = na.omit)
 #g <- xgboost(data = as.matrix(train1[,1:25]),
 #               label = train1[,target],
 #              max.depth=2, nround=2,
 #              objective = "binary:logistic", verbose=0)
#g <- ksvm(target~.,data=train1,kernel="rbfdot",prob.model=T)



  currentData = as.data.frame(currentData)
  rownames(currentData)=NULL
  names(currentData)=headers
#currentData=currentData[,-52]
#testPC=predict(preProc,currentData[,1:58])
#currentData[,1:(dim(currentData)[2]-1)] <- scale(currentData[,-dim(currentData)[2]])
  #p <- predict(g, as.matrix(currentData[,1:25]))
 
#p =predict(g, currentData,type="prob")
#p=p[,2]
  #p=p[,2]
idial=class.ind(train1$target)
ann=nnet(train1[,-69], idial, size=8, softmax=TRUE)
t=predict(ann,currentData[,1:68], type="raw")
p=t[,2]

#p=predict(object=g,newdata=as.matrix(currentData[,1:68]),n.trees=as.numeric(gbm.perf(g,plot.it=FALSE)),type="response")
# train1[,1:(dim(train1)[2]-1)] <- scale(train1[,-dim(train1)[2]])
  labels = sapply(1:200, function(x) paste0(driver,'_', x))
  result = cbind(labels, p)
  submission1 = rbind(submission1, result)
t1=proc.time()-t0
print(t1)
}
 

colnames(submission1) = c("driver_trip","prob")
write.csv(submission1, "submission1.csv", row.names=F, quote=F)

 

 )
