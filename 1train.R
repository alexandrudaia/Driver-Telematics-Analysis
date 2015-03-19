setwd("C:/Users/Home/Desktop")
library('DMwR')
 
library(gbm)
#computes  onicescu energy for  coordinate x modulo 10 classes
speedDistribution <- function(trip)
{
  vitesse = 3.6*sqrt(diff(trip$x,20,1)^2 + diff(trip$y,20,1)^2)/20
  return(quantile(vitesse, seq(0.05,1, by = 0.012)))
}
onicescuEnergySpeed=function(trip)
{
s=speedDistribution(trip)
probabilities=table(round(s))/length(s)
onicescu=sum(probabilities^2)
return(log10(onicescu))
  
}
panta=function(trip)
{  
   
   y=diff(trip$y,1,1)
   x=diff(trip$x,1,1)
  x= ifelse(x==0,0.1,x)
   slope=y/x
  
  return(slope)
   
}
onicescuEnergySlope=function(trip)
{
s=panta(trip)
probabilities=table(round(s))/length(s)
onicescu=sum(probabilities^2)
return(onicescu)
  
}
 
 

 
 

drivers = list.files("drivers")



 

target = 1
names(target) = "target"
submission2 = NULL
for(driver in drivers)
{ t0=proc.time()
  setwd("C:/Users/Home/Desktop/sampling60")
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
    features = c (speedDistribution(trip), target)
    currentData = rbind(currentData, features)
  
  }
 
  rownames(currentData)=NULL
 # letters=c(LETTERS[seq( from = 1, to = 20 )])
  headers=c("sase","haha","sapte","opt","noua","zece","unspe","doispe","treispe","paispe","cinspe","saisep","saptspe","optspe","nouaspe","douazeci")
  headers1=c( "eu","sapteu","optu","nouau","zeceu","unspeu","doispeu","treispeu","paispeu","cinspeu","saisepu","saptspeu","optspeu","nouaspeu","douazeciu")
  other=c("cincisapte","cincisopt","cincinoua","sasezeci","saisunu","saisdoi","saistrei","saispatru","saiscinci","saissase","saisapte","saisopt","saisnoua",
          "sapteji","sapteunu","saptedoi","saptrei","saptepatr","saptecinci","saptesase","saptsapte","sapteopt","saptenoua","optzeci","optzeciunu","obezidoi",
          "obzecitrei","obzecipatru","obzecicinci","obzecisase","opzecisapte","optopt","optnoua","noua","nouaunu","nouadoi","nouatrei","nouapatru","nouacincin",
          "nouasase","nouasapte","nouaopt","nouanouna","suta","sutaunu","sutadoi","sutatrei","sutapatru","sutacinci")
  l=c(headers,headers1,other,"target")
  train = rbind(currentData, refData)
  train = as.data.frame(train)
  names(train)=l
  train$target=as.factor(train$target)
  rownames(train)=NULL
  Over = ( (0.6 * 2735) - 200 ) / 200
  Over_Perc = round(Over, 1) * 100
  Under = (0.4 * 2735) / (200* Over)
  Under_Perc = round(Under, 1) * 100
  train=SMOTE(target ~ ., train, perc.over = Over_Perc,perc.under=Under_Perc,k=40)
  
 
 # preProc=preProcess(train[,1:58],method="pca")
 # trainPC=predict(preProc,train[,1:58])
 #TRAIN WITH  GBM  BEST RESULT AFTER SMOTE METHOD APPLIES
  g= gbm.fit(x=as.matrix(train[,1:80]),y=as.matrix(train[,81]),distribution="bernoulli",n.trees=280,interaction.depth=5,shrinkage=0.01,n.minobsinnode=10,verbose=T,var.monotone=c())
 
 # g = glm(target ~ ., data=train, family = binomial("logit"))
 # g= foreach(ntree=300, .combine=combine, .packages='randomForest') %dopar%
 #randomForest(target~., data=train, ntree=ntree,na.action = na.omit)
 #g <- xgboost(data = as.matrix(train[,1:25]),
 #               label = train[,target],
 #              max.depth=2, nround=2,
 #              objective = "binary:logistic", verbose=0)
#g <- ksvm(target~.,data=train,kernel="rbfdot",prob.model=T)



  currentData = as.data.frame(currentData)
  rownames(currentData)=NULL
  names(currentData)=l
#currentData=currentData[,-52]
#testPC=predict(preProc,currentData[,1:58])
#currentData[,1:(dim(currentData)[2]-1)] <- scale(currentData[,-dim(currentData)[2]])
  #p <- predict(g, as.matrix(currentData[,1:25]))sub
 
#p =predict(g, currentData,type="prob")
#p=p[,2]
  #p=p[,2]
p=predict(object=g,newdata=as.matrix(currentData[,1:81]),n.trees=as.numeric(gbm.perf(g,plot.it=FALSE)),type="response")
# train[,1:(dim(train)[2]-1)] <- scale(train[,-dim(train)[2]])
  labels = sapply(1:200, function(x) paste0(driver,'_', x))
  result = cbind(labels, p)
  submission2 = rbind(submission2, result)
t1=proc.time()-t0
print(t1)
}
 

colnames(submission2) = c("driver_trip","prob")
write.csv(submission2, "submission2.csv", row.names=F, quote=F)

 

 
