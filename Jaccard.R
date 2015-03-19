source('monitor.R')
library(foreach)
library(doParallel)
cl <- makeCluster(2)
registerDoParallel(cl)
speedDistribution <- function(trip)
{
  speed = sqrt(diff(trip$x,1,1)^2 + diff(trip$y,1,1)^2)
  return(speed)
} 
jaccardSimilarity=function(trip,driver)
{
  currentSpeed=trunc(speedDistribution(trip))
  jaccardCumul=0
  foreach(i=1:200,.combine='rbind', 
          .export=c('speedDistribution' ,'monitor'))%dopar%
  {
    newSpeed=trunc(speedDistribution(monitor(driver,i,F)))
    union=union(as.numeric(currentSpeed),as.numeric(newSpeed))
    intersect=intersect(as.numeric(currentSpeed),as.numeric(newSpeed))
    j=length(intersect)/length(union)
    jaccardCumul=jaccardCumul+j
}
return(jaccardCumul-1)
}
