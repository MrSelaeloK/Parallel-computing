install.packages("doParallel")
library(doParallel)
install.packages("foreach")
library(parallel)
library(foreach)
cores<-detectCores()      #detecting the number of cores
cl<-makeCluster(6)        #allocating 6 cores
registerDoParallel(cl)
library("MASS")
#_______________________________________________________
#Question 1

results<-  foreach(i=1:100,.combine=rbind) %do% {
    j<-rexp(50)
    c(mean(j),var(j))
  }
print(results)
#________________________________________________________


#Question 2

system.time(sequential.processing<-for( i in 1:100){
  data<-replicate(1000,median(sample(galaxies,82,replace = TRUE)))
  mean(data)
  
})

getDoParWorkers()           #Checking if my cores are split
system.time(parallel.processing<-foreach(i=1:100,.packages = "MASS",.combine=rbind)%dopar%{
  data<-replicate(1000,median(sample(galaxies,82,replace=T)))
  mean(data)
  
})

#_____________________________________________________________________________
#Question 3

set.seed(123)
count<-foreach(i=1:1000,.combine="+")%dopar%{
  og.sample<-rexp(50)
  boot.means<-replicate(1000,mean(sample(og.sample,50,replace = T)))
  ordered.boot<-sort(boot.means)
  ci <- quantile(ordered.boot, c(0.01, 0.99))
  as.integer(ci[1] <= 1 && ci[2] >= 1)
}
proportion<-count/1000
print(proportion)

#there seems to be a consistent underestimation and an estimation 
#when the numbers are taken up

#_______________________________________________________________________________
#Question 4

library(iterators)
set.seed(1234)
max_values<-foreach(i=1:3,.combine=c) %do%
  {
    itr<-irnorm(5)
  max(nextElem(itr))
  }

as.matrix(max_values)
rownames(max.values)<-c("max value itr1","max value itr2", "max value itr3")
colnames(max.values)<-"Max Values"

#_______________________________________________________________________________
#Question 5
set.seed(1234)
itr<-irnorm(5)
systemT.replicate<-system.time(replicate(3,max(nextElem(itr))))

systemT.foreach<-system.time(max.values<-foreach(i=1:3,.combine=rbind,
                                                 .packages = "iterators") %dopar%
  {
    max.value<-max(nextElem(itr))
  }) 


itr<-irnorm(5)
vec1<-nextElem(itr)
vec2<-nextElem(itr)
vec3<-nextElem(itr)
vec<-list(vec1,vec2,
          vec3)
systemT.Lapply<-system.time(parLapply(cl,vec,max))



systemT.foreach<-data.frame(systemT.foreach)
systemT.replicate<-data.frame(systemT.replicate)
systemT.Lapply<-data.frame(systemT.Lapply)
cbind(systemT.foreach,systemT.Lapply,
      systemT.replicate)


stopCluster(cl)       #releasing the cores

