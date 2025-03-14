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
results<-data.frame(results)
colnames(results)<-c("mean", "variance")
print(data.frame(results))
#________________________________________________________


#Question 2

library(doParallel)
library(foreach)
cores<-detectCores()      #detecting the number of cores
cl<-makeCluster(6)        #allocating 6 cores
registerDoParallel(cl)
library("MASS")

#Sequential processing
sequential.processing<-system.time(for( i in 1:100){
  data<-replicate(1000,median(sample(galaxies,82,replace = TRUE)))
  mean(data)
})


#getDoParWorkers()           #Checking if my cores are split
#parallel processing
parallel.processing<-system.time(foreach(i=1:100,.packages = "MASS",.combine=rbind)%dopar%{
  data<-replicate(1000,median(sample(galaxies,82,replace=T)))
  mean(data)
  
})

data.frame(cbind(sequential.processing,parallel.processing))
stopCluster(cl)       #releasing the cores
#_____________________________________________________________________________
#Question 3
library(doParallel)
library(foreach)
cores<-detectCores()      #detecting the number of cores
cl<-makeCluster(6)        #allocating 6 cores
registerDoParallel(cl)
#_______________

set.seed(123)
count<-foreach(i=1:1000,.combine="+")%dopar%{
  og.sample<-rexp(50)
  boot.means<-replicate(1000,mean(sample(og.sample,50,replace = T)))
  ordered.boot<-sort(boot.means)
  ci <- quantile(ordered.boot, c(0.01, 0.99))
  as.integer(ci[1] <= 1 && ci[2] >= 1)
}
proportion<-count/1000

print(paste("The proprotion is: ",proportion),quote = F)

stopCluster(cl)       #releasing the cores
#there seems to be a consistent underestimation and an estimation 
#when the numbers are taken up

#_______________________________________________________________________________
#Question 4

library(iterators)
library(foreach)
cores<-detectCores()      #detecting the number of cores
cl<-makeCluster(6)        #allocating 6 cores
registerDoParallel(cl)


set.seed(1234)
max_values<-foreach(i=1:3,.combine=c) %do%
  {
    itr<-irnorm(5)
    max(nextElem(itr))
  }

max_values<-data.frame(max_values)
rownames(max_values)<-c("max value vect1","max value vect2", "max value vect3")
colnames(max_values)<-"Max Values"

max_values
stopCluster(cl)       #releasing the cores

#_______________________________________________________________________________
#Question 5
library(doParallel)
#library(parallel)
library(foreach)
cores<-detectCores()      #detecting the number of cores
cl<-makeCluster(cores-2)        #allocating 6 cores
registerDoParallel(cl)


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
systemT.parLapply<-system.time(parLapply(cl,vec,max))



systemT.foreach<-data.frame(systemT.foreach)
systemT.replicate<-data.frame(systemT.replicate)
systemT.parLapply<-data.frame(systemT.parLapply)
cbind(systemT.foreach,systemT.parLapply,
      systemT.replicate)

stopCluster(cl)       #releasing the cores
