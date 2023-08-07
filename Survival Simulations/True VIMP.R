setwd("C:/Users/hlcoo/Dropbox/USI/Research/CI for VIMP/Analysis/Simulation/Survival")

library(randomForestSRC)
library(coxed)

# Need to find the true VIMP per each predictor for each setting first 
# This is described in the Ishwaran and Lu paper, see section 4.2, 
# but basic steps per each simulation model: simulate 1000 independent datasets, 
# per each of those 1000 create the RF and calculate the VIMP, then average those 
# 1000 VIMPs to get the “true” VIMP value. 

######################################################################################################
#Classification Simulation 1
#set simulation number for saving file names
sim_number<-1

#Simulate 1000 independent datasets
#for each, create RF and calculate VIMP

#create VIMP matrix with NAs
vimp_all<-matrix(NA,nrow=10,ncol=1000)
colnames(vimp_all)<-paste("Sim",1:1000,sep="")
rownames(vimp_all)<-paste("X",1:10,sep="")

for(i in 1:1000){
  #create data
  print(paste("Working on run:",i,"Sim",sim_number))
  #set sample size value
  n<-as.integer(250)  
  
  set.seed(123+i)
  
  
  x1<-runif(n,0,1)
  x2<-runif(n,0,1)
  x3<-runif(n,0,1)
  x4<-runif(n,0,1)
  x5<-runif(n,0,1)
  x6<-runif(n,0,1)
  x7<-runif(n,0,1)
  x8<-runif(n,0,1)
  x9<-runif(n,0,1)
  x10<-runif(n,0,1)
  
  
  #X Data frame
  data_x<-data.frame(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10)
  
  #censoring rate
  cens<-0.19
  
  #############################################################################
  #Creating true model
  
  #true model/simulation
  # h<-exp(x1-2*x4+2*x5)
  data_sim<-sim.survdata(N=n,num.data.frames = 1,
                         beta=c(1, 0, 0, -2, 2, rep(0,5)),
                         X=data_x,T=500000,
                         censor = cens)
  
  
  #storing just the dataframe
  data_use<-data_sim$data
  #changing the event variable to an integer
  data_use$event<-ifelse(data_use$failed=="TRUE",1,0)
  data_use<-data_use[,-c(12)] 
  
  save(data_use,file=paste("True VIMP Data\\Sim",sim_number,"_Data",i,".RData",sep=""))
  
  #create RF model
  n=250;n_boots=100;n_subs=100;n_trees=250;n_splitpred=sqrt(10);termnodesize=5
  #Creating RF model
  
  rf.fit<-rfsrc(Surv(y,event)~.,data=data_use,mtry = n_splitpred,ntree = n_trees,importance="permute",
                nodesize=termnodesize,samptype = "swr",seed=-3,block.size = 1)
  save(rf.fit,file=paste("True VIMP RF Models\\Sim",sim_number,"_RFmodel",i,".RData",sep=""))
  
  
  #get the full forest's VIMP
  vimp<-rf.fit$importance
  vimp_all[,i]<-vimp
}



#average VIMP values to get True VIMP
trueVIMP<-rowMeans(vimp_all,na.rm = T)

save(trueVIMP,file=paste("True VIMP\\TrueVIMP_Sim",sim_number,".RData",sep=""))

rm(list=ls())




######################################################################################################
#Classification Simulation 2
#set simulation number for saving file names
sim_number<-2

#Simulate 1000 independent datasets
#for each, create RF and calculate VIMP

#create VIMP matrix with NAs
vimp_all<-matrix(NA,nrow=10,ncol=1000)
colnames(vimp_all)<-paste("Sim",1:1000,sep="")
rownames(vimp_all)<-paste("X",1:10,sep="")

for(i in 1:1000){
  #create data
  print(paste("Working on run:",i,"Sim",sim_number))
  #set sample size value
  n<-as.integer(250)  
  
  set.seed(123+i)
  
  x1<-runif(n,0,1)
  x2<-runif(n,0,1)
  x3<-runif(n,0,1)
  x4<-runif(n,0,1)
  x5<-runif(n,0,1)
  x6<-runif(n,0,1)
  x7<-runif(n,0,1)
  x8<-runif(n,0,1)
  x9<-runif(n,0,1)
  x10<-runif(n,0,1)
  
  
  #X Data frame
  data_x<-data.frame(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10)
  
  #censoring rate
  cens<-0.15
  
  #############################################################################
  #Creating true model
  
  #true model/simulation
  #if x1<=0.5
  # h<-exp(x2)1_{t not in [0.5, 2.5]}
  #if x1>0.5
  # h<-exp(x3)1_{t not in [2.5, 4.5]}
  
  #specify the hazard function
  my_haz<-function(t){
    ifelse(x1<= 0.5,
           exp(x2)*ifelse(t < 0.5 | t > 2.5,1,0),
           exp(x3)*ifelse(t < 2.5 | t > 4.5,1,0))
  }
  data_sim<-sim.survdata(N=n,num.data.frames = 1,
                         hazard.fun = my_haz,
                         X=data_x,
                         T=250,
                         censor = cens)
  
  
  #storing just the dataframe
  data_use<-data_sim$data
  #changing the event variable to an integer
  data_use$event<-ifelse(data_use$failed=="TRUE",1,0)
  data_use<-data_use[,-c(12)]
  
  save(data_use,file=paste("True VIMP Data\\Sim",sim_number,"_Data",i,".RData",sep=""))
  
  #create RF model
  n=250;n_boots=100;n_subs=100;n_trees=250;n_splitpred=sqrt(10);termnodesize=5
  #Creating RF model
  
  rf.fit<-rfsrc(Surv(y,event)~.,data=data_use,mtry = n_splitpred,ntree = n_trees,importance="permute",
                nodesize=termnodesize,samptype = "swr",seed=-3,block.size = 1)
  save(rf.fit,file=paste("True VIMP RF Models\\Sim",sim_number,"_RFmodel",i,".RData",sep=""))
  
  
  #get the full forest's VIMP
  vimp<-rf.fit$importance
  vimp_all[,i]<-vimp
}



#average VIMP values to get True VIMP
trueVIMP<-rowMeans(vimp_all,na.rm = T)

save(trueVIMP,file=paste("True VIMP\\TrueVIMP_Sim",sim_number,".RData",sep=""))

rm(list=ls())


######################################################################################################
#Classification Simulation 3
#set simulation number for saving file names
sim_number<-3

#Simulate 1000 independent datasets
#for each, create RF and calculate VIMP

#create VIMP matrix with NAs
vimp_all<-matrix(NA,nrow=10,ncol=1000)
colnames(vimp_all)<-paste("Sim",1:1000,sep="")
rownames(vimp_all)<-paste("X",1:10,sep="")

for(i in 1:1000){
  #create data
  print(paste("Working on run:",i,"Sim",sim_number))
  #set sample size value
  n<-as.integer(250)  
  
  set.seed(123+i)
  
  x1<-runif(n,0,1)
  x2<-runif(n,0,1)
  x3<-runif(n,0,1)
  x4<-runif(n,0,1)
  x5<-runif(n,0,1)
  x6<-runif(n,0,1)
  x7<-runif(n,0,1)
  x8<-runif(n,0,1)
  x9<-runif(n,0,1)
  x10<-runif(n,0,1)
  
  
  #X Data frame
  data_x<-data.frame(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10)
  
  #censoring rate
  cens<-0.29
  
  #############################################################################
  #Creating true model
  
  #true model/simulation
  #z1=0.5*x1
  #z2=x4+x5
  # h<-(1+z2*t)*exp(z1+z2*t)
  
  #specify the hazard function
  z1<-0.5*x1
  z2<-x4+x5
  my_haz<-function(t){
    (1+z2*t)*exp(z1+z2*t)
  }
  data_sim<-sim.survdata(N=n,num.data.frames = 1,
                         hazard.fun = my_haz,
                         X=data_x,
                         T=250,
                         censor = cens)
  
  
  #storing just the dataframe
  data_use<-data_sim$data
  #changing the event variable to an integer
  data_use$event<-ifelse(data_use$failed=="TRUE",1,0)
  data_use<-data_use[,-c(12)]
  
  save(data_use,file=paste("True VIMP Data\\Sim",sim_number,"_Data",i,".RData",sep=""))
  
  #create RF model
  n=250;n_boots=100;n_subs=100;n_trees=250;n_splitpred=sqrt(10);termnodesize=5
  #Creating RF model
  
  rf.fit<-rfsrc(Surv(y,event)~.,data=data_use,mtry = n_splitpred,ntree = n_trees,importance="permute",
                nodesize=termnodesize,samptype = "swr",seed=-3,block.size = 1)
  save(rf.fit,file=paste("True VIMP RF Models\\Sim",sim_number,"_RFmodel",i,".RData",sep=""))
  
  
  #get the full forest's VIMP
  vimp<-rf.fit$importance
  vimp_all[,i]<-vimp
}



#average VIMP values to get True VIMP
trueVIMP<-rowMeans(vimp_all,na.rm = T)

save(trueVIMP,file=paste("True VIMP\\TrueVIMP_Sim",sim_number,".RData",sep=""))

rm(list=ls())


