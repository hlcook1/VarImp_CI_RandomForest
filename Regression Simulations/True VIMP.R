setwd("C:/Users/hlcoo/Dropbox/USI/Research/CI for VIMP/Analysis/Simulation/Regression")

library(randomForestSRC)


# Need to find the true VIMP per each predictor for each setting first 
# This is described in the Ishwaran and Lu paper, see section 4.2, 
# but basic steps per each simulation model: simulate 1000 independent datasets, 
# per each of those 1000 create the RF and calculate the VIMP, then average those 
# 1000 VIMPs to get the “true” VIMP value. 

######################################################################################################
#Regression Simulation 1
#set simulation number for saving file names
sim_number<-1

#Simulate 1000 independent datasets
#for each, create RF and calculate VIMP

#create VIMP matrix with NAs
vimp_all<-matrix(NA,nrow=20,ncol=1000)
colnames(vimp_all)<-paste("Sim",1:1000,sep="")
rownames(vimp_all)<-paste("X",1:20,sep="")

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
  x11<-runif(n,0,1)
  x12<-runif(n,0,1)
  x13<-runif(n,0,1)
  x14<-runif(n,0,1)
  x15<-runif(n,0,1)
  x16<-runif(n,0,1)
  x17<-runif(n,0,1)
  x18<-runif(n,0,1)
  x19<-runif(n,0,1)
  x20<-runif(n,0,1)
  
  #error
  err<-rnorm(n,0,1)
  
  #true model/simulation
  y<-10*sin(pi*x1*x2)+20*(x3-0.5)^2+10*x4+5*x5+err
  
  #Data frame
  data_use<-data.frame(y,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16,x17,x18,x19,x20)
  
  save(data_use,file=paste("True VIMP Data\\Sim",sim_number,"_Data",i,".RData",sep=""))
  
  #create RF model
  n=250;n_boots=100;n_subs=100;n_trees=250;n_splitpred=20/3;termnodesize=5
  rf.fit<-rfsrc(y~.,data=data_use,mtry = n_splitpred,ntree = n_trees,importance="permute",
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
#Regression Simulation 2
#set simulation number for saving file names
sim_number<-2

#Simulate 1000 independent datasets
#for each, create RF and calculate VIMP

#create VIMP matrix with NAs
vimp_all<-matrix(NA,nrow=20,ncol=1000)
colnames(vimp_all)<-paste("Sim",1:1000,sep="")
rownames(vimp_all)<-paste("X",1:20,sep="")

for(i in 1:1000){
  #create data
  print(paste("Working on run:",i,"Sim",sim_number))
  #set sample size value
  n<-as.integer(250)  
  
  set.seed(123+i)
  
  x1<-runif(n,0,100)
  x2<-runif(n,40*pi,560*pi)
  x3<-runif(n,0,1)
  x4<-runif(n,1,11)
  x5<-runif(n,0,1)
  x6<-runif(n,0,1)
  x7<-runif(n,0,1)
  x8<-runif(n,0,1)
  x9<-runif(n,0,1)
  x10<-runif(n,0,1)
  x11<-runif(n,0,1)
  x12<-runif(n,0,1)
  x13<-runif(n,0,1)
  x14<-runif(n,0,1)
  x15<-runif(n,0,1)
  x16<-runif(n,0,1)
  x17<-runif(n,0,1)
  x18<-runif(n,0,1)
  x19<-runif(n,0,1)
  x20<-runif(n,0,1)
  
  #error
  err<-rnorm(n,0,125)
  
  
  #true model/simulation
  y<-(x1^2+(x2*x3-(x2*x4)^(-1))^2)^(1/2)+err
  
  #Data frame
  data_use<-data.frame(y,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16,x17,x18,x19,x20)
  
  save(data_use,file=paste("True VIMP Data\\Sim",sim_number,"_Data",i,".RData",sep=""))
  
  #create RF model
  n=250;n_boots=100;n_subs=100;n_trees=250;n_splitpred=20/3;termnodesize=5
  rf.fit<-rfsrc(y~.,data=data_use,mtry = n_splitpred,ntree = n_trees,importance="permute",
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
#Regression Simulation 3
#set simulation number for saving file names
sim_number<-3

#Simulate 1000 independent datasets
#for each, create RF and calculate VIMP

#create VIMP matrix with NAs
vimp_all<-matrix(NA,nrow=20,ncol=1000)
colnames(vimp_all)<-paste("Sim",1:1000,sep="")
rownames(vimp_all)<-paste("X",1:20,sep="")

for(i in 1:1000){
  #create data
  print(paste("Working on run:",i,"Sim",sim_number))
  #set sample size value
  n<-as.integer(250)  
  
  set.seed(123+i)
  
  x1<-runif(n,0,100)
  x2<-runif(n,40*pi,560*pi)
  x3<-runif(n,0,1)
  x4<-runif(n,1,11)
  x5<-runif(n,0,1)
  x6<-runif(n,0,1)
  x7<-runif(n,0,1)
  x8<-runif(n,0,1)
  x9<-runif(n,0,1)
  x10<-runif(n,0,1)
  x11<-runif(n,0,1)
  x12<-runif(n,0,1)
  x13<-runif(n,0,1)
  x14<-runif(n,0,1)
  x15<-runif(n,0,1)
  x16<-runif(n,0,1)
  x17<-runif(n,0,1)
  x18<-runif(n,0,1)
  x19<-runif(n,0,1)
  x20<-runif(n,0,1)
  
  #error
  err<-rnorm(n,0,0.1)
  
  #true model/simulation
  y<-atan(abs(x2*x3-(x2*x4)^(-1))/x1)+err
  
  #Data frame
  data_use<-data.frame(y,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16,x17,x18,x19,x20)
  
  save(data_use,file=paste("True VIMP Data\\Sim",sim_number,"_Data",i,".RData",sep=""))
  
  #create RF model
  n=250;n_boots=100;n_subs=100;n_trees=250;n_splitpred=20/3;termnodesize=5
  rf.fit<-rfsrc(y~.,data=data_use,mtry = n_splitpred,ntree = n_trees,importance="permute",
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
#Regression Simulation 4
#set simulation number for saving file names
sim_number<-4

#Simulate 1000 independent datasets
#for each, create RF and calculate VIMP

#create VIMP matrix with NAs
vimp_all<-matrix(NA,nrow=20,ncol=1000)
colnames(vimp_all)<-paste("Sim",1:1000,sep="")
rownames(vimp_all)<-paste("X",1:20,sep="")

for(i in 1:1000){
  #create data
  print(paste("Working on run:",i,"Sim",sim_number))
  #set sample size value
  n<-as.integer(250)  
  
  set.seed(123+i)
  
  x1<-runif(n,-1,1)
  x2<-runif(n,-1,1)
  x3<-runif(n,-1,1)
  x4<-runif(n,-1,1)
  x5<-runif(n,-1,1)
  x6<-runif(n,-1,1)
  x7<-runif(n,-1,1)
  x8<-runif(n,-1,1)
  x9<-runif(n,-1,1)
  x10<-runif(n,-1,1)
  x11<-runif(n,-1,1)
  x12<-runif(n,-1,1)
  x13<-runif(n,-1,1)
  x14<-runif(n,-1,1)
  x15<-runif(n,-1,1)
  x16<-runif(n,-1,1)
  x17<-runif(n,-1,1)
  x18<-runif(n,-1,1)
  x19<-runif(n,-1,1)
  x20<-runif(n,-1,1)
  
  #error
  err<-rnorm(n,0,0.1)

  #true model/simulation
  y<-x1*x2+x3^2-x4*x7+x8*x10-x6^2+err
  
  #Data frame
  data_use<-data.frame(y,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16,x17,x18,x19,x20)
  
  save(data_use,file=paste("True VIMP Data\\Sim",sim_number,"_Data",i,".RData",sep=""))
  
  #create RF model
  n=250;n_boots=100;n_subs=100;n_trees=250;n_splitpred=20/3;termnodesize=5
  rf.fit<-rfsrc(y~.,data=data_use,mtry = n_splitpred,ntree = n_trees,importance="permute",
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
#Regression Simulation 5
#set simulation number for saving file names
sim_number<-5

#Simulate 1000 independent datasets
#for each, create RF and calculate VIMP

#create VIMP matrix with NAs
vimp_all<-matrix(NA,nrow=20,ncol=1000)
colnames(vimp_all)<-paste("Sim",1:1000,sep="")
rownames(vimp_all)<-paste("X",1:20,sep="")

for(i in 1:1000){
  #create data
  print(paste("Working on run:",i,"Sim",sim_number))
  #set sample size value
  n<-as.integer(250)  
  
  set.seed(123+i)
  
  x1<-runif(n,-1,1)
  x2<-runif(n,-1,1)
  x3<-runif(n,-1,1)
  x4<-runif(n,-1,1)
  x5<-runif(n,-1,1)
  x6<-runif(n,-1,1)
  x7<-runif(n,-1,1)
  x8<-runif(n,-1,1)
  x9<-runif(n,-1,1)
  x10<-runif(n,-1,1)
  x11<-runif(n,-1,1)
  x12<-runif(n,-1,1)
  x13<-runif(n,-1,1)
  x14<-runif(n,-1,1)
  x15<-runif(n,-1,1)
  x16<-runif(n,-1,1)
  x17<-runif(n,-1,1)
  x18<-runif(n,-1,1)
  x19<-runif(n,-1,1)
  x20<-runif(n,-1,1)
  
  #error
  err<-rnorm(n,0,0.1)

  #true model/simulation
  y<-ifelse(x1>0,1,0)+x2^3+ifelse(x4+x6-x8-x9 > 1+x10,1,0)+exp(-x2^2)+err
  
  #Data frame
  data_use<-data.frame(y,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16,x17,x18,x19,x20)
  
  save(data_use,file=paste("True VIMP Data\\Sim",sim_number,"_Data",i,".RData",sep=""))
  
  #create RF model
  n=250;n_boots=100;n_subs=100;n_trees=250;n_splitpred=20/3;termnodesize=5
  rf.fit<-rfsrc(y~.,data=data_use,mtry = n_splitpred,ntree = n_trees,importance="permute",
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
#Regression Simulation 6
#set simulation number for saving file names
sim_number<-6

#Simulate 1000 independent datasets
#for each, create RF and calculate VIMP

#create VIMP matrix with NAs
vimp_all<-matrix(NA,nrow=20,ncol=1000)
colnames(vimp_all)<-paste("Sim",1:1000,sep="")
rownames(vimp_all)<-paste("X",1:20,sep="")

for(i in 1:1000){
  #create data
  print(paste("Working on run:",i,"Sim",sim_number))
  #set sample size value
  n<-as.integer(250)  
  
  set.seed(123+i)
  
  x1<-runif(n,-1,1)
  x2<-runif(n,-1,1)
  x3<-runif(n,-1,1)
  x4<-runif(n,-1,1)
  x5<-runif(n,-1,1)
  x6<-runif(n,-1,1)
  x7<-runif(n,-1,1)
  x8<-runif(n,-1,1)
  x9<-runif(n,-1,1)
  x10<-runif(n,-1,1)
  x11<-runif(n,-1,1)
  x12<-runif(n,-1,1)
  x13<-runif(n,-1,1)
  x14<-runif(n,-1,1)
  x15<-runif(n,-1,1)
  x16<-runif(n,-1,1)
  x17<-runif(n,-1,1)
  x18<-runif(n,-1,1)
  x19<-runif(n,-1,1)
  x20<-runif(n,-1,1)
  
  #error
  err<-rnorm(n,0,0.1)

  #true model/simulation
  y<-x1^2+3*x2^2*x3*exp(-abs(x4))+x6-x8+err
  
  #Data frame
  data_use<-data.frame(y,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16,x17,x18,x19,x20)
  
  save(data_use,file=paste("True VIMP Data\\Sim",sim_number,"_Data",i,".RData",sep=""))
  
  #create RF model
  n=250;n_boots=100;n_subs=100;n_trees=250;n_splitpred=20/3;termnodesize=5
  rf.fit<-rfsrc(y~.,data=data_use,mtry = n_splitpred,ntree = n_trees,importance="permute",
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
#Regression Simulation 7
#set simulation number for saving file names
sim_number<-7

#Simulate 1000 independent datasets
#for each, create RF and calculate VIMP

#create VIMP matrix with NAs
vimp_all<-matrix(NA,nrow=20,ncol=1000)
colnames(vimp_all)<-paste("Sim",1:1000,sep="")
rownames(vimp_all)<-paste("X",1:20,sep="")

for(i in 1:1000){
  #create data
  print(paste("Working on run:",i,"Sim",sim_number))
  #set sample size value
  n<-as.integer(250)  
  
  set.seed(123+i)
  
  x1<-runif(n,-1,1)
  x2<-runif(n,-1,1)
  x3<-runif(n,-1,1)
  x4<-runif(n,-1,1)
  x5<-runif(n,-1,1)
  x6<-runif(n,-1,1)
  x7<-runif(n,-1,1)
  x8<-runif(n,-1,1)
  x9<-runif(n,-1,1)
  x10<-runif(n,-1,1)
  x11<-runif(n,-1,1)
  x12<-runif(n,-1,1)
  x13<-runif(n,-1,1)
  x14<-runif(n,-1,1)
  x15<-runif(n,-1,1)
  x16<-runif(n,-1,1)
  x17<-runif(n,-1,1)
  x18<-runif(n,-1,1)
  x19<-runif(n,-1,1)
  x20<-runif(n,-1,1)
  
  #error
  err<-rnorm(n,0,0.1)

  #true model/simulation
  y<-ifelse(x1+x4^3+x9+sin(x2*x8)+err > 0.38,1,0)
  
  #Data frame
  data_use<-data.frame(y,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16,x17,x18,x19,x20)
  
  save(data_use,file=paste("True VIMP Data\\Sim",sim_number,"_Data",i,".RData",sep=""))
  
  #create RF model
  n=250;n_boots=100;n_subs=100;n_trees=250;n_splitpred=20/3;termnodesize=5
  rf.fit<-rfsrc(y~.,data=data_use,mtry = n_splitpred,ntree = n_trees,importance="permute",
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
#Regression Simulation 8
#set simulation number for saving file names
sim_number<-8

#Simulate 1000 independent datasets
#for each, create RF and calculate VIMP

#create VIMP matrix with NAs
vimp_all<-matrix(NA,nrow=20,ncol=1000)
colnames(vimp_all)<-paste("Sim",1:1000,sep="")
rownames(vimp_all)<-paste("X",1:20,sep="")

for(i in 1:1000){
  #create data
  print(paste("Working on run:",i,"Sim",sim_number))
  #set sample size value
  n<-as.integer(250)  
  
  set.seed(123+i)
  
  x1<-runif(n,0.5,1)
  x2<-runif(n,0.5,1)
  x3<-runif(n,0.5,1)
  x4<-runif(n,0.5,1)
  x5<-runif(n,0.5,1)
  x6<-runif(n,0.5,1)
  x7<-runif(n,-1,1)
  x8<-runif(n,-1,1)
  x9<-runif(n,-1,1)
  x10<-runif(n,-1,1)
  x11<-runif(n,-1,1)
  x12<-runif(n,-1,1)
  x13<-runif(n,-1,1)
  x14<-runif(n,-1,1)
  x15<-runif(n,-1,1)
  x16<-runif(n,-1,1)
  x17<-runif(n,-1,1)
  x18<-runif(n,-1,1)
  x19<-runif(n,-1,1)
  x20<-runif(n,-1,1)
  
  #error
  err<-rnorm(n,0,0.1)

  #true model/simulation
  y<-log(x1+x2*x3)-exp(x4*x5^(-1)-x6)+err
  
  #Data frame
  data_use<-data.frame(y,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16,x17,x18,x19,x20)
  
  save(data_use,file=paste("True VIMP Data\\Sim",sim_number,"_Data",i,".RData",sep=""))
  
  #create RF model
  n=250;n_boots=100;n_subs=100;n_trees=250;n_splitpred=20/3;termnodesize=5
  rf.fit<-rfsrc(y~.,data=data_use,mtry = n_splitpred,ntree = n_trees,importance="permute",
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
#Regression Simulation 9
#set simulation number for saving file names
sim_number<-9

#Simulate 1000 independent datasets
#for each, create RF and calculate VIMP

#create VIMP matrix with NAs
vimp_all<-matrix(NA,nrow=20,ncol=1000)
colnames(vimp_all)<-paste("Sim",1:1000,sep="")
rownames(vimp_all)<-paste("X",1:20,sep="")

for(i in 1:1000){
  #create data
  print(paste("Working on run:",i,"Sim",sim_number))
  #set sample size value
  n<-as.integer(250)  
  
  set.seed(123+i)
  
  x1<-runif(n,-1,1)
  x2<-runif(n,-1,1)
  x3<-runif(n,-1,1)
  x4<-runif(n,-1,1)
  x5<-runif(n,-1,1)
  x6<-runif(n,-1,1)
  x7<-runif(n,-1,1)
  x8<-runif(n,-1,1)
  x9<-runif(n,-1,1)
  x10<-runif(n,-1,1)
  x11<-runif(n,-1,1)
  x12<-runif(n,-1,1)
  x13<-runif(n,-1,1)
  x14<-runif(n,-1,1)
  x15<-runif(n,-1,1)
  x16<-runif(n,-1,1)
  x17<-runif(n,-1,1)
  x18<-runif(n,-1,1)
  x19<-runif(n,-1,1)
  x20<-runif(n,-1,1)
  
  #error
  err<-rnorm(n,0,0.1)

  #true model/simulation
  y<-x1*x2^2*(abs(x3))^(1/2)+floor(x4-x5*x6)+err
  
  #Data frame
  data_use<-data.frame(y,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16,x17,x18,x19,x20)
  
  save(data_use,file=paste("True VIMP Data\\Sim",sim_number,"_Data",i,".RData",sep=""))
  
  #create RF model
  n=250;n_boots=100;n_subs=100;n_trees=250;n_splitpred=20/3;termnodesize=5
  rf.fit<-rfsrc(y~.,data=data_use,mtry = n_splitpred,ntree = n_trees,importance="permute",
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
#Regression Simulation 10
#set simulation number for saving file names
sim_number<-10

#Simulate 1000 independent datasets
#for each, create RF and calculate VIMP

#create VIMP matrix with NAs
vimp_all<-matrix(NA,nrow=20,ncol=1000)
colnames(vimp_all)<-paste("Sim",1:1000,sep="")
rownames(vimp_all)<-paste("X",1:20,sep="")

for(i in 1:1000){
  #create data
  print(paste("Working on run:",i,"Sim",sim_number))
  #set sample size value
  n<-as.integer(250)  
  
  set.seed(123+i)
  
  x1<-runif(n,-1,1)
  x2<-runif(n,-1,1)
  x3<-runif(n,-1,1)
  x4<-runif(n,-1,1)
  x5<-runif(n,-1,1)
  x6<-runif(n,-1,1)
  x7<-runif(n,-1,1)
  x8<-runif(n,-1,1)
  x9<-runif(n,-1,1)
  x10<-runif(n,-1,1)
  x11<-runif(n,-1,1)
  x12<-runif(n,-1,1)
  x13<-runif(n,-1,1)
  x14<-runif(n,-1,1)
  x15<-runif(n,-1,1)
  x16<-runif(n,-1,1)
  x17<-runif(n,-1,1)
  x18<-runif(n,-1,1)
  x19<-runif(n,-1,1)
  x20<-runif(n,-1,1)
  
  #error
  err<-rnorm(n,0,0.1)

  #true model/simulation
  y<-x3*(x1+1)^(abs(x2))-(x5^2*(abs(x4)+abs(x5)+abs(x6))^(-1))^(1/2)+err
  
  #Data frame
  data_use<-data.frame(y,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16,x17,x18,x19,x20)
  
  save(data_use,file=paste("True VIMP Data\\Sim",sim_number,"_Data",i,".RData",sep=""))
  
  #create RF model
  n=250;n_boots=100;n_subs=100;n_trees=250;n_splitpred=20/3;termnodesize=5
  rf.fit<-rfsrc(y~.,data=data_use,mtry = n_splitpred,ntree = n_trees,importance="permute",
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
#Regression Simulation 11
#set simulation number for saving file names
sim_number<-11

#Simulate 1000 independent datasets
#for each, create RF and calculate VIMP

#create VIMP matrix with NAs
vimp_all<-matrix(NA,nrow=20,ncol=1000)
colnames(vimp_all)<-paste("Sim",1:1000,sep="")
rownames(vimp_all)<-paste("X",1:20,sep="")

for(i in 1:1000){
  #create data
  print(paste("Working on run:",i,"Sim",sim_number))
  #set sample size value
  n<-as.integer(250)  
  
  set.seed(123+i)
  
  x1<-runif(n,-1,1)
  x2<-runif(n,-1,1)
  x3<-runif(n,-1,1)
  x4<-runif(n,-1,1)
  x5<-runif(n,-1,1)
  x6<-runif(n,-1,1)
  x7<-runif(n,-1,1)
  x8<-runif(n,-1,1)
  x9<-runif(n,-1,1)
  x10<-runif(n,-1,1)
  x11<-runif(n,-1,1)
  x12<-runif(n,-1,1)
  x13<-runif(n,-1,1)
  x14<-runif(n,-1,1)
  x15<-runif(n,-1,1)
  x16<-runif(n,-1,1)
  x17<-runif(n,-1,1)
  x18<-runif(n,-1,1)
  x19<-runif(n,-1,1)
  x20<-runif(n,-1,1)
  
  #error
  err<-rnorm(n,0,0.1)

  #true model/simulation
  y<-cos(x1-x2)+asin(x1*x3)-atan(x2-x3^2)+err
  
  #Data frame
  data_use<-data.frame(y,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16,x17,x18,x19,x20)
  
  save(data_use,file=paste("True VIMP Data\\Sim",sim_number,"_Data",i,".RData",sep=""))
  
  #create RF model
  n=250;n_boots=100;n_subs=100;n_trees=250;n_splitpred=20/3;termnodesize=5
  rf.fit<-rfsrc(y~.,data=data_use,mtry = n_splitpred,ntree = n_trees,importance="permute",
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
#Regression Simulation 12
#set simulation number for saving file names
sim_number<-12

#Simulate 1000 independent datasets
#for each, create RF and calculate VIMP

#create VIMP matrix with NAs
vimp_all<-matrix(NA,nrow=20,ncol=1000)
colnames(vimp_all)<-paste("Sim",1:1000,sep="")
rownames(vimp_all)<-paste("X",1:20,sep="")

for(i in 1:1000){
  #create data
  print(paste("Working on run:",i,"Sim",sim_number))
  #set sample size value
  n<-as.integer(250)  
  
  set.seed(123+i)
  
  x1<-rnorm(n,0,1)
  x2<-rnorm(n,0,1)
  x3<-rnorm(n,0,1)
  x4<-rnorm(n,0,1)
  x5<-rnorm(n,0,1)
  x6<-rnorm(n,0,1)
  x7<-rnorm(n,0,1)
  x8<-rnorm(n,0,1)
  x9<-rnorm(n,0,1)
  x10<-rnorm(n,0,1)
  x11<-rnorm(n,0,1)
  x12<-rnorm(n,0,1)
  x13<-rnorm(n,0,1)
  x14<-rnorm(n,0,1)
  x15<-rnorm(n,0,1)
  x16<-rnorm(n,0,1)
  x17<-rnorm(n,0,1)
  x18<-rnorm(n,0,1)
  x19<-rnorm(n,0,1)
  x20<-rnorm(n,0,1)
  
  #error
  err<-rnorm(n,0,1)

  #true model/simulation
  y<-err
  
  #Data frame
  data_use<-data.frame(y,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16,x17,x18,x19,x20)
  
  save(data_use,file=paste("True VIMP Data\\Sim",sim_number,"_Data",i,".RData",sep=""))
  
  #create RF model
  n=250;n_boots=100;n_subs=100;n_trees=250;n_splitpred=20/3;termnodesize=5
  rf.fit<-rfsrc(y~.,data=data_use,mtry = n_splitpred,ntree = n_trees,importance="permute",
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