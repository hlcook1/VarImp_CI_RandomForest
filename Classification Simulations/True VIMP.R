setwd("C:/Users/hlcoo/Dropbox/USI/Research/CI for VIMP/Analysis/Simulation/Classification")

library(randomForestSRC)


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
vimp_all<-matrix(NA,nrow=20,ncol=1000)
colnames(vimp_all)<-paste("Sim",1:1000,sep="")
rownames(vimp_all)<-paste("X",1:20,sep="")

for(i in 1:1000){
  #create data
  print(paste("Working on run:",i,"Sim",sim_number))
  #set sample size value
  n<-as.integer(250)  
  
  #Creating true model
  library(mlbench)
  
  #threenorm sim with 20 variables
  data<-mlbench.threenorm(n=n,d=20)
  x<-as.data.frame(data$x)
  names(x)<-paste("X",1:ncol(x),sep="")
  y<-data$classes
  
  data_use<-data.frame(x,y)  
  
  save(data_use,file=paste("True VIMP Data\\Sim",sim_number,"_Data",i,".RData",sep=""))
  
  #create RF model
  n=250;n_boots=100;n_subs=100;n_trees=250;n_splitpred=20^(1/2);termnodesize=5
  #Creating RF model
  
  rf.fit<-rfsrc(y~.,data=data_use,mtry = n_splitpred,ntree = n_trees,importance="permute",
                nodesize=termnodesize,samptype = "swr",seed=-3,block.size = 1,
                perf.type="brier")
  save(rf.fit,file=paste("True VIMP RF Models\\Sim",sim_number,"_RFmodel",i,".RData",sep=""))
  
  
  #get the full forest's VIMP
  vimp<-rf.fit$importance[,1]
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
vimp_all<-matrix(NA,nrow=20,ncol=1000)
colnames(vimp_all)<-paste("Sim",1:1000,sep="")
rownames(vimp_all)<-paste("X",1:20,sep="")

for(i in 1:1000){
  #create data
  print(paste("Working on run:",i,"Sim",sim_number))
  #set sample size value
  n<-as.integer(250)  
  
  #Creating true model
  library(caret)
  
  #Data frame
  # 2 factors, 5 linear, 3 nonlinear, 10 noise
  data_use<-twoClassSim(n=n,noiseVars = 10,linearVars = 5)
  names(data_use)[ncol(data_use)]<-"y"
  
  save(data_use,file=paste("True VIMP Data\\Sim",sim_number,"_Data",i,".RData",sep=""))
  
  #create RF model
  n=250;n_boots=100;n_subs=100;n_trees=250;n_splitpred=20^(1/2);termnodesize=5
  #Creating RF model
  
  rf.fit<-rfsrc(y~.,data=data_use,mtry = n_splitpred,ntree = n_trees,importance="permute",
                nodesize=termnodesize,samptype = "swr",seed=-3,block.size = 1,
                perf.type="brier")
  save(rf.fit,file=paste("True VIMP RF Models\\Sim",sim_number,"_RFmodel",i,".RData",sep=""))
  
  
  #get the full forest's VIMP
  vimp<-rf.fit$importance[,1]
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
vimp_all<-matrix(NA,nrow=20,ncol=1000)
colnames(vimp_all)<-paste("Sim",1:1000,sep="")
rownames(vimp_all)<-paste("X",1:20,sep="")

for(i in 1:1000){
  #create data
  print(paste("Working on run:",i,"Sim",sim_number))
  #set sample size value
  n<-as.integer(250)  
  
  #Creating true model
  library(caret)
  
  #Data frame
  # 2 factors, 5 linear, 3 nonlinear, 10 noise
  # also rho = 0.75
  data_use<-twoClassSim(n=n,noiseVars = 10,linearVars = 5,corrValue = 0.75)
  names(data_use)[ncol(data_use)]<-"y"
  
  save(data_use,file=paste("True VIMP Data\\Sim",sim_number,"_Data",i,".RData",sep=""))
  
  #create RF model
  n=250;n_boots=100;n_subs=100;n_trees=250;n_splitpred=20^(1/2);termnodesize=5
  #Creating RF model
  
  rf.fit<-rfsrc(y~.,data=data_use,mtry = n_splitpred,ntree = n_trees,importance="permute",
                nodesize=termnodesize,samptype = "swr",seed=-3,block.size = 1,
                perf.type="brier")
  save(rf.fit,file=paste("True VIMP RF Models\\Sim",sim_number,"_RFmodel",i,".RData",sep=""))
  
  
  #get the full forest's VIMP
  vimp<-rf.fit$importance[,1]
  vimp_all[,i]<-vimp
}



#average VIMP values to get True VIMP
trueVIMP<-rowMeans(vimp_all,na.rm = T)

save(trueVIMP,file=paste("True VIMP\\TrueVIMP_Sim",sim_number,".RData",sep=""))

rm(list=ls())


######################################################################################################
#Classification Simulation 4
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
  
  #Creating true model
  library(caret)
  
  #Data frame
  # 2 factors, 15 linear, 3 nonlinear
  data_use<-twoClassSim(n=n,linearVars = 15)
  names(data_use)[ncol(data_use)]<-"y"
  
  save(data_use,file=paste("True VIMP Data\\Sim",sim_number,"_Data",i,".RData",sep=""))
  
  #create RF model
  n=250;n_boots=100;n_subs=100;n_trees=250;n_splitpred=20^(1/2);termnodesize=5
  #Creating RF model
  
  rf.fit<-rfsrc(y~.,data=data_use,mtry = n_splitpred,ntree = n_trees,importance="permute",
                nodesize=termnodesize,samptype = "swr",seed=-3,block.size = 1,
                perf.type="brier")
  save(rf.fit,file=paste("True VIMP RF Models\\Sim",sim_number,"_RFmodel",i,".RData",sep=""))
  
  
  #get the full forest's VIMP
  vimp<-rf.fit$importance[,1]
  vimp_all[,i]<-vimp
}



#average VIMP values to get True VIMP
trueVIMP<-rowMeans(vimp_all,na.rm = T)

save(trueVIMP,file=paste("True VIMP\\TrueVIMP_Sim",sim_number,".RData",sep=""))

rm(list=ls())



######################################################################################################
#Classification Simulation 5
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
  
  #Creating true model
  library(caret)
  
  #Data frame
  # 2 factors, 15 linear, 3 nonlinear
  # also rho = 0.75
  data_use<-twoClassSim(n=n,linearVars = 15,corrValue = 0.75)
  names(data_use)[ncol(data_use)]<-"y"

  save(data_use,file=paste("True VIMP Data\\Sim",sim_number,"_Data",i,".RData",sep=""))
  
  #create RF model
  n=250;n_boots=100;n_subs=100;n_trees=250;n_splitpred=20^(1/2);termnodesize=5
  rf.fit<-rfsrc(y~.,data=data_use,mtry = n_splitpred,ntree = n_trees,importance="permute",
                nodesize=termnodesize,samptype = "swr",seed=-3,block.size = 1,
                perf.type="brier")
  save(rf.fit,file=paste("True VIMP RF Models\\Sim",sim_number,"_RFmodel",i,".RData",sep=""))
  
  
  #get the full forest's VIMP
  vimp<-rf.fit$importance[,1]
  vimp_all[,i]<-vimp
}



#average VIMP values to get True VIMP
trueVIMP<-rowMeans(vimp_all,na.rm = T)

save(trueVIMP,file=paste("True VIMP\\TrueVIMP_Sim",sim_number,".RData",sep=""))

rm(list=ls())



######################################################################################################
#Classification Simulation 6
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
  
  #############################################################################
  #Sampling random values for variables and error
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
  
  
  #############################################################################
  #Creating true model
  
  #true model/simulation
  cont_y<-x1^2+3*x2^2*x3*exp(-abs(x4))+x6-x8+err
  
  #discretize y into 3 classes based on quartiles
  quants<-quantile(cont_y,probs = c(0.5))
  disc_y<-ifelse(cont_y<quants,"Lower 50","")
  disc_y<-ifelse(cont_y>=quants,"Upper 50",disc_y)
  
  table(disc_y)/sum(table(disc_y))*100
  
  y<-as.factor(disc_y)
  
  #Data frame
  data_use<-data.frame(y,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16,x17,x18,x19,x20)
  
  save(data_use,file=paste("True VIMP Data\\Sim",sim_number,"_Data",i,".RData",sep=""))
  
  #create RF model
  n=250;n_boots=100;n_subs=100;n_trees=250;n_splitpred=20^(1/2);termnodesize=5
  #Creating RF model
  
  rf.fit<-rfsrc(y~.,data=data_use,mtry = n_splitpred,ntree = n_trees,importance="permute",
                nodesize=termnodesize,samptype = "swr",seed=-3,block.size = 1,
                perf.type="brier")
  save(rf.fit,file=paste("True VIMP RF Models\\Sim",sim_number,"_RFmodel",i,".RData",sep=""))
  
  
  #get the full forest's VIMP
  vimp<-rf.fit$importance[,1]
  vimp_all[,i]<-vimp
}



#average VIMP values to get True VIMP
trueVIMP<-rowMeans(vimp_all,na.rm = T)

save(trueVIMP,file=paste("True VIMP\\TrueVIMP_Sim",sim_number,".RData",sep=""))

rm(list=ls())



######################################################################################################
#Classification Simulation 7
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
  
  #############################################################################
  #Sampling random values for variables and error

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
  
  
  #############################################################################
  #Creating true model
  
  #true model/simulation
  cont_y<-x1^2+3*x2^2*x3*exp(-abs(x4))+x6-x8+err
  
  #discretize y into 3 classes based on quartiles
  quants<-quantile(cont_y,probs = c(0.5))
  disc_y<-ifelse(cont_y<quants,"Lower 50","")
  disc_y<-ifelse(cont_y>=quants,"Upper 50",disc_y)
  
  table(disc_y)/sum(table(disc_y))*100
  
  y<-as.factor(disc_y)
  
  #Data frame
  data_use<-data.frame(y,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16,x17,x18,x19,x20)
  
  save(data_use,file=paste("True VIMP Data\\Sim",sim_number,"_Data",i,".RData",sep=""))
  
  #create RF model
  n=250;n_boots=100;n_subs=100;n_trees=250;n_splitpred=20^(1/2);termnodesize=5
  #Creating RF model
  
  rf.fit<-rfsrc(y~.,data=data_use,mtry = n_splitpred,ntree = n_trees,importance="permute",
                nodesize=termnodesize,samptype = "swr",seed=-3,block.size = 1,
                perf.type="brier")
  save(rf.fit,file=paste("True VIMP RF Models\\Sim",sim_number,"_RFmodel",i,".RData",sep=""))
  
  
  #get the full forest's VIMP
  vimp<-rf.fit$importance[,1]
  vimp_all[,i]<-vimp
}



#average VIMP values to get True VIMP
trueVIMP<-rowMeans(vimp_all,na.rm = T)

save(trueVIMP,file=paste("True VIMP\\TrueVIMP_Sim",sim_number,".RData",sep=""))

rm(list=ls())



######################################################################################################
#Classification Simulation 8
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
  
  #############################################################################
  #Sampling random values for variables and error
 
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
  
  
  #############################################################################
  #Creating true model
  
  #true model/simulation
  cont_y<-x1*x2^2*(abs(x3))^(1/2)+floor(x4-x5*x6)+err
  
  
  #discretize y into 3 classes based on quartiles
  quants<-quantile(cont_y,probs = c(0.25,0.75))
  disc_y<-ifelse(cont_y<quants[1],"Lower 25","")
  disc_y<-ifelse(cont_y>quants[2],"Upper 25",disc_y)
  disc_y<-ifelse(cont_y>=quants[1] & cont_y<=quants[2],"Middle 50",disc_y)
  
  table(disc_y)/sum(table(disc_y))*100
  
  y<-as.factor(disc_y)
  
  #Data frame
  data_use<-data.frame(y,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16,x17,x18,x19,x20)
  
  save(data_use,file=paste("True VIMP Data\\Sim",sim_number,"_Data",i,".RData",sep=""))
  
  #create RF model
  n=250;n_boots=100;n_subs=100;n_trees=250;n_splitpred=20^(1/2);termnodesize=5
  #Creating RF model
  
  rf.fit<-rfsrc(y~.,data=data_use,mtry = n_splitpred,ntree = n_trees,importance="permute",
                nodesize=termnodesize,samptype = "swr",seed=-3,block.size = 1,
                perf.type="brier")
  save(rf.fit,file=paste("True VIMP RF Models\\Sim",sim_number,"_RFmodel",i,".RData",sep=""))
  
  
  #get the full forest's VIMP
  vimp<-rf.fit$importance[,1]
  vimp_all[,i]<-vimp
}



#average VIMP values to get True VIMP
trueVIMP<-rowMeans(vimp_all,na.rm = T)

save(trueVIMP,file=paste("True VIMP\\TrueVIMP_Sim",sim_number,".RData",sep=""))

rm(list=ls())



######################################################################################################
#Classification Simulation 9
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
  
  #############################################################################
  #Sampling random values for variables and error
  
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
  
  
  #############################################################################
  #Creating true model
  
  #true model/simulation
  cont_y<-x3*(x1+1)^(abs(x2))-(x5^2*(abs(x4)+abs(x5)+abs(x6))^(-1))^(1/2)+err
  
  
  #discretize y into 3 classes based on quartiles
  quants<-quantile(cont_y,probs = c(0.25,0.75))
  disc_y<-ifelse(cont_y<quants[1],"Lower 25","")
  disc_y<-ifelse(cont_y>quants[2],"Upper 25",disc_y)
  disc_y<-ifelse(cont_y>=quants[1] & cont_y<=quants[2],"Middle 50",disc_y)
  
  table(disc_y)/sum(table(disc_y))*100
  
  y<-as.factor(disc_y)
  
  #Data frame
  data_use<-data.frame(y,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16,x17,x18,x19,x20)
  
  save(data_use,file=paste("True VIMP Data\\Sim",sim_number,"_Data",i,".RData",sep=""))
  
  #create RF model
  n=250;n_boots=100;n_subs=100;n_trees=250;n_splitpred=20^(1/2);termnodesize=5
  #Creating RF model
  
  rf.fit<-rfsrc(y~.,data=data_use,mtry = n_splitpred,ntree = n_trees,importance="permute",
                nodesize=termnodesize,samptype = "swr",seed=-3,block.size = 1,
                perf.type="brier")
  save(rf.fit,file=paste("True VIMP RF Models\\Sim",sim_number,"_RFmodel",i,".RData",sep=""))
  
  
  #get the full forest's VIMP
  vimp<-rf.fit$importance[,1]
  vimp_all[,i]<-vimp
}



#average VIMP values to get True VIMP
trueVIMP<-rowMeans(vimp_all,na.rm = T)

save(trueVIMP,file=paste("True VIMP\\TrueVIMP_Sim",sim_number,".RData",sep=""))

rm(list=ls())



######################################################################################################
#Classification Simulation 10
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
  
  #############################################################################
  #Sampling random values for variables and error

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
  
  
  #############################################################################
  #Creating true model
  
  #true model/simulation
  cont_y<-cos(x1-x2)+asin(x1*x3)-atan(x2-x3^2)+err
  
  #discretize y into 3 classes based on quartiles
  quants<-quantile(cont_y,probs = c(0.25,0.75))
  disc_y<-ifelse(cont_y<quants[1],"Lower 25","")
  disc_y<-ifelse(cont_y>quants[2],"Upper 25",disc_y)
  disc_y<-ifelse(cont_y>=quants[1] & cont_y<=quants[2],"Middle 50",disc_y)
  
  table(disc_y)/sum(table(disc_y))*100
  
  y<-as.factor(disc_y)
  
  #Data frame
  data_use<-data.frame(y,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16,x17,x18,x19,x20)
  
  save(data_use,file=paste("True VIMP Data\\Sim",sim_number,"_Data",i,".RData",sep=""))
  
  #create RF model
  n=250;n_boots=100;n_subs=100;n_trees=250;n_splitpred=20^(1/2);termnodesize=5
  #Creating RF model
  
  rf.fit<-rfsrc(y~.,data=data_use,mtry = n_splitpred,ntree = n_trees,importance="permute",
                nodesize=termnodesize,samptype = "swr",seed=-3,block.size = 1,
                perf.type="brier")
  save(rf.fit,file=paste("True VIMP RF Models\\Sim",sim_number,"_RFmodel",i,".RData",sep=""))
  
  
  #get the full forest's VIMP
  vimp<-rf.fit$importance[,1]
  vimp_all[,i]<-vimp
}



#average VIMP values to get True VIMP
trueVIMP<-rowMeans(vimp_all,na.rm = T)

save(trueVIMP,file=paste("True VIMP\\TrueVIMP_Sim",sim_number,".RData",sep=""))

rm(list=ls())




