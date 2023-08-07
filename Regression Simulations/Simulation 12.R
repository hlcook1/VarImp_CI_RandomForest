#############################################################################
#Regression Simulation 12

setwd("C:/Users/hlcoo/Dropbox/USI/Research/CI for VIMP/Analysis/Simulation/Regression/Simulation 12")

#############################################################################

library(randomForestSRC)

#set simulation number for saving file names
sim_number<-as.integer(12)


#run the whole simulation 250 times
nruns<-250
run<-as.integer(1)
for(run in 1:nruns){
  print(paste("Working on run:",run,"Sim",sim_number))
  
  
  #set sample size value
  n<-as.integer(250)  
  #############################################################################
  #Sampling random values for variables and error
  
  set.seed(1234+run)
  
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
  
  
  #############################################################################
  #Creating true model
  
  #true model/simulation
  y<-err
  
  #Data frame
  data_use<-data.frame(y,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16,x17,x18,x19,x20)
  
  save(data_use,file=paste("Data\\Sim",sim_number,"_RFmodel",run,".RData",sep=""))
  
  
  #############################################################################
  #Use the function to create RF model, calculate CIs, and save info and pictures
  source(file="RegSim.R")
  
  RegSim(data=data_use,sim_number=sim_number)
  
  
  #clear items from the environment to ensure independent runs
  #exceptions are the run counter and sample size, n
  rm(list=ls(all=T)[sapply(mget(ls(all=T)),class)%in%c("data.frame","list","numeric")])

  
  
  #################################
  #increase the run count
  run<-as.integer(run+1)
  
  
}
