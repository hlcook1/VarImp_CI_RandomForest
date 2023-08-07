#############################################################################
#Regression Simulation 3

setwd("C:/Users/hlcoo/Dropbox/USI/Research/CI for VIMP/Analysis/Simulation/Regression/Simulation 3")

#############################################################################

library(randomForestSRC)


#set simulation number for saving file names
sim_number<-as.integer(3)

  
  
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
  
  
  #############################################################################
  #Creating true model
  
  #true model/simulation
  y<-atan(abs(x2*x3-(x2*x4)^(-1))/x1)+err
  
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
