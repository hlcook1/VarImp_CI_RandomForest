#############################################################################
#Regression Simulation 1

setwd("C:/Users/hlcoo/Dropbox/USI/Research/CI for VIMP/Analysis/Simulation/Classification/Simulation 1")

#############################################################################

library(randomForestSRC)


#set simulation number for saving file names
sim_number<-as.integer(1)

  
#run the whole simulation 250 times
nruns<-250
run<-as.integer(1)
for(run in 1:nruns){
  print(paste("Working on run:",run,"Sim",sim_number))
  

  
  #set sample size value
  n<-as.integer(250)
 
  
  #############################################################################
  #Creating true model
  
  # install.packages("mlbench")
  library(mlbench)
  
  #threenorm sim with 20 variables
  data<-mlbench.threenorm(n=n,d=20)
  x<-as.data.frame(data$x)
  names(x)<-paste("X",1:ncol(x),sep="")
  y<-data$classes
  
  data_use<-data.frame(x,y)  
  
  save(data_use,file=paste("Data\\Sim",sim_number,"_RFmodel",run,".RData",sep=""))
  
  
  #############################################################################
  #Use the function to create RF model, calculate CIs, and save info and pictures
  source(file="ClassSim.R")
  
  ClassSim(data=data_use,sim_number=sim_number)
  
  
  #clear items from the environment to ensure independent runs
  #exceptions are the run counter and sample size, n
  rm(list=ls(all=T)[sapply(mget(ls(all=T)),class)%in%c("data.frame","numeric")])
  
  
  
  #################################
  #increase the run count
  run<-as.integer(run+1)
  
  
  
  #################################
  #increase the run count
  run<-as.integer(run+1)
  
}
