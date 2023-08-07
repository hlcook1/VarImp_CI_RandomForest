#############################################################################
#Classification Simulation 4

setwd("C:/Users/hlcoo/Dropbox/USI/Research/CI for VIMP/Analysis/Simulation/Classification/Simulation 4")

#############################################################################

library(randomForestSRC)


#set simulation number for saving file names
sim_number<-as.integer(4)

  
#run the whole simulation 250 times
nruns<-250
run<-as.integer(1)
for(run in 1:nruns){
  print(paste("Working on run:",run,"Sim",sim_number))
  

  
  #set sample size value
  n<-as.integer(250)
 
  
  #############################################################################
  #Creating true model
  
  # install.packages("caret")
  library(caret)
  
  #Data frame
  # 2 factors, 15 linear, 3 nonlinear
  data_use<-twoClassSim(n=n,linearVars = 15)
  names(data_use)[ncol(data_use)]<-"y"
  
  
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
