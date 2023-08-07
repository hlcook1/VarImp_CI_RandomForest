#############################################################################
#Survival Simulation 2

setwd("C:/Users/hlcoo/Dropbox/USI/Research/CI for VIMP/Analysis/Simulation/Survival/Simulation 2")

#############################################################################

library(randomForestSRC)
# install.packages("coxed")
library(coxed)


#set simulation number for saving file names
sim_number<-as.integer(2)



  
#run the whole simulation 250 times
nruns<-250
run<-as.integer(1)

for(run in 1:nruns){
  print(paste("Working on run:",run,"Sim",sim_number))
  
  #censoring rate
  cens<-0.15
  
  #set sample size value
  n<-as.integer(250)  
  #############################################################################
  #Sampling random values for variables and error
  
  set.seed(1234+run)
  
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
  
  #save data
  save(data_use,file=paste("Data\\Sim",sim_number,"_RFmodel",run,".RData",sep=""))

  
  
  #############################################################################
  #Use the function to create RF model, calculate CIs, and save info and pictures
  source(file="SurvSim.R")
  
  SurvSim(data=data_use,sim_number=sim_number)
  
  
  #clear items from the environment to ensure independent runs
  #exceptions are the run counter and sample size, n
  rm(list=ls(all=T)[sapply(mget(ls(all=T)),class)%in%c("data.frame","list","numeric")])
  rm(data_sim)

  
  
  #################################
  #increase the run count
  run<-as.integer(run+1)
  
  
}
