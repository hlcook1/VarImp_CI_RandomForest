setwd("C:/Users/hlcoo/Dropbox/USI/Research/CI for VIMP/Analysis/Simulation/Classification")


cov_prob_my<-matrix(NA,nrow=20,ncol=10)
cov_prob_jack<-matrix(NA,nrow=20,ncol=10)
cov_prob_sub<-matrix(NA,nrow=20,ncol=10)
cov_prob_db<-matrix(NA,nrow=20,ncol=10)

#Now that the true VIMP is calculated, find the coverage probabilities
# Steps:
# Compute the proportion of samples for which the (known) population parameter is contained in 
# the confidence interval. That proportion is an estimate for the empirical coverage 
# probability for the CI. 
# (https://blogs.sas.com/content/iml/2016/09/08/coverage-probability-confidence-intervals.html )

######################################################################################################
#Regression Simulations
#set simulation number for saving file names
for(sim_number in 1:10){
  
  #load in the true VIMPs
  load(file=paste("True VIMP\\","TrueVIMP_Sim",sim_number,".RData",sep=""))
  myVIMP_ind<-matrix(NA,nrow=20,ncol=250)
  jackVIMP_ind<-matrix(NA,nrow=20,ncol=250)
  subVIMP_ind<-matrix(NA,nrow=20,ncol=250)
  dbVIMP_ind<-matrix(NA,nrow=20,ncol=250)
  
  #create empty lists
  myCIs_list<-list()
  jackCIs_list<-list()
  subCIs_list<-list()
  dbCIs_list<-list()  
  
  #calculations have been previously saved, so load in those
  load(file=paste("Sim",sim_number,"_MyCIs_list.RData",sep=""))
  load(file=paste("Sim",sim_number,"_JackknifeCIs_list.RData",sep=""))
  load(file=paste("Sim",sim_number,"_SubsamplingCIs_list.RData",sep=""))
  load(file=paste("Sim",sim_number,"_DoubleBootCIs_list.RData",sep=""))
  
  # For each RF model/Data set, need to find the respective CIs per variable
  for(run in 1:250){
    
    # #our CI
    # load(file=paste("Simulation ",sim_number,"\\MyCIs\\","Sim",sim_number,"_MyCIs_RFmodel",run,".RData",sep=""))
    # CIs$TrueVIMP<-trueVIMP
    # myCIs_list[[run]]<-CIs
    # 
    # #their CI
    # load(file=paste("Simulation ",sim_number,"\\TheirCIs\\","Sim",sim_number,"_TheirCIs_RFmodel",run,".RData",sep=""))
    # 
    # 
    # all_output<-capture.output(print(theirCIs[[1]],standardize=F))
    # 
    # #jackknife
    # jackCIs<-all_output[c(17:18,22)]
    # jackCIs<-paste(jackCIs,collapse = "\n")
    # jackdf<-readr::read_fwf(jackCIs, readr::fwf_empty(jackCIs), )
    # jackdf<-as.data.frame(t(jackdf[,-1]))
    # names(jackdf)<-c("Variable","LB","UB")
    # rownames(jackdf)<-jackdf$Variable
    # jackdf<-jackdf[,-1]
    # 
    # jackdf$TrueVIMP<-trueVIMP
    # 
    # jackCIs_list[[run]]<-jackdf
    # 
    # #subsampling
    # subCIs<-all_output[c(10:11,15)]
    # subCIs<-paste(subCIs,collapse = "\n")
    # subdf<-readr::read_fwf(subCIs, readr::fwf_empty(subCIs), )
    # subdf<-as.data.frame(t(subdf[,-1]))
    # names(subdf)<-c("Variable","LB","UB")
    # rownames(subdf)<-subdf$Variable
    # subdf<-subdf[,-1]
    # 
    # subdf$TrueVIMP<-trueVIMP
    # 
    # subCIs_list[[run]]<-subdf
    # 
    # #Double bootstrap
    # all_output<-capture.output(print(theirCIs[[2]],standardize=F))
    # CIs<-all_output[c(10:11,15)]
    # CIs<-paste(CIs,collapse = "\n")
    # df<-readr::read_fwf(CIs, readr::fwf_empty(CIs), )
    # df<-as.data.frame(t(df[,-1]))
    # names(df)<-c("Variable","LB","UB")
    # rownames(df)<-df$Variable
    # df<-df[,-1]
    # 
    # df$TrueVIMP<-trueVIMP
    # 
    # dbCIs_list[[run]]<-df
    
    
    
    ###############################################################
    # For each CI/variable, indicate if the true VIMP is included.
    myVIMP_ind[,run]<-ifelse(myCIs_list[[run]]$TrueVIMP >= myCIs_list[[run]]$`2.5%` & 
                               myCIs_list[[run]]$TrueVIMP <= myCIs_list[[run]]$`97.5%`,"Covered","Not Covered")
    
    jackVIMP_ind[,run]<-ifelse(jackCIs_list[[run]]$TrueVIMP >= as.numeric(jackCIs_list[[run]]$LB) & 
                                 jackCIs_list[[run]]$TrueVIMP <= as.numeric(jackCIs_list[[run]]$UB),"Covered","Not Covered")
    
    subVIMP_ind[,run]<-ifelse(subCIs_list[[run]]$TrueVIMP >= as.numeric(subCIs_list[[run]]$LB) & 
                                subCIs_list[[run]]$TrueVIMP <= as.numeric(subCIs_list[[run]]$UB),"Covered","Not Covered")
    
    dbVIMP_ind[,run]<-ifelse(dbCIs_list[[run]]$TrueVIMP >= as.numeric(dbCIs_list[[run]]$LB) & 
                               dbCIs_list[[run]]$TrueVIMP <= as.numeric(dbCIs_list[[run]]$UB),"Covered","Not Covered")
    
    print(paste("Working on Run",run,"Sim",sim_number,sep=" "))
    
  }
  
  # save(myCIs_list,file=paste("Sim",sim_number,"_MyCIs_list.RData",sep=""))
  # save(jackCIs_list,file=paste("Sim",sim_number,"_JackknifeCIs_list.RData",sep=""))
  # save(subCIs_list,file=paste("Sim",sim_number,"_SubsamplingCIs_list.RData",sep=""))
  # save(dbCIs_list,file=paste("Sim",sim_number,"_DoubleBootCIs_list.RData",sep=""))
  
  ###############################################################
  # Calculate the percentage of times the true VIMP included.
  
  #my CI
  cov_prob<-NULL
  for(i in 1:nrow(myVIMP_ind)){
    tab_perc<-table(myVIMP_ind[i,])/ncol(myVIMP_ind)*100
    cov_prob[i]<-ifelse(is.na(tab_perc["Covered"])==T,0,tab_perc["Covered"])
  }
  cov_prob_my[,sim_number]<-cov_prob
  
  #jackknife
  cov_prob<-NULL
  for(i in 1:nrow(jackVIMP_ind)){
    tab_perc<-table(jackVIMP_ind[i,])/ncol(jackVIMP_ind)*100
    cov_prob[i]<-ifelse(is.na(tab_perc["Covered"])==T,0,tab_perc["Covered"])
  }
  cov_prob_jack[,sim_number]<-cov_prob
  
  #subsampling
  cov_prob<-NULL
  for(i in 1:nrow(subVIMP_ind)){
    tab_perc<-table(subVIMP_ind[i,])/ncol(subVIMP_ind)*100
    cov_prob[i]<-ifelse(is.na(tab_perc["Covered"])==T,0,tab_perc["Covered"])
  }
  cov_prob_sub[,sim_number]<-cov_prob
  
  #double boot
  cov_prob<-NULL
  for(i in 1:nrow(dbVIMP_ind)){
    tab_perc<-table(dbVIMP_ind[i,])/ncol(dbVIMP_ind)*100
    cov_prob[i]<-ifelse(is.na(tab_perc["Covered"])==T,0,tab_perc["Covered"])
  }
  cov_prob_db[,sim_number]<-cov_prob
  
  
}

#save results in CSV files
colnames(cov_prob_my)<-paste("Sim",1:10,sep="")
rownames(cov_prob_my)<-paste("X",1:20,sep="")
write.csv(cov_prob_my,file="MyCoverage.csv")

colnames(cov_prob_jack)<-paste("Sim",1:10,sep="")
rownames(cov_prob_jack)<-paste("X",1:20,sep="")
write.csv(cov_prob_jack,file="JackknifeCoverage.csv")

colnames(cov_prob_sub)<-paste("Sim",1:10,sep="")
rownames(cov_prob_sub)<-paste("X",1:20,sep="")
write.csv(cov_prob_sub,file="SubsamplingCoverage.csv")

colnames(cov_prob_db)<-paste("Sim",1:10,sep="")
rownames(cov_prob_db)<-paste("X",1:20,sep="")
write.csv(cov_prob_db,file="DoubleBootCoverage.csv")




#######################################################################################
#Changing data to make boxplots

#create a dataframe of the true VIMP
trueVimp_all<-matrix(NA,nrow=20*10,ncol=3)
trueVimp_all[,2]<-paste("Sim",rep(1:10,each=20),sep="")
trueVimp_all[,3]<-paste("X",1:20,sep="")
j<-0
for(i in 1:10){
  load(file=paste("True VIMP\\TrueVIMP_Sim",i,".RData",sep=""))
  trueVimp_all[c(1:20)+j,1]<-trueVIMP
  j<-j+20
}
colnames(trueVimp_all)<-c("TrueVIMP","Sim","X")
trueVimp_all<-as.data.frame(trueVimp_all)
trueVimp_all$TrueVIMP<-as.numeric(trueVimp_all$TrueVIMP)

#find the quantiles for the true VIMP
trueVIMP_quant<-quantile(trueVimp_all[,1],probs=c(0.1, 0.25, 0.5, 0.75, 0.9))

#label which true VIMP is in which quantile
trueVimp_all$quant<-ifelse(trueVimp_all$TrueVIMP<trueVIMP_quant[1],"0%-10%","")
trueVimp_all$quant<-ifelse(trueVimp_all$TrueVIMP >= trueVIMP_quant[1] & trueVimp_all$TrueVIMP < trueVIMP_quant[2],"10%-25%",trueVimp_all$quant)
trueVimp_all$quant<-ifelse(trueVimp_all$TrueVIMP >= trueVIMP_quant[2] & trueVimp_all$TrueVIMP < trueVIMP_quant[3],"25%-50%",trueVimp_all$quant)
trueVimp_all$quant<-ifelse(trueVimp_all$TrueVIMP >= trueVIMP_quant[3] & trueVimp_all$TrueVIMP < trueVIMP_quant[4],"50%-75%",trueVimp_all$quant)
trueVimp_all$quant<-ifelse(trueVimp_all$TrueVIMP >= trueVIMP_quant[4] & trueVimp_all$TrueVIMP < trueVIMP_quant[5],"75%-90%",trueVimp_all$quant)
trueVimp_all$quant<-ifelse(trueVimp_all$TrueVIMP >= trueVIMP_quant[5],"90%-100%",trueVimp_all$quant)

#add the coverage into the dataframe
trueVimp_all$MyCovProb<-NA
j<-0
for(i in 1:10){
  cov_prob<-cov_prob_my[,i]
  trueVimp_all[c(1:20)+j,"MyCovProb"]<-cov_prob
  j<-j+20
}

trueVimp_all$JackCovProb<-NA
j<-0
for(i in 1:10){
  cov_prob<-cov_prob_jack[,i]
  trueVimp_all[c(1:20)+j,"JackCovProb"]<-cov_prob
  j<-j+20
}

trueVimp_all$SubCovProb<-NA
j<-0
for(i in 1:10){
  cov_prob<-cov_prob_sub[,i]
  trueVimp_all[c(1:20)+j,"SubCovProb"]<-cov_prob
  j<-j+20
}

trueVimp_all$DBCovProb<-NA
j<-0
for(i in 1:10){
  cov_prob<-cov_prob_db[,i]
  trueVimp_all[c(1:20)+j,"DBCovProb"]<-cov_prob
  j<-j+20
}

str(trueVimp_all)

save(trueVimp_all,file="TrueVIMP_all.RData")

#make individual boxplots
png(file="CovProb_MyCI.png",width=750,height=500)
boxplot(MyCovProb~quant,data=trueVimp_all,ylim=c(0,100),
        main="Percent Coverage for Our 95% Bootstrap VIMP CI",
        ylab="Coverage",xlab="VIMP Quantiles")
abline(h=95,lty=2)
dev.off()

png(file="CovProb_JackCI.png",width=750,height=500)
boxplot(JackCovProb~quant,data=trueVimp_all,ylim=c(0,100),
        main="Percent Coverage for 95% Jackknife VIMP CI",
        ylab="Coverage",xlab="VIMP Quantiles")
abline(h=95,lty=2)
dev.off()

png(file="CovProb_SubCI.png",width=750,height=500)
boxplot(SubCovProb~quant,data=trueVimp_all,ylim=c(0,100),
        main="Percent Coverage for 95% Subsampling VIMP CI",
        ylab="Coverage",xlab="VIMP Quantiles")
abline(h=95,lty=2)
dev.off()

png(file="CovProb_DoubleBootCI.png",width=750,height=500)
boxplot(DBCovProb~quant,data=trueVimp_all,ylim=c(0,100),
        main="Percent Coverage for 95% Double Bootstrapping VIMP CI",
        ylab="Coverage",xlab="VIMP Quantiles")
abline(h=95,lty=2)
dev.off()







#changing data to make boxplot including all methods together

names(trueVimp_all)
trueVimp_long<-rep(trueVimp_all[,"TrueVIMP"],4)
trueVimp_long<-as.data.frame(trueVimp_long)
trueVimp_long$Sim<-rep(trueVimp_all[,"Sim"],4)
trueVimp_long$X<-rep(trueVimp_all[,"X"],4)
trueVimp_long$Quantile<-rep(trueVimp_all[,"quant"],4)
trueVimp_long$Coverage<-c(trueVimp_all$MyCovProb,trueVimp_all$JackCovProb,trueVimp_all$SubCovProb,trueVimp_all$DBCovProb)
trueVimp_long$Method<-rep(c("Our Bootstrap","Jackknife","Subsampling","Double Bootstrap"),each=nrow(trueVimp_all))

myCI_long<-trueVimp_all[,c(1:5)]
myCI_long$quant<-"All"
myCI_long$Method<-"Our Bootstrap"
names(myCI_long)<-c("trueVimp_long","Sim","X","Quantile","Coverage","Method")

trueVimp_long2<-rbind(trueVimp_long,myCI_long)

names(trueVimp_all)
jackCI_long<-trueVimp_all[,c(1:4,6)]
jackCI_long$quant<-"All"
jackCI_long$Method<-"Jackknife"
names(jackCI_long)<-c("trueVimp_long","Sim","X","Quantile","Coverage","Method")

trueVimp_long3<-rbind(trueVimp_long2,jackCI_long)

names(trueVimp_all)
subCI_long<-trueVimp_all[,c(1:4,7)]
subCI_long$quant<-"All"
subCI_long$Method<-"Subsampling"
names(subCI_long)<-c("trueVimp_long","Sim","X","Quantile","Coverage","Method")

trueVimp_long4<-rbind(trueVimp_long3,subCI_long)


names(trueVimp_all)
dbCI_long<-trueVimp_all[,c(1:4,8)]
dbCI_long$quant<-"All"
dbCI_long$Method<-"Double Bootstrap"
names(dbCI_long)<-c("trueVimp_long","Sim","X","Quantile","Coverage","Method")

trueVimp_long5<-rbind(trueVimp_long4,dbCI_long)

save(trueVimp_long5,file="TrueVIMP_long.RData")




install.packages("svglite")
library(svglite)
svglite("CovProb_all.svg", width = 10, height = 6)

boxplot(Coverage~Method*Quantile,data=trueVimp_long5,ylim=c(0,100),
        main="Percent Coverage for 95% VIMP CI\nClassification",
        ylab="Coverage",xlab="True VIMP Quantiles",
        col=c("blue","green","purple","red"),
        las=2,pch=16,lty=1,xaxt="n")#,cex.main=2,cex.lab=1.5
abline(h=95,lty=2)
axis(1,at=seq(2,28,4)+0.5,labels=names(table(trueVimp_long5$Quantile)))
legend("bottomleft",legend=c("Double Bootstrap","Jackknife","Our Bootstrap","Subsampling"),
       pch=15,col=c("blue","green","purple","red"),bty="n")#cex=1.5,
legend(5,22,legend=c(paste("mean=",round(mean(cov_prob_db),0),sep=""),
                     paste("mean=",round(mean(cov_prob_jack),0),sep=""),
                     paste("mean=",round(mean(cov_prob_my),0),sep=""),
                     paste("mean=",round(mean(cov_prob_sub),0),sep="")),
       bty="n")#cex=1.5,
dev.off()


