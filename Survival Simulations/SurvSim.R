SurvSim <- function(data,sim_number,n=250,n_boots=100,n_subs=100,n_trees=250,n_splitpred=sqrt(10),termnodesize=5){
  
  
  #############################################################################
  #Creating RF model
  
  rf.fit<-rfsrc(Surv(y,event)~.,data=data_use,mtry = n_splitpred,ntree = n_trees,importance="permute",
                nodesize=termnodesize,samptype = "swr",seed=-3,block.size = 1)
  #for survival, may need splitrule="logrank,bs.gradient,logrankscore"

  save(rf.fit,file=paste("Models\\Sim",sim_number,"_RFmodel",run,".RData",sep=""))



  #############################################################################
  #Calculating my CI for VIMP

  #start timer
  mystart_time<-Sys.time()

  #get the full forest's VIMP
  vimp<-rf.fit$importance

  #get the VIMP per tree
  rftree_info<-list()
  rftree_vimp<-as.data.frame(matrix(NA,nrow=length(vimp),ncol=n_trees))
  rownames(rftree_vimp)<-names(vimp)
  colnames(rftree_vimp)<-paste("ntree",1:n_trees,sep="")
  for(i in 1:n_trees){
    rftree<-predict(rf.fit,importance = "permute",block.size = 1,
                    get.tree = i)
    rftree_info[[i]]<-rftree
    rftree_vimp[,i]<-rftree$importance
  }

  #Take bootstrap samples then calculate the mean VIMP per bootstrap sample
  nsamp<-n_boots
  vimp_boots<-list()
  means_samps<-matrix(NA,nrow=nrow(rftree_vimp),ncol=nsamp)
  rownames(means_samps)<-rownames(rftree_vimp)
  colnames(means_samps)<-paste("means_nsamp",1:nsamp,sep="")

  for(i in 1:nsamp){
    set.seed(65432+i+run)
    trees_samp<-sample(1:n_trees,nsamp,replace = T)
    vimp_boots[[i]]<-rftree_vimp[,trees_samp]
    means_samps[,i]<-apply(vimp_boots[[i]],1,mean)
  }

  #Calculate the quantiles which give the CI bounds
  CIs<-as.data.frame(t(apply(means_samps,1,quantile,probs = c(0.025, 0.5,0.975))))
  CIs$Middle<-vimp

  #end timer
  myend_time<-Sys.time()

  #calculate time difference
  mytime<-myend_time-mystart_time

  save(CIs,file=paste("MyCIs\\Sim",sim_number,"_MyCIs_RFmodel",run,".RData",sep=""))



  #make plot with bootstrap CI around the means
  #extract means and bounds so they can be plotted
  meds_mean<-CIs$Middle
  names(meds_mean)<-rownames(CIs)
  meds_mean2<-meds_mean[order(meds_mean)]

  lows95<-CIs$`2.5%`
  names(lows95)<-rownames(CIs)
  lows95_2<-lows95[order(meds_mean)]

  highs95<-CIs$`97.5%`
  names(highs95)<-rownames(CIs)
  highs95_2<-highs95[order(meds_mean)]

  png(paste("MyCIsPlots\\Sim",sim_number,"_MyCIs_RFmodel",run,".png",sep=""),width=750,height=1000)
  par(cex=2)
  barCenters <- barplot(height = meds_mean2,
                        # names.arg = labs104[names(meds_mean2)],
                        beside = true, las = 1,
                        xlim = c(min(lows95_2), max(highs95_2)),
                        #cex.names = 0.75, #xaxt = "n",
                        main = "Bootstrapped 95% VIMP CI",
                        xlab = "Mean Decrease in MSE VIMP",
                        border = "black", axes = TRUE,
                        horiz = T)
  #text(x = barCenters, y = par("usr")[3] - 1, srt = 45,adj = 1, labels = names(meds2), xpd = TRUE)
  segments(lows95_2,barCenters, highs95_2,barCenters,  lwd = 1.5)
  arrows(lows95_2,barCenters, highs95_2,barCenters, lwd = 1.5, angle = 90, code = 3, length = 0.05)
  dev.off()

  #order the variables where the most important variable will be last
  CIs2<-CIs[order(CIs$Middle,decreasing = T),]
  CIs2<-CIs2[,-4]
  CIs2_2<-as.list(as.data.frame(t(CIs2)))

  #make colors for plot where red is significant (CI does not cover 0)
  cols<-ifelse(CIs2$`2.5%`>0,"red","blue")




  #############################################################################
  #Calculating Ishwaran and Lu's CI for VIMP

  ###########################
  #Subsampling and Delete-d Jackknife

  #start timer
  substart_time<-Sys.time()

  #does calculations for both subsampling and delete-d
  rf_samp<-subsample(rf.fit,B=n_subs)

  #end timer
  subend_time<-Sys.time()

  #time difference
  subtime<-subend_time-substart_time


  ###########################
  #Double Bootstrapping

  #start timer
  dboot_start_time<-Sys.time()

  #calculations for double bootstrap method
  rf_samp2<-subsample(rf.fit,B=n_boots,bootstrap = T)

  #end timer
  dboot_end_time<-Sys.time()

  #time difference
  dboot_time<-dboot_end_time-dboot_start_time

  #save all times data
  times<-c(MyTime=mytime,SubDeleteDTime=subtime,DoubleBootTime=dboot_time)
  save(times,file=paste("Times\\Sim",sim_number,"_Times_RFmodel",run,".RData",sep=""))


  #save Ishwaran and Lu's CI results
  theirCIs<-list(rf_samp,rf_samp2)
  save(theirCIs,file=paste("TheirCIs\\Sim",sim_number,"_TheirCIs_RFmodel",run,".RData",sep=""))





  ###########################
  #Plot of all 4 together

  png(paste("Plots\\Sim",sim_number,"_AllCIs_RFmodel",run,".png",sep=""),width=2000,height=750)
  par(mfrow=c(1,4),cex=2)
  plot.subsample(rf_samp,main="Subsampling \n95% VIMP CI",alpha=0.05,standardize = F,normal=T,jknife = F)
  plot.subsample(rf_samp,main="Delete-d Jackknife \n95% VIMP CI",alpha=0.05,standardize = F,normal=T,jknife = T)
  plot.subsample(rf_samp2,main="Double Bootstrap \n95% VIMP CI",alpha=0.05,standardize = F,normal=T,jknife = F)

  boxplot(CIs2_2, horizontal = T, xlab="VIMP",main="Our Bootstrap \n95% VIMP CI",
          col=NULL,las=1,border=NA)
  abline(h=c(1:20),col="lightgray")
  abline(v=0,col="darkgray")
  boxplot(CIs2_2, horizontal = T, xlab="VIMP",main="Our Bootstrap \n95% VIMP CI",
          col=cols,las=1,add=T)
  dev.off()


  
}
