
setwd("C:/Users/hlcoo/Dropbox/USI/Research/CI for VIMP/Analysis/Examples/Regression")

# install.packages("randomForest")
library(randomForest)
library(randomForestSRC)

##################################################################################
#data set


data_use<- read.table("auto-mpg.data", quote="\"", comment.char="")
#n=398
str(data_use)
names(data_use)<-c("mpg","Cylinders","Displacement","Horsepower","Weight","Acceleration","ModelYear","Origin","CarName")
str(data_use)

data_use[data_use$Horsepower=="?",c("ModelYear","CarName")]

data_use<-data_use[data_use$Horsepower!="?",]
#n=392
data_use$Horsepower<-as.numeric(data_use$Horsepower)
str(data_use)

data_use<-data_use[,-c(ncol(data_use))]

data_use$Origin<-as.factor(data_use$Origin)

dim(data_use)

############################################################################################################
############################################################################################################
#train a RF first then create a model with those values


#number of trees to use is between 500 to 5000 by 500 
ntrees<-seq(0,5000,by=500)
ntrees<-ntrees[-1]
m_try<-rep(0,length(ntrees))
oob_tree<-rep(0,length(ntrees))

#loop to find best mtry per level of ntree based on out of bag (oob) error
for(i in 1:length(ntrees)){
  res<-tuneRF(data_use[,-c(1)],data_use$mpg,ntreeTry = ntrees[i],stepFactor = 2,plot = F,trace = F)
  res<-data.frame(res)
  m_try[i]<-res[which.min(res$OOBError),1]
  oob_tree[i]<-res[which.min(res$OOBError),2]
}

#make data frame of tree size, mtry, and error
tune_res<-data.frame(ntrees,m_try,oob_tree)
#choose min error row
min_oob<-tune_res[which.min(tune_res$oob_tree),]

#number of trees to use
ntrees_use<-min_oob$ntrees
#number of mtry to use
mtry_use<-min_oob$m_try



rf.fit<-rfsrc(mpg~.,data=data_use,mtry = mtry_use,ntree = ntrees_use,importance="permute",
              samptype = "swr",seed=-3,block.size = 1)

save(rf.fit,file="RFtrained.RData")


############################################################################################################
############################################################################################################
#Calculate VIMP CI

n_trees<-ntrees_use
n_boots<-100
n_subs<-100

#Calculating my CI for VIMP

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
  set.seed(65432+i)
  trees_samp<-sample(1:n_trees,nsamp,replace = T)
  vimp_boots[[i]]<-rftree_vimp[,trees_samp]
  means_samps[,i]<-apply(vimp_boots[[i]],1,mean)
}

#Calculate the quantiles which give the CI bounds
CIs<-as.data.frame(t(apply(means_samps,1,quantile,probs = c(0.025, 0.5,0.975))))
CIs$Middle<-vimp


save(CIs,file="MyCIs.RData")


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

#does calculations for both subsampling and delete-d
rf_samp<-subsample(rf.fit,B=n_subs)


###########################
#Double Bootstrapping

#calculations for double bootstrap method
rf_samp2<-subsample(rf.fit,B=n_boots,bootstrap = T)

#save Ishwaran and Lu's CI results
theirCIs<-list(rf_samp,rf_samp2)
save(theirCIs,file="TheirCIs.RData")





###########################
#Plot of all 4 together

png("AllCIs.png",width=2500,height=750)
par(mfrow=c(1,4),cex=2,mai=c(2,3,1,1))
plot.subsample(rf_samp,main="Subsampling \n95% VIMP CI",alpha=0.05,standardize = F,normal=T,jknife = F, xlab="VIMP")
plot.subsample(rf_samp,main="Delete-d Jackknife \n95% VIMP CI",alpha=0.05,standardize = F,normal=T,jknife = T, xlab="VIMP")
plot.subsample(rf_samp2,main="Double Bootstrap \n95% VIMP CI",alpha=0.05,standardize = F,normal=T,jknife = F, xlab="VIMP")

boxplot(CIs2_2, horizontal = T, xlab="VIMP",main="Our Bootstrap \n95% VIMP CI",
        col=NULL,las=1,border=NA)
abline(h=c(1:20),col="lightgray")
abline(v=0,col="darkgray")
boxplot(CIs2_2, horizontal = T, xlab="VIMP",main="Our Bootstrap \n95% VIMP CI",
        col=cols,las=1,add=T)
dev.off()







load(file="theirCIs.RData")
rf_samp<-theirCIs[[1]]
rf_samp2<-theirCIs[[2]]

load(file="MyCIs.RData")
#order the variables where the most important variable will be last
CIs2<-CIs[order(CIs$Middle,decreasing = T),]
CIs2<-CIs2[,-4]
CIs2_2<-as.list(as.data.frame(t(CIs2)))

#make colors for plot where red is significant (CI does not cover 0)
cols<-ifelse(CIs2$`2.5%`>0,"red","blue")


###########################
#Plot of all 4 together
# install.packages("svglite")
library(svglite)
svglite("AllCIs.svg", width = 12, height = 4)

# png("AllCIs.png",width=2500,height=750)
par(mfrow=c(1,4),mar=c(5,6,4,2)+0.1)#,cex=2,mai=c(1,1,1,1)
plot.subsample(rf_samp,main="Subsampling \n95% VIMP CI",alpha=0.05,standardize = F,normal=T,jknife = F, xlab="VIMP")
plot.subsample(rf_samp,main="Delete-d Jackknife \n95% VIMP CI",alpha=0.05,standardize = F,normal=T,jknife = T, xlab="VIMP")
plot.subsample(rf_samp2,main="Double Bootstrap \n95% VIMP CI",alpha=0.05,standardize = F,normal=T,jknife = F, xlab="VIMP")

boxplot(CIs2_2, horizontal = T, xlab="VIMP",main="Our Bootstrap \n95% VIMP CI",
        col=NULL,las=1,border=NA)
abline(h=c(1:20),col="lightgray")
abline(v=0,col="darkgray")
boxplot(CIs2_2, horizontal = T, xlab="VIMP",main="Our Bootstrap \n95% VIMP CI",
        col=cols,las=1,add=T)
dev.off()
