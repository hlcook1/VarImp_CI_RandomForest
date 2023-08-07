
setwd("C:/Users/hlcoo/Dropbox/USI/Research/CI for VIMP/Analysis/Simulation/Classification")

# install.packages("car")
library(car)


mean_times<-matrix(NA,ncol=3,nrow=10)
tukey_pvals<-matrix(NA,ncol=3,nrow=10)

anova_test<-rep(NA,10)
welch_pvals<-rep(NA,10)
kruskal_pvals<-rep(NA,10)

tukey<-list()
anova_res<-list()
welch_res<-list()
kruskal_res<-list()
bh<-list()



for(sim_number in 1:10){
  # sim_number<-1
  
  times_mat<-matrix(NA,nrow=250,ncol=3)
  colnames(times_mat)<-c("Our Bootstrap","Subsampling-Jackknife","Double Bootstrap")
  for(run in 1:250){
    load(file=paste("Simulation ",sim_number,"\\Times\\Sim",sim_number,"_Times_RFmodel",run,".RData",sep=""))
    times_mat[run,]<-times
  }
  
  mean_times[sim_number,]<-colMeans(times_mat)
  
  times_mat<-as.data.frame(times_mat)
  
  write.csv(times_mat,file=paste("Times_Sim",sim_number,".csv"),row.names = F)
  
  #boxplot
  png(file=paste("TimesBoxplot_Sim",sim_number,".png",sep=""),width=500,height=750)
  par(mfrow=c(1,1))
  boxplot(times_mat)
  dev.off()
  
  
  
  #check equal variances

  times_long<-stack(times_mat)
  names(times_long)
  var_test<-leveneTest(values ~ ind,data=times_long)
  var_test$`Pr(>F)`
  
  
  anova_test[sim_number] <- ifelse(var_test$`Pr(>F)`[1] < 0.05,"Welch-unequal var","Original-equal var")
  
  #Welch, if variances not equal
  res_aov<-oneway.test(values ~ ind, var.equal = FALSE,data=times_long)
  welch_res[[sim_number]]<-res_aov
  welch_pvals[sim_number]<-res_aov$p.value
  
  #if variances equal
  res_aov<-aov(values ~ ind,data = times_long)
  anova_res[[sim_number]]<-summary(res_aov)
  tukey[[sim_number]]<-TukeyHSD(res_aov)
  tukey_pvals[sim_number,]<-tukey[[1]]$ind[,4]
  
  #Try nonparametric
  kw_res<-kruskal.test(values ~ ind, data = times_long)
  kruskal_res[[sim_number]]<-kw_res
  kruskal_pvals[sim_number]<-kw_res$p.value
  bh[[sim_number]]<-pairwise.wilcox.test(times_long$values, times_long$ind,
                       p.adjust.method = "BH")
  
  
}

###########################
#welch vs regular test
anova_test
#all tests should use welch

###########################
#welch test
welch_res
welch_pvals
#all tests show a sig diff

###########################
#regular test
anova_res
#confirms above

#test which methods are diff
tukey
tukey_pvals
#methods mean times are all sig diff


###########################
#Nonparametric test
round(kruskal_pvals,3)
#all tests show sig diff

#multiple comparisons
bh
#methods mean times are all sig diff


colMeans(mean_times)

mean_times[1,1]
t.test(mean_times[,1],mean_times[,2])



#Check normality
#Normality can be assumed due to large sample size

# png(file=paste("TimesNormPlot_Sim",sim_number,".png"),width=1500,height=500)
# par(mfrow=c(1,2),cex=1.5)
# hist(res_aov$residuals,main="ANOVA Residuals",xlab="Residuals")
# 
# qqnorm(res_aov$residuals,main="ANOVA Residuals")
# qqline(res_aov$residuals)
# my_test<-shapiro.test(res_aov$residuals)
# legend("topleft",legend=paste("Shapiro p-value:",round(my_test$p.value,3)),bty="n")
# dev.off()

