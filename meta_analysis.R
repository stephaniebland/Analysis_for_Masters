rm(list=ls())
setwd("/Users/JurassicPark/Google Drive/GIT/Analysis")
library(R.matlab)
library(matrixStats)
library(RColorBrewer)
library(codyn)
library(knitr)
library(reshape2)
library(dplyr)


N=20
meta_data=c()

for (j in 158){#1:N){
  sim_data=readMat(paste0("Complete_",j,".mat"))
  #names(sim_data)
  attach(sim_data)
  
  ################################################
  ################ RESHAPE DATA ##################
  ################################################
  B_df_days=setNames(melt(B)[,c(2, 1, 3)], c('Nodes_df', 'Day_df', 'Biomass'))
  B_df=as.data.frame(B)
  colnames(B_df)=1:nichewebsize
  tot_yrs=sum(unlist(num.years))
  tot_days=tot_yrs*L.year
  B_df=cbind(Year_df=as.integer(year.index),calen_df=rep.int(1:L.year,tot_yrs),B_df)
  B_df_years=melt(B_df, id.vars=c("Year_df","calen_df"), measure.vars =(1:nichewebsize)+2)[,c(3,1,2,4)]
  B_df=cbind(B_df_years,B_df_days[,2])[,c(1:3,5,4)]
  B_df=setNames(B_df, c('Nodes_df','Year_df', 'calen_df','Days_df','Biomass'))
  B_df[,1]=as.integer(B_df[,1])
  B_df[1:5,]
  
  ################################################
  ################ DATA SUBSETS ##################
  ################################################
  B_tot=rowSums(B)
  B_fish=rowSums(B[,isfish==1])
  B_inver=rowSums(B[,setdiff(which(isfish==0),basalsp)])
  B_plant=rowSums(B[,basalsp])
  unlist(num.years)
  phase_sub=rep(1:length(num.years),unlist(num.years)*L.year)
  year_sub=c()
  for (k in unlist(num.years)[unlist(num.years)>1]){
    year_sub=c(year_sub,rep(1:k,each=L.year))
  }
  calen_sub=rep(1:L.year,times=sum(unlist(num.years)))
  days_sub=1:length(B_tot)
  B_sub=data.frame(phase_sub,year_sub,calen_sub,days_sub,B_tot,B_fish,B_inver,B_plant)
  ## check data
  #ts.plot(year_sub[1:500])
  #B_sub[1:5,]
  #length(year_sub)
  #factor(year_sub)
  
  ################################################
  ########### tapply extract info ################
  ################################################
  yrs2stable=50
  B_sub_ends=B_sub[B_sub$year_sub>yrs2stable,]
  tapply(B_sub_ends$B_tot,B_sub_ends$phase_sub,mean)
  tapply(B_sub_ends$B_tot,B_sub_ends$phase_sub,var)
  tapply(B_sub_ends$B_fish,B_sub_ends$phase_sub,mean)
  tapply(B_sub_ends$B_fish,B_sub_ends$phase_sub,var)
  tapply(B_sub_ends$B_inver,B_sub_ends$phase_sub,mean)
  tapply(B_sub_ends$B_inver,B_sub_ends$phase_sub,var)
  tapply(B_sub_ends$B_plant,B_sub_ends$phase_sub,mean)
  tapply(B_sub_ends$B_plant,B_sub_ends$phase_sub,var)
  
  ################################################
  ################# BIND DATA ####################
  ################################################
  new_val=c(j,lstages.linked,length(surv.fish),length(surv.sp),max(TrophLevel))
  meta_data=rbind(meta_data,new_val)
  detach(sim_data)
}
meta_data=as.data.frame(meta_data)
colnames(meta_data)=c('SimNum',"Linked_Stages","Survive_Fish","Survive_Total","Max_T")

write.csv(meta_data,file = "meta_data.csv")

meta_data=read.csv("meta_data.csv")[,-1]


