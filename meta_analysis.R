rm(list=ls())
setwd("/Users/JurassicPark/Google Drive/GIT/Analysis")
library(R.matlab)
library(matrixStats)
library(RColorBrewer)
library(codyn)
library(knitr)
library(reshape2)
library(dplyr)

exp_type_all=c("complete","unlinked","extended_unlinked")
exp_type=exp_type_all[1]
N=20
meta_data=c()

for (j in 158){#1:N){
  sim_data=readMat(paste0(exp_type,"_",j,".mat"))
  #names(sim_data)
  attach(sim_data)
  
  ################################################
  ################ RESHAPE DATA ##################
  ################################################
  #I THINK THIS PART IS BROKEN IF YEARS CHANGE FROM PHASE TO PHASE
  
  #First step:  Find the biomass by day (days_df type, so complete) and node:
  B_df_days=setNames(melt(B)[,c(2, 1, 3)], c('Nodes_df', 'Day_df', 'Biomass'))# Melt the data frame of B=b_ij into [j, i, biomass]=[nodes,day, biomass]
  
  #Second step:  
  B_df=as.data.frame(B)# Convert biomass B to data frame
  colnames(B_df)=1:nichewebsize# Rename columns to those of the nodes
  
  
  
  tot_yrs=sum(unlist(num.years)) # Find total number of years
  tot_days=tot_yrs*L.year
  B_df=cbind(Year_df=as.integer(year.index),calen_df=rep.int(1:L.year,tot_yrs),B_df)
  B_df_years=melt(B_df, id.vars=c("Year_df","calen_df"), measure.vars =(1:nichewebsize)+2)[,c(3,1,2,4)]
  
  #Compile data, reorder, and rename:
  B_df=cbind(B_df_years,B_df_days[,2])[,c(1:3,5,4)]
  B_df=setNames(B_df, c('Nodes_df','Year_df', 'calen_df','Days_df','Biomass'))
  #B_df[,1]=as.integer(B_df[,1])#get rid of levels
  B_df[1:5,]#Double check
  
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
  thingy=c(tapply(B_sub_ends$B_tot,B_sub_ends$phase_sub,mean),
  tapply(B_sub_ends$B_tot,B_sub_ends$phase_sub,var),
  tapply(B_sub_ends$B_fish,B_sub_ends$phase_sub,mean),
  tapply(B_sub_ends$B_fish,B_sub_ends$phase_sub,var),
  tapply(B_sub_ends$B_inver,B_sub_ends$phase_sub,mean),
  tapply(B_sub_ends$B_inver,B_sub_ends$phase_sub,var),
  tapply(B_sub_ends$B_plant,B_sub_ends$phase_sub,mean),
  tapply(B_sub_ends$B_plant,B_sub_ends$phase_sub,var))
  
  #not quite there yet:
  #tapply(B_sub_ends$B_plant,list(B_sub_ends$phase_sub,B_sub_ends$species),var)
  
  
  ################################################
  ################# BIND DATA ####################
  ################################################
  new_val=c(j,lstages.linked,length(surv.fish),length(surv.sp),max(TrophLevel),thingy)
  meta_data=rbind(meta_data,new_val)
  detach(sim_data)
}
meta_data=as.data.frame(meta_data)
colnames(meta_data)=c('SimNum',"Linked_Stages","Survive_Fish","Survive_Total","Max_T")

write.csv(meta_data,file = paste0("meta_data_",exp_type,".csv"))


################################################
############### RELOAD DATA ####################
################################################
meta_data=read.csv(paste0("meta_data_",exp_type,".csv"))[,-1]
#Reload all the data and plot
meta_data1=read.csv(paste0("meta_data_",exp_type_all[1],".csv"))[,-1]
meta_data1=cbind(1,meta_data1)
meta_data2=read.csv(paste0("meta_data_",exp_type_all[2],".csv"))[,-1]
meta_data3=read.csv(paste0("meta_data_",exp_type_all[3],".csv"))[,-1]
all_data=rbind(meta_data1,meta_data2,meta_data3)
head(all_data)
xk=meta_data1[,13:15]
xk_means=colMeans(xk)
xk_vars=colVars(xk)
y.sd <- apply(xk,2,sd)


error.bar <- function(x, y, upper, lower=upper, length=0.1,...){
  if(length(x) != length(y) | length(y) !=length(lower) | length(lower) != length(upper))
    stop("vectors must be same length")
  arrows(x,y+upper, x, y-lower, angle=90, code=3, length=length, ...)
}


barx=barplot(xk_means, ylim = c(0,max(xk_means+1.96*y.sd/10)),xlab="Phases",ylab="Mean Biomass",main="Fish")
error.bar(barx,xk_means,1.96*y.sd/10)


all_data[,13:15]


plot(meta_data1[,13],meta_data2[,12])
plot(meta_data1[,13],meta_data2[,13])
plot(meta_data1[,14],meta_data2[,14])
