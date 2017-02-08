# ---- Setup ----
rm(list=ls())
setwd("/Users/JurassicPark/Google Drive/GIT/Analysis")
library(R.matlab)
library(matrixStats)
library(RColorBrewer)
library(codyn)
library(knitr)
library(reshape2)
sim_data=readMat("Complete_1.mat")
names(sim_data)
attach(sim_data)
#x11()
logB=log10(B)
logB.year.end=log10(B.year.end)



# ---- Sample_Plots ----

#Plot everything in simplest manner possible
ts.plot(logB)#Plot everything
ts.plot(logB.year.end)#Plot year ends

#Plot Invertebrates first
type=t(species)*isfish
matplot(matrix(rep(day,sum(type==0)),ncol=sum(type==0)),logB[,type==0],type='l',col=1,lty=1)#Plot each invertebrate and autotroph separately

matlines(matrix(rep(day,length(basalsp)),ncol=length(basalsp)),logB[,basalsp],type='l',col=2,lty=1)#Plot each autotroph separately

ts.plot(rowSums(logB[,type==0]))#Plot all invertebrate biomass (summed)

invert_no_fish=isfish
invert_no_fish[basalsp]=1
ts.plot(cbind(rowSums(logB[,invert_no_fish==0]),rowSums(logB[,basalsp])),col=1:2,lty=1)#Plot all invertebrate biomass (summed)
darkcols <- brewer.pal(8, "Dark2")
color_i=0
xkcd=species[isfish==1]
xkcd=unique(xkcd)
for (i in xkcd){
  color_i=color_i+1
  for (j in 1:max(lifestage)){
    single_lifestage=(t(type==i)*lifestage==j)
    #matlines(t(day),logB[,single_lifestage],type='l',col=darkcols[color_i],lty=1+j,lwd=2)
  }
  #matlines(t(day),rowSums(logB[,type==i]),type='l',col=darkcols[color_i],lwd=2)
}

# ---- Analysis ----

par(mfrow=c(1,1))
#x11()
lag.plot(B[,3], lags=100, do.lines =FALSE)
lag.plot(B, lags=2, do.lines =FALSE)
acf(B[,3],lag=1000)
acf(B,lag=1000)
plot_para=ceiling(sqrt(nichewebsize))
par(mfrow=c(plot_para,plot_para))
par(mfrow=c(3,3))
for (i in 1:9){#nichewebsize){
  #acf(B[(1:500)+500,i],lag=1000, main=paste('ACF for node ',i))
}

# ---- Heteroscedastic ----
#Test for Heteroscedasticity:
par(mfrow=c(3,1))
phase_l=unlist(num.years)#Length of each phase
for (i in 2:4){
  phase_start=L.year*sum(phase_l[1:i],-phase_l[i]/2)
  phase_end=L.year*sum(phase_l[1:i])-1
  #var(B[phase_start:phase_end,1])
}

par(mfrow=c(1,1))
hist(logB[30000,])


#Try plotting the variance over time for heteroscedasticity
plot(B[phase_start:phase_end,1],type='l')
testing=matrix(B[phase_start:phase_end,1],100)
trial=colVars(testing)
ts.plot(trial)

# ---- lag_diff_plot ----
#Try differencing by lag 100
logB_diff=logB[101:30000,]-logB[1:29900,]
ts.plot(logB_diff[(5000:9000)+10000,-42])

# ---- Stabilty ----
data(knz_001d)
kable(head(knz_001d))
KNZ_stability <- community_stability(knz_001d, 
                                     time.var = "year",
                                     abundance.var = "abundance", 
                                     replicate.var = "subplot")
kable(head(KNZ_stability))
#The input data frame needs to contain columns for time, species and abundance
B_df_days=setNames(melt(B)[,c(2, 1, 3)], c('Nodes_df', 'Day_df', 'Biomass'))
B_df=as.data.frame(B)
colnames(B_df)=1:nichewebsize
B_df=cbind(Year_df=as.integer(year.index),Day_df=rep.int(1:L.year,sum(unlist(num.years)) ),B_df)
B_df_years=melt(B_df, id.vars=c("Year_df","Day_df"), measure.vars =(1:nichewebsize)+2)
B_df_years=setNames(B_df_years[,c(3,1,2,4)], c('Nodes_df','Year_df', 'Day_df', 'Biomass'))
#B_df_years[,1]=as.integer(B_df_years[,1])





B_stability=community_stability(B_df_years,time.var="Year_df",abundance.var="Biomass",replicate.var="Day_df")
kable(head(B_stability))




