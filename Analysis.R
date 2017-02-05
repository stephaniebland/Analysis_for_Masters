# ---- Setup ----
rm(list=ls())
detach()
setwd("/Users/JurassicPark/Google Drive/GIT/Analysis")
library(R.matlab)
sim_data=readMat("Complete_1.mat")
names(sim_data)
attach(sim_data)


# ---- Sample_Plots ----

x11()
B=log10(B)
B.year.end=log10(B.year.end)

#Plot everything in simplest manner possible
ts.plot(B)#Plot everything
ts.plot(B.year.end)#Plot year ends

#Plot Invertebrates first
type=t(species)*isfish
matplot(matrix(rep(day,sum(type==0)),ncol=sum(type==0)),B[,type==0],type='l',col=1,lty=1)#Plot each invertebrate and autotroph separately

matlines(matrix(rep(day,length(basalsp)),ncol=length(basalsp)),B[,basalsp],type='l',col=2,lty=1)#Plot each autotroph separately

ts.plot(rowSums(B[,type==0]))#Plot all invertebrate biomass (summed)

invert_no_fish=isfish
invert_no_fish[basalsp]=1
ts.plot(cbind(rowSums(B[,invert_no_fish==0]),rowSums(B[,basalsp])),col=1:2,lty=1)#Plot all invertebrate biomass (summed)
library(RColorBrewer)
darkcols <- brewer.pal(8, "Dark2")
color_i=0
xkcd=species[isfish==1]
xkcd=unique(xkcd)
for (i in xkcd){
  color_i=color_i+1
  for (j in 1:max(lifestage)){
    single_lifestage=(t(type==i)*lifestage==j)
    matlines(t(day),B[,single_lifestage],type='l',col=darkcols[color_i],lty=1+j,lwd=2)
  }
  #matlines(t(day),rowSums(B[,type==i]),type='l',col=darkcols[color_i],lwd=2)
}

# ---- Analysis ----

par(mfrow=c(1,1))
x11()
lag.plot(B[,3], lags=100, do.lines =FALSE)
lag.plot(B, lags=2, do.lines =FALSE)
acf(B[,3],lag=1000)
acf(B,lag=1000)
plot_para=ceiling(sqrt(nichewebsize))
par(mfrow=c(plot_para,plot_para))
par(mfrow=c(3,3))
for (i in 1:9){#nichewebsize){
  plot_title=paste('acf for node ',i)
  acf(B[,i],lag=1000, main=paste('ACF for node ',i))
}

