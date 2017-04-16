# ---- Setup ----
rm(list=ls())
setwd("/Users/JurassicPark/Google Drive/GIT/Analysis")
library(R.matlab)
library(matrixStats)
library(RColorBrewer)
library(codyn)
library(knitr)
library(reshape2)
library(dplyr)
sim_data=readMat("Complete_1.mat")
names(sim_data)
attach(sim_data)
#x11()
logB=log10(B)
logB.year.end=log10(B.year.end)
B_saved=B


# ---- Sample_Plots ----

#Plot everything in simplest manner possible
ts.plot(logB)#Plot everything
ts.plot(logB.year.end)#Plot year ends

#Plot Invertebrates first
type=t(species)*isfish
matplot(matrix(rep(day,sum(type==0)),ncol=sum(type==0)),logB[,type==0],type='l',col=1,lty=1,ylab="log Biomass", main="Invertebrates and Autotrophs, individual nodes",xlab="Time (days)")#Plot each invertebrate and autotroph separately

matlines(matrix(rep(day,length(basalsp)),ncol=length(basalsp)),logB[,basalsp],type='l',col=2,lty=1)#Plot each autotroph separately
legend("bottomleft",c("Invertebrates","Autotrophs"),col=1:2,lty=1)

ts.plot(log10(rowSums(B[,type==0])),xlab="Time (days)",ylab="log Biomass",main="Sum of all Non-Fish Biomass")#Plot all invertebrate biomass (summed)

invert_no_fish=isfish
invert_no_fish[basalsp]=1
ts.plot(cbind(log10(rowSums(B[,invert_no_fish==0])),log10(rowSums(B[,basalsp]))),col=1:2,lty=1,xlab="Time (days)",ylab="log Biomass",main="Summed Biomass for non-fish and autotrophs")#Plot all invertebrate biomass (summed)
legend("bottomleft",c("Not Fish","Autotrophs"),col=1:2,lty=1)

darkcols <- brewer.pal(8, "Dark2")
color_i=0
xkcd=species[isfish==1]
xkcd=unique(xkcd)
ts.plot(cbind(log10(rowSums(B[,invert_no_fish==0])),log10(rowSums(B[,basalsp]))),col=1:2,lty=1,xlab="Time (days)",ylab="log Biomass",main="with Fish",ylim=c(min(log10(rowSums(B[,xkcd]))),max(log10(rowSums(B[,invert_no_fish==0])))))#Plot all invertebrate biomass (summed)
for (i in xkcd){
  color_i=color_i+1
  for (j in 1:max(lifestage)){
    single_lifestage=(t(type==i)*lifestage==j)
    matlines(t(day),logB[,single_lifestage],type='l',col=darkcols[color_i],lty=1+j,lwd=2)
  }
  #matlines(t(day),log10(rowSums(B[,type==i])),type='l',col=darkcols[color_i],lwd=2)
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
plot(B[phase_start:phase_end,1],type='l',xlab="Time (days)",ylab="Biomass",main="Biomass of one species in one phase, after stable")
testing=matrix(B[phase_start:phase_end,1],100)
trial=colVars(testing)
ts.plot(trial,xlab="Time (days)",ylab="Variance of Biomass",main="Variance of Biomass of one species in one phase, after stable")

# ---- lag_diff_plot ----
#Try differencing by lag 100
logB_diff=logB[101:30000,]-logB[1:29900,]
ts.plot(logB_diff[(5000:9000)+10000,-42],xlab="Time (days)",ylab="log of Biomass",main="Difference of log(Biomass) by 1 year")

# ---- Stabilty ----
data(knz_001d)
kable(head(knz_001d))
KNZ_stability <- community_stability(knz_001d, 
                                     time.var = "year",
                                     abundance.var = "abundance", 
                                     replicate.var = "subplot")
kable(head(KNZ_stability))

KNZ_variance_ratio <- variance_ratio(df = knz_001d, 
                                     species.var = "species", 
                                     time.var = "year",
                                     abundance.var = "abundance", 
                                     bootnumber = 10, 
                                     replicate.var = "subplot")

kable(KNZ_variance_ratio)

KNZ_variance_ratio_avgrep <- variance_ratio(knz_001d, 
                                            time.var = "year",
                                            species.var = "species",
                                            abundance.var = "abundance",  
                                            bootnumber = 10, 
                                            replicate.var = "subplot", 
                                            average.replicates = FALSE)

kable(head(KNZ_variance_ratio_avgrep))
rank_shift(knz_001d,
           time.var = "year",
           species.var = "species",
           abundance.var = "abundance",
           replicate.var = "subplot")
rank_shift(subset(knz_001d, subplot=="A_1"),
           time.var = "year",
           species.var = "species",
           abundance.var = "abundance")
#The input data frame needs to contain columns for time, species and abundance
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

B_stability=community_stability(B_df,time.var="Year_df",abundance.var="Biomass")
B_stability

B_stability=community_stability(B_df,time.var="Days_df",abundance.var="Biomass")
B_stability

xkcd=B_df
#xkcd=xkcd[xkcd$Year_df<=100,]
#unique(subset(xkcd, xkcd$Biomass==0, select = c(Nodes_df)))
extant=which(B[tot_days,]!=0)
extinct=which(B[tot_days,]==0)
#which(isfish==1)
intersect(which(isfish==0),unlist(unique(subset(xkcd, xkcd$Biomass==0, select = c(Nodes_df)))))
xkcd=xkcd[xkcd$Year_df %in% ((50:100)+200),]
xkcd=xkcd[xkcd$Nodes_df  %in% extant,]
#xkcd=xkcd[,xkcd$Year_df==(1:300)]
dim(xkcd)
head(xkcd)

B_stability=community_stability(xkcd,time.var="Days_df",abundance.var="Biomass")
B_stability



species
calvin=data_frame(x=sample.int(10,20,T),y=1:20)
group_by(calvin,x)


# ---- Plot_Von_bert_Curve ----

plot(1:4,Mass[which(type==25),],xlab='Age',ylab='Mass',type="l",main="Von Bertalanffy curve")

# ---- Fish_Size_Distribution ----
fish_weights=Mass[which(lifestage==4)]
fish_weights_scaled=fish_weights/W.scalar

# ---- Phase_diagrams ----
par(mfrow=c(1,1))
xkcd=logB[200:3000,1]
xkcd=logB[1000:3000,1]
xkcd1=lag(xkcd,n=L.year/2)
xkcd2=lag(xkcd,n=L.year)
plot(xkcd,xkcd2,type="l")

lag_h=0.21#0.21 is nice
xkcd_h=lag(xkcd,n=L.year*lag_h)
plot(xkcd,xkcd_h,type="l")


library(plot3D)
lag_h=0.21#0.21 is nice
xkcd_h=lag(xkcd,n=L.year*lag_h)
xkcd_2h=lag(xkcd,n=L.year*lag_h*2)
lines3D(xkcd,xkcd_h,xkcd_2h)



xkcd_h=lag(xkcd,n=21)
xkcd_2h=lag(xkcd,n=5)
lines3D(xkcd,xkcd_h,xkcd_2h)

par(mfrow=c(4,5),mar=c(0,0,0,0))
for (lag_h in seq(5,95,5)){#Plot for different lags
  xkcd_h=lag(xkcd,n=lag_h)
  xkcd_2h=lag(xkcd,n=lag_h*2)
  lines3D(xkcd,xkcd_h,xkcd_2h,colkey=(add=F),xlab="Biomass",ylab=paste0("Lag of ",lag_h),zlab=paste0("Lag of ",lag_h*2))
  mtext(side = 3, text = paste0("lag=",lag_h), line = -2.5)
}


par(mfrow=c(4,5),mar=c(0,0,0,0))
for (lag_h in seq(5,95,5)){#Another visualization of different lags
  xkcd_h=lag(xkcd,n=lag_h)
  xkcd_2h=lag(xkcd,n=lag_h*2)
  scatter3D(xkcd,xkcd_h,xkcd_2h, phi = 0, bty = "g", type = "l", ticktype = "detailed", lwd = 2,colkey=(add=F),xlab="Biomass",ylab=paste0("Lag of ",lag_h),zlab=paste0("Lag of ",lag_h*2))
  mtext(side = 3, text = paste0("lag=",lag_h), line = -2.5)
}

par(mfrow=c(2,2))
lag_h=55
for (i in 1:4){# Plot for different species
  xkcd=logB[100:3000,i]
  xkcd_h=lag(xkcd,n=lag_h)
  xkcd_2h=lag(xkcd,n=lag_h*2)
  scatter3D(xkcd,xkcd_h,xkcd_2h, phi = 0, bty = "g", type = "l", ticktype = "detailed", lwd = 1,colkey=(add=F),xlab="Biomass",ylab=paste0("Lag of ",lag_h),zlab=paste0("Lag of ",lag_h*2))
  mtext(side = 3, text = paste0("Node #",i), line = -2.5)
}


par(mfrow=c(1,1),mar=c(2,2,1,1))
lag_h=55
for (i in 1){# Plot Species agains each other
  t_0=1000
  t_f=dim(logB)[1]
  xkcd1=logB[t_0:t_f,1]
  xkcd2=logB[t_0:t_f,40]
  xkcd3=logB[t_0:t_f,2]
  all_fish=logB[t_0:t_f,]
  scatter3D(xkcd1,xkcd2,xkcd3, phi = 0, bty = "g", type = "l", ticktype = "detailed", lwd = 1,colkey=(add=F),xlab="Biomass of Species A",ylab="Biomass of Species B",zlab="Biomass of Species C")
}


par(mfrow=c(1,1),mar=c(2,2,1,1))
lag_h=55
for (i in 1){# Plot for different lifestages of one species
  t_0=1000
  t_f=dim(logB)[1]
  plot_fish=23
  xkcd1=logB[t_0:t_f,plot_fish]
  xkcd2=logB[t_0:t_f,plot_fish+1]
  xkcd3=logB[t_0:t_f,plot_fish+2]
  all_fish=logB[t_0:t_f,]
  scatter3D(xkcd1,xkcd2,xkcd3, phi = 0, bty = "g", type = "l", ticktype = "detailed", lwd = 1,colkey=(add=F),xlab="Youngest",ylab="Older",zlab="Oldest",main="Lifestages of 1 species")
}




par(mfrow=c(1,1),mar=c(2,2,1,1))
lag_h=55
for (i in 1){# DONT CHANGE THIS PLOT
  t_0=401
  t_f=dim(logB)[1]
  xkcd1=logB[t_0:t_f,10]
  xkcd2=logB[t_0:t_f,22]
  xkcd3=logB[t_0:t_f,39]
  scatter3D(xkcd1,xkcd2,xkcd3, phi = 0, bty = "g", type = "l", ticktype = "detailed", lwd = 3,colkey=(add=F),xlab="Biomass of Node A",ylab="Biomass of Node B",zlab="Biomass of Node C")
  mtext(side = 3, text = "Node #10 is interesting because it goes to 0, and then recovers from other lifestages", line = -2.5)
}


par(mfrow=c(1,1),mar=c(2,2,1,1))
lag_h=55
for (i in 1){# Plot for different fish species
  t_0=500
  t_f=dim(B)[1]
  xkcd1=log10(rowSums(B[t_0:t_f,7:10]))
  xkcd2=log10(rowSums(B[t_0:t_f,19:22]))
  xkcd3=log10(rowSums(B[t_0:t_f,37:40]))
  scatter3D(xkcd1,xkcd2,xkcd3, phi = 0, bty = "g", type = "l", ticktype = "detailed", lwd = 1,colkey=(add=F),xlab="Biomass of Species A",ylab="Biomass of Species B",zlab="Biomass of Species C")
  mtext(side = 3, text = "Sum all lifestages", line = -2.5)
}

par(mfrow=c(1,1),mar=c(2,2,1,1))
lag_h=55
for (i in 1){# DONT CHANGE THIS PLOT (Plot for different types of species)- so this is interesting because it shows how phase plots causality can still be misinterpreted. Here we show the invertebrates seem to cause the fish collapse, but really it's the fish collapse that causes the invertebrate boom.  
  t_0=1000
  t_f=dim(logB)[1]
  xkcd1=log10(rowSums(B[t_0:t_f,basalsp]))
  xkcd2=log10(rowSums(B[t_0:t_f,which(isfish==1)]))
  xkcd3=log10(rowSums(B[t_0:t_f,setdiff(which(isfish==0),basalsp)]))
  scatter3D(xkcd1,xkcd2,xkcd3, phi = 0, bty = "g", type = "l", ticktype = "detailed", lwd = 1,colkey=(add=F),xlab="Basal Species",ylab="Fish",zlab="Invertebrates")
  mtext(side = 3, text = "", line = -2.5)
}

par(mfrow=c(1,1),mar=c(4,4,1,1))
colored = rainbow(length(xkcd2))
plot(xkcd2,xkcd3,type="l",xlab="Fish",ylab="Basal Species",col=0)
for (i in 2:length(xkcd2)){
  lines(xkcd2[(i-1):i],xkcd3[(i-1):i],type="l",xlab="Fish",ylab="Basal Species",col=colored[i])
}




par(mfrow=c(1,1),mar=c(4,4,1,1))
t_0=1
t_f=dim(logB)[1]
xkcd1=log10(rowSums(B[t_0:t_f,basalsp]))
xkcd2=log10(rowSums(B[t_0:t_f,which(isfish==1)]))
xkcd3=log10(rowSums(B[t_0:t_f,setdiff(which(isfish==0),basalsp)]))
t_0=500
plot(xkcd2[t_0:t_f],xkcd3[t_0:t_f],type="l",xlab="Fish",ylab="Basal Species",col=0)
for (i in 1:3){
  t_f=c(1000,2000,3000)[i]
  lines(xkcd2[t_0:t_f],xkcd3[t_0:t_f],type="l",xlab="Fish",ylab="Invertebrates",col=i,lwd=i)
  print(paste(t_0,t_f))
  t_0=t_f
}



par(mfrow=c(1,1),mar=c(4,4,1,1))
t_0=500
t_f=dim(logB)[1]
scatter3D(xkcd1[t_0:t_f],xkcd2[t_0:t_f],xkcd3[t_0:t_f], phi = 0, bty = "g", type = "l", ticktype = "detailed",xlab="Basal Species",ylab="Fish",zlab="Invertebrates",col="purple")
for (i in 1:3){
  t_f=c(10000,20000,30000)[i]
  lines3D(xkcd1[t_0:t_f],xkcd2[t_0:t_f],xkcd3[t_0:t_f],col=c("darkgreen","red","blue")[i],add=TRUE,lwd=2)
  t_0=t_f
}
legend(-0.35,0.2,legend=c("Pre-fishing","Fishing","Recovery"),col=c("darkgreen","red","blue"),lwd=2)


par(mfrow=c(1,1),mar=c(2,2,1,1))
t_0=1000
t_f=dim(logB)[1]
xkcd1=logB[,1]
xkcd2=logB[,40]
xkcd3=logB[,2]
scatter3D(xkcd1[t_0:t_f],xkcd2[t_0:t_f],xkcd3[t_0:t_f], phi = 0, bty = "g", type = "l", ticktype = "detailed", lwd = 1,colkey=(add=F),xlab="Biomass of Species A",ylab="Biomass of Species B",zlab="Biomass of Species C")
for (i in 1:3){
  t_f=c(10000,20000,30000)[i]
  lines3D(xkcd1[t_0:t_f],xkcd2[t_0:t_f],xkcd3[t_0:t_f],col=c("darkgreen","red","blue")[i],add=TRUE,lwd=2)
  t_0=t_f
}
legend(-0.35,0.2,legend=c("Pre-fishing","Fishing","Recovery"),col=c("darkgreen","red","blue"),lwd=2)


library(plot3Drgl)
t_0=1000
t_f=dim(logB)[1]
scatter3Drgl(xkcd1[t_0:t_f],xkcd2[t_0:t_f],xkcd3[t_0:t_f], phi = 0, bty = "g", type = "l", ticktype = "detailed",xlab="Basal Species",ylab="Fish",zlab="Invertebrates")



par(mfrow=c(1,1),mar=c(4,4,0,0))
t_0=500
t_f=dim(logB)[1]
t_scale=0.000005
scatter3D(xkcd1[t_0:t_f],xkcd2[t_0:t_f],xkcd3[t_0:t_f], phi = 0, type = "l", ticktype = "detailed",xlab="Basal Species",ylab="Fish",zlab="Invertebrates",col="purple",zlim=c(min(xkcd3[t_0:t_f]),day[t_F]*t_scale+max(xkcd3[t_S:t_F])),xlim=c(min(xkcd1[t_0:t_f]),day[t_F]*t_scale+max(xkcd1[t_S:t_F])),perspbox=FALSE)
lines3D(c(min(xkcd1[t_S:t_F]),max(xkcd1[t_S:t_F])),c(min(xkcd2[t_S:t_F]),min(xkcd2[t_S:t_F])),c(min(xkcd3[t_S:t_F]),min(xkcd3[t_S:t_F])),col=1,add=TRUE,lwd=2,xlim=0:50)
lines3D(c(min(xkcd1[t_S:t_F]),max(xkcd1[t_S:t_F])),c(max(xkcd2[t_S:t_F]),max(xkcd2[t_S:t_F])),c(min(xkcd3[t_S:t_F]),min(xkcd3[t_S:t_F])),col=1,add=TRUE,lwd=2,xlim=0:50)
lines3D(c(min(xkcd1[t_S:t_F]),max(xkcd1[t_S:t_F])),c(min(xkcd2[t_S:t_F]),min(xkcd2[t_S:t_F])),c(max(xkcd3[t_S:t_F]),max(xkcd3[t_S:t_F])),col=1,add=TRUE,lwd=2,xlim=0:50)
lines3D(c(min(xkcd1[t_S:t_F]),max(xkcd1[t_S:t_F])),c(max(xkcd2[t_S:t_F]),max(xkcd2[t_S:t_F])),c(max(xkcd3[t_S:t_F]),max(xkcd3[t_S:t_F])),col=1,add=TRUE,lwd=2,xlim=0:50)
lines3D(c(min(xkcd1[t_S:t_F]),min(xkcd1[t_S:t_F])),c(min(xkcd2[t_S:t_F]),max(xkcd2[t_S:t_F])),c(max(xkcd3[t_S:t_F]),max(xkcd3[t_S:t_F])),col=1,add=TRUE,lwd=2,xlim=0:50)
lines3D(c(min(xkcd1[t_S:t_F]),min(xkcd1[t_S:t_F])),c(min(xkcd2[t_S:t_F]),max(xkcd2[t_S:t_F])),c(min(xkcd3[t_S:t_F]),min(xkcd3[t_S:t_F])),col=1,add=TRUE,lwd=2,xlim=0:50)
lines3D(c(max(xkcd1[t_S:t_F]),max(xkcd1[t_S:t_F])),c(min(xkcd2[t_S:t_F]),max(xkcd2[t_S:t_F])),c(max(xkcd3[t_S:t_F]),max(xkcd3[t_S:t_F])),col=1,add=TRUE,lwd=2,xlim=0:50)
lines3D(c(max(xkcd1[t_S:t_F]),max(xkcd1[t_S:t_F])),c(min(xkcd2[t_S:t_F]),max(xkcd2[t_S:t_F])),c(min(xkcd3[t_S:t_F]),min(xkcd3[t_S:t_F])),col=1,add=TRUE,lwd=2,xlim=0:50)
lines3D(c(min(xkcd1[t_S:t_F]),min(xkcd1[t_S:t_F])),c(min(xkcd2[t_S:t_F]),min(xkcd2[t_S:t_F])),c(min(xkcd3[t_S:t_F]),max(xkcd3[t_S:t_F])),col=1,add=TRUE,lwd=2,xlim=0:50)
lines3D(c(max(xkcd1[t_S:t_F]),max(xkcd1[t_S:t_F])),c(max(xkcd2[t_S:t_F]),max(xkcd2[t_S:t_F])),c(min(xkcd3[t_S:t_F]),max(xkcd3[t_S:t_F])),col=1,add=TRUE,lwd=2,xlim=0:50)
lines3D(c(min(xkcd1[t_S:t_F]),min(xkcd1[t_S:t_F])),c(max(xkcd2[t_S:t_F]),max(xkcd2[t_S:t_F])),c(min(xkcd3[t_S:t_F]),max(xkcd3[t_S:t_F])),col=1,add=TRUE,lwd=2,xlim=0:50)
t_S=t_0; t_F=t_f
for (i in 1:3){
  t_f=c(10000,20000,30000)[i]
  phase_col=c("darkgreen","red","blue")[i]
  lines3D(xkcd1[t_0:t_f],xkcd2[t_0:t_f],xkcd3[t_0:t_f],col=phase_col,add=TRUE,lwd=2)
  lines3D(day[t_0:t_f-t_S+1]*t_scale+max(xkcd1[t_S:t_F]),rep(max(xkcd2[t_S:t_F]),t_f-t_0+1),xkcd3[t_0:t_f],col=phase_col,add=TRUE,lwd=2,xlim=0:50)
  
  lines3D(xkcd1[t_0:t_f],rep(min(xkcd2[t_S:t_F]),t_f-t_0+1),day[t_0:t_f-t_S+1]*t_scale+max(xkcd3[t_S:t_F]),col=phase_col,add=TRUE,lwd=2,ylim=0:50)
  
  lines3D(rep(max(xkcd1[t_S:t_F]),t_f-t_0+1),xkcd2[t_0:t_f],day[t_0:t_f-t_S+1]*t_scale+max(xkcd3[t_S:t_F]),col=phase_col,add=TRUE,lwd=2,ylim=0:50)
  t_0=t_f
}
legend(-0.35,0.2,legend=c("Pre-fishing","Fishing","Recovery"),col=c("darkgreen","red","blue"),lwd=2)







par(mfrow=c(1,1),mar=c(4,4,2,2))
t_0=500
t_f=dim(logB)[1]
t_scale=0.0001
scatter3D(xkcd1[t_0:t_f],xkcd2[t_0:t_f],xkcd3[t_0:t_f], phi = 0,bty="g", type = "l", ticktype = "detailed",xlab="Basal Species",ylab="Fish",zlab="Invertebrates",col="purple")
t_S=t_0; t_F=t_f
for (i in 1:3){
  t_f=c(10000,20000,30000)[i]
  phase_col=c("darkgreen","red","blue")[i]
  lines3D(xkcd1[t_0:t_f],xkcd2[t_0:t_f],xkcd3[t_0:t_f],col=phase_col,add=TRUE,lwd=2)
  lines3D(day[t_0:t_f-t_S+1]*t_scale+max(xkcd1[t_S:t_F]),rep(max(xkcd2[t_S:t_F]),t_f-t_0+1),xkcd3[t_0:t_f],col=phase_col,add=TRUE,lwd=2,xlim=0:50)
  
  lines3D(xkcd1[t_0:t_f],rep(min(xkcd2[t_S:t_F]),t_f-t_0+1),day[t_0:t_f-t_S+1]*t_scale+max(xkcd3[t_S:t_F]),col=phase_col,add=TRUE,lwd=2,ylim=0:50)
  
  lines3D(rep(max(xkcd1[t_S:t_F]),t_f-t_0+1),xkcd2[t_0:t_f],day[t_0:t_f-t_S+1]*t_scale+max(xkcd3[t_S:t_F]),col=phase_col,add=TRUE,lwd=2,ylim=0:50)
  t_0=t_f
}
legend(-0.35,0.2,legend=c("Pre-fishing","Fishing","Recovery"),col=c("darkgreen","red","blue"),lwd=2)


# ---- ACFs ----
par(mar=c(4,4,4,2))
hobbes1=acf(xkcd1,lag.max=400,main="Basal Species")
hobbes2=acf(xkcd2,lag.max=400,main="Fish")
hobbes3=acf(xkcd3,lag.max=400,main="Invertebrates")
hobbes4=acf(rnorm(400),lag.max = 400)

hobbes1=as.numeric(as.vector(unlist(hobbes1))[1:400])
hobbes2=as.numeric(as.vector(unlist(hobbes2))[1:400])
hobbes3=as.numeric(as.vector(unlist(hobbes3))[1:400])
hobbes4=as.numeric(as.vector(unlist(hobbes4))[1:400])

sum(abs(hobbes1))
sum(abs(hobbes2))
sum(abs(hobbes3))
sum(abs(hobbes4))

plot(c(min(hobbes1,hobbes2),1),c(min(hobbes1,hobbes2),1),type="l",xlab="Basal Species",ylab="Fish")
lines(hobbes1,hobbes2,type="l",add=T)
plot(c(min(hobbes1,hobbes3),1),c(min(hobbes1,hobbes3),1),type="l",xlab="Basal Species",ylab="Invertebrates")
lines(hobbes1,hobbes3,type="l",add=T)
plot(c(min(hobbes2,hobbes3),1),c(min(hobbes2,hobbes3),1),type="l",xlab="Fish",ylab="Invertebrates")
lines(hobbes2,hobbes3,type="l",add=T)
plot(hobbes2,hobbes3,type="l",add=T)


scatter3Drgl(hobbes1,hobbes2,hobbes3, type="l",xlab="Basal Species",ylab="Fish",zlab="Invertebrates")





