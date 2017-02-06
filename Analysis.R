# ---- Analysis ----
rm(list=ls())
detach()
setwd("/Users/JurassicPark/Google Drive/GIT/Analysis")
library(R.matlab)
sim_data=readMat("Complete_1.mat")
names(sim_data)
attach(sim_data)


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
  
t=sample(1:10, 10, replace=TRUE)
x=matrix(sample(50:100, 100, replace=TRUE),nrow = 10)

t==1
x[t==1,]
plot(matrix(rep(1:10,each=2),nrow=2),x[1:2,],type='l')
matplot(matrix(rep(1:10,2),ncol=2),x[,1:2],type='l',col=1)
matlines(matrix(rep(1:10,sum(t==1)),ncol=sum(t==1)),x[,t==1],type='l',col=2)

ggplot(dd, aes(Year, Value,colour=School_ID)) + 
  geom_line() + 
  geom_point()
A=as.data.frame(test)
ggplot(A, aes())



df <- data.frame(date = c("01-04-2001 00:00","01-04-2001 00:00","01-04-2001 00:00",
                          "01-05-2001 00:00","01-05-2001 00:00","01-05-2001 00:00",
                          "01-06-2001 00:00","01-06-2001 00:00","01-06-2001 00:00",
                          "01-07-2001 00:00","01-07-2001 00:00","01-07-2001 00:00"), 
                 id = c(1,2,3,1,2,3,1,2,3,1,2,3), a = c(1,2,3,4,5,6,7,8,9,10,11,12), 
                 b = c(2,2.5,3,3.2,4,4.6,5,5.6,8,8.9,10,10.6))


