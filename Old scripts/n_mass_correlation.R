rm(list=ls())
detach(neither)
detach(fish)
setwd("/Users/JurassicPark/Google Drive/GIT/Masters Project/Testing Code Files/Niche_mass_correlation")
n_mass=matrix(,0,5)
for (i in 1:12158){
  x=paste("n_mass_",i,".txt",sep="")
  temp=read.csv(x,header=F)
  n_mass=rbind(n_mass,temp)
}

colnames(n_mass)=c("niche","mass","fish","plant","either")
# neither=n_mass[n_mass$either==0,]
# neither$mass=log10(neither$mass)
# attach(neither)
fish=n_mass[n_mass$either==0,]
fish$mass=log10(fish$mass)
attach(fish)
plot(niche,mass)
M0=lm(mass~niche)
summary(M0)
adjR.M0=summary(M0)$adj.r.squared
adjR.M0
abline(M0)

