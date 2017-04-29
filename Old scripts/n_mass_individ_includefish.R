rm(list=ls())
detach(neither)
setwd("/Users/JurassicPark/Google Drive/GIT/Masters Project/Testing Code Files/Niche_metabolic_rate")
#n_mass=matrix(,0,5)
N=10#10000
adjR_m=R_sq_m=rep(0,N)

for (i in 1:N){
  x=paste0("n_mass_",i,".txt")
  n_mass=read.csv(x,header=F)
  n_mass=cbind(n_mass,matrix(,dim(n_mass)[1],3))
  colnames(n_mass)=c("niche","mass","fish","plant","either","Trophic","Metabolic","m","v","extra")
  neither=n_mass[n_mass$either==0,]
  attach(neither)
  #mass=log10(mass)
  extra=Metabolic^(Trophic-1)
  #plot(niche,mass)
  M0=lm(mass~niche)
  MZ=lm(Metabolic~niche)
  MT=lm(Trophic~niche)
  #summary(M0)
  adjR_m[i]=summary(M0)$adj.r.squared
  R_sq_m[i]=summary(M0)$r.squared
  #adjR.M0
  #abline(M0)
  detach(neither)
}
#adjR
#R_sq
hist(adjR_m,main="Histogram of adjusted R squared for mass")
hist(R_sq_m,main="Histogram of R squared for mass")
mean(adjR_m)
#setwd("/Users/JurassicPark/Google Drive/GIT/Masters Project/Testing Code Files")
#write.csv(adjR,file=paste0("adjR_",N,".txt"))
#write.csv(R_sq,file=paste0("Rsq_",N,".txt"))


