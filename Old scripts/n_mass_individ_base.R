rm(list=ls())
detach(neither)
setwd("/Users/JurassicPark/Google Drive/GIT/Masters Project/Testing Code Files/Niche_mass_correlation")
#n_mass=matrix(,0,5)
N=100#10000
log_base=c(exp(1),2:10,seq(30,100,10),seq(200,1000,100))
n=length(log_base)
adjR=R_sq=matrix(0,N,n)
for (i in 1:N){
  x=paste0("n_mass_",i,".txt")
  n_mass=read.csv(x,header=F)
  colnames(n_mass)=c("niche","mass","fish","plant","either")
  neither_orig=n_mass[n_mass$either==0,]
  for (j in 1:n){
    neither=neither_orig
    if (j>n){
      neither$mass=sqrt(neither$mass)
      dog=1
    } else {
      neither$mass=log(neither$mass, base=log_base[j])
      dog=0
    }
    attach(neither)
    #plot(niche,mass)
    M0=lm(mass~niche)
    summary(M0)
    adjR[i,j]=summary(M0)$adj.r.squared
    R_sq[i,j]=summary(M0)$r.squared
    #adjR.M0
    #abline(M0)
    detach(neither)
  }
}
#adjR
#R_sq
#hist(adjR,main="Histogram of adjusted R squared")
#hist(R_sq,main="Histogram of R squared")
setwd("/Users/JurassicPark/Google Drive/GIT/Masters Project/Testing Code Files")
write.csv(adjR,file=paste0("adjR_",N,".txt"))
write.csv(R_sq,file=paste0("Rsq_",N,".txt"))

x=adjR
x=R_sq
y=colMeans(x)
y
