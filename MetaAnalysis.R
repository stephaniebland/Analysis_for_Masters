library(reshape2)
library(ggbiplot)
seed_0=0
lifestages_linked=1
Adults_only=0
DATE="2017Jun08"
Version="1"
#simnum=1
#Exper=1
location="/GIT/Analysis"#For Running on my Mac
#location=""#For Clusters
run_name=paste0(DATE,"_",Version)
setwd(paste0("~/",location,"/",run_name))

alldata=read.table("Melted.txt",header=F)
colnames(alldata)=colnames(melt_B.yr.end)


pca_func <- function(data,group_by){
	ir.pca <- prcomp(data,center = F, scale. = F) 
	g <- ggbiplot(ir.pca, obs.scale = 1, var.scale = 1, groups = group_by, ellipse = TRUE, circle = TRUE)
	g <- g + scale_color_discrete(name = '')
	g <- g + theme(legend.direction = 'horizontal', legend.position = 'top')
	print(g)
}




pca_groups=tapply(alldata$Biomass,list(alldata$Simnum,alldata$Nodes_df,alldata$Exper),mean)
pca_groups=pca_groups[,c("Fish_tot_df","inverts_tot_df","basal_tot_df"),]
pca_bound=rbind(pca_groups[,,1],pca_groups[,,2],pca_groups[,,3])
pca_bound=rbind(pca_groups[,,1],pca_groups[,,2])#,pca_groups[,,3])
# log transform 
pca_logGroups=log10(pca_bound+0.1)
ir.species=rep(c("one","two","three"),each=dim(pca_groups)[1])
ir.species=rep(c("one","two"),each=dim(pca_groups)[1])
# apply PCA - scale. = TRUE is highly 
# advisable, but default is FALSE. 
row.names(pca_logGroups)=1:dim(pca_bound)[1]
pca_func(pca_logGroups,ir.species)






pca_groups=tapply(alldata$Biomass,list(alldata$Simnum,alldata$Exper,alldata$Nodes_df),mean)
pca_groups=pca_groups[,,c("Fish_tot_df","inverts_tot_df","basal_tot_df")]
pca_bound=rbind(pca_groups[,,1],pca_groups[,,2],pca_groups[,,3])
# log transform 
pca_logGroups=log10(pca_bound+0.1)
ir.species=rep(c("Fish_tot_df","inverts_tot_df","basal_tot_df"),each=dim(pca_groups)[1])
# apply PCA - scale. = TRUE is highly 
# advisable, but default is FALSE. 
row.names(pca_logGroups)=1:dim(pca_bound)[1]
pca_func(pca_logGroups,ir.species)



pca_groups=tapply(alldata$Biomass,list(alldata$Simnum,alldata$Nodes_df,alldata$Exper),mean)
pca_groups=pca_groups[,grep("Fish_ls_",colnames(pca_groups)),]
pca_bound=rbind(pca_groups[,,1],pca_groups[,,2],pca_groups[,,3])

pca_bound=rbind(pca_groups[,,1],pca_groups[,,2])#,pca_groups[,,3])
# log transform 
pca_logGroups=log10(pca_bound+0.1)
ir.species=rep(c("one","two","three"),each=dim(pca_groups)[1])
ir.species=rep(c("one","two"),each=dim(pca_groups)[1])
# apply PCA - scale. = TRUE is highly 
# advisable, but default is FALSE. 
row.names(pca_logGroups)=1:dim(pca_bound)[1]
pca_func(pca_logGroups,ir.species)




xk=prcomp(pca_logGroups,center = T,scale. = F)
summary(xk)














#############################################
# Linear Discriminant Analysis
#############################################



alldata=backupdata
#alldata=alldata[alldata$Exper<3,]
pca_groups=tapply(alldata$Biomass,list(alldata$Simnum,alldata$Nodes_df,alldata$Exper),mean)
pca_bound=rbind(pca_groups[,,1],pca_groups[,,2],pca_groups[,,3])
#pca_bound=rbind(pca_groups[,,1],pca_groups[,,2])#,pca_groups[,,3])
# log transform 
pca_logGroups=log10(pca_bound+0.1)
ir.species=rep(c("one","two","three"),each=dim(pca_groups)[1])
#ir.species=rep(c("one","two"),each=dim(pca_groups)[1])
row.names(pca_logGroups)=1:dim(pca_bound)[1]



#tmp=dim(pca_logGroups)
#tmp=matrix(rnorm(tmp[1]*tmp[2]),tmp[1],tmp[2])
#colnames(tmp)=colnames(pca_logGroups)


snake <- read.csv(file="~/GIT/HalAnalysisOfBiologicalData/Tutorials/Tutorial\ 1d/snake.csv", header=TRUE)
Iris=data.frame(pca_logGroups,ir.species)
head(Iris)
snake.lda<-lda(ir.species ~ Fish_ls_1+Fish_ls_2+Fish_ls_3+Fish_ls_4, data=Iris)
#prior: the prior probabilities used.
snake.lda$prior
#means: the group means.
snake.lda$mean
#svd: the singular values, which give the ratio of the 
#between- and within-group standard deviations on the linear
#discriminant variables. Their squares are the canonical F-statistics.
snake.lda$svd
#scaling: a matrix which transforms observations to discriminant
#functions, normalized so that within groups covariance matrix is
#spherical. In our case we will have just one Linear discriminant.
snake.lda$scaling

# the scores
lscore = cbind(Iris$Fish_ls_1,Iris$Fish_ls_2,Iris$Fish_ls_3,Iris$Fish_ls_4)%*%snake.lda$scaling 
# Plotting
plot(lscore[,1],col=Iris$ir.species,asp=1,ylab="1st canonical variate"
	 ,main="Discriminant analysis")#Asp is just the y/x aspect ratio, so we don't need this plot.
plot(lscore[,1],col=Iris$ir.species,ylab="1st canonical variate"
	 ,main="Discriminant analysis")
plot(lscore[,1],lscore[,2],col=Iris$ir.species,xlab="1st canonical variate",ylab="2nd canonical variate"
	 ,main="Discriminant analysis")
legend("topright",1,pch=1,legend=unique(Iris$ir.species),
	   col=unique(Iris$ir.species))


#############################################
# Try again with looser parameters
#############################################

Iris=data.frame(pca_logGroups,ir.species)
snake.lda<-lda(ir.species ~ Fish_tot_df+inverts_tot_df+basal_tot_df, data=Iris)
#prior: the prior probabilities used.
snake.lda$prior
#means: the group means.
snake.lda$mean
#svd: the singular values, which give the ratio of the 
#between- and within-group standard deviations on the linear
#discriminant variables. Their squares are the canonical F-statistics.
snake.lda$svd
#scaling: a matrix which transforms observations to discriminant
#functions, normalized so that within groups covariance matrix is
#spherical. In our case we will have just one Linear discriminant.
snake.lda$scaling

# the scores
lscore = cbind(Iris$Fish_tot_df,Iris$inverts_tot_df,Iris$basal_tot_df)%*%snake.lda$scaling 
# Plotting
plot(lscore[,1],col=Iris$ir.species,asp=1,ylab="1st canonical variate"
	 ,main="Discriminant analysis")#Asp is just the y/x aspect ratio, so we don't need this plot.
plot(lscore[,1],col=Iris$ir.species,ylab="1st canonical variate"
	 ,main="Discriminant analysis")
plot(lscore[,1],lscore[,2],col=Iris$ir.species,xlab="1st canonical variate",ylab="2nd canonical variate"
	 ,main="Discriminant analysis")
legend("topright",1,pch=1,legend=unique(Iris$ir.species),
	   col=unique(Iris$ir.species))








require(MASS)
Iris=data.frame(pca_logGroups,ir.species)
#r <- lda(formula = pca_logGroups ~ ir.species,,grouping=ir.species,  prior = c(1,1,1)/3)
z <- lda(ir.species ~ ., Iris)#, prior = c(1,1,1)/3, subset = train)
lda=z
prop.lda = r$svd^2/sum(r$svd^2)

plda <- predict(object = lda,newdata = Iris)

dataset = data.frame(species = ir.species,
					 pca = xk$x, lda = plda$x)
lda
p1 <- ggplot(dataset) + geom_point(aes(lda.LD1, lda.LD2, colour = species, shape = species), size = 2.5) + 
	labs(x = paste("LD1 (", percent(prop.lda[1]), ")", sep=""),
		 y = paste("LD2 (", percent(prop.lda[2]), ")", sep=""))


Iris <- data.frame(rbind(iris3[,,1], iris3[,,2], iris3[,,3]),
				   Sp = rep(c("s","c","v"), rep(50,3)))
train <- sample(1:150, 75)
table(Iris$Sp[train])
## your answer may differ
##  c  s  v
## 22 23 30
z <- lda(Sp ~ ., Iris, prior = c(1,1,1)/3, subset = train)
predict(z, Iris[-train, ])$class
##  [1] s s s s s s s s s s s s s s s s s s s s s s s s s s s c c c
## [31] c c c c c c c v c c c c v c c c c c c c c c c c c v v v v v
## [61] v v v v v v v v v v v v v v v
(z1 <- update(z, . ~ . - Petal.W.))



