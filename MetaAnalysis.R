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


# The first step should be setting up better names, so the legends will automatically be named properly. This is to avoid having vague graphs with names like "experiment 1" and experiment 2" because people will definitely forget what that means.
node_names=levels(factor(alldata$Nodes_df))
# In the following line we have some difficulty with node names - after 10 the levels function tries to sort alphabetically but fails miserably.
node_names=cbind(node_names,c("Autotrophs",paste("Life stage",1:4),paste("Fish",1:3),"Fish","Invertebrates",rep("Node",39),"Non Fish","Total Biomass"))
exper_name=c("Life history","New Nodes","No Life history")



# I want to write a function that takes in all data, the experiment numbers, species (values in Nodes_df), and sort factor (group by experiment or species?) to produce a plot LD1 against LD2 maybe?
### WARNING I need to transform the data also
N_Exper=as.integer(nlevels(factor(alldata$Exper)))
# So I'll break it down in two sections:
# FIRST rearrange data into a 
bland_tapply <- function(alldata,){
	# 1. Find the mean of Biomass, grouped by list vector items -> produces an array where the dimensions match the order in list. 
	tapplied=tapply(alldata$Biomass,list(alldata$Simnum,alldata$Nodes_df,alldata$Exper),mean)
	tapplied=tapply(alldata$Biomass,list(alldata$Simnum,alldata$Exper,alldata$Nodes_df),mean)
	# SHOULD I BE TAKING MEAN OF LOG OR LOG OF MEAN - SAME THING FOR VAR CHECK STATS AHHHHHHHHHHHH - Search for arcsine biological abundances in literature. 
	# 2. Reshape the data to the correct format: this rowbinds the array along the last dimension:
	bound=aperm(tapplied,c(1,3,2))
	dim(bound)=c(prod(dim(bound)[1:2]),dim(bound)[3])
	
	row.names(bound)=c()
	colnames(bound)=colnames(tapplied)
	ir.species=rep(exper_name,each=dim(pca_groups)[1])
	ir.species=rep(levels(factor(alldata$Nodes_df)),each=dim(pca_groups)[1])
	#########THE EACH ELEMENT HERE MAKES NO SENSE I DONT KNOW WHY BOTH SEEM TO WORK THIS IS CRAZY
	X=data.frame(ir.species,bound)
}
library(dplyr)


pca_func <- function(data,group_by){
	ir.pca <- prcomp(data,center = F, scale. = F) 
	g <- ggbiplot(ir.pca, obs.scale = 1, var.scale = 1, groups = group_by, ellipse = TRUE, circle = TRUE)
	g <- g + scale_color_discrete(name = '')
	g <- g + theme(legend.direction = 'horizontal', legend.position = 'top')
	print(g)
}




pca_groups=tapply(alldata$Biomass,list(alldata$Simnum,alldata$Nodes_df,alldata$Exper),mean)

test <- alldata %>% group_by(Exper, Simnum, Nodes_df) %>% 
	summarise(mean = mean(Biomass),var=var(Biomass)) %>% 
	mutate(logmean = log(mean + 0.1), logvar=log(var+.1)) %>% 
	filter(Nodes_df %in% c("Fish_tot_df","inverts_tot_df","basal_tot_df"))

select(test, Simnum, Nodes_df, logmean) %>% 
	spread(key = Exper, value = logmean)

xkcd<-func(mean)


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

pca_groups[,,1:3] %>% dim




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
#pca_groups=tapply(alldata$Biomass,list(alldata$Simnum,alldata$Nodes_df,alldata$Exper),var)
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



	
# I want to write a function that takes in all data, the experiment numbers, species (values in Nodes_df), and sort factor (group by experiment or species?) to produce a plot LD1 against LD2 maybe?
### WARNING I need to transform the data also

# So I'll break it down in two sections:
# FIRST rearrange data into a 
bland_tapply <- function(alldata){
	# 1. Find the mean of Biomass, grouped by list vector items -> produces an array where the dimensions match the order in list. 
	lda_tapply=tapply(alldata$Biomass,list(alldata$Simnum,.....),mean)
	# SHOULD I BE TAKING MEAN OF LOG OR LOG OF MEAN - SAME THING FOR VAR CHECK STATS AHHHHHHHHHHHH - Search for arcsin biological abundances in literature. 
	reshape(lda_tapply)
	pca_bound=rbind(pca_groups[,,1],pca_groups[,,2],pca_groups[,,3])
	row.names(pca_bound)=c()
	ir.species=rep(paste("Experiment",1:3),each=dim(pca_groups)[1])
	#ir.species=rep(c("one","two"),each=dim(pca_groups)[1])
}




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
	 ,main="Discriminant analysis using Fish Lifestages")
plot(lscore[,1],lscore[,2],col=Iris$ir.species,xlab="1st canonical variate",ylab="2nd canonical variate"
	 ,main="Discriminant analysis using Fish Lifestages")
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
plot(lscore[,1],col=Iris$ir.species,ylab="1st canonical variate", main="Discriminant analysis using Total Biomass of Groups")
plot(lscore[,1],lscore[,2],col=Iris$ir.species,xlab="1st canonical variate",ylab="2nd canonical variate", main="Discriminant analysis using Total Biomass of Groups")
legend("topright",1,pch=1,legend=unique(Iris$ir.species), col=unique(Iris$ir.species))








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



