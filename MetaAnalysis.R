




xkcd <- function(thing1,group_by){
	ir.pca <- prcomp(thing1,center = TRUE, scale. = TRUE) 
	g <- ggbiplot(ir.pca, obs.scale = 1, var.scale = 1, groups = group_by, ellipse = TRUE, circle = TRUE)
	g <- g + scale_color_discrete(name = '')
	g <- g + theme(legend.direction = 'horizontal', legend.position = 'top')
	print(g)
}


backupdata=alldata
alldata=alldata[alldata$Exper<3,]

pca_groups=tapply(alldata$Biomass,list(alldata$Simnum,alldata$Nodes_df,alldata$Exper),mean)
pca_groups=pca_groups[,c("Fish_tot_df","inverts_tot_df","basal_tot_df"),]
pca_bound=rbind(pca_groups[,,1],pca_groups[,,2])#,pca_groups[,,3])
# log transform 
pca_logGroups=log10(pca_bound+0.1)
ir.species=rep(c("one","two","three"),each=dim(pca_groups)[1])
ir.species=rep(c("one","two"),each=dim(pca_groups)[1])
# apply PCA - scale. = TRUE is highly 
# advisable, but default is FALSE. 
row.names(pca_logGroups)=1:dim(pca_bound)[1]
xkcd(pca_logGroups,ir.species)






pca_groups=tapply(alldata$Biomass,list(alldata$Simnum,alldata$Exper,alldata$Nodes_df),mean)
pca_groups=pca_groups[,,c("Fish_tot_df","inverts_tot_df","basal_tot_df")]
pca_bound=rbind(pca_groups[,,1],pca_groups[,,2],pca_groups[,,3])
# log transform 
pca_logGroups=log10(pca_bound+0.1)
ir.species=rep(c("Fish_tot_df","inverts_tot_df","basal_tot_df"),each=dim(pca_groups)[1])
# apply PCA - scale. = TRUE is highly 
# advisable, but default is FALSE. 
row.names(pca_logGroups)=1:dim(pca_bound)[1]
xkcd(pca_logGroups,ir.species)










