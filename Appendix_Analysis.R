#library(ggbiplot)
library(tidyverse)
library(stringr)
library(knitr)
z=0

## ----load_data-----------------------------------------------------------
DATE="2017Nov28"
Version="0"
location="/GIT/Analysis"#For Running on my Mac
#location=""#For Clusters
run_name=paste0(DATE,"_",Version)
setwd(paste0("~/",location,"/",run_name))
pardefault <- par()
#---- LOAD_DATA ----
dat=read.table("clean.txt",header=F)
colnames(dat)=as.matrix(read.table("colnames_clean.txt"))
colnames(dat)[9]="Model"
# The first step should be setting up better names, so the legends will automatically be named properly. This is to avoid having vague graphs with names like "model 1" and model 2" because people will definitely forget what that means.
exper_name=c("Original Web","Extended Web","Leslie & History")
# Make Processing faster, I think? - DONT USE THIS YET - IT WILL BREAK THINGS!
# dat=dat %>% mutate_at(c("isfish","basal_ls"),as.logical) %>%
#	mutate_at(c("Phase_df","Nodes_df","Seed","Model","pred","prey","species","lifestage"),as.factor)

## ----DefineMultiplot-----------------------------------------------------
# From https://stackoverflow.com/a/24387436
# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

## ----Percent_Stable, include=FALSE---------------------------------------
# Subset the data that fit criteria 1 and 2
subdat_ls=dat %>% filter(Year_df==max(Year_df),isfish==1) %>% 
	group_by(simnum,Model) %>%
	summarise(Tot_species=sum(Biomass)) %>% # But now it needs to survive in ALL models
	summarise(any=sum(Tot_species),all=prod(Tot_species)) %>%
	mutate_at(c("any","all"),as.logical)
persist=subdat_ls %>% summarise_at(c("any","all"),mean)*100
# Criteria 1: Probability of fish persisting in at least one of the models
# Criteria 2: Probability of fish persisting in all of the models
subdat1=dat %>% filter(simnum %in% (subdat_ls %>% filter(any==TRUE))$simnum)
subdat2=dat %>% filter(simnum %in% (subdat_ls %>% filter(all==TRUE))$simnum)

## ----Von-Bert-Multi, fig.cap=cap-----------------------------------------
# Caution: The clunky formatting here is because we need to plot ALL life stages if a single life stage survives. This way you won't end up with a partial line between two life stages
postscript("Figure1_Von-Bert-Multi.eps",horiz=FALSE,width=8.5,height=11)

subdat1 %>% filter(simnum<20,Model==3,Year_df==max(Year_df),isfish==1) %>%
	group_by(simnum,species) %>%
	mutate(tot_fish_biom=sum(Biomass)) %>% # Make sure you only grab surviving species, but make sure you grab ALL nodes
	filter(tot_fish_biom>0) %>% # Can't filter by Biomass>0 because some life stages go extinct and you end up with incomplete curves
	ungroup() %>%
	mutate(species=factor(as.integer(species)+(39*simnum)),lifestage=as.integer(lifestage),simnum=as.factor(simnum)) %>% 
	#select(simnum,species,Biomass, tot_fish_biom,Mass)
	ggplot(aes(x=lifestage, y=log10(Mass))) + geom_line(aes(group=species, colour=simnum)) + labs(x="Fish life stage",y="log of individual body mass")

z=z+1;VBmult=z
cap=paste("Figure",VBmult,"Von-Bertalanffy curves for surviving fish in several simulated food webs. Each colour represents a different food web.")
dev.off()

## ----Von-Bert-Multi2, fig.cap=cap----------------------------------------
VB_orig_fish=subdat1 %>% filter(simnum<20,Model==3,Year_df==max(Year_df),isfish==1) %>%
	group_by(simnum,species) %>%
	mutate(tot_fish_biom=sum(Biomass)) %>% # Make sure you only grab surviving species, but make sure you grab ALL nodes
	ungroup() %>%
	mutate(species=factor(as.integer(species)+(39*simnum)),lifestage=as.integer(lifestage),simnum=as.factor(simnum)) %>% 
	#select(simnum,species,Biomass, tot_fish_biom,Mass)
	ggplot(aes(x=lifestage, y=log10(Mass))) + geom_line(aes(group=species, colour=simnum)) + labs(x="Fish life stage",y="log of individual\n body mass")

# Caution: The clunky formatting here is because we need to plot ALL life stages if a single life stage survives. This way you won't end up with a partial line between two life stages
VB_end_fish=subdat1 %>% filter(simnum<20,Year_df==max(Year_df),isfish==1) %>%
	group_by(Model,simnum,species) %>%
	mutate(tot_fish_biom=sum(Biomass)) %>% # Make sure you only grab surviving species, but make sure you grab ALL nodes
	filter(tot_fish_biom>0) %>% # Can't filter by Biomass>0 because some life stages go extinct and you end up with incomplete curves
	ungroup() %>%
	mutate(species=factor(as.integer(species)+(39*simnum)),lifestage=as.integer(lifestage),simnum=as.factor(simnum)) %>% 
	group_by(Model) %>%
	do(g=ggplot(.,aes(x=lifestage, y=log10(Mass))) + geom_line(aes(group=species, colour=simnum))  + labs(x="Fish life stage",y="log of individual\n body mass"))

#multiplot(VB_orig_fish, VB_end_fish$g[[1]], VB_end_fish$g[[2]], VB_end_fish$g[[3]],cols=1)

#z=z+1;VBmult2=z
#cap=paste("Figure",VBmult2,"Von-Bertalanffy curves for surviving fish in several simulated food webs. Each colour represents a different food web.")

## ----VB-Hist-------------------------------------------------------------
VB_orig=dat %>% filter(Year_df==1,Model==1) %>%
	group_by(simnum) %>%
	mutate(scaled_mass=10^5*Mass/max(Mass)) %>%
	filter(lifestage==4)
VB_hist_orig = VB_orig %>% ggplot(.,aes(Z))+geom_histogram() + coord_cartesian(xlim=c(min(VB_orig$Z),max(VB_orig$Z))) + labs(x="Allometric Ratio")

VB_hist=subdat1 %>% filter(Year_df==max(Year_df),Biomass>0) %>%
	group_by(Model,simnum) %>%
	mutate(scaled_mass=10^5*Mass/max(Mass)) %>%
	filter(lifestage==4)

## ----VB_Exper_compare, fig.cap=cap,echo=FALSE----------------------------
meh=VB_hist %>% group_by(Model) %>%
	do(k=ggplot(.,aes(Z))+geom_histogram()+coord_cartesian(xlim=c(min(VB_orig$Z),max(VB_orig$Z))) + labs(x="Allometric Ratio"))

postscript("Figure2_VB_Exper_compare.eps",horiz=FALSE,width=8.5,height=11)
multiplot(VB_orig_fish, VB_end_fish$g[[1]], VB_end_fish$g[[2]], VB_end_fish$g[[3]],
		  VB_hist_orig, meh$k[[1]], meh$k[[2]], meh$k[[3]], cols=2)

z=z+1;VB_Exper_compare=z
cap=paste("Figure",VB_Exper_compare,"A histogram of the allometric ratios for all the surviving adult fish life stages.")
dev.off()

## ----Mass_Overlap, fig.cap=cap-------------------------------------------
mass_overlap=dat %>% filter(Year_df==1,Model==1,isfish==1,lifestage %in% range(lifestage)) %>%
	select(simnum,species,lifestage,Mass) %>%
	group_by(simnum) %>%
	spread(key=lifestage,value=Mass) %>%
	summarise(youngest_large=max(`1`),oldest_small=min(`4`)) %>%
	mutate(range_overlap=youngest_large<oldest_small,size_ratio=oldest_small/youngest_large)
percent_range_overlap=mass_overlap %>% summarise(100*sum(range_overlap)/n())

postscript("SuppFigure1_AllometricOverlap.eps",horiz=FALSE,width=8.5,height=11)
hist(log10(mass_overlap$size_ratio),main="",xlab="Allometric ratio of the smallest fish adult\n to the youngest life stage of the largest fish (log10)")

z=z+1;mass_overlap_cap=z
cap=paste("Supplementary Figure",mass_overlap_cap,"A histogram of the logged allometric ratios between the oldest life stage of the smallest fish species and the youngest life stage of the largest fish species for any model.")
dev.off()

## ----TS_solo, fig.cap=cap------------------------------------------------
postscript("Figure3_TS_solo.eps",horiz=FALSE,width=8.5,height=11)
subdat2 %>% filter(Model==3,simnum==unique(simnum)[4]) %>%
	mutate(lifestage=as.factor(lifestage),species=as.factor(isfish*species)) %>%
	group_by(Year_df,lifestage,species) %>%
	mutate(Biomass=sum(Biomass)) %>% ungroup() %>% mutate(species=c("Other","Fish 1","Fish 2","Fish 3")[species]) %>%
	ggplot(aes(x=Year_df,y=log10(Biomass))) + geom_line(aes(group=Nodes_df, colour=species,linetype=lifestage)) + labs(x="Year",y="Biomass (log 10)")

z=z+1;TSsolo=z
cap=paste("Figure",TSsolo,"A typical time series for model 1. This shows the logged biomass at the end of each year cycle for each fish life stage along with the combined biomass of the rest of the ecosystem.")
dev.off()

## ----freq_extant_fish, fig.cap=cap---------------------------------------
xkcd=dat %>% filter(Year_df==max(Year_df),isfish==1) %>%
	group_by(Model,simnum,species) %>% 
	summarise(extant=as.logical(sum(Biomass))) %>%
	summarise(Num_extant=sum(extant)) %>%
	ungroup() %>% mutate(Model=as.factor(Model)) %>%
	group_by(Num_extant,Model) %>%
	summarise(n=n()) %>% group_by(Model) %>%
	mutate(Freq=n/sum(n))

xkcd1=xkcd %>% 
	ggplot(., aes(x=Num_extant, y=Freq)) + geom_point(aes(group=Model, color=Model))+ labs(x="Number of surviving fish species", y="Frequency of simulations")
xkcd2=xkcd %>% filter(Num_extant %in% range(Num_extant)) %>%
	mutate(Num_extant=as.factor(Num_extant)) %>%
	ggplot(., aes(x=Num_extant, y=Freq)) + geom_point(aes(group=Model, color=Model))+ labs(x="Number of surviving fish species", y="Frequency of simulations")+ scale_x_discrete(labels=c("None","All"))
postscript("Figure5_freq_extant_fish.eps",horiz=FALSE,width=8.5,height=11)
multiplot(xkcd1,xkcd2)

z=z+1;freq_ex_fish=z
cap=paste("Figure",freq_ex_fish,"The frequency of fish surviving in each model.")
dev.off()

## ----CV_total, fig.cap=cap-----------------------------------------------
# Coefficient of Variation Function
CV <- function(dat){sd(dat)/mean(dat)*100}
sem <- function(x) {sd(x,na.rm=T)/sqrt(sum(is.na(x)))} # Standard error

CV_plot=subdat2 %>% group_by(Model,simnum,Year_df) %>%
	filter(Phase_df==2) %>%
	summarise(Tot_bio=sum(Biomass),Tot_fish=sum(isfish*Biomass)) %>%
	summarise(CV_tot=CV(Tot_bio),CV_fish=CV(Tot_fish)) # %>% ### So here we see the CVs for fish and total
	#summarise_at(c("CV_tot","CV_fish"),c(mean,sem))

boxplot(CV_tot~Model,CV_plot,xlab="Model",ylab="Coefficient of Variation")

#ggplot(CV_plot,aes(x=Model,y=CV_tot))+geom_boxplot() + xlab("Model") + ylab("Mean Coefficient of Variation") + labs(caption="Figure 3 Box plots for the CV of the total biomass.")

z=z+1;totCV=z
cap=paste("Figure",totCV,"The coefficient of variation for the total biomass in each model")

## ----CV_fish, fig.cap=cap, include=F-------------------------------------
boxplot(CV_fish~Model,CV_plot,xlab="Model",ylab="Coefficient of Variation") 
#	labs(caption="Figure 4 The CV of total fish biomass.")

z=z+1;fishCV=z
cap=paste("Figure",fishCV,"The coefficient of variation for the total fish biomass in each model")



######################################################
######################################################
######################################################
######################################################
######################################################







subdat2 %>% filter(Model==3,simnum==unique(simnum)[4]) %>%
	mutate(lifestage=as.factor(lifestage),species=as.factor(isfish*species)) %>%
	group_by(Year_df,lifestage,species) %>%
	mutate(Biomass=sum(Biomass)) %>% ungroup() %>% mutate(species=c("Other","Fish 1","Fish 2","Fish 3")[species]) %>%
	ggplot(aes(x=Year_df,y=log10(Biomass))) + geom_line(aes(group=Nodes_df, colour=species,linetype=lifestage)) + labs(x="Year",y="Biomass (log 10)")




# Percent of webs that stabilize for any species in every model
min_viable_webs=dat %>% filter(Year_df==max(Year_df)) %>% 
	group_by(simnum,Model) %>%
	summarise(Tot_species=sum(Biomass)) %>% # But now it needs to survive in ALL models
	summarise(any=sum(Tot_species),all=prod(Tot_species)) %>%
	mutate_at(c("any","all"),as.logical) %>% 
	summarise_at("all",mean)*100



extant_nodes=dat %>% filter(Year_df==max(Year_df)) %>%
	group_by(Model,simnum,Nodes_df) %>% 
	summarise(extant=as.logical(Biomass)) %>%
	summarise(Num_extant=sum(extant)) %>% 
	mutate(Per_extant=Num_extant/c(39,39,30)[Model])
boxplot(Per_extant~Model,extant_nodes,xlab="Model",ylab="Percent of surviving nodes")
tbl_nodes=extant_nodes %>% summarise(mean(Per_extant),var(Per_extant))





extant_species=dat %>% filter(Year_df==max(Year_df)) %>%
	group_by(Model,simnum,species) %>% 
	summarise(extant=as.logical(sum(Biomass))) %>%
	summarise(Num_extant=sum(extant)) #%>%
boxplot(Num_extant~Model,extant_species,xlab="Model",ylab="Number of surviving species")
tbl_species=extant_species %>% summarise(mean(Num_extant),var(Num_extant))




extant_fish=dat %>% filter(Year_df==max(Year_df),isfish==1) %>%
	group_by(Model,simnum,species) %>% 
	summarise(extant=as.logical(sum(Biomass))) %>%
	summarise(Num_extant=sum(extant)) #%>%
boxplot(Num_extant~Model,extant_fish,xlab="Model",ylab="Number of surviving fish")
tbl_fish=extant_fish %>% summarise(mean(Num_extant),var(Num_extant))




dat %>% filter(Year_df==max(Year_df),isfish==1) %>%
	group_by(Model,simnum,species) %>% 
	summarise(extant=as.logical(sum(Biomass))) %>%
	summarise(Num_extant=sum(extant)) %>%
	ungroup() %>% mutate(Model=as.factor(Model)) %>%
	ggplot(.,aes(Num_extant,group=Model,fill=Model)) + geom_histogram(position="dodge",binwidth=0.5) + theme_bw() + labs(x="Number of surviving fish species") + theme(legend.position=c(0.85,0.8))




dat %>% filter(Year_df==max(Year_df),isfish==1) %>%
	group_by(Model,simnum,species) %>% 
	summarise(extant=as.logical(sum(Biomass))) %>%
	summarise(Num_extant=sum(extant)) %>%
	ungroup() %>% mutate(Model=as.factor(Model)) %>%
	group_by(Num_extant,Model) %>%
	summarise(n=n()) %>% group_by(Model) %>%
	mutate(Freq=n/sum(n)) %>%
	ggplot(., aes(x=Num_extant, y=Freq)) + geom_point(aes(group=Model, color=Model))+ labs(x="Number of surviving fish species", y="Frequency of simulations")





# Coefficient of Variation Function
CV <- function(dat){sd(dat)/mean(dat)*100}
sem <- function(x) {sd(x,na.rm=T)/sqrt(sum(is.na(x)))} # Standard error

CV_plot=subdat2 %>% group_by(Model,simnum,Year_df) %>%
	filter(Phase_df==2) %>%
	summarise(Tot_bio=sum(Biomass),Tot_fish=sum(isfish*Biomass)) %>%
	summarise(CV_tot=CV(Tot_bio),CV_fish=CV(Tot_fish)) # %>% ### So here we see the CVs for fish and total
#summarise_at(c("CV_tot","CV_fish"),c(mean,sem))

boxplot(CV_tot~Model,CV_plot,xlab="Model",ylab="Coefficient of Variation")

#ggplot(CV_plot,aes(x=Model,y=CV_tot))+geom_boxplot() + xlab("Model") + ylab("Mean Coefficient of Variation") + labs(caption="Figure 3 Box plots for the CV of the total biomass.")




boxplot(CV_fish~Model,CV_plot,xlab="Model",ylab="Coefficient of Variation") 
#	labs(caption="Figure 4 The CV of total fish biomass.")



ratio=dat %>% group_by(simnum, Model,isfish) %>%
	filter(Year_df %in% max(Year_df)) %>% 
	summarise(Tot_group=sum(Biomass)) %>%
	spread(isfish,Tot_group) %>%
	mutate(Fish_ratio=`1`/(`0`+`1`)) %>%
	group_by(Model) %>%
	#ggboxplot(x="Model",y="Fish_ratio")
	summarise(mean(Fish_ratio,na.rm=T),var(Fish_ratio,na.rm=T))
kable(ratio)



dat %>% filter(simnum==3,Model==3,Year_df==1,isfish==1,Biomass>0) %>%
	mutate(species=factor(species),lifestage=as.integer(lifestage)) %>%
	ggplot(aes(x=lifestage, y=Mass, colour=species)) + geom_line() + labs(x="Fish life stage",y="Individual body mass")




# Caution: The clunky formatting here is because we need to plot ALL life stages if a single life stage survives. This way you won't end up with a partial line between two life stages
subdat1 %>% filter(simnum<20,Model==3,Year_df==max(Year_df),isfish==1) %>%
	group_by(simnum,species) %>%
	mutate(tot_fish_biom=sum(Biomass)) %>% # Make sure you only grab surviving species, but make sure you grab ALL nodes
	filter(tot_fish_biom>0) %>% # Can't filter by Biomass>0 because some life stages go extinct and you end up with incomplete curves
	ungroup() %>%
	mutate(species=factor(as.integer(species)+(39*simnum)),lifestage=as.integer(lifestage),simnum=as.factor(simnum)) %>% 
	#select(simnum,species,Biomass, tot_fish_biom,Mass)
	ggplot(aes(x=lifestage, y=log10(Mass))) + geom_line(aes(group=species, colour=simnum)) + labs(x="Fish life stage",y="log of individual body mass")




VB_grid=dat %>% filter(simnum<10,Model==3,Year_df==1,isfish==1,Biomass>0) %>%
	mutate(species=factor(species),lifestage=as.integer(lifestage)) %>%
	group_by(simnum) %>%
	do(g=ggplot(.,aes(x=lifestage, y=Mass, colour=species)) + geom_line() + labs(x="Life stage") + theme(legend.position="none"))
multiplot(VB_grid$g[[1]], VB_grid$g[[2]], VB_grid$g[[3]], VB_grid$g[[4]], VB_grid$g[[5]], VB_grid$g[[6]], VB_grid$g[[7]], VB_grid$g[[8]], VB_grid$g[[9]], cols=3)



VB_hist=subdat1 %>% filter(Model==3,Year_df==max(Year_df),Biomass>0) %>%
	group_by(simnum) %>%
	mutate(scaled_mass=10^5*Mass/max(Mass)) %>%
	filter(lifestage==4)



VB_hist %>% with(hist(Z,xlab="Allometric Ratio",main=""))

VB_hist=subdat1 %>% filter(Model==1,Year_df==max(Year_df),Biomass>0) %>%
	group_by(simnum) %>%
	mutate(scaled_mass=10^5*Mass/max(Mass)) %>%
	filter(lifestage==4)



VB_hist %>% with(hist(Z,xlab="Allometric Ratio",main=""))

VB_hist=subdat1 %>% filter(Model==3,Year_df==max(Year_df)) %>%
	group_by(simnum) %>%
	mutate(scaled_mass=10^5*Mass/max(Mass)) %>%
	filter(lifestage==4)



VB_hist %>% with(hist(Z,xlab="Allometric Ratio",main=""))




VB_hist %>% with(hist(log10(Mass),xlab="log of body mass (unitless)",main=""))




# First setup plots where you just find life his stats for largest surviving fish species and compare them to the tot fish biomass (all species combined) and total biomass
sim_stats=subdat2 %>% filter(Year_df==max(Year_df),Model==3) %>%
	group_by(simnum,Model,species) %>% 
	mutate(Tot_spec=sum(Biomass)) %>%
	filter(Tot_spec>0) %>% # Important: Filter out extinct species first so you only get stats for surviving species
	group_by(simnum,Model) %>% 
	summarise(Tot_Bio=sum(Biomass),Tot_fish=sum(Biomass*isfish),max_Z=max(Z),max_Mass=max(Mass),max_fish_mass=max(Mass*isfish))

CV_plot=subdat2 %>% group_by(Model,simnum,Year_df) %>%
	filter(Phase_df==2) %>%
	summarise(Tot_bio=sum(Biomass),Tot_fish=sum(isfish*Biomass)) %>%
	summarise(CV_tot=CV(Tot_bio),CV_fish=CV(Tot_fish),mean_tot=mean(Tot_bio),mean_fish=mean(Tot_fish))

full_stats=left_join(sim_stats,CV_plot) %>% 
	mutate(log_tot=log10(mean_tot),log_fish=log10(mean_fish),log_max_mass=log10(max_Mass),log_max_fish_mass=log10(max_fish_mass),FT_ratio=mean_fish/mean_tot)



# Also setup plots where you're looking at individual surviving fish species, and comparing their life history stats to that specific species' biomass.
# Where it's by species W_infty against (that same species') final biomass and CV
CV_spec_stats=subdat2 %>% filter(Phase_df==2,Model==3) %>%
	group_by(simnum,Model,species,Year_df) %>%
	summarise(Tot_spec=sum(Biomass)) %>%
	summarise(CV_spec=CV(Tot_spec),mean_spec=mean(Tot_spec))

CV_tot_stats=subdat2 %>% filter(Phase_df==2,Model==3) %>%
	group_by(simnum,Model,Year_df) %>%
	summarise(Tot_Bio=sum(Biomass)) %>%
	summarise(CV_tot=CV(Tot_Bio),mean_tot=mean(Tot_Bio))

gen_spec_stats=subdat2 %>% filter(Year_df==max(Year_df),Model==3) %>%
	group_by(simnum,Model,species) %>%
	mutate(Tot_spec=sum(Biomass),max_Z=max(Z),max_Mass=max(Mass)) %>%
	filter(Tot_spec>0,isfish==1,lifestage==1)

all_spec_stats=left_join(gen_spec_stats,CV_spec_stats)
all_spec_stats=left_join(all_spec_stats,CV_tot_stats) %>% 
	mutate(log_spec=log10(mean_spec),log_tot=log10(mean_tot),log_max_mass=log10(max_Mass)) %>%
	filter(!(simnum==11 && species==30)) # Remove that outlier!!



xkcd=matrix(NA,5,4)
lm_test=matrix(NA,5,4)
corr_test=matrix(NA,5,4)

res <- globalenv()

plot_relations <- function(dat,xvar,yvar,xlab,ylab){
	dat = dat %>% 
		mutate_(xvar=xvar,yvar=yvar)
	# Correlation Values
	cor_val=cor.test(dat$xvar,dat$yvar)
	cor_vals=paste0("t=",round(cor_val$statistic,2),", df=",cor_val$parameter,", p=",round(cor_val$p.value,3))
	cor_pval=cor_val$p.value
	# lm values
	mod=lm(yvar~xvar,dat)
	lm_pval=summary(mod)$coefficients[2,"Pr(>|t|)"]
	# Significance Indicator 
	sig_val=cor_pval
	# Plot the graph
	graph=dat %>% 
		ggplot(aes(x=xvar, y=yvar)) + 
		geom_point() + 
		labs(x=xlab,y=ylab,title=paste0(c("a","b","c","d")[xk_plot],sig_val)) + 
		geom_smooth(method = "lm")
	# Return values
	envir[[ "corr_test" ]][xk_fig,xk_plot] <- cor_vals
	envir[[ "lm_test" ]][xk_fig,xk_plot] <- lm_pval
	return(graph)
}

xk_fig=1 # The figure (in relation to all these figures)
xk_plot=1 # The plot number within the figure
k=plot_relations(iris,quo(Sepal.Length),quo(Sepal.Width),"Allometric Ratio","CV of total biomass")

xk1=full_stats %>% ggplot(aes(x=max_Z,y=log_tot)) + geom_point() + labs(x="Allometric Ratio",y="log of total biomass",title="a")+geom_smooth(method = "lm")
xk2=full_stats %>% ggplot(aes(x=max_Z,y=log_fish)) + geom_point() + labs(x="Allometric Ratio",y="log of fish biomass",title="b**")+geom_smooth(method = "lm")
xk3=full_stats %>% ggplot(aes(x=max_Z,y=CV_tot)) + geom_point() + labs(x="Allometric Ratio",y="CV of total biomass",title="c*")+geom_smooth(method = "lm")
xk4=full_stats %>% ggplot(aes(x=max_Z,y=CV_fish)) + geom_point() + labs(x="Allometric Ratio",y="CV of fish biomass",title="d")+geom_smooth(method = "lm")


mod1=lm(log_tot~max_Z,full_stats)
mod2=lm(log_fish~max_Z,full_stats)
mod3=lm(CV_tot~max_Z,full_stats)
mod4=lm(CV_fish~max_Z,full_stats)
summary(mod1)
summary(mod2)
summary(mod3)
summary(mod4)

xkcd[1,1]=summary(mod1)$coefficients[2,"Pr(>|t|)"]
xkcd[1,2]=summary(mod2)$coefficients[2,"Pr(>|t|)"]
xkcd[1,3]=summary(mod3)$coefficients[2,"Pr(>|t|)"]
xkcd[1,4]=summary(mod4)$coefficients[2,"Pr(>|t|)"]
postscript("Figure6_Model3_row1_Allometric_full_stats.eps",horiz=FALSE,width=8.5,height=11)
multiplot(xk1,xk2,xk3,xk4,cols=2)
dev.off()


xk1=full_stats %>% ggplot(aes(x=log_max_fish_mass,y=log_tot)) + geom_point() + labs(x="log of fish asymptotic body mass",y="log of total biomass",title="a**")+geom_smooth(method = "lm")
xk2=full_stats %>% ggplot(aes(x=log_max_fish_mass,y=log_fish)) + geom_point() + labs(x="log of fish asymptotic body mass",y="log of fish biomass",title="b")+geom_smooth(method = "lm")
xk3=full_stats %>% ggplot(aes(x=log_max_fish_mass,y=CV_tot)) + geom_point() + labs(x="log of fish asymptotic body mass",y="CV of total biomass",title="c")+geom_smooth(method = "lm")
xk4=full_stats %>% ggplot(aes(x=log_max_fish_mass,y=CV_fish)) + geom_point() + labs(x="log of fish asymptotic body mass",y="CV of fish biomass",title="d")+geom_smooth(method = "lm")

mod1=lm(log_tot~log_max_fish_mass,full_stats)
mod2=lm(log_fish~log_max_fish_mass,full_stats)
mod3=lm(CV_tot~log_max_fish_mass,full_stats)
mod4=lm(CV_fish~log_max_fish_mass,full_stats)
summary(mod1)
summary(mod2)
summary(mod3)
summary(mod4)

xkcd[2,1]=summary(mod1)$coefficients[2,"Pr(>|t|)"]
xkcd[2,2]=summary(mod2)$coefficients[2,"Pr(>|t|)"]
xkcd[2,3]=summary(mod3)$coefficients[2,"Pr(>|t|)"]
xkcd[2,4]=summary(mod4)$coefficients[2,"Pr(>|t|)"]
postscript("Figure7_Model3_row2_logmass_full_stats.eps",horiz=FALSE,width=8.5,height=11)
multiplot(xk1,xk2,xk3,xk4,cols=2)
dev.off()


xk1=all_spec_stats %>% ggplot(aes(x=max_Z,y=CV_spec)) + geom_point() + labs(x="Allometric Ratio",y="CV of fish biomass",title="a")+geom_smooth(method = "lm")
xk2=all_spec_stats %>% ggplot(aes(x=max_Z,y=log_spec)) + geom_point() + labs(x="Allometric Ratio",y="log of fish biomass",title="b")+geom_smooth(method = "lm")
xk3=all_spec_stats %>% ggplot(aes(x=orig_T,y=CV_spec)) + geom_point() + labs(x="Trophic Level",y="CV of fish biomass",title="c")+geom_smooth(method = "lm")
xk4=all_spec_stats %>% ggplot(aes(x=orig_T,y=log_spec)) + geom_point() + labs(x="Trophic Level",y="log of fish biomass",title="d*")+geom_smooth(method = "lm")

mod1=lm(CV_spec~max_Z,all_spec_stats)
mod2=lm(log_spec~max_Z,all_spec_stats)
mod3=lm(CV_spec~orig_T,all_spec_stats)
mod4=lm(log_spec~orig_T,all_spec_stats)
summary(mod1)
summary(mod2)
summary(mod3)
summary(mod4)

xkcd[3,1]=summary(mod1)$coefficients[2,"Pr(>|t|)"]
xkcd[3,2]=summary(mod2)$coefficients[2,"Pr(>|t|)"]
xkcd[3,3]=summary(mod3)$coefficients[2,"Pr(>|t|)"]
xkcd[3,4]=summary(mod4)$coefficients[2,"Pr(>|t|)"]
postscript("SuppFigure2_Model3_row3_all_stats.eps",horiz=FALSE,width=8.5,height=11)
multiplot(xk1,xk2,xk3,xk4,cols=2)
dev.off()


xk1=all_spec_stats %>% ggplot(aes(x=log_max_mass,y=log_tot)) + geom_point() + labs(x="log of fish asymptotic body mass",y="log of total biomass",title="a***")+geom_smooth(method = "lm")
xk2=all_spec_stats %>% ggplot(aes(x=log_max_mass,y=log_spec)) + geom_point() + labs(x="log of fish asymptotic body mass",y="log of fish biomass",title="b*")+geom_smooth(method = "lm")
xk3=all_spec_stats %>% ggplot(aes(x=log_max_mass,y=CV_tot)) + geom_point() + labs(x="log of fish asymptotic body mass",y="CV of total biomass",title="c")+geom_smooth(method = "lm")
xk4=all_spec_stats %>% ggplot(aes(x=log_max_mass,y=CV_spec)) + geom_point() + labs(x="log of fish asymptotic body mass",y="CV of fish biomass",title="d")+geom_smooth(method = "lm")

mod1=lm(log_tot~log_max_mass,all_spec_stats)
mod2=lm(log_spec~log_max_mass,all_spec_stats)
mod3=lm(CV_tot~log_max_mass,all_spec_stats)
mod4=lm(CV_spec~log_max_mass,all_spec_stats)
summary(mod1)
summary(mod2)
summary(mod3)
summary(mod4)

xkcd[4,1]=summary(mod1)$coefficients[2,"Pr(>|t|)"]
xkcd[4,2]=summary(mod2)$coefficients[2,"Pr(>|t|)"]
xkcd[4,3]=summary(mod3)$coefficients[2,"Pr(>|t|)"]
xkcd[4,4]=summary(mod4)$coefficients[2,"Pr(>|t|)"]
postscript("Figure8_Model3_row4_all_stats2.eps",horiz=FALSE,width=8.5,height=11)
multiplot(xk1,xk2,xk3,xk4,cols=2)
dev.off()


xk1=full_stats %>% ggplot(aes(x=max_Z,y=FT_ratio)) + geom_point() + labs(x="Allometric Ratio",y="Fish to total biomass ratio",title="a")+geom_smooth(method = "lm")
xk2=full_stats %>% ggplot(aes(x=log_max_fish_mass,y=FT_ratio)) + geom_point() + labs(x="log of fish mass",y="Fish to total biomass ratio",title="b")+geom_smooth(method = "lm")

mod1=lm(FT_ratio~max_Z,full_stats)
mod2=lm(FT_ratio~log_max_fish_mass,full_stats)
summary(mod1)
summary(mod2)

xkcd[5,1]=summary(mod1)$coefficients[2,"Pr(>|t|)"]
xkcd[5,2]=summary(mod2)$coefficients[2,"Pr(>|t|)"]
postscript("SuppFigure3_Model3_row5_full_stats.eps",horiz=FALSE,width=8.5,height=11)
multiplot(xk1,xk2,cols=2)
dev.off()



postscript("Figure4_CV_boxplot.eps",horiz=FALSE,width=8.5,height=11)
par(mfrow=c(2,1), mai = c(0.7, 1, 0.5, 0.1),mgp=c(2,1,0))
boxplot(CV_tot~Model,CV_plot,xlab="Model Type",ylab="Coefficient of Variation",main="Total Ecosystem Biomass",ylim=c(0,100))

boxplot(CV_fish~Model,CV_plot,xlab="Model Type",ylab="Coefficient of Variation",main="Total Fish Biomass",ylim=c(0,100))
dev.off()
par(pardefault)



