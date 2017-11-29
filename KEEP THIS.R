

#---- Hidden ----
library(ggbiplot)
library(tidyverse)
library(stringr)
library(knitr)
z=0


DATE="2017Nov28"
Version="0"
location="/GIT/Analysis"#For Running on my Mac
#location=""#For Clusters
run_name=paste0(DATE,"_",Version)
setwd(paste0("~/",location,"/",run_name))
#---- LOAD_DATA ----
dat=read.table("clean.txt",header=F)
colnames(dat)=as.matrix(read.table("colnames_clean.txt"))
# The first step should be setting up better names, so the legends will automatically be named properly. This is to avoid having vague graphs with names like "experiment 1" and experiment 2" because people will definitely forget what that means.
exper_name=c("Leslie & History","Extended Web","Original Web")
# Make Processing faster, I think? - DONT USE THIS YET - IT WILL BREAK THINGS!
# dat=dat %>% mutate_at(c("isfish","basal_ls"),as.logical) %>%
#	mutate_at(c("Phase_df","Nodes_df","Seed","Exper","pred","prey","species","lifestage"),as.factor)


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



# Subset the data that fit criteria 1 and 2
subdat_ls=dat %>% filter(Year_df==max(Year_df),isfish==1) %>% 
	group_by(simnum,Exper) %>%
	summarise(Tot_species=sum(Biomass)) %>% # But now it needs to survive in ALL experiments
	summarise(any=sum(Tot_species),all=prod(Tot_species)) %>%
	mutate_at(c("any","all"),as.logical)
persist=subdat_ls %>% summarise_at(c("any","all"),mean)*100
# Criteria 1: Probability of fish persisting in at least one of the experiments
# Criteria 2: Probability of fish persisting in all of the experiments
subdat1=dat %>% filter(simnum %in% (subdat_ls %>% filter(any==TRUE))$simnum)
subdat2=dat %>% filter(simnum %in% (subdat_ls %>% filter(all==TRUE))$simnum)



subdat2 %>% filter(Exper==1,simnum==unique(simnum)[4]) %>%
	mutate(lifestage=as.factor(lifestage),species=as.factor(isfish*species)) %>%
	group_by(Year_df,lifestage,species) %>%
	mutate(Biomass=sum(Biomass)) %>% ungroup() %>% mutate(species=c("Other","Fish 1","Fish 2","Fish 3")[species]) %>%
	ggplot(aes(x=Year_df,y=log10(Biomass))) + geom_line(aes(group=Nodes_df, colour=species,linetype=lifestage)) + labs(x="Year",y="Biomass (log 10)")




# Percent of webs that stabilize for any species in every experiment
min_viable_webs=dat %>% filter(Year_df==max(Year_df)) %>% 
	group_by(simnum,Exper) %>%
	summarise(Tot_species=sum(Biomass)) %>% # But now it needs to survive in ALL experiments
	summarise(any=sum(Tot_species),all=prod(Tot_species)) %>%
	mutate_at(c("any","all"),as.logical) %>% 
	summarise_at("all",mean)*100



extant_nodes=dat %>% filter(Year_df==max(Year_df)) %>%
	group_by(Exper,simnum,Nodes_df) %>% 
	summarise(extant=as.logical(Biomass)) %>%
	summarise(Num_extant=sum(extant)) %>% 
	mutate(Per_extant=Num_extant/c(39,39,30)[Exper])
boxplot(Per_extant~Exper,extant_nodes,xlab="Experiment",ylab="Percent of surviving nodes")
tbl_nodes=extant_nodes %>% summarise(mean(Per_extant),var(Per_extant))





extant_species=dat %>% filter(Year_df==max(Year_df)) %>%
	group_by(Exper,simnum,species) %>% 
	summarise(extant=as.logical(sum(Biomass))) %>%
	summarise(Num_extant=sum(extant)) #%>%
boxplot(Num_extant~Exper,extant_species,xlab="Experiment",ylab="Number of surviving species")
tbl_species=extant_species %>% summarise(mean(Num_extant),var(Num_extant))




extant_fish=dat %>% filter(Year_df==max(Year_df),isfish==1) %>%
	group_by(Exper,simnum,species) %>% 
	summarise(extant=as.logical(sum(Biomass))) %>%
	summarise(Num_extant=sum(extant)) #%>%
boxplot(Num_extant~Exper,extant_fish,xlab="Experiment",ylab="Number of surviving fish")
tbl_fish=extant_fish %>% summarise(mean(Num_extant),var(Num_extant))




dat %>% filter(Year_df==max(Year_df),isfish==1) %>%
	group_by(Exper,simnum,species) %>% 
	summarise(extant=as.logical(sum(Biomass))) %>%
	summarise(Num_extant=sum(extant)) %>%
	ungroup() %>% mutate(Exper=as.factor(Exper)) %>%
	ggplot(.,aes(Num_extant,group=Exper,fill=Exper)) + geom_histogram(position="dodge",binwidth=0.5) + theme_bw() + labs(x="Number of surviving fish species") + theme(legend.position=c(0.85,0.8))




dat %>% filter(Year_df==max(Year_df),isfish==1) %>%
	group_by(Exper,simnum,species) %>% 
	summarise(extant=as.logical(sum(Biomass))) %>%
	summarise(Num_extant=sum(extant)) %>%
	ungroup() %>% mutate(Exper=as.factor(Exper)) %>%
	group_by(Num_extant,Exper) %>%
	summarise(n=n()) %>% group_by(Exper) %>%
	mutate(Freq=n/sum(n)) %>%
	ggplot(., aes(x=Num_extant, y=Freq)) + geom_line(aes(group=Exper, color=Exper))+ labs(x="Number of surviving fish species", y="Frequency of simulations")





# Coefficient of Variation Function
CV <- function(dat){sd(dat)/mean(dat)*100}
sem <- function(x) {sd(x,na.rm=T)/sqrt(sum(is.na(x)))} # Standard error

CV_plot=subdat2 %>% group_by(Exper,simnum,Year_df) %>%
	filter(Phase_df==2) %>%
	summarise(Tot_bio=sum(Biomass),Tot_fish=sum(isfish*Biomass)) %>%
	summarise(CV_tot=CV(Tot_bio),CV_fish=CV(Tot_fish)) # %>% ### So here we see the CVs for fish and total
#summarise_at(c("CV_tot","CV_fish"),c(mean,sem))

boxplot(CV_tot~Exper,CV_plot,xlab="Experiment",ylab="Coefficient of Variation")

#ggplot(CV_plot,aes(x=Exper,y=CV_tot))+geom_boxplot() + xlab("Experiment") + ylab("Mean Coefficient of Variation") + labs(caption="Figure 3 Box plots for the CV of the total biomass.")




boxplot(CV_fish~Exper,CV_plot,xlab="Experiment",ylab="Coefficient of Variation") 
#	labs(caption="Figure 4 The CV of total fish biomass.")



ratio=dat %>% group_by(simnum, Exper,isfish) %>%
	filter(Year_df %in% max(Year_df)) %>% 
	summarise(Tot_group=sum(Biomass)) %>%
	spread(isfish,Tot_group) %>%
	mutate(Fish_ratio=`1`/(`0`+`1`)) %>%
	group_by(Exper) %>%
	#ggboxplot(x="Exper",y="Fish_ratio")
	summarise(mean(Fish_ratio),var(Fish_ratio))
kable(ratio)



dat %>% filter(simnum==3,Exper==1,Year_df==1,isfish==1,Biomass>0) %>%
	mutate(species=factor(species),lifestage=as.integer(lifestage)) %>%
	ggplot(aes(x=lifestage, y=Mass, colour=species)) + geom_line() + labs(x="Fish life stage",y="Individual body mass")




# Caution: The clunky formatting here is because we need to plot ALL life stages if a single life stage survives. This way you won't end up with a partial line between two life stages
subdat1 %>% filter(simnum<20,Exper==1,Year_df==max(Year_df),isfish==1) %>%
	group_by(simnum,species) %>%
	mutate(tot_fish_biom=sum(Biomass)) %>% # Make sure you only grab surviving species, but make sure you grab ALL nodes
	filter(tot_fish_biom>0) %>% # Can't filter by Biomass>0 because some life stages go extinct and you end up with incomplete curves
	ungroup() %>%
	mutate(species=factor(as.integer(species)+(39*simnum)),lifestage=as.integer(lifestage),simnum=as.factor(simnum)) %>% 
	#select(simnum,species,Biomass, tot_fish_biom,Mass)
	ggplot(aes(x=lifestage, y=log10(Mass))) + geom_line(aes(group=species, colour=simnum)) + labs(x="Fish life stage",y="log of individual body mass")




VB_grid=dat %>% filter(simnum<10,Exper==1,Year_df==1,isfish==1,Biomass>0) %>%
	mutate(species=factor(species),lifestage=as.integer(lifestage)) %>%
	group_by(simnum) %>%
	do(g=ggplot(.,aes(x=lifestage, y=Mass, colour=species)) + geom_line() + labs(x="Life stage") + theme(legend.position="none"))
multiplot(VB_grid$g[[1]], VB_grid$g[[2]], VB_grid$g[[3]], VB_grid$g[[4]], VB_grid$g[[5]], VB_grid$g[[6]], VB_grid$g[[7]], VB_grid$g[[8]], VB_grid$g[[9]], cols=3)



VB_hist=subdat1 %>% filter(Exper==1,Year_df==max(Year_df),Biomass>0) %>%
	group_by(simnum) %>%
	mutate(scaled_mass=10^5*Mass/max(Mass)) %>%
	filter(lifestage==4)



VB_hist %>% with(hist(Z,xlab="Allometric Ratio",main=""))





VB_hist %>% with(hist(log10(Mass),xlab="log of body mass (unitless)",main=""))




# First setup plots where you just find life his stats for largest surviving fish species and compare them to the tot fish biomass (all species combined) and total biomass
sim_stats=subdat2 %>% filter(Year_df==max(Year_df),Exper==1) %>%
	group_by(simnum,Exper,species) %>% 
	mutate(Tot_spec=sum(Biomass)) %>%
	filter(Tot_spec>0) %>% # Important: Filter out extinct species first so you only get stats for surviving species
	group_by(simnum,Exper) %>% 
	summarise(Tot_Bio=sum(Biomass),Tot_fish=sum(Biomass*isfish),max_Z=max(Z),max_Mass=max(Mass),max_fish_mass=max(Mass*isfish))

CV_plot=subdat2 %>% group_by(Exper,simnum,Year_df) %>%
	filter(Phase_df==2) %>%
	summarise(Tot_bio=sum(Biomass),Tot_fish=sum(isfish*Biomass)) %>%
	summarise(CV_tot=CV(Tot_bio),CV_fish=CV(Tot_fish),mean_tot=mean(Tot_bio),mean_fish=mean(Tot_fish))

full_stats=left_join(sim_stats,CV_plot) %>% 
	mutate(log_tot=log10(mean_tot),log_fish=log10(mean_fish),log_max_mass=log10(max_Mass),log_max_fish_mass=log10(max_fish_mass),FT_ratio=mean_fish/mean_tot)



# Also setup plots where you're looking at individual surviving fish species, and comparing their life history stats to that specific species' biomass.
# Where it's by species W_infty against (that same species') final biomass and CV
CV_spec_stats=subdat2 %>% filter(Phase_df==2,Exper==1) %>%
	group_by(simnum,Exper,species,Year_df) %>%
	summarise(Tot_spec=sum(Biomass)) %>%
	summarise(CV_spec=CV(Tot_spec),mean_spec=mean(Tot_spec))

CV_tot_stats=subdat2 %>% filter(Phase_df==2,Exper==1) %>%
	group_by(simnum,Exper,Year_df) %>%
	summarise(Tot_Bio=sum(Biomass)) %>%
	summarise(CV_tot=CV(Tot_Bio),mean_tot=mean(Tot_Bio))

gen_spec_stats=subdat2 %>% filter(Year_df==max(Year_df),Exper==1) %>%
	group_by(simnum,Exper,species) %>%
	mutate(Tot_spec=sum(Biomass),max_Z=max(Z),max_Mass=max(Mass)) %>%
	filter(Tot_spec>0,isfish==1,lifestage==1)

all_spec_stats=left_join(gen_spec_stats,CV_spec_stats)
all_spec_stats=left_join(all_spec_stats,CV_tot_stats) %>% 
	mutate(log_spec=log10(mean_spec),log_tot=log10(mean_tot),log_max_mass=log10(max_Mass)) %>%
	filter(!(simnum==11 && species==30)) # Remove that outlier!!



xk1=full_stats %>% ggplot(aes(x=max_Z,y=log_tot)) + geom_point() + labs(x="Allometric Ratio",y="log of total biomass")+geom_smooth(method = "lm")
xk2=full_stats %>% ggplot(aes(x=max_Z,y=log_fish)) + geom_point() + labs(x="Allometric Ratio",y="log of fish biomass")+geom_smooth(method = "lm")
xk3=full_stats %>% ggplot(aes(x=max_Z,y=CV_tot)) + geom_point() + labs(x="Allometric Ratio",y="CV of total biomass")+geom_smooth(method = "lm")
xk4=full_stats %>% ggplot(aes(x=max_Z,y=CV_fish)) + geom_point() + labs(x="Allometric Ratio",y="CV of fish biomass")+geom_smooth(method = "lm")

multiplot(xk1,xk2,xk3,xk4,cols=2)



xk1=full_stats %>% ggplot(aes(x=log_max_fish_mass,y=log_tot)) + geom_point() + labs(x="log of fish asymptotic body mass",y="log of total biomass")+geom_smooth(method = "lm")
xk2=full_stats %>% ggplot(aes(x=log_max_fish_mass,y=log_fish)) + geom_point() + labs(x="log of fish asymptotic body mass",y="log of fish biomass")+geom_smooth(method = "lm")
xk3=full_stats %>% ggplot(aes(x=log_max_fish_mass,y=CV_tot)) + geom_point() + labs(x="log of fish asymptotic body mass",y="CV of total biomass")+geom_smooth(method = "lm")
xk4=full_stats %>% ggplot(aes(x=log_max_fish_mass,y=CV_fish)) + geom_point() + labs(x="log of fish asymptotic body mass",y="CV of fish biomass")+geom_smooth(method = "lm")

multiplot(xk1,xk2,xk3,xk4,cols=2)



xk1=all_spec_stats %>% ggplot(aes(x=max_Z,y=CV_spec)) + geom_point() + labs(x="Allometric Ratio",y="CV of fish biomass")+geom_smooth(method = "lm")
xk2=all_spec_stats %>% ggplot(aes(x=max_Z,y=log_spec)) + geom_point() + labs(x="Allometric Ratio",y="log of fish biomass")+geom_smooth(method = "lm")
xk3=all_spec_stats %>% ggplot(aes(x=orig_T,y=CV_spec)) + geom_point() + labs(x="Trophic Level",y="CV of fish biomass")+geom_smooth(method = "lm")
xk4=all_spec_stats %>% ggplot(aes(x=orig_T,y=log_spec)) + geom_point() + labs(x="Trophic Level",y="log of fish biomass")+geom_smooth(method = "lm")

multiplot(xk1,xk2,xk3,xk4,cols=2)



xk1=all_spec_stats %>% ggplot(aes(x=log_max_mass,y=log_tot)) + geom_point() + labs(x="log of fish asymptotic body mass",y="log of total biomass")+geom_smooth(method = "lm")
xk2=all_spec_stats %>% ggplot(aes(x=log_max_mass,y=log_spec)) + geom_point() + labs(x="log of fish asymptotic body mass",y="log of fish biomass")+geom_smooth(method = "lm")
xk3=all_spec_stats %>% ggplot(aes(x=log_max_mass,y=CV_tot)) + geom_point() + labs(x="log of fish asymptotic body mass",y="CV of total biomass")+geom_smooth(method = "lm")
xk4=all_spec_stats %>% ggplot(aes(x=log_max_mass,y=CV_spec)) + geom_point() + labs(x="log of fish asymptotic body mass",y="CV of fish biomass")+geom_smooth(method = "lm")

multiplot(xk1,xk2,xk3,xk4,cols=2)



xk1=full_stats %>% ggplot(aes(x=max_Z,y=FT_ratio)) + geom_point() + labs(x="Allometric Ratio",y="Fish to total biomass ratio")+geom_smooth(method = "lm")
xk2=full_stats %>% ggplot(aes(x=log_max_fish_mass,y=FT_ratio)) + geom_point() + labs(x="log of fish mass",y="Fish to total biomass ratio")+geom_smooth(method = "lm")

multiplot(xk1,xk2,cols=2)









