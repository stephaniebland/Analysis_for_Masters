# Cluster R script
# Stephanie Bland
# Analysis of Data in the Cluster
# This function takes data from each simulation and merges it into one file per data type (so output will give a file for each variable, and each file will have data across all experiments and simulations) - actually i cant do that easily (bc # nodes might change and woah there's lots of data)

# Easiest way to prevent duplicate rows or missing rows (in case cluster processes a job multiple times or not at all) would probably be to create a NaN matrix fill rows up with values **at the end** (so if there's an error you'll be able to find it easily, and rows will only be entered if all data was collected)
# Input k tells you what row in the matrix to fill

################################################
############### Temp Testing ###################
################################################
rm(list=ls())
library(reshape2)
seed_0=0
lifestages_linked=1
Adults_only=0
DATE="2017May22"
Version="0"
#k_val=1
#simnum=1
#Exper=1
for (simnum in 1:5){
  for (Exper in 1:3){
    ################################################
    ############### Read in Data ###################
    ################################################
    run_name=paste0(DATE,"_",Version)
    setwd(paste0("~/GIT/Analysis/",run_name))
    name=paste0(run_name,"_seed",seed_0,"_sim",simnum,"_Exper",Exper)
    
    import_vars_sim='B_year_end'#c('B','B_year_end','B_stable_phase')
    import_vars_web=c('isfish','basalsp','basal_ls','species','numyears','nichewebsize','ext_thresh','N_stages','lifestage','L_year','Mass')
    import_vars=c(import_vars_sim,import_vars_web)
    for (item in 1:length(import_vars)){
      trial=paste0(name,"_",import_vars[item],".txt")
      trial=as.matrix(read.csv(trial,header=F))
      #if (sum(dim(trial))==2) {trial=as.integer(trial)}
      do.call("<-",list(import_vars[item], trial))
    }
    
    ################################################
    ############### Extract Data ###################
    ################################################
    # Set up - helpful data reformatting:
    inverts_only=setdiff(which(isfish==0),basalsp) #Find invertebrates (not fish or autotrophs)
    fish_names=unique(species[isfish==1]) #The species number for fish
    yr_ls=cumsum(unlist(numyears)) # Cumulative sums of years for phases
    yr_ls_start=yr_ls-numyears
    Timegroups=cbind(yr_ls_start,yr_ls,numyears)
    Timegroups=cbind(Timegroups,Timegroups*c(L_year))
    colnames(Timegroups)=c("Year_start","Year_end","Year_length","Day_start","Day_end","Days_length")
    
    # ---- DATA_FOR_TAPPLY ----
    lump_Bio_sums <- function(B_mat){# 1 Add columns for sum of nodes so we have biomass of groups of species (ex. all fish)
      colnames(B_mat)=paste0('Node_',1:nichewebsize)
      #Fish total per species
      Fish_tot_per_sp_df=c()
      for (item in fish_names){
        Fish_tot_per_sp_df=cbind(Fish_tot_per_sp_df,rowSums(B_mat[,species==item]))
      }
      colnames(Fish_tot_per_sp_df)=paste0('Fish_sp_',fish_names)
      
      #Fish total per lifestage
      Fish_tot_per_ls_df=c()
      for (item in 1:max(N_stages)){
        fish_stages=(lifestage==item & t(isfish)==1)
        Fish_tot_per_ls_df=cbind(Fish_tot_per_ls_df,rowSums(B_mat[,fish_stages]))
      }
      colnames(Fish_tot_per_ls_df)=paste0('Fish_ls_',1:max(N_stages))
      
      #Find sum of basic categories
      Tot_df=rowSums(B_mat)
      Fish_tot_df=rowSums(B_mat[,isfish==1])#Total biomass of all fish species
      non_fish_df=rowSums(B_mat[,isfish==0])#Total Biomass of Non Fish species
      basal_tot_df=rowSums(B_mat[,basalsp])#Total Biomass of all autotrophs
      inverts_tot_df=rowSums(B_mat[,inverts_only])#Total Biomass of invertebrates only
      
      #Bind them to regular Data frame
      B_df=cbind(B_mat,Fish_tot_per_sp_df,Fish_tot_per_ls_df,Tot_df,Fish_tot_df,non_fish_df,basal_tot_df,inverts_tot_df)
      B_df=as.matrix(B_df)
    }
    
    melt_new_col=function(melted_df){
      # 3 Add columns into melt for each category:
      melted_df$Calen_df=(melted_df$Day_df-1)%%(L_year)+1
      melted_df$Year_df=(melted_df$Day_df-1)%/%L_year+1
      for (item in length(numyears):1){
        melted_df$Phase_df[melted_df$Year_df <=yr_ls[item]]=item
      }
      melted_df$yr_in_phase=melted_df$Year_df-c(0,yr_ls)[melted_df$Phase_df]
      melted_df[,"Seed"]=c(seed_0)
      melted_df[,"Simnum"]=c(simnum)
      melted_df[,"Exper"]=c(Exper)
      #melted_df[,"k_val"]=c(k_val)
      result=melted_df
    }
    
    #B_df=lump_Bio_sums(B)
    B_df_yr_ends=lump_Bio_sums(B_year_end)
    # 2 Melt data so [i j B] = [day, node (or sum of nodes), Biomass]
    #melt_B=setNames(melt(B_df), c('Day_df','Nodes_df','Biomass'))
    melt_B.yr.end=setNames(melt(B_df_yr_ends), c('Day_df','Nodes_df','Biomass'))
    
    melt_B.yr.end$Day_df=melt_B.yr.end$Day_df*100
    #melt_B=melt_new_col(melt_B)
    melt_B.yr.end=melt_new_col(melt_B.yr.end)
    
    # ---- SURVIVING_SPECIES ----
    
    B_year_end[yr_ls,]>c(ext_thresh)
    B_year_end[yr_ls,]>0
    
    #extant=which(B[tot_days,]>0)
    #extinct=which(B[tot_days,]==0)
    
    ################################################
    ############### Sample Plots ###################
    ################################################
    samp_plot=1:10
    if (simnum %in% samp_plot){
      # # Von Bertalanffy curve
      # matplot(matrix(log10(Mass[species %in% fish_names]),max(lifestage),length(fish_names)),type="l",lwd=3,xlab="Lifestage",ylab="Individual Body Mass (log10)",main="Von Bertalanffy curve")
      # #dev.print()
      # # Phase Diagrams
      # library(plot3D)
      # library(plot3Drgl)
      # xkcd=log10(B_df[,5])
      # lag_h=0.21#0.21 is nice
      # xkcd_h=lag(xkcd,n=L_year*lag_h)
      # xkcd_2h=lag(xkcd,n=L_year*lag_h*2)
      # lines3D(xkcd,xkcd_h,xkcd_2h)
      # # Species plot against each other
      # t_0=500
      # t_f=dim(B)[1]
      # 
      # plot_time=Timegroups[2,"Day_start"]:Timegroups[2,"Day_end"]
      # xkcd1=log10(B_df[plot_time,"Fish_tot_df"])
      # xkcd2=log10(B_df[plot_time,"basal_tot_df"])
      # xkcd3=log10(B_df[plot_time,"inverts_tot_df"])
      # scatter3Drgl(xkcd1,xkcd2,xkcd3, phi = 0, bty = "g", type = "l", ticktype = "detailed", lwd = 1,colkey=(add=F),xlab="Biomass of All Fish (log)",ylab="Biomass of All Autotrophs (log)",zlab="Biomass of All Invertebrates (log)")
      # mtext(side = 3, text = "Sum all lifestages", line = -2.5)
      # 
      # 
      # plot_time=Timegroups[2,"Year_start"]:Timegroups[2,"Year_end"]
      # xkcd1=log10(B_df_yr_ends[plot_time,"Fish_tot_df"])
      # xkcd2=log10(B_df_yr_ends[plot_time,"basal_tot_df"])
      # xkcd3=log10(B_df_yr_ends[plot_time,"inverts_tot_df"])
      # scatter3Drgl(xkcd1,xkcd2,xkcd3, phi = 0, bty = "g", type = "l", ticktype = "detailed", lwd = 1,colkey=(add=F),xlab="Biomass of All Fish (log)",ylab="Biomass of All Autotrophs (log)",zlab="Biomass of All Invertebrates (log)")
      # mtext(side = 3, text = "Sum all lifestages", line = -2.5)
    }
    ################################################
    ################## Save Data ###################
    ################################################
    # Last line is Data entry into matrix, because line should only be entered if all data was collected
    # It loads it into line k for the data matrix. 
    c(seed_0,simnum,Exper)
    #melt_B
    melt_B.yr.end
    write.table(melt_B.yr.end,"Melted.txt",append=T,col.names = F)
  }
}


################################################
############# May be of interest ###############
################################################
matplot(log10(t(tail(tapply(melt_B.yr.end$Biomass,list(melt_B.yr.end$Nodes_df,melt_B.yr.end$Phase_df),mean)))),type="l",lwd=3, xlab="Phase",ylab="Mean of Biomass (log)")
legend("bottomleft",c("fish adults","Tot_B","fish","nonfish","Basal","inverts"),col=1:6,lty=1:6,lwd=3)

matplot((t(tail(tapply(melt_B.yr.end$Biomass,list(melt_B.yr.end$Nodes_df,melt_B.yr.end$Phase_df),mean)))),type="l",lwd=3, xlab="Phase",ylab="Mean of Biomass")
legend("bottomleft",c("fish adults","Tot_B","fish","nonfish","Basal","inverts"),col=1:6,lty=1:6,lwd=3)





