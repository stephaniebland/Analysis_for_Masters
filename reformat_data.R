
################################################
############### Read in Data ###################
################################################
name=paste0(run_name,"_seed",seed_0,"_sim",simnum,"_Exper",Exper,"_pred",pred,"_prey",prey)

import_vars_sim='B_year_end'#c('B','B_year_end','B_stable_phase')
import_vars_web=c('isfish','basalsp','basal_ls','species','numyears','nichewebsize','ext_thresh','N_stages','lifestage','L_year','Mass','lifehis.splitdiet','lifehis.fishpred')
import_vars=c(import_vars_sim,import_vars_web)
for (item in 1:length(import_vars)){
	trial=paste0(name,"_",import_vars[item],".txt")
	trial=as.matrix(read.csv(trial,header=F))
	if (sum(dim(trial))==2) {trial=as.integer(trial)}
	do.call("<-",list(import_vars[item], trial))
}

################################################
############### Extract Data ###################
################################################
B_year_end


























