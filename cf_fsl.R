#####Con_Frame Script

#Prepare, setting up resources
rm(list = ls())
require("devtools")
if("dependlab" %in% installed.packages()){"GREAT, DEPENDLAB PACK IS INSTALLED"}else{devtools::install_github("PennStateDEPENdLab/dependlab")}
#Load utility functions from both sources
devtools::source_url("https://raw.githubusercontent.com/Jiazhouchen/pecina/master/pecina_R_utility_function.R")
devtools::source_url("https://raw.githubusercontent.com/DecisionNeurosciencePsychopathology/fMRI_R/master/dnpl_utility.R")
devtools::source_url("https://raw.githubusercontent.com/DecisionNeurosciencePsychopathology/fMRI_R/master/fslpipe.R")
devtools::source_url("https://raw.githubusercontent.com/Jiazhouchen/con_frame/master/cf_behav.R")
#Setting up FSL global enviroment variables in case we are using RStudio 
fsl_2_sys_env()

#Load in behavioral data:
#boxdir<-findbox()
boxdir <- "/Volumes/bek/Box Sync"
#Setting default options
argu<-as.environment(list(nprocess=4,onlyrun=NULL,proc_id_subs=NULL,regtype=".1D",ifnuisa=FALSE,adaptive_gfeat=TRUE,
                          hig_lvl_path_filter=NULL,cluster_thresh = 3,whichttest = c("paired","onesample"),
                          group_id_sep=c('Nalt','Plac'),graphic.threshold=0.95,forcereg=FALSE,ifoverwrite_secondlvl=F,
                          cfgpath="/Volumes/bek/autopreprocessing_pipeline/Neurofeedback/con_framing.cfg",
                          regpath="/Volumes/bek/neurofeedback/sonrisa2/con_framing/regs/R_fsl_reg",func.nii.name="nfswudktm*[0-9]_[0-9].nii.gz",
                          gsub_fsl_templatepath="/Volumes/bek/neurofeedback/scripts/fsl/templates/fsl_gfeat_general_adaptive_template.fsf",
                          ssub_outputroot="/Volumes/bek/neurofeedback/sonrisa2/con_framing/ssanalysis/fsl",
                          glvl_outputroot="/Volumes/bek/neurofeedback/sonrisa2/con_framing/grpanal/fsl",
                          templatedir="/Volumes/bek/Newtemplate_may18/fsl_mni152/MNI152_T1_2mm_brain.nii"
))

#Which model to run:
M_base=FALSE
M_RTConv=TRUE

#Differentiate argument for different models here:
if (M_base) {
  argu$gridpath<-"grid_sc.csv"
  argu$model.name="con_framing_basic_redo"
  argu$model.varinames=c("PxH",        
                         "PxF",
                         "PxN",
                         "UxH",
                         "UxF",
                         "UxN")
  argu$ssub_fsl_templatepath="/Volumes/bek/neurofeedback/scripts/fsl/templates/fsl_cf2_template_R.fsf"
}
if (M_RTConv) {
  argu$gridpath<-"grid_sc_rt.csv"
  argu$model.name="con_framing_rt_conv"
  argu$model.varinames=c("PxH",        
                         "PxF",
                         "PxN",
                         "UxH",
                         "UxF",
                         "UxN")
  argu$ssub_fsl_templatepath="/Volumes/bek/neurofeedback/scripts/fsl/templates/fsl_cf2_template_R.fsf"
}

#Get behavioral data
datalist<-proc_behav_cf(boxdir = boxdir,fmriproc = T)

#Run fsl_pipe
fsl_pipe(argu=argu,
         prep.call.func="prep.confram", #This should be a character string that's the name of the prep proc function
         prep.call.allsub=datalist #List of ID list of arguments for prep.call.
)