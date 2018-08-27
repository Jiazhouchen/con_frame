#####Con_Frame Script

#Prepare, setting up resources
rm(list = ls())
require("devtools")
if("dependlab" %in% installed.packages()){"GREAT, DEPENDLAB PACK IS INSTALLED"}else{devtools::install_github("PennStateDEPENdLab/dependlab")}
#Load utility functions from both sources
devtools::source_url("https://raw.githubusercontent.com/Jiazhouchen/pecina/master/pecina_R_utility_function.R")
devtools::source_url("https://raw.githubusercontent.com/DecisionNeurosciencePsychopathology/fMRI_R/master/dnpl_utility.R")
devtools::source_url("https://raw.githubusercontent.com/DecisionNeurosciencePsychopathology/fMRI_R/master/fslpipe.R")

source("cf_behav.R")
#Setting up FSL global enviroment variables in case we are using RStudio 
fsl_2_sys_env()

#Load in behavioral data:
#boxdir<-findbox()
boxdir <- "/Volumes/bek/Box Sync"

#Setting some global options (Putting moving variables here so the function down there could just grab them)
argu<-as.environment(list(
  #Number of processes to allow for paralle processing
  nprocess=4,
  #If at any point you wish to stop the function, input step number here: ; if NULL then will be ignored.
  onlyrun=NULL,
  #Where is the cfg config file:
  cfgpath="/Volumes/bek/autopreprocessing_pipeline/Neurofeedback/con_framing.cfg",
  #Where to put/are the regressors 
  regpath="/Volumes/bek/neurofeedback/sonrisa2/con_framing/regs/R_fsl_reg",
  #Where is the grid to make signal?
  gridpath="grid_sc.csv",
  #What pre-proc data to grab:
  func.nii.name="nfswudktm*[0-9]_[0-9].nii.gz",
  #Does the ID have a tails:
  proc_id_subs=NULL,
  #How to separate IDs into groups;
  group_id_sep=c('Nalt','Plac'),
  #Now set up the model:
  model.name="con_framing_basic_redo",
  #Look at the grid! 
  model.varinames=c("PxH",         
                    "PxF",
                    "PxN",
                    "UxH",
                    "UxF",
                    "UxN"),
  regtype=".1D",
  #If to convolve with nuisance regressors with dependlab package:
  ifnuisa=FALSE,
  #Single subject FSL template path
  ssub_fsl_templatepath="/Volumes/bek/neurofeedback/scripts/fsl/templates/fsl_cf2_template_R.fsf",
  adaptive_gfeat=TRUE,
  #Group level FSL template path
  gsub_fsl_templatepath="/Volumes/bek/neurofeedback/scripts/fsl/templates/fsl_gfeat_general_adaptive_template.fsf",
  #Single Subject output root path (before model name folder)
  ssub_outputroot="/Volumes/bek/neurofeedback/sonrisa2/con_framing/ssanalysis/fsl",
  #Group lvl output rootpath (before model name folder)
  glvl_outputroot="/Volumes/bek/neurofeedback/sonrisa2/con_framing/grpanal/fsl",
  #Brain template path
  templatedir="/Volumes/bek/Newtemplate_may18/fsl_mni152/MNI152_T1_2mm_brain.nii",
  #If there's anyother folder within $output/$model.name that contains *.feat, please remove it from here
  hig_lvl_path_filter=NULL,
  whichttest = c("paired","onesample"),
  cluster_thresh = 3,
  graphic.threshold=0.95,
  forcereg=FALSE,
  ifoverwrite_secondlvl=T
  #Add more universal arguements in here: 
))

#Get behavioral data
datalist<-proc_behav_cf(boxdir = boxdir,fmriproc = T)

#Run fsl_pipe
fsl_pipe(argu=argu,
         prep.call.func="prep.confram", #This should be a character string that's the name of the prep proc function
         prep.call.allsub=datalist #List of ID list of arguments for prep.call.
)










