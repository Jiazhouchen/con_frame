#Behavioral data processing function;

#To use this script, source it and you shall have a new rdata file saved to your current working directory;
#The file should contain the following:
#CF: a list of data.frames that each contains a single subject data; it's useful to apply per subject work 
#CF_P: a list of data.frames that each contains a single subject's percentage of given condition (pleasant fearful and responded negative)
#Each of those has a non-list single data.frame merged version (CF_ALL,CF_P_ALL) 
#proc_singlesub_cf function can be modified to include additional variable or change variable class or naming;
#exclude_cf function can be modified to exclude subject based on conditions, currently only has miss rate of higher than 20.2%; 

#Change your box dir here: Windows remember to change it to / instead of \
boxdir<-"/Volumes/bek/Box Sync"

#########FUNCTIONS################
#Base get behavioral data function:
proc_behav_cf<-function(boxdir=NULL,datafolder=NULL,fmriproc=F,behav.list=F,behav.df=F,outputdir=NULL) {
  
  if (grepl(" ",boxdir)) {
    sub("Box Sync","/'Box Sync/'",boxdir)->boxdir
  }
  #GitHub/SC_task/OutScan_responses
  #Sort data into right format: 
  datafolder<-file.path(boxdir,"GitHub","SC_task","SC_responses")
  lfilepath<-system(paste0("find ",datafolder," -name '*.csv' -maxdepth 2 -mindepth 1 -type f"),intern = T)
  sapply(strsplit(lfilepath,split = "/"), "[[",length(strsplit(lfilepath,split = "/")[[1]])-1)->IDCON
  IDCON<-sub("SC_","_",IDCON)
  split(lfilepath,IDCON)->filexsplit
  tempenvir<-as.environment(list())
  #tbh...it makes perfect sense when I wrote it...now it's like what happened in there????? it works tho so....
  for (i in 1:length(filexsplit)) {
    xz<-filexsplit[[i]]
    aal<-as.environment(list())
    for (xj in xz){
      tep<-read.csv(xj)
      assign(paste0("xd",xj),tep,envir = aal)  
    }
    aal<-as.list(aal)
    xzj<-list(singlesub=list())
    for (z in aal) {
      unique(z$Order)->ordernum
      xzj$singlesub[[paste0("Order",ordernum)]]<-z
    }
    xzj$singlesub$ID_CON<-names(filexsplit)[i]
    assign(names(filexsplit)[i],xzj,envir = tempenvir)
  }
  
  cf_list_x<-lapply(as.list(tempenvir),function(x) {return(x$singlesub)})
  cfx<-lapply(cf_list_x,function(x) {do.call(rbind,x[which(sapply(x,is.data.frame))])})
  for (kz in names(cfx)) {
    cfx[[kz]]$ID<-kz

  }
  cfxz<-do.call(rbind,cfx)

  if (fmriproc) {return(as.list(tempenvir))} else if (behav.df) {return(cfxz)} else if (behav.list) {return(cfx)}
    
  if (!is.null(outputdir)) {
      write.csv(cfxz,file.path("compiled_conframe_behavdata.csv"))}
}
#Label probability function
lableVar<-function(dfx) {
  if (length(grep("if",names(dfx)))>0) {
    for (jx in grep("if*",names(dfx))) {
      as.logical(dfx[[jx]])->temp
      dfx[which(temp),jx]<-gsub("if","",names(dfx)[jx])
      dfx[which(!temp),jx]<-paste0("Not_",gsub("if","",names(dfx)[jx]))
    } }
  return(dfx)
}
#clean up list function
cleanuplist<-function(listx){
  if (any(sapply(listx, is.null))){
    listx[sapply(listx, is.null)] <- NULL}
  return(listx)
}
#Generate probability function
genProbability<-function(dfx,whichone=c('ifRare','ifWon','ifSwitched1','ifSwitched2')) {
  interaction(dfx[whichone])->interactions
  prob<-data.frame(
    p=sapply(attributes(interactions)$levels, function(x) {length(which(interactions==x))/length(interactions)})
  )
  for (n in 1:length(whichone)) {
    prob[whichone[n]]<-sapply(strsplit(rownames(prob),split = ".",fixed = T),"[[",n)
  }
  rownames(prob)<-NULL
  prob$ID<-unique(dfx$ID)
  return(lableVar(prob))
}
#Processing single subject function
proc_singlesub_cf<-function(CF) {
  CF_a<-lapply(CF, function(x) {
    x$DRUG<-NA
    x$DRUG[grep("_Plac",x$ID)]<-"Plac"
    x$DRUG[grep("_Nalt",x$ID)]<-"Nalt"
    
    x$uID<-gsub(pattern = "_Plac",replacement = "",x = x$ID)
    x$uID<-gsub(pattern = "_Nalt",replacement = "",x = x$uID)
    x$uID<-as.factor(x$uID)
    #Rename variables
    x$Rating<-NA
    x$Rating[x$FaceResponseText=='Positive'] <-1
    x$Rating[x$FaceResponseText=='Negative'] <-0
    x$Resp<-as.factor(x$FaceResponseText)
    
    x$ContextNum<-NA
    x$ContextNum[x$Context=='Pleasant'] <-1
    x$ContextNum[x$Context=='Unpleasant'] <-0
    x$ContextM<-as.character(x$Context)
    x$ContextM[x$ContextM=="Unpleasant"]<-"0-Unpleasant"
    x$ContextM<-as.factor(x$ContextM)
    
    x$EmotionNum<-NA
    x$EmotionNum[x$Emotion=='Happy'] <-2
    x$EmotionNum[x$Emotion=='Neutral'] <-1
    x$EmotionNum[x$Emotion=='Fearful'] <-0
    x$Emotion<-as.factor(x$Emotion)
    
    x$ifCongruent<-FALSE
    x$ifCongruent[x$Context=='Pleasant' & x$Emotion=='Happy'] <-TRUE
    x$ifCongruent[x$Context=='Unpleasant' & x$Emotion=='Fearful'] <-TRUE
    x$ifCongruent<-as.factor(x$ifCongruent)
    
    x$ifMatchResp<-FALSE
    x$ifMatchResp[x$Context=='Pleasant' & x$Emotion=='Happy' & x$FaceResponseText=='Positive'] <-TRUE
    x$ifMatchResp[x$Context=='Unpleasant' & x$Emotion=='Fearful' & x$FaceResponseText=='Negative'] <-TRUE
    x$ifMatchResp<-as.factor(x$ifMatchResp)
    
    x$Rating<-as.factor(x$Rating)
    x$ContextNum<-as.factor(x$ContextNum)
    x$EmotionNum<-as.factor(x$EmotionNum)
    
    
    x$RT<-as.numeric(x$FaceRt)
    x$RT_lag<-lag(x$RT)
    x$missrate<-as.numeric(table(is.na(x$RT))[2]/sum(table(is.na(x$RT))[1],table(is.na(x$RT))[2]))
    x$misstrial<-as.logical(is.na(x$RT))
    x$outlier <- x$RT<.2 | x$RT > 4 
    
    x$Gender<-as.factor(x$Gender)
    
    return(x)
  })
}
#Single subject exlusion function:
exclude_cf<-function(dfx) {
  p_miss_if<-(length(which(is.na(dfx$RT))) / length(dfx$RT)) > 0.202
  #p_comswit_if <- any(dfx$ID %in% names(shark_switchrate[shark_switchrate<0.75]))
  if (!p_miss_if) {return(dfx)} else {
    print(unique(dfx$ID))
    return(NULL)}
}
#VIF function
vif.lme <- function (fit) {
  ## adapted from rms::vif
  v <- vcov(fit)
  nam <- names(fixef(fit))
  ## exclude intercepts
  ns <- sum(1 * (nam == "Intercept" | nam == "(Intercept)"))
  if (ns > 0) {
    v <- v[-(1:ns), -(1:ns), drop = FALSE]
    nam <- nam[-(1:ns)] }
  d <- diag(v)^0.5
  v <- diag(solve(v/(d %o% d)))
  names(v) <- nam
  v }
##########END FUNCTIONS##########


#############ACTUAL SCRIPT###########################
CF<-proc_singlesub_cf(proc_behav_cf(boxdir = boxdir,behav.list = T))
#CF_P<-lapply(CF, genProbability, whichone = c("Context","FaceResponseText"))
CF_P<-lapply(CF, genProbability, whichone = c("Context","Emotion","FaceResponseText"))

#Exclude Participant
CF_P_ALL<-do.call(rbind,CF_P)
rownames(CF_P_ALL)<-NULL

#This calculate a number of p(congruent respn) / p(incongruent respn)
cf_congrurate<-sapply(CF_P,function(x) {(
   (x$p[x$Emotion=="Happy" & x$Context=="Pleasant" & x$FaceResponseText=="Positive"] + x$p[x$Emotion=="Fearful" & x$Context=="Unpleasant" & x$FaceResponseText=="Negative"])
   /
   (sum(x$p[x$Emotion=="Happy" & x$Context=="Pleasant" ], x$p[x$Emotion=="Fearful" & x$Context=="Unpleasant" ]))  
   )
   })
#Check the distribution of the congurent rate:
hist(cf_congrurate)


CF<-lapply(CF,exclude_cf)
if (any(sapply(CF, is.null))){
CF<-CF[sapply(CF, is.null)] <- NULL}

CF_ALL<-do.call(rbind,CF)
rownames(CF_ALL)<-NULL


#Do if outlier stuff
hist(CF_ALL$RT,1000)


###print miss rate;
print(paste0("The overall miss rate of this sample is: ",as.numeric(table(is.na(CF_ALL$RT))[2]/sum(table(is.na(CF_ALL$RT))[1],table(is.na(CF_ALL$RT))[2]))*100," %."))

save(CF,CF_ALL,CF_P,CF_P_ALL,file = "cf_behav_data.rdata")
#Separate single sub proc as a different function for easy editing:







