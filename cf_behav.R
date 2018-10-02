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
proc_behav_cf<-function(boxdir=NULL,datafolder=NULL,fmriproc=F,behav.list=F,behav.df=F,outputdir=NULL,inscan=T) {
  
  if (grepl(" ",boxdir)) {
    sub("Box Sync","/'Box Sync/'",boxdir)->boxdir
  }
  #GitHub/SC_task/OutScan_responses
  #Sort data into right format: 
  if (inscan){
  datafolder<-file.path(boxdir,"GitHub","SC_task","SC_responses")} else {
  datafolder<-file.path(boxdir,"GitHub","SC_task","OutScan_responses")
  }
  lfilepath<-system(paste0("find ",datafolder," -name '*.csv' -maxdepth 2 -mindepth 1 -type f"),intern = T)
  IDCON<-sapply(strsplit(lfilepath,split = "/"), "[[",length(strsplit(lfilepath,split = "/")[[1]])-1)
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
  cfx<-lapply(cf_list_x,function(x) {
    ck<-do.call(rbind,x[which(sapply(x,is.data.frame))])
    ck$ID<-x$ID_CON
    return(ck)
    })
  #for (kz in names(cfx)) {
  #  cfx[[kz]]$ID<-kz
  #}
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
genProbability<-function(dfx,condition=c("Context","Emotion"),response=c("FaceResponseText"),excludeNA=T,missresp=NA) {
  if (excludeNA) {
  if (is.na(missresp)) {
  dfx<-dfx[which(!is.na(dfx[[response]])),] } else {dfx<-dfx[which(dfx[[response]]!=missresp),]}
  }
  dfx<-droplevels(dfx)
  #whichone<-c(condition,response)
  interaction(dfx[condition])->interactions

  nwx<-do.call(rbind,lapply(attributes(as.factor(dfx[[response]]))$levels, function(resp) {
    prob<-data.frame(
    p=sapply(attributes(interactions)$levels, function(x) {
      ( length(which(as.character(dfx[[response]])==resp & interactions==x)) / length(which(interactions==x)) ) -> px
      return(px)
      }),
    resp=resp)
    for (n in 1:length(condition)) {
      prob[condition[n]]<-sapply(strsplit(rownames(prob),split = ".",fixed = T),"[[",n)
    }
    rownames(prob)<-NULL
    prob$ID<-unique(dfx$ID)
    lableVar(prob)
  }) )
  
  

  return(nwx)
}
#Processing single subject function
proc_singlesub_cf<-function(CF) {
  CF_a<-lapply(CF, function(x) {
    x$Drug<-NA
    x$Drug[grep("_Plac",x$ID)]<-"Plac"
    x$Drug[grep("_Nalt",x$ID)]<-"Nalt"
    x$Drug = factor(x$Drug,levels = c("Plac","Nalt"))
    
    x$uID<-gsub(pattern = "_Plac",replacement = "",x = x$ID)
    x$uID<-gsub(pattern = "_Nalt",replacement = "",x = x$uID)
    x$uID<-as.factor(x$uID)
    #Rename variables
    x$Rating<-NA
    x$Rating[x$FaceResponseText=='Positive'] <-1
    x$Rating[x$FaceResponseText=='Negative'] <-0
    x$Rating<-as.numeric(as.character(x$Rating))
    x$Resp<-factor(x$FaceResponseText,c("Negative","Positive"))
    
    x$ContextNum<-NA
    x$ContextNum[x$Context=='Pleasant'] <-1
    x$ContextNum[x$Context=='Unpleasant'] <-0
    x$ContextM<-as.character(x$Context)
    x$ContextM[x$ContextM=="Unpleasant"]<-"0-Unpleasant"
    x$ContextM<-as.factor(x$ContextM)
    x$Context = factor(x$Context,levels = c("Pleasant","Unpleasant"))
    
    
    x$EmotionNum<-NA
    x$EmotionNum[x$Emotion=='Happy'] <-2
    x$EmotionNum[x$Emotion=='Neutral'] <-0
    x$EmotionNum[x$Emotion=='Fearful'] <-1
    x$Emotion<-as.factor(x$Emotion)
    #x$Emotion = factor(x$Emotion,levels = c("Neutral","Happy","Fearful"))
    x$Emotion = factor(x$Emotion,levels = c("Fearful","Happy","Neutral"))
    x$ifCongruent<-FALSE
    x$ifCongruent[x$Context=='Pleasant' & x$Emotion=='Happy'] <-TRUE
    x$ifCongruent[x$Context=='Unpleasant' & x$Emotion=='Fearful'] <-TRUE
    x$ifCongruent<-as.factor(x$ifCongruent)
    
    x$ifMatchResp<-FALSE
    x$ifMatchResp[x$Context=='Pleasant' & x$Emotion=='Happy' & x$FaceResponseText=='Positive'] <-TRUE
    x$ifMatchResp[x$Context=='Unpleasant' & x$Emotion=='Fearful' & x$FaceResponseText=='Negative'] <-TRUE
    x$ifMatchResp<-as.factor(x$ifMatchResp)
    
    x$ContextNum<-as.factor(x$ContextNum)
    x$EmotionNum<-as.factor(x$EmotionNum)
    
    x$RT<-as.numeric(x$FaceRt)
    x$RT_lag<-lag(x$RT)
    x$missrate<-as.numeric(table(is.na(x$RT))[2]/sum(table(is.na(x$RT))[1],table(is.na(x$RT))[2]))
    x$misstrial<-as.logical(is.na(x$RT))
    x$outlier <- x$RT<.2 | x$RT > 4 
    
    x$Gender<-as.factor(x$Gender)
    
    x$decision<-NA
    x$decision[x$FaceResponseText=="Positive"]<-0
    x$decision[x$FaceResponseText=="Negative"]<-1
    x$decision<-as.numeric(x$decision)
    
    
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
#Proc outscanner data:
proc_outscan_cf<-function(cfo) {
  uID<-unique(cfo$ID)
  misslogic<-is.na(cfo$ConditionRt)
  per_miss<-length(which(misslogic))/length(misslogic)
  cfo_cen<-cfo[!misslogic,]
  p_neu<-length(which(cfo_cen$Condition=="neutral"&cfo_cen$ConditionResposne=="1!")) / length(which(cfo_cen$Condition=="neutral"))
  p_neg<-length(which(cfo_cen$Condition=="negative"&cfo_cen$ConditionResposne=="1!")) / length(which(cfo_cen$Condition=="negative"))
  p_hap<-length(which(cfo_cen$Condition=="positive"&cfo_cen$ConditionResposne=="1!")) / length(which(cfo_cen$Condition=="positive"))
  return(data.frame(uID=uID,
                    per_miss=per_miss,
                    p_neu=p_neu,
                    p_neg=p_neg,
                    p_hap=p_hap))
}

proc2_outscan_cf<-function(cfo) {
  cfo$uID<-unique(cfo$ID)
  cfo$RT<-as.numeric(cfo$ConditionRt)
  cfo$RT_lag<-lag(cfo$RT)
  cfo$misstrial<-as.logical(is.na(cfo$RT))
  cfo$outlier <- cfo$RT<.2 | cfo$RT > 4 
  
  cfo$Resp<-factor(plyr::mapvalues(cfo$ConditionResposne,from = c("1!","7&"),to = c("Negative","Positive"),warn_missing = F),levels = c("Positive","Negative"))
  cfo$Emotion<-factor(plyr::mapvalues(cfo$Condition,to = c("Happy","Neutral","Fearful"),from = c("positive","neutral","negative")),levels = c("Neutral","Happy","Fearful"))
  
  #cfo$Drug<-"NoDrug"
  cfo$Context<-"NoContext"
  
  cfo$Order<-0
  
  #cfo$ID<-paste0(cfo$uID,"_NoDrug")
  cfo$ID<-cfo$uID
  
  cfa<-cfo[names(cfo)[!(names(cfo) %in% c("Participant","Block","Condition","RestImage","ConditionOnset","ConditionResposne","ConditionRt"))]]
  
  return(cfa)
}
##########END FUNCTIONS##########

#LISTEN!!!!
#1! is Negative and 7& is Positive

#############ACTUAL SCRIPT###########################
CF<-proc_singlesub_cf(proc_behav_cf(boxdir = boxdir,behav.list = T))
CF_outscan<-lapply(proc_behav_cf(boxdir = boxdir,behav.list = T,inscan = F),proc_outscan_cf)

CF_P_outscan<-lapply(lapply(proc_behav_cf(boxdir = boxdir,behav.list = T,inscan = F),proc2_outscan_cf), genProbability, 
                    condition=c("Emotion"),response=c("Resp"),missresp="")
CF_P_outscan_rbind<-do.call(rbind,CF_P_outscan)

CF_P_pos<-do.call(rbind,CF_P_outscan)

CF_outscan_trial<-lapply(proc_behav_cf(boxdir = boxdir,behav.list = T,inscan = F),proc2_outscan_cf)
#CF_P_pos<-CF_P_outscan_ALL[CF_P_outscan_ALL$resp=="7&",]
CF_outscan_t_all<-do.call(rbind,CF_outscan_trial)

CF_prc2<-lapply(CF, function(x) {
  ID<-as.character(unique(x$uID))
  print(ID)
  x$Rating_w_bias<-NA
  for (emo in unique(CF_P_pos$Condition)) {
    if (length(CF_P_pos$p[CF_P_pos$ID==ID & CF_P_pos$Condition==emo])>0) {
    x$Rating_w_bias[which(tolower(x$Emotion)==emo)]<-as.numeric(as.character(x$Rating[which(tolower(x$Emotion)==emo)])) - CF_P_pos$p[CF_P_pos$ID==ID & CF_P_pos$Condition==emo]
    } 
  }
  if (all(is.na(x$Rating_w_bias))){return(NULL)}else{
    #x$Rating_w_bias<-as.factor(x$Rating_w_bias)
  return(x)}
})

CF_prc3<-lapply(as.character(unique(df$uID)),function(uID){
  if (length(unique(df[df$uID==uID,]$DRUG))==2) {
    df[df$uID==uID,]->dfx
    return(dfx)
  } else {return(NULL)}
})

CF_P<-lapply(CF, genProbability, condition=c("Context","Emotion","Run"),response=c("FaceResponseText"),missresp="NaN")
CF_P_prc<-lapply(CF_P,function(x) {
  #x<-x[x$resp=="Positive",]
  x$uID<-gsub(pattern = "_Plac",replacement = "",x = x$ID)
  x$uID<-gsub(pattern = "_Nalt",replacement = "",x = x$uID)
  x$uID<-as.factor(x$uID)
  x$Drug<-NA
  x$Drug[grep("_Plac",x$ID)]<-"Plac"
  x$Drug[grep("_Nalt",x$ID)]<-"Nalt"
  x$Emotion_f <- factor(x$Emotion,levels = c("Fearful","Happy","Neutral"))
  x$Emotion_h <- factor(x$Emotion,levels = c("Happy","Fearful","Neutral"))
  x$Drug_plac <- factor(x$Drug,levels = c("Plac","Nalt"))
  x$Context_p <- factor(x$Context,levels = c("Pleasant","Unpleasant"))
  x$Emotion <- factor(x$Emotion,levels = c("Neutral","Happy","Fearful"))
  x$Drug <- factor(x$Drug,levels = c("Nalt","Plac"))
  x$Context <- factor(x$Context,levels = c("Unpleasant","Pleasant"))
  #Emotion<-plyr::mapvalues(x$Emotion,from = c("Happy","Neutral","Fearful"),to = c("positive","neutral","negative"))
  x$Outscan_rate<-NA
  #x$resp<-NULL
  ID<-unique(x$uID)
  for (emo in unique(CF_P_pos$Emotion)) {
    if (length(CF_P_pos$p[CF_P_pos$ID==ID & CF_P_pos$Emotion==emo])>0) {
      x$Outscan_rate[which(x$Emotion==emo)]<-CF_P_pos$p[CF_P_pos$ID==ID & CF_P_pos$Emotion==emo]
    } 
  }
  if (all(is.na(x$Outscan_rate))){return(NULL)}else{
    #x$Rating_w_bias<-as.factor(x$Rating_w_bias)
    return(x)}
})

CF_P_ALL<-do.call(rbind,CF_P_prc)
rownames(CF_P_ALL)<-NULL

CF_prc4<-lapply(CF, function(x) {
  ID<-as.character(unique(x$uID))
  #print(ID)
  x$Accuracy<-NA
  CF_df_outscan<-CF_P_outscan_rbind
  #for (res in unique(CF_df_outscan$resp)) {
  for (emo in unique(CF_df_outscan$Emotion)) {
    #res="Positive"
    if (length(CF_df_outscan$p[CF_df_outscan$ID==ID & CF_df_outscan$Emotion==emo])>0) {
      #x$Accuracy[which(x$Emotion==emo) & x$Resp==res]<-CF_df_outscan$p[CF_df_outscan$ID==ID & CF_df_outscan$Emotion==emo & CF_df_outscan$resp==res]
      x$Accuracy[which(x$Emotion==emo)]<-CF_df_outscan$p[CF_df_outscan$ID==ID & CF_df_outscan$Emotion==emo]
    } 
  }
  #}
  if (all(is.na(x$Accuracy))){return(NULL)}else{
    #x$Rating_w_bias<-as.factor(x$Rating_w_bias)
    return(x)}
})

CF_prc5<-lapply(CF, function(x) {
  ID<-as.character(unique(x$uID))
  #print(ID)
  
  #for (res in unique(CF_df_outscan$resp)) {
  x$Outscan_Resp<-CF_outscan_t_all[CF_outscan_t_all$ID==ID,]$Resp[match(x$FaceFile,CF_outscan_t_all[CF_outscan_t_all$uID==ID,]$Image)]
  
  x$Resp_control[x$Outscan_Resp=="Negative" & x$Resp=="Negative"] <- "Neg_Neg"
  x$Resp_control[x$Outscan_Resp=="Positive" & x$Resp=="Negative"] <- "Pos_Neg"
  x$Resp_control[x$Outscan_Resp=="Negative" & x$Resp=="Positive"] <- "Neg_Pos"
  x$Resp_control[x$Outscan_Resp=="Positive" & x$Resp=="Positive"] <- "Pos_Pos"
  
  x$Switch<-as.factor(x$Resp!=x$Outscan_Resp)
  return(x)
})

df4<-do.call(rbind,cleanuplist(CF_prc4))

df5<-do.call(rbind,cleanuplist(CF_prc5))

CF_outscan_rbind<-do.call(rbind,CF_outscan)


#This calculate a number of p(congruent respn) / p(incongruent respn)
CF<-lapply(CF,exclude_cf)
if (any(sapply(CF, is.null))){
CF<-CF[sapply(CF, is.null)] <- NULL}

CF_ALL<-do.call(rbind,CF)
CF_ALL<-CF_ALL[order(CF_ALL$uID),]
CF_ALL$ifOutscan<-F
rownames(CF_ALL)<-NULL
CF_split<-split(CF_ALL,CF_ALL$uID)

CF_Outscan_ALL<-do.call(rbind,CF_outscan_trial)
CF_Outscan_ALL$ifOutscan<-T
rownames(CF_Outscan_ALL)<-NULL

CF_ALL_wout<-CF_ALL[which(CF_ALL$uID %in% CF_Outscan_ALL$uID),]

df<-merge(CF_ALL_wout,CF_Outscan_ALL,all = T)
df$Context <- factor(df$Context,levels = c("NoContext","Pleasant","Unpleasant"))
df$Drug <- factor(df$Drug,levels = c("NoDrug","Plac","Nalt"))
df$Order <- as.factor(df$Order)
df$uID <- as.factor(df$uID)
df$ifOutscan<-as.factor(df$ifOutscan)
#Do if outlier stuff
hist(CF_ALL$RT,1000)


###print miss rate;
print(paste0("The overall miss rate of this sample is: ",as.numeric(table(is.na(CF_ALL$RT))[2]/sum(table(is.na(CF_ALL$RT))[1],table(is.na(CF_ALL$RT))[2]))*100," %."))

save(CF,CF_ALL,CF_P,CF_P_ALL,CF_outscan,CF_outscan_ALL,file = "cf_behav_data.rdata")
#Separate single sub proc as a different function for easy editing:







