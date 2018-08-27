#Behavioral data processing function;


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

proc_singlesub_cf<-function(CF) {
  CF_a<-lapply(CF, function(x) {
    x$DRUG<-NA
    x$DRUG[grep("_Plac",x$ID)]<-"Plac"
    x$DRUG[grep("_Nalt",x$ID)]<-"Nalt"
    
    #Rename variables
    x$Rating<-NA
    x$Rating[x$FaceResponseText=='Positive'] <-1
    x$Rating[x$FaceResponseText=='Negative'] <-0
    
    x$ContextNum<-NA
    x$ContextNum[x$Context=='Pleasant'] <-1
    x$ContextNum[x$Context=='Unpleasant'] <-0
    
    x$EmotionNum<-NA
    x$EmotionNum[x$Emotion=='Happy'] <-2
    x$EmotionNum[x$Emotion=='Neutral'] <-1
    x$EmotionNum[x$Emotion=='Fearful'] <-0
    
    x$Rating<-as.factor(x$Rating)
    x$ContextNum<-as.factor(x$ContextNum)
    x$EmotionNum<-as.factor(x$EmotionNum)
    
  })
}

#Do if outlier stuff
hist(bdf$rts1,1000)
hist(bdf$rts2,1000)

# missed trials -- 2-3%
mean(bdf$rts1==0)
mean(bdf$rts2==0)

bdf$missed <- bdf$rts1==0 | bdf$rts2==0
bdf$outlier <- bdf$rts1<.2 | bdf$rts2<.2 | bdf$rts1 > 4 | bdf$rts2 > 4
bdf$outlier_stay <- bdf$rts1<.2 | bdf$rts1 > 4 


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

CF<-proc_singlesub_cf(proc_behav_cf(boxdir = "/Volumes/bek/Box Sync",behav.list = T))


save(CF,file = "cf_behav_data.rdata")
#Separate single sub proc as a different function for easy editing:







