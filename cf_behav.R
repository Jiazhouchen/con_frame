#Behavioral data processing function;


proc_behav_cf<-function(boxdir=NULL,datafolder=NULL,fmriproc=T,behavproc=F,outputdir=NULL) {
  
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
  cfxz$DRUG<-NA
  cfxz$DRUG[grep("_Plac",cfxz$ID)]<-"Plac"
  cfxz$DRUG[grep("_Nalt",cfxz$ID)]<-"Nalt"
  
  if (fmriproc) {return(as.list(tempenvir))} else if (behavproc) {return(cfxz)
    if (!is.null(outputdir)) {
      write.csv(cfxz,file.path("compiled_conframe_behavdata.csv"))}
    }
  
}

