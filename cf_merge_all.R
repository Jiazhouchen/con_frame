#SONRISA CLINICAL

startup()

reportid_son2<-8783
reportid_son1<-8778

son2_clinical<-redcap.getreport(redcap_uri = ptcs$son2$redcap_uri,token = ptcs$son2$token,reportid = reportid_son2,message = T,config_options = NULL)$data
son1_clinical<-redcap.getreport(redcap_uri = ptcs$son1$redcap_uri,token = ptcs$son1$token,reportid = reportid_son1,message = T,config_options = NULL)$data

son2_clinical$Drug<-NA
son2_clinical$Drug[grep("Plac",son2_clinical$med_type)]<-"Plac"
son2_clinical$Drug[grep("Nalt",son2_clinical$med_type)]<-"Nalt"

son1_clinical$Drug<-NA
son1_clinical$Drug[grep("Plac",son1_clinical$med_type)]<-"Plac"
son1_clinical$Drug[grep("Nalt",son1_clinical$med_type)]<-"Nalt"

son2_id<-son2_clinical[,c("record_id","subject_id","otherid_1","otherid_2")]
son1_id<-son1_clinical[,c("record_id","subject_id","otherid_1","otherid_2")]

son2_id<-proc_iddf(iddf = son2_id,patterns=c("SON2_","SON1_","NOP3_"),preserve=c("record_id"))
son1_id<-proc_iddf(iddf = son1_id,patterns=c("SON2_","SON1_","NOP3_"),preserve=c("record_id"))

colnames(son2_id)[1]<-"son2_record_id"
colnames(son1_id)[1]<-"son1_record_id"

son_idmap<-merge(son2_id,son1_id,all = T)

son2_clinical<-merge(x=son2_clinical,y=son_idmap,by.x = "record_id",by.y = "son2_record_id",all.x = T)
son1_clinical<-merge(x=son1_clinical,y=son_idmap,by.x = "record_id",by.y = "son1_record_id",all.x = T)

son2_clinical<-son2_clinical[order(son2_clinical$SON2_ID),]
son1_clinical<-son1_clinical[order(son1_clinical$SON1_ID),]

son2_clinical$uID<-son2_clinical$SON2_ID
son1_clinical$uID<-son1_clinical$SON1_ID

CF_clinical_list<-split(son2_clinical,son2_clinical$SON2_ID)
son1_clinical_list<-split(son1_clinical,son1_clinical$SON2_ID)

source("cf_behav.R")
source("cf_roi.R")

allids<-unique(c(names(CF_clinical_list),names(CF_split),names(CF_outscan),names(CF_roi_split)))

cf_summary<-do.call(rbind,lapply(allids,function(xid){
  xk<-data.frame(
  uID=xid,
  Clinical_Nalt= "Nalt" %in%  CF_clinical_list[[xid]]$Drug ,
  Clinical_Plac= "Plac" %in%  CF_clinical_list[[xid]]$Drug ,
  Behavioral_Nalt= "Nalt" %in% unique(CF_split[[xid]]$Drug) ,
  Behavioral_Plac= "Plac" %in% unique(CF_split[[xid]]$Drug) ,
  OutscanBehavioral= !is.null(CF_outscan[[xid]]) ,
  ROI_Nalt= "Nalt" %in% unique(CF_roi_split[[xid]]$Condition) ,
  ROI_Plac= "Plac" %in% unique(CF_roi_split[[xid]]$Condition) )
  if (any(!xk[2:8])) {
    xk$all<-F
  } else {xk$all<-T}
  return(xk)
}))       

print(cf_summary)
print(cf_summary[cf_summary$all,])

save(CF_clinical_list,CF_split,CF_outscan,CF_roi_split,file = "cf_alldata.rdata") 