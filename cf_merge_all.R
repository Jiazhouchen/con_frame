#SONRISA CLINICAL

startup()

reportid_son2<-8783
reportid_son1<-8778
son1_madrslong<-9451

son2_clinical<-redcap.getreport(redcap_uri = ptcs$son2$redcap_uri,token = ptcs$son2$token,reportid = reportid_son2,message = T,config_options = NULL)$data
son1_clinical<-redcap.getreport(redcap_uri = ptcs$son1$redcap_uri,token = ptcs$son1$token,reportid = reportid_son1,message = T,config_options = NULL)$data
#son1_madrs<-redcap.getreport(redcap_uri = ptcs$son1$redcap_uri,token = ptcs$son1$token,reportid = son1_madrslong,message = T,config_options = NULL)$data


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


scoretotake<-c("madrsscore","qids_score","aesscore","shps_total_score","bai_total","gad7score")
son1_subset<-son1_clinical[c("record_id","redcap_event_name",scoretotake)]
son1_subset_wide<-reshape(son1_subset,direction = "wide",idvar = "record_id",timevar = "redcap_event_name")
son1_subset_wide$ID<-as.character(son_idmap$SON1_ID[match(son1_subset_wide$record_id,son_idmap$son1_record_id)])
for (score in scoretotake) {
  s_v6<-son1_subset_wide[[paste0(score,".","visit_6_inperson_arm_1")]]
  s_v1<-son1_subset_wide[[paste0(score,".","inperson_screening_arm_1")]]
  son1_subset_wide[[paste0(score,"_diff")]]<-s_v1-s_v6
}

#names(son1_madrs_wide)<-c("record_id","madrs_inperson","madrs_visit2","madrs_visit4","madrs_visit6")
son1_madrs_wide$ID<-as.character(son_idmap$SON1_ID[match(son1_madrs_wide$record_id,son_idmap$son1_record_id)])
son1_madrs_wide$madrs_SixToBaseline<-(son1_madrs_wide$madrs_inperson - son1_madrs_wide$madrs_visit6)


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

IDs<-cf_summary$uID[cf_summary$ROI_Plac]

save(CF_clinical_list,CF_split,CF_outscan,CF_roi_split,file = "cf_alldata.rdata") 

source("cf_behav.R")
CF_roi_split<-lapply(CF_roi_split,function(x) {
  y<-data.frame(uID=x$uID,
                Drug=x$Drug,
                Cluster=x$Cluster
               )
  #print(unique(y$uID))
  y$BOLD<-as.numeric(x$p_n) - as.numeric(x$u_n)
  return(reshape(data = y,idvar =c("uID","Drug"),direction = "wide", timevar = "Cluster"))
  
  })

df5x<-merge(df5,do.call(rbind,CF_roi_split),all.x = T,by = c("uID","Drug"))
df5y<-merge(df5x,do.call(rbind,CF_clinical_list),all.x = T,by = c("uID","Drug"))


m35a <- glmer(Resp ~ Emotion*Context*BOLD.Cingulum_Ant_L_31 + Outscan_Resp + Context*Emotion+Drug*Emotion+Drug*Context+(1|uID), family=binomial,
              data = df5y[which(! (df5y$misstrial | df5y$outlier | is.na(df5y$Switch))),] , control=glmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 1000000)))
summary(m35a)
car::Anova(m35a, "III")
vif.lme(m35a)




