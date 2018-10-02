
cf_roi_list<-lapply(list.files(path = file.path(getwd(),"ROI_cf"),pattern = "*.csv",full.names = T),function(dxr) {read.csv(dxr,stringsAsFactors = F)})

cf_roi<-do.call(rbind,cf_roi_list)
cf_roi<-cf_roi[order(cf_roi$ID),]
cf_roi$Drug<-cf_roi$Condition
cf_roi$uID<-cf_roi$ID


CF_roi_split<-split(cf_roi,cf_roi$ID)

write.csv(do.call(rbind,cf_roi_list),"complete_set_roi.csv")
