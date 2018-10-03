
cf_roi_list<-lapply(list.files(path = file.path(getwd(),"ROI_cf"),pattern = "*.csv",full.names = T),function(dxr) {read.csv(dxr,stringsAsFactors = F)})
cf_roi<-do.call(rbind,cf_roi_list)
cf_roi<-cf_roi[which(!apply(is.na(cf_roi[1:6]),1,function(tj) {all(tj)})),]
cf_roi<-cf_roi[order(cf_roi$ID),]
cf_roi$Drug<-cf_roi$Condition
cf_roi$uID<-cf_roi$ID
CF_roi_split<-split(cf_roi,cf_roi$ID)
write.csv(cf_roi,"complete_set_roi.csv")
