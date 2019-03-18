#add libraries
library(data.table)

#divide files
#read the data
gps_data=fread("points_long_distance_dist.csv",stringsAsFactors = F)
#remove duplicate rows
gps_data=unique(gps_data,by=c("device_id","start_time"))

#remove id's with single observation
obs_by_id=gps_data[,.(.N),device_id]
single_obs_ids=obs_by_id[N==1,device_id]
gps_data=gps_data[!device_id %in% single_obs_ids,]

#sort the data
gps_data=gps_data[order(device_id,start_time),]

#calculate indices for the observations of each device
trip_start_id=which(!duplicated(gps_data$trip_id))
trip_end_id=which(!duplicated(gps_data$trip_id,fromLast=TRUE))
trip_ind_array = cbind(trip_start_id,trip_end_id)

len=nrow(trip_ind_array)/32

for (i in 0:31){
  point=gps_data[trip_ind_array[len*i+1,1]:trip_ind_array[min(nrow(trip_ind_array),len*(i+1)),2],]
  fwrite(point,paste0("points_long_distance",i,".csv"))
}

#bind files
binded=fread("trips_long_distance0.csv",stringsAsFactors = F)
for (i in 1:31){
  eval(parse(text=paste0("binded=rbind(binded,fread(\"trips_long_distance",i,".csv\",stringsAsFactors = F))")))
}

fwrite(binded,"trips_long_distance.csv")