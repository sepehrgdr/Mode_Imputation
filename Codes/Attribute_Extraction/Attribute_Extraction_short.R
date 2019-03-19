#Install the following libraries if they are not already installed (Use install.packages())
library(data.table)
library(plyr)
library(geosphere)
require(parallel)
library(chron)
library(foreach)
library(doSNOW)

files <- list.files(path = "../../Inputs", pattern = "*.csv", full.names = TRUE)

#function to calculate timedifference


#function to calculate distance, time, and speed attribute for each trip
calc_attr = function(trip_index,file=point_file) {
  
  # temporary point array for for each trip
  tmp_df = file[trip_index[1]:trip_index[2],]
  tmp_df = as.data.frame(tmp_df)
  npoint = nrow(tmp_df)
  # OD distance and time
  od_dist = distGeo(tmp_df[1,c("Longitude","Latitude")],tmp_df[npoint,c("Longitude","Latitude")])
  od_time = (tmp_df[npoint,"Initial Epoch Time"]-tmp_df[1,"Initial Epoch Time"])/60
  # Average number of record per hour
  avg_record = round(npoint/od_time*60,2)
  # Average speed of trip meters/sec
  avg_spd = round(od_dist/od_time/60,3)
  #point to point speed distribution
  time_vec = tmp_df[2:npoint,"Initial Epoch Time"]-tmp_df[1:(npoint-1),"Initial Epoch Time"]
  nonzero_id = which(time_vec>0)
  if(length(nonzero_id)>=3) {
    time_vec = time_vec[nonzero_id]
    dist_vec = distGeo(tmp_df[1:(npoint-1),c("Longitude","Latitude")],tmp_df[2:npoint,c("Longitude","Latitude")])
    dist = sum(dist_vec)
    dist_vec= dist_vec[nonzero_id]
    spd_vec = round(dist_vec/time_vec,3)
    max_spd = max(spd_vec,na.rm = T)
    min_spd = min(spd_vec,na.rm = T)
    med_spd = median(spd_vec,na.rm = T)
    Q_spd = quantile(spd_vec,c(.05,.75,.95),na.rm = T)
  } else {
    dist = od_dist
    max_spd = avg_spd
    min_spd = avg_spd
    med_spd = avg_spd
    Q_spd = rep(avg_spd,3)
  }
    device_weight=file[trip_index[1],]$penetration
  #cretae a vetor of attributes and return it as the output
  attr_vec = c(npoint,od_dist, dist, od_time, avg_record, avg_spd, max_spd, min_spd, med_spd, Q_spd,device_weight)
  attr_vec
}

#function to add distance to network attributes to trip files
append_dist_attr = function(trip_file, point_file,mode) {
  #seperated by mode, calculate average distance, max and min distances, and quantiles of distance
  if(mode=="Rail") {
    dist_sum = ddply(point_file,.(trip_id),summarise,Bus_Avg = mean(rail_dist),
                     Bus_Max = max(rail_dist),Bus_Min = min(rail_dist),
                     Bus_Med = median(rail_dist), Bus_Q5=quantile(rail_dist, 0.05),
                     Bus_Q75 = quantile(rail_dist, 0.75),
                     Bus_Q95 = quantile(rail_dist, 0.95))
  } else if (mode=="Bus") {
    dist_sum = ddply(point_file,.(trip_id),summarise,Bus_Avg = mean(bus_dist),
                     Bus_Max = max(bus_dist),Bus_Min = min(bus_dist),
                     Bus_Med = median(bus_dist), Bus_Q5=quantile(bus_dist, 0.05),
                     Bus_Q75 = quantile(bus_dist, 0.75),
                     Bus_Q95 = quantile(bus_dist, 0.95))
  } else if (mode=="Road") {
    dist_sum = ddply(point_file,.(trip_id),summarise,Bus_Avg = mean(road_dist),
                     Bus_Max = max(road_dist),Bus_Min = min(road_dist),
                     Bus_Med = median(road_dist), Bus_Q5=quantile(road_dist, 0.05),
                     Bus_Q75 = quantile(road_dist, 0.75),
                     Bus_Q95 = quantile(road_dist, 0.95))
  } else if (mode=="Air") {
    dist_sum = ddply(point_file,.(trip_id),summarise,Bus_Avg = mean(air_dist),
                     Bus_Max = max(air_dist),Bus_Min = min(air_dist),
                     Bus_Med = median(air_dist), Bus_Q5=quantile(air_dist, 0.05),
                     Bus_Q75 = quantile(air_dist, 0.75),
                     Bus_Q95 = quantile(air_dist, 0.95))
  } else {
    return(paste(mode,"doesn't exist!",sep=" "))
  }
  #set column names
  colnames(dist_sum)<-c("trip_id",paste(mode,c("Avg","Max","Min",
                                               "Med","Q5","Q75","Q95"),sep="_"))
  #merge the results with the current trp dataset and return the merged dataset
  new_attr = merge(trip_file,dist_sum,by="trip_id",sort=F,all.x=T,all.y=F)
  new_attr
}

attr_extr=function(core){
    #Read the trip file
    point_file = fread(files[core],stringsAsFactors = F)
    
    #remove duplicate rows
    point_file=unique(point_file,by=c("trip_id","Initial Epoch Time"))
    
    #remove id's with single observation
    obs_by_id=point_file[,.(.N),trip_id]
    single_obs_ids=obs_by_id[N==1,trip_id]
    point_file=point_file[!trip_id %in% single_obs_ids,]
    
    # prepare trip start/end index array
    trip_start_id=which(!duplicated(point_file$trip_id))
    trip_end_id=which(!duplicated(point_file$trip_id,fromLast=TRUE))
    trip_ind_array = cbind(trip_start_id,trip_end_id)
    dim(trip_ind_array)

    # calculate the distance,time, and speed attributes
    trip_file = as.data.frame(t(apply(trip_ind_array, 1, calc_attr,file = point_file)))
    colnames(trip_file)<-c("No_point","OD_dist", "trip_dist","trip_time","Avg_record", "speed_avg", 
                               "speed_max","speed_min","speed_med",
                               "speed_Q5","speed_Q75","speed_Q95","device_weight")
    trip_file$trip_id = unique(point_file$trip_id)


    ## Add trip distance-to-network attributes to trip file
      trip_file = append_dist_attr(trip_file, point_file, mode="Rail")
      trip_file = append_dist_attr(trip_file, point_file, mode="Bus")
      trip_file = append_dist_attr(trip_file, point_file, mode="Road")
      trip_file = append_dist_attr(trip_file, point_file, mode="Air")

    # Export full attribute table
    write.csv(trip_file,paste0("../../Inputs/trips_short_distance",core,".csv"),row.names = F)
}

#mclapply(1:30, attr_extr, mc.cores=10)

#paralel application
cl = makeCluster(30)
registerDoSNOW(cl)
getDoParWorkers()
clusterExport(cl,c("calc_attr","append_dist_attr"),envir=.GlobalEnv)
foreach(core=1:30,.packages=c("data.table","plyr","geosphere")) %dopar%  attr_extr(core)
stopCluster(cl)
rm(cl)
closeAllConnections()



