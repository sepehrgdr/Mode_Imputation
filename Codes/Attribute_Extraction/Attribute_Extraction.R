#Install the following libraries if they are not already installed (Use install.packages())
library(data.table)
library(plyr)
library(geosphere)
library(sf)
library(sp)

#function to calculate distances from points to network
dist_to_net=function(points,network_dir){
  
  #read network 
  network <- st_read(network_dir)
  #set the projectoin
  network$geometry <- st_set_crs(network$geometry, value = 4326)
  #identify empty routes if they exist, as they produce errors
  empty_routes=c()
  for (i in 1:length(network$geometry)){
    if (length(network$geometry[i][[1]])==0){
      empty_routes=c(empty_routes,i)
    }
  }
  #function to calculate the distance for single point
  single_point_fun =function(i){
    point_sfc=st_sfc(st_point(as.numeric(points[i,])),crs=4326)
    if (length(empty_routes)==0){
      distance=min(st_distance(x = point_sfc, y = network$geometry))
    }else{
      distance=min(st_distance(x = point_sfc, y = network$geometry[-empty_routes]))
    }
  }
  #vectorize the single function to pass multiple points
  single_point_fun_vec=Vectorize(single_point_fun)
  #pass multiple points to the vectorized function and produce distance vector as output
  distance_mat=single_point_fun_vec(1:nrow(points))
}

#function to calculate distance, time, and speed attribute for each trip
calc_attr = function(trip_index,file=point_file) {
  
  # temporary point array for for each trip
  tmp_df = file[trip_index[1]:trip_index[2],]
  tmp_df = as.data.frame(tmp_df)
  npoint = nrow(tmp_df)
  # OD distance and time
  od_dist = distGeo(tmp_df[1,c("longitude","latitude")],tmp_df[npoint,c("longitude","latitude")])
  od_time = as.numeric(difftime(tmp_df[npoint,"start_time"],tmp_df[1,"end_time"],units="mins"))
  # Average number of record per hour
  avg_record = round(npoint/od_time*60,2)
  # Average speed of trip meters/sec
  avg_spd = round(od_dist/od_time/60,3)
  #point to point speed distribution
  if(npoint>=3) {
    dist_vec = distGeo(tmp_df[1:(npoint-1),c("longitude","latitude")],tmp_df[2:npoint,c("longitude","latitude")])
    dist = sum(dist_vec)
    time_vec = as.numeric(difftime(tmp_df[2:npoint,"start_time"],tmp_df[1:(npoint-1),"end_time"],units="secs"))
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
  #cretae a vetor of attributes and return it as the output
  attr_vec = c(npoint,od_dist, dist, od_time, avg_record, avg_spd, max_spd, min_spd, med_spd, Q_spd)
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

#Read the trip file
point_file = fread("../../Inputs/Trip_Data/AirSage_Data/points_long_distance.csv")

#add distances to network
point_file$rail_dist=dist_to_net(point_file[1:3,c("longitude","latitude")],"../../Inputs/Network_Data/Track_Network/National_Passenger_Rail_Plus_Metro/Psg_rail_NTM_metro.shp")
point_file$bus_dist=dist_to_net(point_file[1:3,c("longitude","latitude")],"../../Inputs/Network_Data/Bus_Network/National/NTM_Bus_v1.0.shp")
point_file$road_dist=dist_to_net(point_file[1:3,c("longitude","latitude")],"../../Inputs/Network_Data/Road_Network/National_Major_Roads_and_Interstates/Roads_national.shp")
point_file$air_dist=dist_to_net(point_file[1:3,c("longitude","latitude")],"../../Inputs/Network_Data/Commercial_Airports/Airport_commercial.shp")

# Convert time columns to POSIXct
point_file$start_time = as.POSIXct(point_file$start_time,format="%F %T")
point_file$end_time = as.POSIXct(point_file$end_time,format="%F %T")

# prepare trip start/end index array
trip_start_id=which(!duplicated(point_file$trip_id))
trip_end_id=which(!duplicated(point_file$trip_id,fromLast=TRUE))
trip_ind_array = cbind(trip_start_id,trip_end_id)
dim(trip_ind_array)

# calculate the distance,time, and speed attributes
trip_file = as.data.frame(t(apply(trip_ind_array, 1, calc_attr,file = point_file)))
colnames(trip_file)<-c("No_point","OD_dist", "trip_dist","trip_time","Avg_record", "speed_avg", 
                           "speed_max","speed_min","speed_med",
                           "speed_Q5","speed_Q75","speed_Q95")
trip_file$trip_id = unique(point_file$trip_id)


## Add trip distance-to-network attributes to trip file
  trip_file = append_dist_attr(trip_file, point_file, mode="Rail")
  trip_file = append_dist_attr(trip_file, point_file, mode="Bus")
  trip_file = append_dist_attr(trip_file, point_file, mode="Road")
  trip_file = append_dist_attr(trip_file, point_file, mode="Air")

# Export full attribute table
write.csv(trip_file,"../../Inputs/Trip_Data/AirSage_Test_Data/trips_long_distance.csv",row.names = F)


