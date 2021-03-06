#add libraries
library(data.table)
library(geosphere)
library(hashids)
library(foreach)
library(doSNOW)
require(parallel)

trip_identification_function=function(core){
    
    #set the parameters
    distance_threshold=100 #meters
    time_threshold=300 #seconds
    speed_threshold= 0.5 #meters/second
    
    #read the data
    gps_data=fread(paste0("points",core,".csv"),stringsAsFactors = F)

    #remove duplicate rows
    gps_data=unique(gps_data,by=c("device_id","start"))

    #remove id's with single observation
    obs_by_id=gps_data[,.(.N),device_id]
    single_obs_ids=obs_by_id[N==1,device_id]
    gps_data=gps_data[!device_id %in% single_obs_ids,]

    #sort the data
    gps_data=gps_data[order(device_id,start),]

    #convert times
    gps_data$start_time=as.POSIXct(as.numeric(gps_data$start),origin = "1970-01-01", tz = "EST")
    gps_data$end_time=as.POSIXct(as.numeric(gps_data$end),origin = "1970-01-01", tz = "EST")

    #function to generate hashid
    h = hashid_settings(salt = 'this is my salt', min_length = 10)
    hash_generator=function(){
      random=runif(1,100000000,900000000)
      encode(as.integer(random),h)
    }

    #calculate indices for the observations of each device
    device_start_id=which(!duplicated(gps_data$device_id))
    device_end_id=which(!duplicated(gps_data$device_id,fromLast=TRUE))
    device_ind_array = cbind(device_start_id,device_end_id)

    #add_time_from
    gps_data$time_from=mat.or.vec(nrow(gps_data),1)
    add_time=function(i){
      tmp_df = gps_data[device_ind_array[i,1]:device_ind_array[i,2],]
      tmp_df = as.data.frame(tmp_df)
      npoint = nrow(tmp_df)
      time_vec = c(as.numeric(difftime(tmp_df[2:npoint,"start_time"],tmp_df[1:(npoint-1),"end_time"],units="secs")),0)
      gps_data[device_ind_array[i,1]:device_ind_array[i,2],time_from:=time_vec]
    }
    add_time_vec=Vectorize(add_time)
    add_time_vec(1:nrow(device_ind_array))

    #add_distance_from
    gps_data$dist_from=mat.or.vec(nrow(gps_data),1)
    add_dist=function(i){
      tmp_df = gps_data[device_ind_array[i,1]:device_ind_array[i,2],c("latitude","longitude")]
      colnames(tmp_df)=c("lat","long")
      npoint = nrow(tmp_df)

      dist_vec = c(distGeo(tmp_df[1:(nrow(tmp_df)-1),c("long","lat")],tmp_df[2:nrow(tmp_df),c("long","lat")]),0)
      gps_data[device_ind_array[i,1]:device_ind_array[i,2],dist_from:=dist_vec]
    }
    add_dist_vec=Vectorize(add_dist)
    add_dist_vec(1:nrow(device_ind_array))

    #add speed from
    gps_data$speed_from=mat.or.vec(nrow(gps_data),1)
    gps_data[time_from!=0,speed_from:=gps_data[time_from!=0,dist_from]/gps_data[time_from!=0,time_from]]

    #function to identify trips
    trip_identifier=function(row,datatable){

      dist_from=datatable$dist_from[row-1]
      time_from=datatable$time_from[row-1]
      speed_from=datatable$speed_from[row-1]
      hash_id=datatable$trip_id[row-1]
      if (dist_from< distance_threshold){
        if (time_from < time_threshold){
          datatable[row,trip_id:=hash_id]
        }
        else{
          datatable[row,trip_id:=hash_generator()]
        }
      }else{
        if (speed_from < speed_threshold){
          datatable[row,trip_id:=hash_generator()]
        }
        else{
          datatable[row,trip_id:=hash_id]
        }
      }
    }

    #set all trip IDs
    gps_data$trip_id=as.character(mat.or.vec(nrow(gps_data),1))

    set_trip=function(device){
      first_row= device_ind_array[device,1]
      last_row=device_ind_array[device,2]
      gps_data[first_row,trip_id:=hash_generator()]
      for (obs in (first_row+1):last_row){
        trip_identifier(obs,gps_data)
      }
    }

    set_trip_vec=Vectorize(set_trip)
    K=floor(nrow(device_ind_array)/50)
    for (j in 0:K){
        set_trip_vec((j*50+1):min((j+1)*50,nrow(device_ind_array)))
        print (paste("Finished",((j+1)*50),"devices at core",core))
    }

    #add device id to trip id
    gps_data$trip_id=paste(gps_data$device_id,gps_data$trip_id,sep='')

    #create trip files
    trips=gps_data[,.(device_id,start_time,end_time,latitude,longitude),trip_id]

    #find trips with single observation
    obs_by_trip=gps_data[,.(.N),trip_id]
    single_obs_trips=obs_by_trip[N==1,trip_id]
    trips=trips[!trip_id%in%single_obs_trips,]

    #Filter Long-Distance Trips
    long_distance_trips=c()
    #calculate indices for the observations of each trip
    trip_start_id=which(!duplicated(trips$trip_id))
    trip_end_id=which(!duplicated(trips$trip_id,fromLast=TRUE))
    trip_ind_array = cbind(trip_start_id,trip_end_id)
    for (trip in 1:nrow(trip_ind_array)){
      first_row= trip_ind_array[trip,1]
      last_row=trip_ind_array[trip,2]
      if (distGeo(trips[first_row,c("longitude","latitude")],trips[last_row,c("longitude","latitude")])>75000){
        long_distance_trips=c(long_distance_trips,trips[trip_ind_array[trip,1],trip_id])
      }
    }
    trips_long_distance=trips[trips$trip_id%in%long_distance_trips,]

    #write files
    fwrite(trips_long_distance,paste0("points_long_distance",core,".csv"))
}

#Run parallel
#cl<- makeCluster(20)
#registerDoSNOW(cl)
#getDoParWorkers()
#foreach (core = 0:19,.packages = c('hashids','geosphere','data.table'))%dopar%{
#    trip_identification_function(i)
#}

mclapply(0:19, trip_identification_function, mc.cores=20)
