library(tidyverse)
library(hms)



## SETUP
#----------------------------------------------------------------------------------
#file paths
dat_path<-"D:/Flood_Index/Data/"

#read in iButton data
ct_nest_data<-read.csv(paste0(dat_path,"Outputs/Cleaned_iButton_Data/ct_ibutton_2013.csv"))
nj_nest_data<-read.csv(paste0(dat_path,"Outputs/Cleaned_iButton_Data/nj_ibutton_2013.csv"))
ME_nest_data<-read.csv(paste0(dat_path,"Outputs/Cleaned_iButton_Data/me_ibutton_2016_2020.csv"))
NH_nest_data<-read.csv(paste0(dat_path,"Outputs/Cleaned_iButton_Data/nh_ibutton_2016.csv"))


## Get date and location information from iButton data to query and download NOAA water levels
#-------------------------------------------------------------------------------------------------
#what sites are in the data? -> What are the closest NOAA tide gauge stations to these sites?
unique(ct_nest_data$site) #New Haven and New London
unique(nj_nest_data$site) #Atlantic city
unique(NH_nest_data$site) #Portsmith/Seavey Island (Station ID: 841987) (no data for this, used Portland)
unique(ME_nest_data$site) #Portland (Station ID: 8418150)

#Get date range of each dataset
min(ct_nest_data$date)
max(ct_nest_data$date)

min(nj_nest_data$date)
max(nj_nest_data$date)

min(NH_nest_data$date)#2019-06-09 through 6-22
max(NH_nest_data$date)

min(ME_nest_data[ME_nest_data$year==2016,]$date)#2016-06-15 through 9-07
max(ME_nest_data[ME_nest_data$year==2016,]$date)
min(ME_nest_data[ME_nest_data$year==2017,]$date)#2017-06-08 through 8-13
max(ME_nest_data[ME_nest_data$year==2017,]$date)
min(ME_nest_data[ME_nest_data$year==2019,]$date)#2019-06-12 through 6-25
max(ME_nest_data[ME_nest_data$year==2019,]$date)
min(ME_nest_data[ME_nest_data$year==2020,]$date)#2020-06-22 through 8-22
max(ME_nest_data[ME_nest_data$year==2020,]$date)



## Datum info for stations near ibutton sites (NOAA CO-OPs)
#-------------------------------------------------------------------------------
# New haven Per MLLW, 
nh_mtl =1.013 #Mean tide level
nh_mhw =1.952 #Mean high water
nh_mhhw =2.05 #Mean highest high water
  # calculate the midway point between mean tide level and mean high water level
nh_mid=((nh_mhw-nh_mtl)/2)+nh_mtl

# New london Per MLLW
nl_mtl =0.449
nl_mhw=0.839
nl_mhhw=0.929
nl_mid=((nl_mhw-nl_mtl)/2)+nl_mtl

# Atlantic City Per MLLW, 
ac_mtl =0.664
ac_mhw=1.276
ac_mhhw=1.403
ac_mid=((ac_mhw-ac_mtl)/2)+ac_mtl

# Portland Per MLLW, 
p_mtl =1.496
p_mhw=2.886
p_mhhw=3.019
p_mid=((p_mhw-p_mtl)/2)+p_mtl

#add them to a list
  # make sure this order matches the file names below
datum_list<-c(p_mid,nl_mid,nh_mid,ac_mid)



## READ IN TIDE HEIGHTS DOWNLOADED NOAA (based on query information above)
#------------------------------------------------------------------------------------
#load the tide gauge datafiles
tide_files<-list.files(paste0(dat_path,"Inputs/NOAA_Tides/"),full.names = T)

#get the station names
station<-lapply(tide_files,function(x) substr(x,start=54,stop=57))

#calculate high tide events for each station file (StationxYear)...
hightide_events<-list()
for(k in 1:length(tide_files)){
  # format date-times
  tides<-read.csv(tide_files[k])%>%
    select(date=Date, time=Time..LST.LDT., height=Verified..m.)%>%
    mutate(date_time=ymd_hm(paste(date,time)),
           date=ymd(date),
           time=lubridate::hm(time))
  # select the appropriate datum for the station
  if(station[[k]]=="port"){
    datum<-datum_list[1]
  }else if(station[[k]]=="NLon"){
    datum<-datum_list[2]
  }else if(station[[k]]=="NHav"){
    datum<-datum_list[3]
  }else if(station[[k]]=="ACit"){
    datum<-datum_list[4]
  }
  
  hightides<-tides%>%
    arrange(date_time)%>%
    # keep date-times with tides above halfway between mean tide and high tide ("caps" of the high tide events).
    filter(height>datum)%>% 
    arrange(date_time)%>%
    #calculate the time interval of each high tide event and the highest tide height within the event
           # get the previous date-time step
    mutate(lag=lag(date_time),
           # calculate the interval of time between the last and current time step
           int=interval(date_time,lag),
           # get the duration of the interval in hours by dividing by seconds
           dur=as.numeric(as.duration(int)/3600),
           # the tide data is at hour intervals, so anything more than an hour difference is a different tide event
           # mark the date-time that starts and ends each new high tide event
           start=ifelse(dur == -1,0,1),#anything that's not an hour apart
           end=ifelse(lead(start)==1,2,0))%>%
    select(-lag,-int,-dur)
    #fill in the last and first values without lags and leads
  hightides$start[is.na(hightides$start)]<-1
  hightides$end[is.na(hightides$end)]<-2
    #define the start and end row indexes to each high tide event
  starts<-which(hightides$start==1)
  ends<-which(hightides$end==2)
    #then summarize the time interval and highest tide within each high tide event
  temp_list<-list()
  for (i in 1:length(starts)){
      # subset the data to only include rows between the start and end of the tide event
    temp<-hightides[starts[i]:ends[i],]
      # get time interval and high tide and add to a list of all tide event summaries
    temp_list[[i]]<-summarize(temp,
                              height=max(height),
                              time_interval=interval(min(date_time),max(date_time)))
  }
      # combine all tide event summaries into one dataframe for the Station x Year
  hightide_events[[k]]<-do.call("rbind",temp_list)
}

names(hightide_events)<-c("Portland16","Portland17","Portland19","Portland20","NewLondon","NewHaven","AtlanticCity")


## SAVE HIGH TIDE EVENTS
#-------------------------------------------------------------------------------
save(hightide_events, file=paste0(dat_path,"Outputs/hightide_events.Rdata"))
