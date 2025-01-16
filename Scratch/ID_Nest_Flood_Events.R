library(tidyverse)
library(hms)

## Read in iButton Data
#----------------------------------------------------------------------------------
dat_path<-"D:/Flood_Index/Data/"

# 1. CONNECTICUT SITES
######
# CT data is in individual txt files, read each and convert to table
ct_nest_data<- lapply(list.files(paste0(dat_path,"Inputs/ibutton_data/CT_Nests/CT_2013_Nest_Data"),full.names = T),
                     read.table,sep=",",header=F)

#add the nest id to each file as a variable
nest_ids<-list.files(paste0(dat_path,"Inputs/ibutton_data/CT_Nests/CT_2013_Nest_Data"),full.names = F)
nest_ids<-gsub(".txt","",nest_ids)
for (i in 1:length(ct_nest_data)){
  ct_nest_data[[i]]<-ct_nest_data[[i]]%>%
                     mutate(nest=nest_ids[i],
                            site=substr(nest,1,2),
                            nest_num=substr(nest,(nchar(nest)-2),nchar(nest)))
}

#combine all of the tables by column into one dataframe
ct_nest_data <- do.call(rbind,ct_nest_data)%>%
  rename(date_time=V1, temp_unit=V2,temp=V3)
#CTmetadata
ct_nest_metadata<-read.csv(paste0(dat_path,"Inputs/ibutton_data/CT_Nests/CT_2013_Nest_Metadata.csv"))


# 2. NEW JERSEY SITES
######
#NJ data is already in formatted csvs
nj_nest_data<-read.csv(paste0(dat_path,"Inputs/ibutton_data/NJ_Nests/NJ_2013_Nest_Data.csv"))%>%
  mutate(site=substr(nest,1,2),
         nest_num=substr(nest,(nchar(nest)-2),nchar(nest)))
#NJmetadata
nj_nest_metadata<-read.csv(paste0(dat_path,"Inputs/ibutton_data/NJ_Nests/NJ_2013_Nest_Metadata.csv"))


# 3. MAINE SITES
######
#In csv format but needs some reformating. There is also no metadata on deploy/collection times.
#read in all the nest data for each year at a time
years<-c(2016,2017,2019,2020)
ME_nest_data_list<-list()
for(i in 1:length(years)){
  #remove the header lines and select standard variables
ME_data<- lapply(list.files(paste0(dat_path,"Inputs/ibutton_data/ME_Nests/",years[i]),full.names = T),
                      function(x) dplyr::select(read.csv(x,skip=str_which(readLines(x),"Value")-1),
                                                date_time=Date.Time,temp=Value)
)

#add the nest id to each file as a variable
nest_ids<-list.files(paste0(dat_path,"Inputs/ibutton_data/ME_Nests/",years[i]),full.names = F)
nest_ids<-gsub(".csv|.csv|\\(.*\\)| .*|_.*_.*","",nest_ids)#account for the various file naming styles
for (j in 1:length(ME_data)){
  ME_data[[j]]<-ME_data[[j]]%>%
    mutate(date_time=lubridate::mdy_hms(date_time),
           nest=nest_ids[j],
           site=substr(nest,1,2),
           nest_num=substr(nest,(nchar(nest)-2),nchar(nest)),
           nest_num=ifelse(!(substr(nest_num,1,1)%in%c("0","1")),#assuming nest counts dont go above 100's
                           paste0("0",substr(nest_num,2,nchar(nest_num))),
                           nest_num)
           )%>%
    arrange()
  
  #remove the first and last hour of monitoring to be safe because we don't have deploy/collect times
  front<-interval(ME_data[[j]]$date_time[1],ME_data[[j]]$date_time[1]+minutes(60))
  back<-interval(ME_data[[j]]$date_time[nrow(ME_data[[j]])]-minutes(60),ME_data[[j]]$date_time[nrow(ME_data[[j]])])
  ME_data[[j]]<-ME_data[[j]]%>%
    mutate(int=interval(date_time,date_time),
           int_flag=ifelse(int_overlaps(int,front)|int_overlaps(int,back),0,1))%>%
    filter(int_flag==1)%>%
    dplyr::select(-int,-int_flag)
}


#combine all of the tables by column into one dataframe
ME_nest_data_list[[i]] <- do.call(rbind,ME_data)%>%
  mutate(temp_unit="C")
}

ME_nest_data <- do.call(rbind,ME_nest_data_list)



# 4. NEW HAMPSHIRE SITES
######
#In csv format but needs some reformating. There is also no metadata on deploy/collection times.
  
#remove the header lines and select standard variables
NH_data<- lapply(list.files(paste0(dat_path,"Inputs/ibutton_data/NH_Nests/"),full.names = T),
                   function(x) dplyr::select(read.csv(x,skip=str_which(readLines(x),"Value")-1),
                                             date_time=Date.Time,temp=Value)
  )
  
#add the nest id to each file as a variable
nest_ids<-list.files(paste0(dat_path,"Inputs/ibutton_data/NH_Nests/"),full.names = F)
nest_ids<-gsub(".csv|.csv|\\(.*\\)| .*|_.*_.*","",nest_ids)#account for the various file naming styles
  for (j in 1:length(NH_data)){
    NH_data[[j]]<-NH_data[[j]]%>%
      mutate(date_time=lubridate::mdy_hms(date_time),
             nest=nest_ids[j],
             site=substr(nest,1,2),
             nest_num=substr(nest,(nchar(nest)-2),nchar(nest)),
             nest_num=ifelse(!(substr(nest_num,1,1)%in%c("0","1")),#assuming nest counts dont go above 100's
                             paste0("0",substr(nest_num,2,nchar(nest_num))),
                             nest_num)
      )%>%
      arrange(date_time)
    #remove the first and last hour of monitoring to be safe because we don't have deploy/collect times
front<-interval(NH_data[[j]]$date_time[1],NH_data[[j]]$date_time[1]+minutes(60))
back<-interval(NH_data[[j]]$date_time[nrow(NH_data[[j]])]-minutes(60),NH_data[[j]]$date_time[nrow(NH_data[[j]])])
NH_data[[j]]<-NH_data[[j]]%>%
  mutate(int=interval(date_time,date_time),
         int_flag=ifelse(int_overlaps(int,front)|int_overlaps(int,back),0,1))%>%
  filter(int_flag==1)%>%
  dplyr::select(-int,-int_flag)
    
    #check if there are any extreme and unreasonable values and remove all observations before or after this point depending on if its at the begining or end of the time series.
    #these are also probably taken during deployment or after collection
    non_data_indx<-max(which(NH_data[[j]]$temp>70|NH_data[[j]]$temp<=-10))
    
    if(non_data_indx!="-Inf"&non_data_indx<nrow(NH_data[[j]])){
      NH_data[[j]]<-NH_data[[j]][!(1:non_data_indx),] 
    }
    if(non_data_indx!="-Inf"&non_data_indx>nrow(NH_data[[j]])){
      NH_data[[j]]<-NH_data[[j]][!(non_data_indx:nrow(NH_data[[j]])),] 
    }
  }
  
  #combine all of the tables by column into one dataframe
NH_nest_data <- do.call(rbind,NH_data)%>%
  mutate(temp_unit="C")


## Do some data cleaning
#----------------------------------------------------------------------------------

ct_nest_data<-ct_nest_data%>%
         #convert to date-times
  mutate(date_time=lubridate::dmy_hms(date_time),
         date=lubridate::as_date(date_time),
         time=hms::as_hms(date_time),
         #add site names
         site=substr(nest,start=1,stop=2),
         #add nest number
         nest_num=substr(nest,(nchar(nest)-2),nchar(nest))
         )
         #remove ibuttons that malfunctioned, or are missing collection data
         #remove times before deployment and after retrieval from the nest
ct_nest_metadata$time_deploy[ct_nest_metadata$time_deploy==""]<-"6:00:00"
ct_nest_metadata$time_retrieve[ct_nest_metadata$time_retrieve==""]<-"6:00:00"
ct_nest_metadata<-ct_nest_metadata%>%
  mutate(date_time_deploy=mdy_hms(paste(date_deploy,time_deploy)),
         date_time_retrieve=mdy_hms(paste(date_retrieve,time_retrieve)),
         valid_interval=interval(date_time_deploy,date_time_retrieve))%>%
  filter(data_available%in%c("Y","y")&!is.na(date_time_deploy)&!is.na(date_time_retrieve))
ct_nest_data<-ct_nest_data%>%
  right_join(ct_nest_metadata,by=c("nest"))%>%
  filter(int_overlaps(interval(date_time,date_time),valid_interval))%>%
  select(button,nest,site,date_time,date,time,temp,temp_unit)


nj_nest_data<-nj_nest_data%>%
         #convert to date-times
  mutate(date_time=lubridate::mdy_hm(date_time),
         date=lubridate::as_date(date_time),
         time=hms::as_hms(date_time),
         #add site names
         site=substr(nest,start=1,stop=2),
         #add nest number
         nest_num=substr(nest,(nchar(nest)-2),nchar(nest))
         )
         #remove ibuttons that malfunctioned, or are missing collection data
         #remove times before deployment and after retrieval from the nest
nj_nest_metadata$time_deploy[nj_nest_metadata$time_deploy==""]<-"6:00"
nj_nest_metadata$time_retrieve[nj_nest_metadata$time_retrieve==""]<-"6:00"
nj_nest_metadata<-nj_nest_metadata%>%
  mutate(date_time_deploy=mdy_hm(paste(date_deploy,time_deploy)),
         date_time_retrieve=mdy_hm(paste(date_retrieve,time_retrieve)),
         valid_interval=interval(date_time_deploy,date_time_retrieve))%>%
  filter(data_available%in%c("Y","y")&!is.na(date_time_deploy)&!is.na(date_time_retrieve))
nj_nest_data<-nj_nest_data%>%
  right_join(nj_nest_metadata,by=c("button","nest"))%>%
  filter(int_overlaps(interval(date_time,date_time),valid_interval))%>%
  select(button,nest,site,date_time,date,time,temp,temp_unit)



ME_nest_data2<-ME_nest_data%>%
         #convert to date-times
  mutate(date_time=lubridate::mdy_hm(date_time),
         date=lubridate::as_date(date_time),
         time=hms::as_hms(date_time))%>%

  

## Get NOAA tide gauge information to confirm flood events align with high tides
#----------------------------------------------------------------------------------
#what sites are in the data?
unique(ct_nest_data$site)
unique(nj_nest_data$site)

#Get tide information for the date range
min(ct_nest_data$date)
max(ct_nest_data$date)


min(nj_nest_data$date)
max(nj_nest_data$date)

# Datum info for stations near ibutton sites
# New haven Per MLLW, 
nh_mtl =1.013
nh_mhw =1.952
nh_mhhw =2.05
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
#add them to a list
datum_list<-c(nl_mid,nh_mid,ac_mid)#make sure this order matches the file names below

# read in tide height dates and times from NOAA stations
tide_files<-list.files(paste0(dat_path,"Inputs/NOAA_Tides/"),full.names = T)

#calculate high tide events for each station file...
hightide_events<-list()
for(k in 1:length(tide_files)){
  #format date-times
tides<-read.csv(tide_files[k])%>%
  select(date=Date, time=Time..LST.LDT., height=Verified..m.)%>%
  mutate(date_time=ymd_hm(paste(date,time)),
         date=ymd(date),
         time=lubridate::hm(time))
  #filter for only date-times higher than halfway between the mean tide line and mean high tide line. The "caps" of the high tide events.
hightides<-tides%>%
  filter(height>datum_list[k])%>% 
  arrange(date_time)%>%
  #calculate the real time interval of each high tide event and the highest tide height within the event
    #first segment which date-times are a continuous high tide event
         #get the previous date-time step
  mutate(lag=lag(date_time),
         #calculate the interval of time between the last and current time step
         int=interval(date_time,lag),
         #get the duration of the interval in hours by dividing by seconds
         dur=as.numeric(as.duration(int)/3600),
         #the tide data is at hour intervals, so anything more than an hour difference is a different tide event
         #mark the date-time that starts and ends each new sequence
         start=ifelse(dur == -1,0,1),#anything that's not an hour apart
         end=ifelse(lead(start)==1,2,0))%>%
  select(-lag,-int,-dur)
         #fill in the last and first values without lags and leads
hightides$start[is.na(hightides$start)]<-1
hightides$end[is.na(hightides$end)]<-2
         #define the start and end to each segment
starts<-which(hightides$start==1)
ends<-which(hightides$end==2)
    #then summarize the time interval and highest tide within each high tide event
temp_list<-list()
for (i in 1:length(starts)){
  temp<-hightides[starts[i]:ends[i],]
  temp_list[[i]]<-summarize(temp,
                  height=max(height),
                  time_interval=interval(min(date_time),max(date_time)))
}
hightide_events[[k]]<-do.call("rbind",temp_list)
}

names(hightide_events)<-c("NewLondon","NewHaven","AtlanticCity")



## Identify time intervals of high tide flooding at nest sites
#----------------------------------------------------------------------------------
# Test 3 methods of identify flooding:
# Threshold method: when temp drops below 26.7C (Bayard and Elphick 2011)
# Minimum method: when temp hits its minimum (Gjerdrum)
# Drop Method: Gjerdrum et al 2008 found nests dropped at least 10C for nests with eggs, on avg 12.6C

# divide the data into groups based on the NOAA tide station they are closest to
CT_NH_nest_data<-filter(ct_nest_data,site%in%c("HA","ER","HM"))
CT_NL_nest_data<-filter(ct_nest_data,site%in%c("BI"))

#add the nest temperature data grouped by station reference to a list
nest_dat_list<-list(nj_nest_data,CT_NH_nest_data,CT_NL_nest_data)

#empty object to hold the outputted high tide event intervals for all nests
thresh_intervals_all_site_list<-list()

#for each nest temp-station dataset 
for(k in 1:length(nest_dat_list)){
  #divide the data into each nest's timeseries
button_list<-split(nest_dat_list[[k]],nest_dat_list[[k]]$nest)


# 1. Threshold method
#empty object to hold flood intervals based on threshold method for each nest
thresh_intervals_nest_list<-list()
#for each nest time series...
for(j in 1:length(button_list)){
temp<-button_list[[j]]
#find the start and end of the time that a nest is below 27.6C
thresh<- arrange(temp,date_time)%>%
  mutate(temp_thresh=ifelse(temp<=27.6,1,0),
         temp_start=ifelse(temp_thresh==1&(lag(temp_thresh)==0|is.na(lag(temp_thresh))),1,0),
         temp_end=ifelse(temp_thresh==1&(lead(temp_thresh)==0|is.na(lead(temp_thresh))),1,0))
#define the start and end to each time segment
temp_starts<-which(thresh$temp_start==1)
temp_ends<-which(thresh$temp_end==1)
#create time intervals based on these start and end points
if(length(temp_starts)==0){
  temp_seg_list<-
  thresh_intervals_nest_list[[j]]<-NA
}

if(length(temp_starts)>0){
temp_seg_list<-list()
for (i in 1:length(temp_starts)){
  temp_seg<-temp[temp_starts[i]:temp_ends[i],]
  temp_seg_list[[i]]<-summarize(temp_seg,
                            nest=first(nest),
                            button=first(button),
                            site=first(site),
                            thresh_time_interval=interval(min(date_time),max(date_time)))
}
#combine all time intervals for each nest back into one dataframe
thresh_intervals_nest_list[[j]]<-do.call("rbind",temp_seg_list)
}
}
#combine the results for all nests back into one dataframe
thresh_intervals<-do.call("rbind",thresh_intervals_nest_list[!is.na(thresh_intervals_nest_list)])%>%
  #get rid of super short intervals less than half an hour
  mutate(dur=as.numeric(as.duration(thresh_time_interval))/3600)%>%
  filter(dur>=0.5)%>%
  select(-dur)

#select only the intervals that overlap high tide intervals at the closest tide station
if(c("AT")%in%unique(thresh_intervals$site)){

tide_ref<-hightide_events[["AtlanticCity"]]
  
test<-lapply(thresh_intervals$thresh_time_interval,function(x){
  sum(unlist(lapply(tide_ref$time_interval, function(y) int_overlaps(y,x))))
})
thresh_intervals$flood_thresh<-unlist(lapply(thresh_intervals$thresh_time_interval, 
                                             function(x) sum(unlist(int_overlaps(x,tide_ref$time_interval)))))
}

if(c("BI")%in%unique(thresh_intervals$site)){
  
  tide_ref<-hightide_events[["NewLondon"]]
  
  test<-lapply(thresh_intervals$thresh_time_interval,function(x){
    sum(unlist(lapply(tide_ref$time_interval, function(y) int_overlaps(y,x))))
  })
  thresh_intervals$flood_thresh<-unlist(lapply(thresh_intervals$thresh_time_interval, 
                                               function(x) sum(unlist(int_overlaps(x,tide_ref$time_interval)))))
}

if(c("ER")%in%unique(thresh_intervals$site)){
  
  tide_ref<-hightide_events[["NewHaven"]]
  
  test<-lapply(thresh_intervals$thresh_time_interval,function(x){
    sum(unlist(lapply(tide_ref$time_interval, function(y) int_overlaps(y,x))))
  })
  thresh_intervals$flood_thresh<-unlist(lapply(thresh_intervals$thresh_time_interval, 
                                               function(x) sum(unlist(int_overlaps(x,tide_ref$time_interval)))))
}

thresh_intervals_all_site_list[[k]]<-thresh_intervals

#*****
}

# get the final nest flood events. Those that drop in temperature within the time window of a high tide event
thresh_nest_flood_events<-do.call("rbind",thresh_intervals_all_site_list)%>%
  filter(flood_thresh>0)

#write.csv(thresh_nest_flood_events,paste0(dat_path,"Outputs/Nest_flood_events/nest_floods_26.7thresh.csv"),row.names = F)

## *****Explore the other methods of thresholding a flood event##

test<-nj_nest_data%>%
  arrange(nest,date_time)%>%
  group_by(nest)%>%
  mutate(temp_diff=temp-dplyr::lag(temp,default=temp[1]),
         temp_trend=sign(temp_diff))
test2<-test%>%
  filter(temp_trend!=0)%>%
  mutate(temp_trend_dif=temp_trend-dplyr::lag(temp_trend,default=2),
         temp_trend_start=ifelse(temp_trend_dif!=0,1,0),
         temp_trend_end=ifelse(lead(temp_trend_start)==1,1,0))%>%
  select(-temp_trend_dif)
test<-test%>%
  filter(temp_trend==0)%>%
  mutate(temp_trend_start=0,
         temp_trend_end=0)
test3<-rbind(test,test2)%>%
  arrange(nest,date_time)%>%
  group_by(nest)%>%
  #does the interval contain a minimum temperature?
  #does the interval cover a difference of more than 10C?
  mutate(temp_min=ifelse(temp<=(min(temp,na.rm=T)+5)&temp>=(min(temp,na.rm=T)-5), 1,0),
         temp_diff=temp-dplyr::lag(temp,default=temp[1])
  )
#define the start and end to each segment
temp_starts<-which(test3$temp_trend_start==1)
temp_ends<-which(test3$temp_trend_end==2)
temp_list<-list()
for (i in 1:length(temp_starts)){
  temp<-test3[temp_starts[i]:temp_ends[i],]
  temp_list[[i]]<-summarize(temp,
                            nest=first(nest),
                            button=first(button),
                            site=first(site)
                            height=max(height),
                            time_interval=interval(min(date_time),max(date_time)))
}
ggplot(nj_nest_data[nj_nest_data$button=="2AF604",],aes(x=date_time,y=temp))+
  geom_line()




## Explore nest flooding events
#---------------------------------------------------------------------------------
thresh_nest_flood_events<-read.csv(paste0(dat_path,"Outputs/Nest_flood_events/nest_floods_26.7thresh.csv"))


thresh_nest_flood_events<-thresh_nest_flood_events%>%
  mutate(dur=as.numeric(as.duration(thresh_time_interval))/3600,#in hours
         tod=hms::as_hms(int_start(thresh_time_interval)),
         hr=hour(hms::as_hms(round_date(int_start(thresh_time_interval),"hour"))))

# what time of day are they usually?
hist(thresh_nest_flood_events$dur, breaks = 100)#outliers?
hist(thresh_nest_flood_events$dur, breaks = 100,xlim = c(0,50)) #many within an hour but a good number last for 10-20 hours

# how long do they last?
hist(thresh_nest_flood_events$hr) #most occur between 3-8pm.
