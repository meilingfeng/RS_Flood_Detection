
library(tidyverse)
library(hms)


## SETUP
#----------------------------------------------------------------------------------
# Set file paths
dat_path<-"D:/Flood_Index/Data/"

# Function to split dataframe by row number
source("Functions/split_by_rows.R")

# Load data:
# iButtons
ct_nest_data<-read.csv(paste0(dat_path,"Outputs/Cleaned_iButton_Data/ct_ibutton_2013.csv"))%>%
  mutate(date_time=ifelse(nchar(date_time)==10,paste0(date_time," 00:00:00"),date_time),
         date_time=ymd_hms(date_time))
nj_nest_data<-read.csv(paste0(dat_path,"Outputs/Cleaned_iButton_Data/nj_ibutton_2013.csv"))%>%
  mutate(date_time=ifelse(nchar(date_time)==10,paste0(date_time," 00:00:00"),date_time),
         date_time=ymd_hms(date_time))
ME_nest_data<-read.csv(paste0(dat_path,"Outputs/Cleaned_iButton_Data/me_ibutton_2016_2020.csv"))%>%
  mutate(date_time=ifelse(nchar(date_time)==10,paste0(date_time," 00:00:00"),date_time),
         date_time=ymd_hms(date_time))
NH_nest_data<-read.csv(paste0(dat_path,"Outputs/Cleaned_iButton_Data/nh_ibutton_2016.csv"))%>%
  mutate(date_time=ifelse(nchar(date_time)==10,paste0(date_time," 00:00:00"),date_time),
         date_time=ymd_hms(date_time))

 #NOAA high tide events
load(paste0(dat_path,"Outputs/hightide_events.Rdata"))



## Identify time intervals of high tide flooding at nest sites
#----------------------------------------------------------------------------------
# Test 3 methods of identify flooding:
# Threshold method: when temp drops below 26.7C (Bayard and Elphick 2011)
# Minimum method: when temp hits its minimum (Gjerdrum)
# Drop Method: Gjerdrum et al 2008 found nests dropped at least 10C for nests with eggs, on avg 12.6C

# divide the data into groups based on the NOAA tide station they are closest to
CT_NHav_nest_data<-filter(ct_nest_data,site!="BI")
CT_NLon_nest_data<-filter(ct_nest_data,site=="BI")
# and the year they were sampled
ME_nest_data16<-filter(ME_nest_data,year==2016)
ME_nest_data17<-filter(ME_nest_data,year==2017)
ME_nest_data19<-filter(ME_nest_data,year==2019)
ME_nest_data20<-filter(ME_nest_data,year==2020)


#add the grouped iButton data to a list for processing
nest_dat_list<-list(NH_nest_data, ME_nest_data16, ME_nest_data17,ME_nest_data19,
                    ME_nest_data20, nj_nest_data, CT_NHav_nest_data, CT_NLon_nest_data)


#empty object to hold the outputted high tide event intervals for groups
thresh_intervals_all_site_list<-list()



#for each iButton and corresponding station high tide dataset 
for(k in 1:length(nest_dat_list)){
  
  #divide the iButton data into individual time series for each nest
button_list<-split(nest_dat_list[[k]],nest_dat_list[[k]]$nest)


# 1. Threshold method of flood detection at nests

#empty object to hold outputted flood intervals
thresh_intervals_nest_list<-list()

#for each nest time series...
for(j in 1:length(button_list)){
temp<-button_list[[j]]
#plot(temp$date_time,temp$temp)

# check if time series are discontinuous
temp_list<-temp%>%
  arrange(date_time)%>%
          # time between each observation in minutes
  mutate(int_dur=as.duration(interval(date_time,lead(date_time)))/60,
          # shouldnt be more than 15 min if it's continuous
         divide = ifelse(int_dur>15,1,0))




# for nest data that was split between multiple buttons,
# separate data based on discontinuous time gaps greater than 15 min. 
if(any(temp_list$divide==1,na.rm=T)){
temp_list<-split_by_rows(temp_list,c(which(temp_list$divide==1),nrow(temp_list)))

    # Run through each split separately.
temp_seg_list<-list()
for(q in 1:length(temp_list)){
temp<-temp_list[[q]]

    # find the start and end of the time that a nest is below 27.6C
thresh<- arrange(temp,date_time)%>%
  mutate(temp_thresh=ifelse(temp<=27.6,1,0),
         temp_start=ifelse(temp_thresh==1&(lag(temp_thresh)==0|is.na(lag(temp_thresh))),1,0),
         temp_end=ifelse(temp_thresh==1&(lead(temp_thresh)==0|is.na(lead(temp_thresh))),1,0))
    # define the start and end row index of each low temp time interval
temp_starts<-which(thresh$temp_start==1)
temp_ends<-which(thresh$temp_end==1)
    # If the data meets this threshold/has flood events
if(length(temp_starts)>0){
  temp_seg_list1<-list()
for (i in 1:length(temp_starts)){
  # subset the data and create time intervals based on these start and end rows
  temp_seg<-thresh[temp_starts[i]:temp_ends[i],]
  # make a time interval summary for each flood event and then bind into one df of flood events for the nest
  temp_seg_list1[[i]]<-summarize(temp_seg,
                            nest=first(nest),
                            site=first(site),
                            year=first(year),
                            nest_num=first(nest_num),
                            thresh_time_interval=interval(min(date_time),max(date_time)))
}
temp_seg_list[[q]]<-do.call("rbind",temp_seg_list1)
}
}
  # combine all time intervals for each discontinuous time series back into one dataframe for the nest
thresh_intervals_nest_list[[j]]<-do.call("rbind",temp_seg_list)



# For nest data that was collected continuously from 1 ibutton
}else{
  # find the start and end of the time that a nest is below 27.6C
thresh<- arrange(temp,date_time)%>%
  mutate(temp_thresh=ifelse(temp<=27.6,1,0),
         temp_start=ifelse(temp_thresh==1&(lag(temp_thresh)==0|is.na(lag(temp_thresh))),1,0),
         temp_end=ifelse(temp_thresh==1&(lead(temp_thresh)==0|is.na(lead(temp_thresh))),1,0))
  # define the start and end to each time segment
temp_starts<-which(thresh$temp_start==1)
temp_ends<-which(thresh$temp_end==1)
  # create time intervals based on these start and end points
if(length(temp_starts)>0){
  temp_seg_list<-list()
  for (i in 1:length(temp_starts)){
    temp_seg<-thresh[temp_starts[i]:temp_ends[i],]
    temp_seg_list[[i]]<-summarize(temp_seg,
                                  nest=first(nest),
                                  site=first(site),
                                  year=first(year),
                                  nest_num=first(nest_num),
                                  thresh_time_interval=interval(min(date_time),max(date_time)))
  }
  # combine all time intervals for each nest back into one dataframe
  thresh_intervals_nest_list[[j]]<-do.call("rbind",temp_seg_list)
}
}
}

#combine the results for all nests back into one dataframe for the state region x year
thresh_intervals<-do.call("rbind",thresh_intervals_nest_list)%>%
  #get rid of super short intervals less than half an hour and super long intervals
  mutate(dur=as.numeric(as.duration(thresh_time_interval))/3600)%>%
  filter(dur>=0.5&dur<=72)%>%
  select(-dur)



#select only the intervals that overlap high tide intervals at the closest tide station
if(any(c("AT","OC","MW")%in%unique(thresh_intervals$site),na.rm=T)){

tide_ref<-hightide_events[["AtlanticCity"]]
  
thresh_intervals$flood_thresh<-unlist(lapply(thresh_intervals$thresh_time_interval, 
                                             function(x) sum(unlist(int_overlaps(x,tide_ref$time_interval)))))
}

if(any(c("BI")%in%unique(thresh_intervals$site),na.rm=T)){
  
  tide_ref<-hightide_events[["NewLondon"]]

  thresh_intervals$flood_thresh<-unlist(lapply(thresh_intervals$thresh_time_interval, 
                                               function(x) sum(unlist(int_overlaps(x,tide_ref$time_interval)))))
}

if(any(c("ER","HM")%in%unique(thresh_intervals$site),na.rm=T)){
  
  tide_ref<-hightide_events[["NewHaven"]]

  thresh_intervals$flood_thresh<-unlist(lapply(thresh_intervals$thresh_time_interval, 
                                               function(x) sum(unlist(int_overlaps(x,tide_ref$time_interval)))))
}

if(any(c("CL","PB", "EL", "MQ")%in%unique(thresh_intervals$site),na.rm=T)){
  if(unique(thresh_intervals$year)==2016){
  tide_ref<-hightide_events[["Portland16"]]

  thresh_intervals$flood_thresh<-unlist(lapply(thresh_intervals$thresh_time_interval, 
                                               function(x) sum(unlist(int_overlaps(x,tide_ref$time_interval)))))
  }
  if(unique(thresh_intervals$year)==2017){
    tide_ref<-hightide_events[["Portland17"]]
    
    thresh_intervals$flood_thresh<-unlist(lapply(thresh_intervals$thresh_time_interval, 
                                                 function(x) sum(unlist(int_overlaps(x,tide_ref$time_interval)))))
  }
  if(unique(thresh_intervals$year)==2019){
    tide_ref<-hightide_events[["Portland19"]]
    
    thresh_intervals$flood_thresh<-unlist(lapply(thresh_intervals$thresh_time_interval, 
                                                 function(x) sum(unlist(int_overlaps(x,tide_ref$time_interval)))))
  }
  if(unique(thresh_intervals$year)==2020){
    tide_ref<-hightide_events[["Portland20"]]

    thresh_intervals$flood_thresh<-unlist(lapply(thresh_intervals$thresh_time_interval, 
                                                 function(x) sum(unlist(int_overlaps(x,tide_ref$time_interval)))))
  }
}


thresh_intervals_all_site_list[[k]]<-thresh_intervals

#***** add other flood detection methods here
}


## Identify time intervals of dry periods at nest sites
#----------------------------------------------------------------------------------

#empty object to hold the outputted dry event intervals for groups
thresh_int_dry_all_site_list<-list()


#for each iButton and corresponding station high tide dataset 
for(k in 1:length(nest_dat_list)){
  
  #divide the iButton data into individual time series for each nest
  button_list<-split(nest_dat_list[[k]],nest_dat_list[[k]]$nest)
  
  
  # 1. Threshold method of dry period detection at nests
  
  #empty object to hold outputted flood intervals
  thresh_intervals_nest_list<-list()
  
  #for each nest time series...
  for(j in 1:length(button_list)){
    temp<-button_list[[j]]
    #plot(temp$date_time,temp$temp)
    
    # check if time series are discontinuous
    temp_list<-temp%>%
      arrange(date_time)%>%
      # time between each observation in minutes
      mutate(int_dur=as.duration(interval(date_time,lead(date_time)))/60,
             # shouldnt be more than 15 min if it's continuous
             divide = ifelse(int_dur>15,1,0))
    
    
    
    
    # for nest data that was split between multiple buttons,
    # separate data based on discontinuous time gaps greater than 15 min. 
    if(any(temp_list$divide==1,na.rm=T)){
      temp_list<-split_by_rows(temp_list,c(which(temp_list$divide==1),nrow(temp_list)))
      
      # Run through each split separately.
      temp_seg_list<-list()
      for(q in 1:length(temp_list)){
        temp<-temp_list[[q]]
        
        # find the start and end of the time that a nest is above 30 C (inverse from the 27.6C flood event)
        thresh<- arrange(temp,date_time)%>%
          mutate(temp_thresh=ifelse(temp>=28,1,0),
                 temp_start=ifelse(temp_thresh==1&(lag(temp_thresh)==0|is.na(lag(temp_thresh))),1,0),
                 temp_end=ifelse(temp_thresh==1&(lead(temp_thresh)==0|is.na(lead(temp_thresh))),1,0))
        # define the start and end row index of each high temp time interval
        temp_starts<-which(thresh$temp_start==1)
        temp_ends<-which(thresh$temp_end==1)
        # If the data meets this threshold/has a dry period
        if(length(temp_starts)>0){
          temp_seg_list1<-list()
          for (i in 1:length(temp_starts)){
            # subset the data and create time intervals based on these start and end rows
            temp_seg<-thresh[temp_starts[i]:temp_ends[i],]
            # make a time interval summary for each flood event and then bind into one df of dry periods for the nest
            temp_seg_list1[[i]]<-summarize(temp_seg,
                                           nest=first(nest),
                                           site=first(site),
                                           year=first(year),
                                           nest_num=first(nest_num),
                                           thresh_time_interval=interval(min(date_time),max(date_time)))
          }
          temp_seg_list[[q]]<-do.call("rbind",temp_seg_list1)
        }
      }
      # combine all time intervals for each discontinuous time series back into one dataframe for the nest
      thresh_intervals_nest_list[[j]]<-do.call("rbind",temp_seg_list)
      
      
      
      # For nest data that was collected continuously from 1 ibutton
    }else{
      # find the start and end of the time that a nest is below 27.6C
      thresh<- arrange(temp,date_time)%>%
        mutate(temp_thresh=ifelse(temp>=28,1,0),
               temp_start=ifelse(temp_thresh==1&(lag(temp_thresh)==0|is.na(lag(temp_thresh))),1,0),
               temp_end=ifelse(temp_thresh==1&(lead(temp_thresh)==0|is.na(lead(temp_thresh))),1,0))
      # define the start and end to each time segment
      temp_starts<-which(thresh$temp_start==1)
      temp_ends<-which(thresh$temp_end==1)
      # create time intervals based on these start and end points
      if(length(temp_starts)>0){
        temp_seg_list<-list()
        for (i in 1:length(temp_starts)){
          temp_seg<-thresh[temp_starts[i]:temp_ends[i],]
          temp_seg_list[[i]]<-summarize(temp_seg,
                                        nest=first(nest),
                                        site=first(site),
                                        year=first(year),
                                        nest_num=first(nest_num),
                                        thresh_time_interval=interval(min(date_time),max(date_time)))
        }
        # combine all time intervals for each nest back into one dataframe
        thresh_intervals_nest_list[[j]]<-do.call("rbind",temp_seg_list)
      }
    }
  }
  
  #combine the results for all nests back into one dataframe for the state region x year
  thresh_intervals<-do.call("rbind",thresh_intervals_nest_list)%>%
    #get rid of super short intervals less than half an hour 
    mutate(dur=as.numeric(as.duration(thresh_time_interval))/3600)%>%
    filter(dur>=0.5)%>%
    select(-dur)
  
  
  
  #select only the intervals that DO NOT overlap high tide intervals at the closest tide station
  # we can be more confident these are true dry events
  if(any(c("AT","OC","MW")%in%unique(thresh_intervals$site),na.rm=T)){
    
    tide_ref<-hightide_events[["AtlanticCity"]]
    
    thresh_intervals$flood_thresh<-unlist(lapply(thresh_intervals$thresh_time_interval, 
                                                 function(x) sum(unlist(int_overlaps(x,tide_ref$time_interval)))))
  }
  
  if(any(c("BI")%in%unique(thresh_intervals$site),na.rm=T)){
    
    tide_ref<-hightide_events[["NewLondon"]]
    
    thresh_intervals$flood_thresh<-unlist(lapply(thresh_intervals$thresh_time_interval, 
                                                 function(x) sum(unlist(int_overlaps(x,tide_ref$time_interval)))))
  }
  
  if(any(c("ER","HM")%in%unique(thresh_intervals$site),na.rm=T)){
    
    tide_ref<-hightide_events[["NewHaven"]]
    
    thresh_intervals$flood_thresh<-unlist(lapply(thresh_intervals$thresh_time_interval, 
                                                 function(x) sum(unlist(int_overlaps(x,tide_ref$time_interval)))))
  }
  
  if(any(c("CL","PB", "EL", "MQ")%in%unique(thresh_intervals$site),na.rm=T)){
    if(unique(thresh_intervals$year)==2016){
      tide_ref<-hightide_events[["Portland16"]]
      
      thresh_intervals$flood_thresh<-unlist(lapply(thresh_intervals$thresh_time_interval, 
                                                   function(x) sum(unlist(int_overlaps(x,tide_ref$time_interval)))))
    }
    if(unique(thresh_intervals$year)==2017){
      tide_ref<-hightide_events[["Portland17"]]
      
      thresh_intervals$flood_thresh<-unlist(lapply(thresh_intervals$thresh_time_interval, 
                                                   function(x) sum(unlist(int_overlaps(x,tide_ref$time_interval)))))
    }
    if(unique(thresh_intervals$year)==2019){
      tide_ref<-hightide_events[["Portland19"]]
      
      thresh_intervals$flood_thresh<-unlist(lapply(thresh_intervals$thresh_time_interval, 
                                                   function(x) sum(unlist(int_overlaps(x,tide_ref$time_interval)))))
    }
    if(unique(thresh_intervals$year)==2020){
      tide_ref<-hightide_events[["Portland20"]]
      
      thresh_intervals$flood_thresh<-unlist(lapply(thresh_intervals$thresh_time_interval, 
                                                   function(x) sum(unlist(int_overlaps(x,tide_ref$time_interval)))))
    }
  }
  
  
  thresh_int_dry_all_site_list[[k]]<-thresh_intervals
  
  #***** add other flood detection methods here
}



## Add additional fate, site, and location info to the nest flood events
#----------------------------------------------------------------------------------

# combine all nest flood events (Those that drop in temperature within the time window of a high tide event)
thresh_nest_flood_events<-do.call("rbind",thresh_intervals_all_site_list)%>%
  filter(flood_thresh>0)%>%
  mutate(flood=1,
         dry=0)

# combine all nest dry periods (Those that stay above 30 C and are not during a high tide event)
thresh_nest_dry_periods<-do.call("rbind",thresh_intervals_all_site_list)%>%
  filter(flood_thresh==0)%>%
  mutate(flood=0,
         dry=1)

all_times<-rbind(thresh_nest_dry_periods,thresh_nest_flood_events)


nest_locations<-read.csv(paste0(dat_path,"Inputs/Nests/nest_locations_01_3_23.csv"))%>%
  rename(nest_id=id,site_name=Site,year=Year)%>%
  # Add nest identifying columns
  #add site names
  mutate(site=substr(nest_id,start=1,stop=2),
         #add nest number
         nest_num=as.numeric(substr(nest_id,(nchar(nest_id)-2),nchar(nest_id))))

fates<-read.csv(paste0(dat_path,"Inputs/Nests/NestFates_2001-2020.csv"),na.strings=c("","NOT REC","NA"))%>%
  #select and format variables
  dplyr::select("nest_id"="SHARPNestID","UltimateNestFate","FirstEggDate"="EstFirstEggDate",
                "maxeggs"="MaxNumEggs","Nsuccess"="NumFledged",
                "NumEggsFloodedInNest","NumEggsFloodedFromNest","NumEggsFloodedMissing",
                "NumChicksFlooded","NumChicksFloodedMissing",
                "NumEggsDepredated", "NumEggsDepredatedMissing","NumChicksDepredated","NumChicksDepredatedMissing")%>%
  mutate(across(c("NumEggsFloodedInNest","NumEggsFloodedFromNest","NumEggsFloodedMissing",
                  "NumChicksFlooded","NumChicksFloodedMissing","maxeggs","Nsuccess",
                  "NumEggsDepredated", "NumEggsDepredatedMissing","NumChicksDepredated","NumChicksDepredatedMissing"),
                as.numeric))%>%
  # Add nest identifying columns
  mutate(year=as.numeric(paste0("20",substr(nest_id,start=3,stop=4))),
         #add site names
         site=substr(nest_id,start=1,stop=2),
         #add nest number
         nest_num=as.numeric(substr(nest_id,(nchar(nest_id)-2),nchar(nest_id))))%>%
  # Format fate response variables
  rowwise()%>%
  # 1. proportion nestlings lost to flooding
  mutate(flooded_prop=sum(c(NumEggsFloodedInNest,NumEggsFloodedFromNest,NumEggsFloodedMissing,
                            NumChicksFlooded,NumChicksFloodedMissing),na.rm = T),
         flooded_prop=round(flooded_prop/maxeggs,2),
         # 2. Probability nest failed/was successful in general
         fate=case_when(
           UltimateNestFate%in%c("FLEDGED")~1,
           UltimateNestFate%in%c("DEPREDATED","FLOODED","FLOODED/DEPREDATED","NEVER HAD EGGS","NEVER HAD","FAIL UNKNOWN")~0,
           !(UltimateNestFate%in%c("FLEDGED","DEPREDATED","FLOODED","FLOODED/DEPREDATED",
                                   "NEVER HAD EGGS","NEVER HAD","FAIL UNKNOWN"))~NA),
         # 3. Probability nest failed due to flooding
         flooded_fate=ifelse(UltimateNestFate%in%c("FLOODED","FLOODED/DEPREDATED"),1,0))%>%
  ungroup()%>%
  select(nest_id,FirstEggDate, year, site, nest_num, flooded_prop,fate,flooded_fate)

#set all fate variables to NA for nests with NA for ultimate nest fate
fates[is.na(fates$fate),c(6,8)]<-NA
fates[is.nan(fates$flooded_prop),6]<-NA


# add the site and nest information to each state dataset, then merge into one df
#CT and NJ have duplicated nest numbers in a single field season. Use full nest ID for these
nest_num_dups<-nest_locations%>%
  group_by(site,year,nest_num) %>%
  filter(n()>1) %>%
  ungroup()%>%
  arrange(site,year,nest_num)

all_times_ct_nj<-all_times%>%
  filter(site%in%c(unique(ct_nest_data$site),unique(nj_nest_data$site)))
all_times_nh_me<-all_times%>%
  filter(site%in%c(unique(NH_nest_data$site),unique(ME_nest_data$site)))

nest_events1<-left_join(all_times_ct_nj, nest_locations, by=c("site","year","nest"="nest_id"))%>%
                            left_join(fates, by=c("site","year","nest"="nest_id"))%>%
  select(-c(starts_with("nest_num.")))
                        
nest_events2<-left_join(all_times_nh_me, nest_locations, by=c("site","year","nest_num"))%>%
  left_join(fates, by=c("site","year","nest_num"))%>%
  select(-c(starts_with("nest_id.")))

nest_events3<-rbind(nest_events1,nest_events2)%>%
  #remove any nest events at nests without location data
  filter(!is.na(lat))


#write.csv(nest_events3,paste0(dat_path,"Outputs/Nest_flood_events/nest_floods_26.7thresh.csv"),row.names = F)
#save(nest_events3,file=paste0(dat_path,"Outputs/Nest_flood_events/nest_floods_26.7thresh.Rdata"))


## Explore nest flooding events
#---------------------------------------------------------------------------------
nest_events3<-nest_events3%>%
  mutate(dur=as.numeric(as.duration(thresh_time_interval))/3600,#in hours
         tod=hms::as_hms(int_start(thresh_time_interval)),
         hr=hour(hms::as_hms(round_date(int_start(thresh_time_interval),"hour"))))

# what time of day are flood events usually?
hist(nest_events3[nest_events3$flood==1,]$hr) #most occur between 3-8pm.
# how long do they last?
hist(nest_events3[nest_events3$flood==1,]$dur, breaks = 100)#outliers?
hist(nest_events3[nest_events3$flood==1,]$dur, breaks = 100,xlim = c(0,50)) #many within an hour but a good number last for 10-20 hours

#When are the dry periods?
hist(nest_events3[nest_events3$dry==1,]$hr) #most occur between 3-8pm.

# how long do they last?
hist(nest_events3[nest_events3$dry==1,]$dur, breaks = 100)#outliers?
hist(nest_events3[nest_events3$dry==1,]$dur, breaks = 100,xlim = c(0,50)) #many within an hour but a good number last for 10-20 hours



## *****Explore the other methods of thresholding a flood event
#------------------------------------------------------------------------------------

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


