library(tidyverse)
library(hms)



## SETUP
#----------------------------------------------------------------------------------

#Set file paths
dat_path<-"D:/Flood_Index/Data/"


#Load ibutton data by state and clean:
#------------------
# 1. Connecticut- in individual txt files, read each and convert to tables
ct_nest_data<- lapply(list.files(paste0(dat_path,"Inputs/ibutton_data/CT_Nests/CT_2013_Nest_Data"),full.names = T),
                      read.table,sep=",",header=F)

    # add the nest id to each file as a variable
nest_ids<-list.files(paste0(dat_path,"Inputs/ibutton_data/CT_Nests/CT_2013_Nest_Data"),full.names = F)
nest_ids<-gsub(".txt","",nest_ids)
for (i in 1:length(ct_nest_data)){
  ct_nest_data[[i]]<-ct_nest_data[[i]]%>%
    mutate(nest=nest_ids[i],
           site=substr(nest,1,2),
           nest_num=substr(nest,(nchar(nest)-2),nchar(nest)))
}

    # combine all of the tables by column into one dataframe
ct_nest_data <- do.call(rbind,ct_nest_data)%>%
  rename(date_time=V1, temp_unit=V2,temp=V3)

    # add variables of interest
ct_nest_data<-ct_nest_data%>%
      # convert to date-times
  mutate(date_time=lubridate::dmy_hms(date_time),
         date=lubridate::as_date(date_time),
         time=hms::as_hms(date_time),
         year=year(date_time),
         #add site names
         site=substr(nest,start=1,stop=2),
         #site=ifelse(site=="HA","HM",site),#fix typo? supposed to be Hammonasett?
         #add nest number
         nest_num=substr(nest,(nchar(nest)-2),nchar(nest))
  )%>%
    # also filter out HA for now, unsure if its hammonasset or hammock river
  filter(site!="HA")



    # CT metadata files- info on ibutton deployment and retrieval dates
ct_nest_metadata<-read.csv(paste0(dat_path,"Inputs/ibutton_data/CT_Nests/CT_2013_Nest_Metadata.csv"))



#------------------
# 2. New Jersey - csv format
nj_nest_data<-read.csv(paste0(dat_path,"Inputs/ibutton_data/NJ_Nests/NJ_2013_Nest_Data.csv"))%>%
  mutate(site=substr(nest,1,2),
         nest_num=substr(nest,(nchar(nest)-2),nchar(nest)))

    # Add variables of interest
nj_nest_data<-nj_nest_data%>%
      # convert to date-times
  mutate(date_time=lubridate::mdy_hm(date_time),
         date=lubridate::as_date(date_time),
         time=hms::as_hms(date_time),
         year=year(date_time),
         #add site names
         site=substr(nest,start=1,stop=2),
         #add nest number
         nest_num=substr(nest,(nchar(nest)-2),nchar(nest))
  )

    # NJ metadata
nj_nest_metadata<-read.csv(paste0(dat_path,"Inputs/ibutton_data/NJ_Nests/NJ_2013_Nest_Metadata.csv"))



#------------------
# 3. Maine- individual nest csv files with header lines before column names. Multiple years.
    # read in all the nest data one year at a time
years<-c(2016,2017,2019,2020)
ME_nest_data_list<-list()
for(i in 1:length(years)){
    # remove the header lines and select column variables
  ME_data<- lapply(list.files(paste0(dat_path,"Inputs/ibutton_data/ME_Nests/",years[i]),full.names = T),
                   function(x) dplyr::select(read.csv(x,skip=str_which(readLines(x),"Value")-1),#starts reading at the column names which start with "Value"
                                             date_time=Date.Time,temp=Value)
  )
  
    # add the nest id to each file as a variable
  nest_ids<-list.files(paste0(dat_path,"Inputs/ibutton_data/ME_Nests/",years[i]),full.names = F)
  nest_ids<-gsub(".csv|.csv|\\(.*\\)| .*|_.*_.*","",nest_ids)#account for the various file naming styles
  for (j in 1:length(ME_data)){
    # accommodate different date-time formats:
    # Hours Minutes
    if(any(is.na(mdy_hms(ME_data[[j]]$date_time)))){
      ME_data[[j]]<-ME_data[[j]]%>%
        mutate(date_time=lubridate::mdy_hm(date_time),
               nest=nest_ids[j],
               site=substr(nest,1,2),
               nest_num=substr(nest,(nchar(nest)-2),nchar(nest)),
               # add missing leading zeros on nest numbers
               nest= ifelse(!(substr(nest_num,1,1)%in%c("0","1")),#assuming nest counts dont go above 199 in a given year
                      paste0(substr(nest,1,(nchar(nest)-2)),"0",substr(nest_num,2,nchar(nest_num))),
                      nest),
               nest_num=ifelse(!(substr(nest_num,1,1)%in%c("0","1")),
                               paste0("0",substr(nest_num,2,nchar(nest_num))),
                               nest_num)
        )%>%
        arrange()
      # Hours Min Seconds
    }else{
      ME_data[[j]]<-ME_data[[j]]%>%
        mutate(date_time=lubridate::mdy_hms(date_time),
               nest=nest_ids[j],
               site=substr(nest,1,2),
               nest_num=substr(nest,(nchar(nest)-2),nchar(nest)),
               # add missing leading zeros on nest numbers
               nest= ifelse(!(substr(nest_num,1,1)%in%c("0","1")),#assuming nest counts dont go above 199 in a given year
                            paste0(substr(nest,1,(nchar(nest)-2)),"0",substr(nest_num,2,nchar(nest_num))),
                            nest),
               nest_num=ifelse(!(substr(nest_num,1,1)%in%c("0","1")),
                               paste0("0",substr(nest_num,2,nchar(nest_num))),
                               nest_num)
        )%>%
        arrange()
    }
    
      # remove the first and last hour of monitoring to be safe because we don't have deploy/retrieval times
    front<-interval(ME_data[[j]]$date_time[1],ME_data[[j]]$date_time[1]+minutes(60))
    back<-interval(ME_data[[j]]$date_time[nrow(ME_data[[j]])]-minutes(60),ME_data[[j]]$date_time[nrow(ME_data[[j]])])
    ME_data[[j]]<-ME_data[[j]]%>%
      mutate(int=interval(date_time,date_time),
             int_flag=ifelse(int_overlaps(int,front)|int_overlaps(int,back),0,1))%>%
      filter(int_flag==1)%>%
      filter(!duplicated(date_time))%>%#for nests with dupicated or overlapping ibutton data files
      dplyr::select(-int,-int_flag)
    
      # check if there are any extreme/unreasonable values 
      # these are also probably taken during deployment or after collection
    if(any(ME_data[[j]]$temp>50|ME_data[[j]]$temp<=-5)){
      
        # get middle half of time series
      mid<-ME_data[[j]][c(round(nrow(ME_data[[j]])/4):(3*(round(nrow(ME_data[[j]])/4)))),]
      if(any(mid$temp>50|mid$temp<=-5)){
        # toss all data for readings with extremes in the middle of the time series.
        ME_data[[j]]<-ME_data[[j]][0,]
        
      }else{
        
        # otherwise remove temps with extreme readings in the beginning or end of the timeseries
        hold<-ME_data[[j]]%>%
          rowid_to_column()
        half1<-hold[c(1:round(nrow(hold)/2)),]
        half2<-hold[c(round(nrow(hold)/2):nrow(hold)),]
        indxtop<-half1[max(which(half1$temp>50|half1$temp<=-5)),]$rowid 
        indxbtm<-half2[min(which(half2$temp>50|half2$temp<=-5)),]$rowid 
          # if there is no extreme value and there isn't a row number, give the first or last row of the data
        indxtop<-1
        if(!(indxtop%in%c(1:10000))){
        }
        if(!(indxbtm%in%c(1:10000))){
          indxbtm<-nrow(ME_data[[j]])
        }
        ME_data[[j]]<-ME_data[[j]][indxtop:indxbtm,]
        
      }
    }
  }
  
  
  #combine all of the tables by column into one dataframe
  ME_nest_data_list[[i]] <- do.call(rbind,ME_data)%>%
    mutate(temp_unit="C")
}

ME_nest_data <- do.call(rbind,ME_nest_data_list)

    # add standard columns that match the other state datasets
ME_nest_data<-ME_nest_data%>%
  mutate(year=year(date_time),
         date=lubridate::as_date(date_time),
         time=hms::as_hms(date_time))%>%
  select(date_time,year,site,nest_num,temp,nest)%>%
  #remove ibuttons set to incorrect dates
  filter(year%in%c(2016,2017,2019,2020))



#------------------
# 4. New Hampshire- individual nest csv files with header lines before column names. Multiple years.
    # remove the header lines and select column variables
NH_data<- lapply(list.files(paste0(dat_path,"Inputs/ibutton_data/NH_Nests/"),full.names = T),
                 function(x) dplyr::select(read.csv(x,skip=str_which(readLines(x),"Value")-1),
                                           date_time=Date.Time,temp=Value)
)

    # add the nest id to each file as a variable
nest_ids<-list.files(paste0(dat_path,"Inputs/ibutton_data/NH_Nests/"),full.names = F)
nest_ids<-gsub(".csv|.csv|\\(.*\\)| .*|_.*_.*","",nest_ids)#account for the various file naming styles
for (j in 1:length(NH_data)){
  NH_data[[j]]<-NH_data[[j]]%>%
    mutate(date_time=lubridate::mdy_hms(date_time),
           nest=nest_ids[j],
           site=substr(nest,1,2),
           nest_num=substr(nest,(nchar(nest)-2),nchar(nest)),
           # add missing leading zeros on nest numbers
           nest= ifelse(!(substr(nest_num,1,1)%in%c("0","1")),#assuming nest counts dont go above 199 in a given year
                        paste0(substr(nest,1,(nchar(nest)-2)),"0",substr(nest_num,2,nchar(nest_num))),
                        nest),
           nest_num=ifelse(!(substr(nest_num,1,1)%in%c("0","1")),
                           paste0("0",substr(nest_num,2,nchar(nest_num))),
                           nest_num)
    )%>%
    arrange(date_time)
  
    # remove the first and last hour of monitoring to be safe because we don't have deploy/collect times
  front<-interval(NH_data[[j]]$date_time[1],NH_data[[j]]$date_time[1]+minutes(60))
  back<-interval(NH_data[[j]]$date_time[nrow(NH_data[[j]])]-minutes(60),NH_data[[j]]$date_time[nrow(NH_data[[j]])])
  NH_data[[j]]<-NH_data[[j]]%>%
    mutate(int=interval(date_time,date_time),
           int_flag=ifelse(int_overlaps(int,front)|int_overlaps(int,back),0,1))%>%
    filter(int_flag==1)%>%
    filter(!duplicated(date_time))%>%#for nests with duplicated or overlapping ibutton data files
    dplyr::select(-int,-int_flag)
  
    # check if there are any extreme/unreasonable values 
    # these are also probably taken during deployment or after collection
  if(any(NH_data[[j]]$temp>50|NH_data[[j]]$temp<=-5)){
    
      # get middle half of time series
    mid<-NH_data[[j]][c(round(nrow(NH_data[[j]])/4):(3*(round(nrow(NH_data[[j]])/4)))),]
    if(any(mid$temp>50|mid$temp<=-5)){
      # toss all data for readings with extremes in the middle of the time series.
      NH_data[[j]]<-NH_data[[j]][0,]
      
    }else{
      
      #otherwise remove temps with extreme readings in the beginning or end.
      hold<-NH_data[[j]]%>%
        rowid_to_column()
      half1<-hold[c(1:round(nrow(hold)/2)),]
      half2<-hold[c(round(nrow(hold)/2):nrow(hold)),]
      indxtop<-half1[max(which(half1$temp>50|half1$temp<=-5)),]$rowid 
      indxbtm<-half2[min(which(half2$temp>50|half2$temp<=-5)),]$rowid 
        # if there is no extreme value, so no row number, use the first and last row of the data
      if(!(indxtop%in%c(1:10000))){
        indxtop<-1
      }
      if(!(indxbtm%in%c(1:10000))){
        indxbtm<-nrow(NH_data[[j]])
      }
      NH_data[[j]]<-NH_data[[j]][indxtop:indxbtm,]
      
    }
  }
}

#combine all of the tables by column into one dataframe
NH_nest_data <- do.call(rbind,NH_data)%>%
  mutate(temp_unit="C")

# add standard columns that match the other state datasets
NH_nest_data<-NH_nest_data%>%
  mutate(year=year(date_time),
         date=lubridate::as_date(date_time),
         time=hms::as_hms(date_time))%>%
  select(date_time,year,site,nest_num,temp,nest)





## REMOVE DATES OUTSIDE OF iBUTTON DEPLOYMENT-RETRIEVAL FOR DATASETS WITH METADATA
#----------------------------------------------------------------------------------
#---------------------
# 1. Connecticut
#those with deployment dates but not times, just assume the earliest is 6 am to start a field day
ct_nest_metadata$time_deploy[ct_nest_metadata$time_deploy==""]<-"6:00:00"
ct_nest_metadata$time_retrieve[ct_nest_metadata$time_retrieve==""]<-"6:00:00"

#turn dates and times into date-time objects and time intervals of active data collection
ct_nest_metadata<-ct_nest_metadata%>%
  mutate(date_time_deploy=mdy_hms(paste(date_deploy,time_deploy)),
         date_time_retrieve=mdy_hms(paste(date_retrieve,time_retrieve)),
         valid_interval=interval(date_time_deploy,date_time_retrieve))%>%
  #remove ibuttons that malfunctioned, or are missing collection data
  filter(data_available%in%c("Y","y")&!is.na(date_time_deploy)&!is.na(date_time_retrieve))
ct_nest_data<-ct_nest_data%>%
  right_join(ct_nest_metadata,by=c("nest"))%>%
  #remove times before deployment and after retrieval from the nest
  filter(int_overlaps(interval(date_time,date_time),valid_interval))%>%
  select(date_time,year,site,nest_num,temp,nest)


#---------------------
# 2. New Jersey
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
  select(date_time,year,site,nest_num,temp,nest)



## EXPORT CLEAN iBUTTON DATA
#-------------------------------------------------------------------------------
if(!file.exists(paste0(dat_path,"Outputs/Cleaned_iButton_Data/ct_ibutton_2013.csv"))){
write.csv(ct_nest_data,paste0(dat_path,"Outputs/Cleaned_iButton_Data/ct_ibutton_2013.csv"),row.names = F)
write.csv(nj_nest_data,paste0(dat_path,"Outputs/Cleaned_iButton_Data/nj_ibutton_2013.csv"),row.names = F)
write.csv(ME_nest_data,paste0(dat_path,"Outputs/Cleaned_iButton_Data/me_ibutton_2016_2020.csv"),row.names = F)
write.csv(NH_nest_data,paste0(dat_path,"Outputs/Cleaned_iButton_Data/nh_ibutton_2016.csv"),row.names = F)
}