#Data tidying
library(tidyverse)
library(lubridate)
#spatial analysis
library(sf)
library(terra)


########################################################
# Fix any coordinate issues with the nest locations
########################################################


## Set file path to data
# -------------------------------------------
dat_path<-"D:/Flood_Index/Data/"
path_out<-"D:/Flood_Index/Outputs/"



## 1. Load Nest data
# -------------------------------------------
## Sals nest locations cleaned
nest_sals<-st_read(paste0(dat_path,"Nests/SALS_nests_2010_2020_dist_err_removed.shp"))

## Nest fates information 
fates<-read.csv(paste0(dat_path,"Nests/NestFates_2001-2020.csv"),na.strings=c("","NOT REC","NA"))%>%
  dplyr::select("id"="SHARPNestID","fate_ult"="UltimateNestFate","date"="EstFirstEggDate",
                "maxeggs"="MaxNumEggs","Nsuccess"="NumFledged",
                "NumEggsFloodedInNest","NumEggsFloodedFromNest","NumEggsFloodedMissing",
                "NumChicksFlooded","NumChicksFloodedMissing",
                "NumEggsDepredated", "NumEggsDepredatedMissing","NumChicksDepredated","NumChicksDepredatedMissing")%>%
  mutate(across(c("NumEggsFloodedInNest","NumEggsFloodedFromNest","NumEggsFloodedMissing",
                  "NumChicksFlooded","NumChicksFloodedMissing","maxeggs","Nsuccess",
                  "NumEggsDepredated", "NumEggsDepredatedMissing","NumChicksDepredated","NumChicksDepredatedMissing"),
                as.numeric))

## site info
sites<-read.csv(paste0(dat_path,"Nests/compiled_site_table_12_9_22.csv"))



## 2. calculate nesting response variables
# -------------------------------------------
fates<-rowwise(fates)%>%
  #reclassify some fate categories
  mutate(# sum all the different flood mortality counts at each nest
    Nflood_fail=sum(c(NumEggsFloodedInNest,NumEggsFloodedFromNest,NumEggsFloodedMissing,
                      NumChicksFlooded,NumChicksFloodedMissing),na.rm = T),
    # take the proportion of flood moralities to the total eggs that were in a nest
    flood_prop=round(Nflood_fail/maxeggs,2),
    # Overall proportion of eggs that fledge out of eggs that were laid, regardless of cause
    Nfail=maxeggs-Nsuccess, 
    #reclassify some ultimate fate categories that are unknown into one group
    fate_ult=case_when(
      fate_ult%in%c("NEVER HAD","NEVER HAD EGGS")~"INACTIVE",
      is.na(fate_ult)~"UNKNOWN",
      !(fate_ult%in%c("NEVER HAD","NEVER HAD EGGS")|is.na(fate_ult))~fate_ult
    )
  )




#3. Combine the response data with the nest location data
#--------------------------------------------------------------------------
nest_dat<-left_join(nest_sals, fates, by="id")%>%
  #keep only locations with fate data (had a max egg count)
  filter(!is.na(maxeggs),!is.na(flood_prop),flood_prop!="NaN")%>%
  mutate(date= mdy(date),
         month=month(date),
         day=day(date))%>%
  # keep records that have an estimated first egg date
  filter(!is.na(date))%>%
  #add site information
  left_join(sites,by=c("site"="site.code"))


#2. Add some nuisance variables
#--------------------------------------------------------------------------
# 1. Time nest was initiated since the last new moon - estimate of how late in the flood free window the nest was started
    # later nests, more time since last moon, have less time to fledge before the nest highest high tide. 
moon<-read.csv(paste0(dat_path,"Nests/new_moon_dates.csv"))%>%
  mutate(new.moon.date=mdy(new.moon.date),
         lag=lead(new.moon.date))

#create a list of potential dates that come after each moon date to join to the nest data
moon.dates<-list()
for(i in 1:nrow(moon)){
moon.dates[[i]]<-seq(moon[i,]$new.moon.date,moon[i,]$lag,by="days")
}

#add to each new.moon.date
moon.dat<-expand.grid(new.moon.date=moon$new.moon.date, date=do.call("c",moon.dates))

#***Needs work still
#nest_dat<-nest_dat%>%
#  left_join(moon,by=c("date"="new.moon.date"))%>%
#  mutate(time.since.moon=date-new.moon.date)
  
# 2. Proportion of nest mortalities due to non-flooding causes, predation
nest_dat<-mutate(rowwise(nest_dat),
   # sum all the different flood mortality counts at each nest
                  Npred_fail=sum(c(NumEggsDepredated,NumEggsDepredatedMissing,
                                    NumChicksDepredated,NumChicksDepredatedMissing),
                                  na.rm = T),
  # take the proportion of flood moralities to the total eggs that were in a nest
                  pred_prop=round(Npred_fail/maxeggs,2))



#3. Filter to subset of sites and years (too many landsat images to cover all of them!)
#------------------------------------------------------------------------------------
# select only sites with 2013-2015 observation years and have data in 2019 (to extrapolate to)
t<-summarize(group_by(nest_dat,
                      Year,Site,State),
             n=n())%>%
  filter(Year%in%c(2013:2015,2019))

nest_dat2<-nest_dat%>%
  filter(site%in%c("AT","HM","CL"))%>% #choose AT&T, Hammo, and Chapmans Landing because they have the highest data availability in these years and cover a lat gradient.
  filter(Year%in%c(2013:2015,2019))%>%
  select(-lat_bin,-Region,-utm.zone)

nest_dat2<-rename(nest_dat2,site_name=Site)

#sample size
nrow(nest_dat2)
table(nest_dat2$site) #chapmans landing has almost double number of obs
table(nest_dat2$Year) #pretty even across years though


st_write(nest_dat2,paste0(dat_path,"Nests/nests_sals_flood.shp"),delete_layer=T)
write.csv(st_drop_geometry(nest_dat2),paste0(dat_path,"Nests/nests_sals_flood.csv"),row.names = F)





