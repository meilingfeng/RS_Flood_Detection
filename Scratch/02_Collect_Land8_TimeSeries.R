
## Packages and dependencies
#----------------------------------------------------------------------------
library(tidyverse)
library(sf)
library(terra)

## Set file path to data
# ------------------------------------------------------------
dat_path<-"D:/Flood_Index/Data/"
path_out<-"D:/Flood_Index/Outputs/"


## Load data
# ------------------------------------------------------------
## Landsat8 images
  #Tried selecting same path-row except when there was 100% cloud cover
  # CT
bands_ct<-map(unlist(map(paste0(dat_path,"Landsat8_C2/CT"),~list.files(.,pattern = "B[2,3,4,5,6,7].TIF$",full.names=T))),rast) # L8 bands are 2=blue,3=green, 4=red, 5=NIR, 6=SWIR, 7=SWIR
qa_ct<-map(unlist(map(paste0(dat_path,"Landsat8_C2/CT"),~list.files(.,pattern = "QA_PIXEL.TIF$",full.names=T))),rast)
  # NJ
bands_nj<-map(unlist(map(paste0(dat_path,"Landsat8_C2/NJ"),~list.files(.,pattern = "B[2,3,4,5,6,7].TIF$",full.names=T))),rast)
qa_nj<-map(unlist(map(paste0(dat_path,"Landsat8_C2/NJ"),~list.files(.,pattern = "QA_PIXEL.TIF$",full.names=T))),rast)
  # NH
bands_nh<-map(unlist(map(paste0(dat_path,"Landsat8_C2/NH"),~list.files(.,pattern = "B[2,3,4,5,6,7].TIF$",full.names=T))),rast)
qa_nh<-map(unlist(map(paste0(dat_path,"Landsat8_C2/NH"),~list.files(.,pattern = "QA_PIXEL.TIF$",full.names=T))),rast)


## Nests
nest<-st_read(paste0(dat_path,"Nests/nests_sals_flood.shp"))

  # divide nest data by site
nest_ct<-filter(nest,site=="HM")%>%
  st_transform(crs(bands_ct[[1]]))
nest_nh<-filter(nest,site=="CL")%>%
  st_transform(crs(bands_nj[[1]]))
nest_nj<-filter(nest,site=="AT")%>%
  st_transform(crs(bands_nh[[1]]))

## arrange data by type: spectral bands, QA bands, nest locations
# pair in order of their geographic locations
land_list<-list(bands_nj,bands_ct,bands_nh)
qa_list<-list(qa_nj,qa_ct,qa_nh)
nest_list<-list(nest_nj,nest_ct,nest_nh)


## Create Functions
#----------------------------------------------------------------
# Function to rename rasters in a list
Naming<- function(b,n){
  names(b)<-n
  return(b)
}

# Function to extract whether a pixel is clear from bit coded QA values 
# (From LandsatTS package, "lsat_clear_data()" function)
clear_value = function(x) {
  # reverse order of bits so read from left to right
  bit_str = paste(as.integer(intToBits(x)), collapse="")
  # conditions
  filled = substr(bit_str, 1, 1) == '1'
  cloud_shadow = substr(bit_str, 5, 5) == '1'
  not_clear = substr(bit_str, 7, 7) == '0'
  
  if(filled | cloud_shadow | not_clear){
    return(0)
  }  else{
    return(1)
  }
}

# Function to extract whether a pixel is water from bit coded QA values 
# (From LandsatTS package, "lsat_clear_data()" function)
# filter water
water_flag = function(x) {
  # reverse order of bits, left to right
  bit_str = paste(as.integer(intToBits(x)), collapse="")
  water = substr(bit_str, 8, 8) == '1'
  if(water){return(1)}  else{return(0)}
}



## Reformatting
#----------------------------------------------------------------
# for each set of landsat images, rename them as date-band# combos
# loop through lists for each site
for (i in 1:length(nest_list)){
# get the band list corresponding with each site
bands<-land_list[[i]]
# rename the raster layers by date_band
  # first extract the dates and bands for each (want sections 4 and 9 since these have date and band info in file names)
name_list<-map(map(bands,names),~paste(strsplit(.,"_")[[1]][c(4,9)],collapse="_"))
  # then rename the bands as these date_bands
bands2<-mapply(Naming, bands, name_list)

land_list[[i]]<-bands2
}

#Do the same for the QA bands
for (i in 1:length(qa_list)){
  # get the band list corresponding with each site
  qa<-qa_list[[i]]
  # rename the raster layers by date_band
  # first extract the dates and bands for each (want sections 4 and 8 since these have date and QA info in file names)
  name_list<-map(map(qa,names),~paste(strsplit(.,"_")[[1]][c(4,8)],collapse="_"))
  # then rename the bands as these date_bands
  qa2<-mapply(Naming, qa, name_list)
  
  qa_list[[i]]<-qa2
}



## Extract Landsat surface reflectance (SR) data at nests
#----------------------------------------------------------------
#create empty lists to hold processed surface reflectance for nests at each site
sr_list<-list()

for (i in 1:length(nest_list)){
# extract pixel SR values at each location
t<-map(land_list[[i]],~terra::extract(.,vect(nest_list[[i]])))

#extract pixel QA values at each location
t_qa<-map(qa_list[[i]],~terra::extract(.,vect(nest_list[[i]])))

  #combines all the band extraction columns into one table like cbind
t2<-t%>% reduce(full_join, by='ID')%>%
  cbind(nest_list[[i]][,"id"])%>%
  dplyr::select(-ID)
t2_qa<-t_qa%>% reduce(full_join, by='ID')%>%
  cbind(nest_list[[i]][,"id"])%>%
  dplyr::select(-ID)




## Apply landsat pre-processing
#-------------------------------------------------------------
# arrange dataframe with nest id, date, and band numbers
t3<-t2%>%
  pivot_longer(-c('id','geometry'), names_to = c("date","band"),names_sep = "_",values_to="surf_r")%>%
  mutate(date=ymd(date))%>%
  pivot_wider(names_from = band, values_from = surf_r)%>%
#apply rescaling to surface reflectance values
  mutate(across(starts_with("B"),~.x*0.0000275-0.2))
t4<-t3%>%
#remove impossibly high or low reflectance 
  filter(across(starts_with("B"),~.x>0.005&.x<1.0))

# also rearrange the QA values and add them to the SR dataframe
sr<-t2_qa%>%
  pivot_longer(-c('id','geometry'), names_to = c("date","band"),names_sep = "_",values_to="surf_r")%>%
  mutate(date=ymd(date))%>%
  pivot_wider(names_from = band, values_from = surf_r)%>%
  dplyr::select(-geometry)%>%
  right_join(t4,by=c("id","date"))

#Create a new column signifying if the pixel is clear or not
sr$QA_clear<-as.numeric(lapply(sr$QA,clear_value))
#and then set values to NA if they are not clear
sr<-sr%>%
  mutate(across(starts_with("B"),~.x*QA_clear),
         across(starts_with("B"),~ifelse(.x==0,NA,.x))
         )

## Create FMask water indicator
#-------------------------------------------------------------
#Create a new column signifying if the pixel is water or not (FLOOD INDICATOR)
sr$fmask<-as.numeric(lapply(sr$QA,water_flag))
#remove QA band column and filter only rows with clear pixels
sr2<-sr%>%
  dplyr::select(-QA,-QA_clear, -geometry)%>%
  filter(across(starts_with("B"),~!is.na(.x)))

sr_list[[i]]<-sr2
}


## Create FLATS and Tasseled Cap water indicators
#---------------------------------------------------------------------------
#Combine spectral time series for each nest location across nesting sites.
sr_all <- do.call("rbind", sr_list)%>%
  mutate(Year=year(date))%>%
  dplyr::select(-date)

#Calculate FLATS and Tasseled Cap indicators
flood<-sr_all%>%
         #first calculate normalized water index (NDWI) bands 4-6/4+6
  mutate(ndwi=(B4-B6)/(B4+B6),
         #then calculate phenology index bands 3-4/3+4
         pheno=(B3-B4)/(B3+B4),
         #finally, calculate FLATS 
         #flats= 1-(1/(exp(-1.6+20*ndwi+68.6*pheno))), # this was in the paper, but it's giving me weird values.
         flats= 1/(1+exp(-(-1.6+20*ndwi+68.6*pheno))), #expit backtransform from logit?
         tcwet=(0.1509*B2)+(0.1973*B3)+(0.3279*B4)+(0.3406*B5)-(0.7112*B6)-(0.4572*B7))

# create a set of thresholds to see which cut off leads to the most accurate predictions

flood2<-flood%>%
  mutate(flats.1= ifelse(flats>0.1,1,0),
         flats.2= ifelse(flats>0.2,1,0),
         flats.3= ifelse(flats>0.3,1,0),
         flats.4= ifelse(flats>0.4,1,0),
         flats.5= ifelse(flats>0.5,1,0),
         flats.6= ifelse(flats>0.6,1,0),
         flats.7= ifelse(flats>0.7,1,0),
         flats.8= ifelse(flats>0.8,1,0),
         flats.9= ifelse(flats>0.9,1,0),
         tcwet.1=ifelse(tcwet>quantile(tcwet,0.1),1,0),
         tcwet.2=ifelse(tcwet>quantile(tcwet,0.2),1,0),
         tcwet.3=ifelse(tcwet>quantile(tcwet,0.3),1,0),
         tcwet.4=ifelse(tcwet>quantile(tcwet,0.4),1,0),
         tcwet.5=ifelse(tcwet>quantile(tcwet,0.5),1,0),
         tcwet.6=ifelse(tcwet>quantile(tcwet,0.6),1,0),
         tcwet.7=ifelse(tcwet>quantile(tcwet,0.7),1,0),
         tcwet.8=ifelse(tcwet>quantile(tcwet,0.8),1,0),
         tcwet.9=ifelse(tcwet>quantile(tcwet,0.9),1,0)
  )

#Fmask did not yield any water pixels


## Calculate standardized flooding frequency 
#----------------------------------------------------------------------------
flood_freq<-group_by(flood2, id)%>%
  filter(Year!=2019)%>% #remove 2019 from this since we are using it as testing data
  summarise(across(contains(c("flats.","tcwet.")),~sum(.x,na.rm = T)/n()))%>%
  ungroup()


##annual frequency
#flood_freq_an<-group_by(flood, id, Year)%>%
#  summarise(flood_freq_annual=sum(flood,na.rm = T)/n(),
#            mean_ndwi_annual=mean(ndwi,na.rm = T),
#            mean_pheno_annual=mean(pheno,na.rm = T))%>%
#  ungroup()
        


## Add flooding metrics to the nest data
#--------------------------------------------------------------
# flooding frequencies for TC and FLATS
nest_dat_final<-flood_freq%>%
  right_join(st_drop_geometry(nest),by=c("id"))


# indirect flood predictors - low marsh proportion and elevation

nest_dat_final<-read.csv(paste0(dat_path,"Indirect_Flood_Measures/SALS_nest_vars_buff15.csv"))%>%
  dplyr::select(id,LOMARSH,elevation,pca)%>% 
  right_join(nest_dat_final,by="id")

# fill in 0's for low marsh proportions with NAs 
nest_dat_final[,"LOMARSH"]<-nest_dat_final[,"LOMARSH"]%>%
  replace(is.na(.),0)


#write final datasets to file
write.csv(nest_dat_final,paste0(path_out,"nests_flood_metrics_dataset.csv"),row.names = F)


