library(tidyverse)
library(sf)


inDir<-"D:/Flood_Index/Data/Inputs/"
outDir<-"D:/Flood_Index/Data/Outputs/"

nests<-st_read(paste0(inDir,"Nests/nest_locations_01_3_23.shp"))
flood_info<-read.csv(paste0(outDir,"Nest_flood_events/nest_floods_26.7thresh.csv"))
