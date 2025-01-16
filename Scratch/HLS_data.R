
#load packages
packages <- c('terra','jsonlite','sp','httr',
              'rasterVis','tidyverse','magrittr','RColorBrewer','xml2','dygraphs',
              'xts','DT', 'rprojroot','sf','leaflet')
invisible(lapply(packages, library, character.only = TRUE))

#set directories
outDir<-"D:/Flood_Index/Data/Outputs/"
inDir<-"D:/Flood_Index/Data/Inputs/"

#assign the LPCLOUD STAC Search URL to an object
search_URL <- 'https://cmr.earthdata.nasa.gov/stac/LPCLOUD/search'

#set the path to your netrc file. This has your login credentials for NASA earthdata.
source("Functions/earthdata_netrc_setup.R")



# Query data that meet your location and time of interest
#1. list which data products you want to query from. We want both the sentinel and landsat data.
HLS_col <- list("HLSS30.v2.0", "HLSL30.v2.0")

#2. read in your spatial regions of interest
marsh<-terra::vect(paste0(inDir,"Nests/SALS_breeding_19Apr23.shp"))
marsh_sf<-st_as_sf(marsh)%>%
  sf::st_transform('+proj=longlat +datum=WGS84')

ggplot(filter(marsh_sf,STATE=="CT", area_m>50000)[1,])+
  geom_sf()+
  #coord_sf(xlim=c(-73.5,-73.0),ylim=c(41.3,41.0),expand=F,crs="EPSG:4326")

leaflet() %>% 
  addPolygons(data = filter(marsh_sf,STATE=="CT", area_m>2000)[1,], fill = FALSE) %>%
  addProviderTiles(providers$Esri.WorldImagery) %>% 
  addMiniMap(zoomLevelFixed = 5)
