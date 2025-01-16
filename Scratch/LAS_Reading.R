
library(lidR)
library(tidyverse)
library(terra)
library(sf)


#https://r-lidar.github.io/lidRbook/io.html

dat_path<-"D:/Flood_Index/Data/lidar/"



## Read the data into R
#--------------------------------------------------------------------------------------

# LAS or LAZ file
bb1<-readLAS(paste0(dat_path,"Hammonasset/08_16_21/LiDAR/LAS/Hammonasset_1.las"),
             #select jsut the point attributes of interst to save memory (i=intensity,r= return number,c=classification)
             select = "xyzirc")#,
             #filter="-keep_class 2") #readLAS(filter = "-help") filters the points based on attributes

# Process all the tiles at once
  # 1. read in all the tiles as a LAScatalog object
ctg <- readLAScatalog(paste0(dat_path,"Hammonasset/08_16_21/LiDAR/LAS/"))
ctg
las_check(ctg)

# use this to write any applications to the tiles to new files 
opt_output_files(ctg) <- paste0(tempdir(), "{*}_classified")

  # 2. read in nests to clip around a 30m buffer of nests
nest<-st_read(paste0(dat_path,"../Nests/SALS_nests_2010_2020_dist_err_removed.shp"))%>%
  mutate(x = st_coordinates(.)[,1],
         y = st_coordinates(.)[,2])%>%
  st_transform(st_crs(ctg))%>%
  #crop to the extent of the tiles (probably want to buffer this too to avoid points near edges)
  st_crop(ctg)%>%
  #buffer your ROI buffer, if you want 30m, do 50m
  st_buffer(50)%>%
  #dissolve the buffered regions of interest
  st_union()%>%
  st_cast("POLYGON")

#clip the LAS collection of tiles to the buffered nest locations
roi <- clip_roi(ctg, nest)
#this gives you a list of LAS objects, one for each nest buffer or ROI
writeLAS(roi)

## Validate the data. Check if it has good enough quality for analysis
#----------------------------------------------------------------------------------------------
  # will flag any duplicate points, invalid return numbers, invalid coordinate references systems
las_check(bb1)


## Plot the data
#----------------------------------------------------------------------------------------------

# 1. plot point cloud (3D)
plot(bb1,
     #can specify other attributes to display like intensity, Z is elevation
     color="Z",
     bg="white",
     axis=T,
     legend=T)

# 2. show a cross section (2D)
#clip the point cloud to a start and end point of a transect you are interested in
  #take a 200m transect from the middle, going north/south
  #take the range of y axis, divide in half and add to the min to get the middle.
  #take +- 100 of this
halfy<-(ext(bb1)[4]-ext(bb1)[3])/2
y1<-(ext(bb1)[3]+halfy)-100
y2<-(ext(bb1)[3]+halfy)+100
  #pick an x location
halfx<-(ext(bb1)[2]-ext(bb1)[1])/2
x<-(ext(bb1)[1]+halfx)
  #create transect points with these coordinates
p1<-c(x,y1)
p2<-c(x,y2)
  #clip the point cloud to this transect, specify some width of the transect to capture points along it
las_tr <- clip_transect(bb1, p1, p2, width = 5, xz = TRUE) 
  #plot the X and Z coordinates as a dataframe using payload()
ggplot(payload(las_tr), aes(X,Z, color = Z)) + 
  geom_point(size = 0.5) + 
  coord_fixed(ratio = 10)+
  scale_color_gradientn(colours = height.colors(50))


## Classify true ground points
#----------------------------------------------------------------------------------
# provide a window size or sequence of window sizes and a height difference threshold to mark a point as ground
# default uses the parameters from Zhang et al 2003
  # the benefit to using sequential window sizes is they can remove non ground objects of different sizes (if a building is larger than a window size, no points will be removed)
    # but if your terrain has small peaks in terrain, you could lose these higher ground elevations
  # an elevation threshold helps retain these high ground points
    # points in the window that fall within a height above the lowest elevation are selected as ground points
    # the height of this window is determined by  the vertical accuracy of the lidar data
#This data is in ft
ws=seq(3,33,3) #range ~1-10m by 1m intervals
th=rep(0.2,length(ws))
las_tr_class <- classify_ground(las_tr, algorithm = pmf(ws = 33, th = 0.1))

#plot the newly classified ground points and original
ggplot(payload(las_tr_class), aes(X,Z, color = Classification)) + 
  geom_point(size = 0.5) + 
  coord_fixed(ratio = 10)+
  scale_color_gradientn(colours = height.colors(50))
ggplot(payload(las_tr), aes(X,Z, color = Classification)) + 
  geom_point(size = 0.5) + 
  coord_fixed(ratio = 10)+
  scale_color_gradientn(colours = height.colors(50))


## DTM/DEMs
#------------------------------------------------------------------------------------
#TINs create more edge artifacts
dtm_tin <- rasterize_terrain(bb1, res = 3, algorithm = tin()) #res is cell resolution in data units, defaults to 1.3ft = 1m res
plot_dtm3d(dtm_tin, bg = "white") 

#Inverse distance weighting/K nearest neighbor (taking the weighted mean of neighboring points, higher weights=closer points) creates bumps that are less accurate
dtm_idw <- rasterize_terrain(bb1, res=3, algorithm = knnidw(k = 10, p = 2)) # k= # nearest neighbors to include, p= power for distance weighting (higher power means only closest points are used)
plot_dtm3d(dtm_idw, bg = "white") 

#kriging is most accurate with least edge artifacts but takes longer to compute. It's not recommended for large areas and also struggles with water bodies.
dtm_kriging <- rasterize_terrain(bb1, algorithm = kriging(k = 40)) #k nearest neighbors
plot_dtm3d(dtm_kriging, bg = "white") 

#most important for all methods is to buffer your area of interest!


# read in the premade DTMs from whiteout
bb1_dtm<-rast(paste0(dat_path,"Bride_Brook/LiDAR/DTM/BrideBrook_1.tif"))
res(bb1_dtm)
crs(bb1_dtm)
