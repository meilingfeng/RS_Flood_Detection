library(tidyverse)

## Set file path to data
# ------------------------------------------------------------
dat_path<-"D:/Flood_Index/Data/"
path_out<-"D:/Flood_Index/Outputs/"

dat<-read.csv(paste0(path_out,"nests_flood_metrics_dataset.csv"))
#adjust the one proportion over 1 back down to 1
dat[dat$fld_prp>1,]$fld_prp<-1

#what does our response look like?
hist(dat$fld_prp)


#sample size per site and year
table(dat$site)
table(dat$Year)



# what is flood risk at nests plotted against drivers of flood risk look like?
plot(dat$fld_prp,dat$LOMARSH,xlab="Proportion of Nest Flooded",ylab="Proportion Low Marsh Surrounding Nest")
# plot the fit of flooded proportion over each flood predictor
ggplot(dat,aes(x=fld_prp,y=flats.1,group=site))+
  geom_point()+
  stat_smooth(method="gam",formula=y~s(x,k=5,bs="cr"),
              se=F,col='gray')+
  xlab("Proportion of Nest Flooded")+
  ylab("Flooding Frequency (FLATS>0.1)")+
  theme_bw()

ggplot(dat,aes(x=fld_prp,y=flats.9,group=site))+
  geom_point()+
  stat_smooth(method="gam",formula=y~s(x,k=5,bs="cr"),
              se=F,col='gray')+
  xlab("Proportion of Nest Flooded")+
  ylab("Flooding Frequency (FLATS>0.9)")+
  theme_bw()

ggplot(dat,aes(x=fld_prp,y=LOMARSH,group=site))+
  geom_point()+
  stat_smooth(method="gam",formula=y~s(x,k=5,bs="cr"),
              se=F,col='gray')+
  xlab("Proportion of Nest Flooded")+
  ylab("Proportion Low Marsh Surrounding Nest")+
  theme_bw()

ggplot(dat,aes(x=fld_prp,y=tcwet.1,group=site))+
  geom_point()+
  stat_smooth(method="gam",formula=y~s(x,k=5,bs="cr"),
              se=F,col='gray')+
  xlab("Proportion of Nest Flooded")+
  ylab("Flooding Frequency (Tasseled Cap>0.1)")+
  theme_bw()

ggplot(dat,aes(x=fld_prp,y=tcwet.9,group=site))+
  geom_point()+
  stat_smooth(method="gam",formula=y~s(x,k=5,bs="cr"),
              se=F,col='gray')+
  xlab("Proportion of Nest Flooded")+
  ylab("Flooding Frequency (Tasseled Cap>0.9)")+
  theme_bw()



#looks like zero inflation....
#what might be determining the 0's whether or not a nest floods in general?
#elevation, presence of other threats like predators, time since new moon?
dat<-dat%>%
  mutate(flooded=ifelse(fld_prp>0,1,0))

ggplot(dat,aes(x=elevation,y=flooded,group=site))+
  geom_point(aes(color=site))+
  stat_smooth(method="gam",formula=y~s(x,k=5,bs="cr"),
              se=F,col='gray')+
  xlab("Nest Floods?")+
  ylab("Elevation")+
  theme_bw()

ggplot(dat,aes(x=prd_prp,y=flooded,group=site))+
  geom_point(aes(color=site))+
  stat_smooth(method="gam",formula=y~s(x,k=5,bs="cr"),
              se=F,col='gray')+
  xlab("Nest Floods?")+
  ylab("Proportion Nest Predated")+
  theme_bw()


#sum(resid(mod3.1,type='pearson')ˆ2)/df.residual(mod3.1)