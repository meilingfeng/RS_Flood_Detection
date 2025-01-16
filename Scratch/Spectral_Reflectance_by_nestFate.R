

#average spectral reflectance for each band for different nest fates
spect_sum<-spect%>%
  pivot_longer(starts_with("B"),names_to = "Band",values_to = "SR")%>%
  group_by(Band,fat_spc)%>%
  summarise(Mean=mean(SR, na.rm=T)#,
            #Min=min(SR, na.rm=T),
            #Max=max(SR, na.rm=T)
  )%>%
  filter(fat_spc%in%c("DEPREDATED","FLOODED","FLEDGED"))

ggplot(spect_sum,aes(x=Band,y=Mean,group=fat_spc,color=fat_spc))+
  geom_line()+
  #geom_ribbon(data=spect_sum,aes(ymin=Min,ymax=Max,color=fat_spc,fill=fat_spc),alpha=0.1)+
  labs(color="Nest Fate", x="Wavelength", y="Mean Surface Reflectance")+
  theme_classic(base_size=12)
# Normalized difference between blue band and red or NIR seems most distinguishable for predation?
# Red and NIR for flooding


#site average flooding frequency over time
flood_annual<-flood%>%
  group_by(Year,site)%>%
  summarise(mean_flood=mean(flood_freq_annual,na.rm=T))

ggplot(flood_annual,aes(x=Year,y=mean_flood,group=site,color=site))+
  geom_line()


hist(flood$fldg_rt)

ggplot(data=flood,aes(x=flood_freq_annual,y=fate,group=fat_spc))+
  geom_point()+
  geom_smooth(method="glm")


#distribution of surface reflectance for all images at all nests
hist(spect_ts$B6)
hist(spect_ts$B4)
hist(spect_ts$B3)
