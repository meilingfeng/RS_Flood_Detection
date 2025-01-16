# optimal cut off to max nest flooding sense and spec?

thr_dat<-data.frame(threshold=seq(0.1:1,by=0.1)[-10],sensitivity=rep(NA,9),specificity=rep(NA,9))
#when flood =1 and nest flood =0, type 1, false positive
#when flood= 0 when nest flood =1, tpye 2, false negative
#sensitivity is true positive, floods both =1
#specificity is true negative, floods both =0
thr_dat[1,3]<-as.vector(count(spect_flood, flood.1, nest_flood)[1,3])
thr_dat[1,2]<-as.vector(count(spect_flood, flood.1, nest_flood)[4,3])

thr_dat[2,3]<-as.vector(count(spect_flood, flood.2, nest_flood)[1,3])
thr_dat[2,2]<-as.vector(count(spect_flood, flood.2, nest_flood)[4,3])

thr_dat[3,3]<-as.vector(count(spect_flood, flood.3, nest_flood)[1,3])
thr_dat[3,2]<-as.vector(count(spect_flood, flood.3, nest_flood)[4,3])

thr_dat[4,3]<-as.vector(count(spect_flood, flood.4, nest_flood)[1,3])
thr_dat[4,2]<-as.vector(count(spect_flood, flood.4, nest_flood)[4,3])

thr_dat[5,3]<-as.vector(count(spect_flood, flood.5, nest_flood)[1,3])
thr_dat[5,2]<-as.vector(count(spect_flood, flood.5, nest_flood)[4,3])

thr_dat[6,3]<-as.vector(count(spect_flood, flood.6, nest_flood)[1,3])
thr_dat[6,2]<-as.vector(count(spect_flood, flood.6, nest_flood)[4,3])

thr_dat[7,3]<-as.vector(count(spect_flood, flood.7, nest_flood)[1,3])
thr_dat[7,2]<-as.vector(count(spect_flood, flood.7, nest_flood)[4,3])

thr_dat[8,3]<-as.vector(count(spect_flood, flood.8, nest_flood)[1,3])
thr_dat[8,2]<-as.vector(count(spect_flood, flood.8, nest_flood)[4,3])

thr_dat[9,3]<-as.vector(count(spect_flood, flood.9, nest_flood)[1,3])
thr_dat[9,2]<-as.vector(count(spect_flood, flood.9, nest_flood)[4,3])

ggplot(thr_dat%>%pivot_longer(starts_with("s"),names_to = "Test",values_to = "Count"),aes(x=threshold,y=Count,group=Test,color=Test))+
  geom_line()

#looks like a value around 0.5 will max both sens and spec
count(spect_flood, flood.5, nest_flood)[1,3]/nrow(spect_flood)#spec
count(spect_flood, flood.5, nest_flood)[4,3]/nrow(spect_flood)#sense
count(spect_flood, flood.5, nest_flood)[3,3]/nrow(spect_flood)#false pos (1)
count(spect_flood, flood.5, nest_flood)[1,3]/nrow(spect_flood)#false neg (2)

count(spect_flood, flood.6, nest_flood)[1,3]/nrow(spect_flood)#spec
count(spect_flood, flood.6, nest_flood)[4,3]/nrow(spect_flood)#sense
count(spect_flood, flood.6, nest_flood)[3,3]/nrow(spect_flood)#false pos (1)
count(spect_flood, flood.6, nest_flood)[1,3]/nrow(spect_flood)#false neg (2)