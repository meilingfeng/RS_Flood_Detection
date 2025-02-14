
library(tidyverse)
library(car)
library(gbm)
library(raster)
library(terra)
library(dismo)
library(PresenceAbsence)
library(patchwork)


## 1. Set up
#--------------------------------------------
outDir<-"D:/Flood_Index/Data/Outputs/"
inDir<-"D:/Flood_Index/Data/Inputs/"


nest_dat<-read.csv(paste0(outDir,"CT_nests_flood_index.csv"))

  
#Create objects to hold BRT parameters for each fold
    n.tree.p<-c()
    n.tree.s<-c()
    lr.p<-c()
    lr.s<-c()
    contrib_pres<-list()
    contrib_surv<-list()
    d.pres.brt<-list()
    d.surv.brt<-list()
    
#divide data into folds
    
    nest_dat2<-nest_dat%>%
      filter(flooded_fate==0)%>%
      group_by(cell)%>%
      summarise(across(everything(),first))
    nest_dat3<-nest_dat2%>%
      rbind(filter(nest_dat,flooded_fate==1))
    
    k<-5
    set.seed(123)
    nest_dat3$group<-kfold(nest_dat3,k)
    #check balance in each split dataset
    table(nest_dat3$flooded_fate,nest_dat3$group) # stays mostly well balanced

hist(nest_dat3$flooded_fate)    
    
    # select all predictors and response
nest_dat4<-nest_dat3%>%dplyr::select("nest_id","group","flooded_fate",starts_with(c("water","NDWI","FLATS","TCWet"))&ends_with(c("max",'mean')))

    
    
    for(i in 1:k){
      #divide into testing and training
      nest_dat_train<-as.data.frame(nest_dat4[nest_dat4$group!=i,]%>%dplyr::select(-group,-nest_id))
      
      nest_dat_test<-nest_dat4[nest_dat4$group==i,]%>%dplyr::select(-group,-nest_id)
      
      
      # 2. determine optimal number of trees
      #---------------------------------------------
      set.seed(123)
      #First for nest presence
      # typically want at least 1000 trees, so decrease learning rate if you get less
      brt_nest<-gbm.step(data=nest_dat_train, gbm.x = 2:length(nest_dat_train), gbm.y=1, 
                         family = "bernoulli",
                         tree.complexity = 3,
                         learning.rate=0.0001)# default bag fraction is 0.75
      #start at lr of 0.05, decrease until optimal trees hits 1000
      if(brt_nest$gbm.call$best.trees<1000){
        brt_nest<-gbm.step(data=nest_dat_train, gbm.x = 2:length(nest_dat_train), gbm.y=1, 
                           family = "bernoulli",
                           tree.complexity = 3,
                           learning.rate=0.01)
      }
      if(brt_nest$gbm.call$best.trees<1000){
        brt_nest<-gbm.step(data=nest_dat_train, gbm.x = 2:length(nest_dat_train), gbm.y=1, 
                           family = "bernoulli",
                           tree.complexity = 3,
                           learning.rate=0.005)
      }
      if(brt_nest$gbm.call$best.trees<1000){
        brt_nest<-gbm.step(data=nest_dat_train, gbm.x = 2:length(nest_dat_train), gbm.y=1, 
                           family = "bernoulli",
                           tree.complexity = 3,
                           learning.rate=0.001)
      }
      
      
      
      # list the optimal number of trees and lr for the final models
      n.tree.p[i]<-brt_nest$gbm.call$best.trees
      
      lr.p[i]<-brt_nest$gbm.call$learning.rate
      
      # 2. Variable contribution/importance
      #------------------------------------
      contrib_nest[[i]]<-brt_nest$contributions
      
      
      # 3. predict to testing data
      #--------------------------------------
      preds.nest <- predict.gbm(brt_nest, nest_dat_test,
                                n.trees=brt_nest$gbm.call$best.trees, type="response")

      
      # create df of test observations and predictions to evaluate model performance
      d.nest.brt[[i]] <- data.frame(nest_id=nest_dat2[nest_dat2$group==i,]$nest_id,
                                    obs=nest_dat_test$flooded_fate, 
                                    pred=preds.nest)
    }
    
  
  
  
  
  
  # 4. Create final predictions using all data
  #----------------------------------------------------------------
# select all predictors and response
nest_dat4<-nest_dat3%>%dplyr::select("flooded_fate",starts_with(c("water","NDWI","FLATS","TCWet"))&ends_with(c("max",'mean')))%>%
  mutate(flooded_fate=as.numeric(flooded_fate))%>%
  as.data.frame()

    set.seed(123)
    brt_pres<-gbm.step(data=nest_dat4, gbm.x = 2:length(nest_dat4), gbm.y=1, 
                       family = "bernoulli",
                       tree.complexity = 5,
                       learning.rate=0.0000000000000000001)# default bag fraction is 0.75
    #start at lr of 0.05, decrease until optimal trees hits 1000
    if(brt_pres$gbm.call$best.trees<1000||length(brt_pres)==0){
      brt_pres<-gbm.step(data=pres_dat2[,-1], gbm.x = 2:length(pres_dat2[,-1]), gbm.y=1,
                         family = "bernoulli",
                         tree.complexity = 5,
                         learning.rate=0.01)
    }
    if(brt_pres$gbm.call$best.trees<1000||length(brt_pres)==0){
      brt_pres<-gbm.step(data=pres_dat2[,-1], gbm.x = 2:length(pres_dat2[,-1]), gbm.y=1, 
                         family = "bernoulli",
                         tree.complexity = 5,
                         learning.rate=0.005)
    }
    if(brt_pres$gbm.call$best.trees<1000||length(brt_pres)==0){
      brt_pres<-gbm.step(data=pres_dat2[,-1], gbm.x = 2:length(pres_dat2[,-1]), gbm.y=1, 
                         family = "bernoulli",
                         tree.complexity = 5,
                         learning.rate=0.001)
    }
    
    
    # Final model for nest survival
    set.seed(123)
    surv_dat2<-surv_dat%>%dplyr::select("id","y",all_of(all_terms))
    brt_surv<-gbm.step(data=surv_dat2[,-1], gbm.x = 2:length(surv_dat2[,-1]), gbm.y=1, 
                       family = "bernoulli",
                       tree.complexity = 5,
                       learning.rate=0.005)
    
    # start at lr of 0.05, decrease until optimal trees hits 1000
    if(brt_surv$gbm.call$best.trees<1000||length(brt_surv)==0){
      brt_surv<-gbm.step(data=surv_dat2[,-1], gbm.x = 2:length(surv_dat2[,-1]), gbm.y=1, 
                         family = "bernoulli",
                         tree.complexity = 5,
                         learning.rate=0.001)
    }
    if(brt_surv$gbm.call$best.trees<1000||length(brt_surv)==0){
      brt_surv<-gbm.step(data=surv_dat2[,-1], gbm.x = 2:length(surv_dat2[,-1]), gbm.y=1, 
                         family = "bernoulli",
                         tree.complexity = 2,
                         learning.rate=0.001)
    }
    
    
    
    
    # list the optimal number of trees and lr for the final models
    brt_pres$gbm.call$best.trees
    brt_surv$gbm.call$best.trees
    
    brt_pres$gbm.call$learning.rate    
    brt_surv$gbm.call$learning.rate
    
    
    
    # save final models and their thresholds
    #plot fitted values against predictor
    gbm.plot(brt_pres, n.plots=8, plot.layout=c(2, 4), write.title = FALSE, smooth=T) #, continuous.resolution=100, type="response"
    
    gbm.plot(brt_surv, n.plots=8, plot.layout=c(2, 4), write.title = FALSE, smooth=T)
    
    
    # get thresholds
    # predictions
    preds.pres <- predict.gbm(brt_pres, pres_dat2,
                              n.trees=brt_pres$gbm.call$best.trees, type="response")
    preds.surv <- predict.gbm(brt_surv, surv_dat2,
                              n.trees=brt_surv$gbm.call$best.trees, type="response")
    
    # create df of test observations and predictions to evaluate model performance
    pres.brt.predict <- data.frame(id=pres_dat2$id,
                                   obs=pres_dat2$y, 
                                   pred=preds.pres)
    surv.brt.predict <- data.frame(id=surv_dat2$id,
                                   obs=surv_dat2$y, 
                                   pred=preds.surv)
    thr.p.brt<-optimal.thresholds(pres.brt.predict,opt.methods = "MaxPCC")$pred
    thr.s.brt<-optimal.thresholds(surv.brt.predict,opt.methods = "MaxPCC")$pred
    
    save(brt_pres,brt_surv,thr.p.brt,thr.s.brt,file=paste0(path_out,"Final_outputs/Nest_Predictions/",speciesnames[s],"_final_BRT_mods.RDS"))
    
    