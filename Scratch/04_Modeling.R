library(tidyverse)
library(lme4)
library(glmmTMB)
library(car)
library(bbmle)
library(DHARMa)
library(broom.mixed)
library(ggstats)

## Set file path to data
# ------------------------------------------------------------
dat_path<-"D:/Flood_Index/Data/"
path_out<-"D:/Flood_Index/Outputs/"

dat<-read.csv(paste0(path_out,"nests_flood_metrics_dataset.csv"))
#adjust the one proportion over 1 back down to 1
dat[dat$fld_prp>1,]$Nfld_fl<-dat[dat$fld_prp>1,]$Nfld_fl-1
dat[dat$fld_prp>1,]$fld_prp<-1

#proportion of 0's in response
nrow(dat[dat$fld_prp==0,])/nrow(dat)


# look at correlation among variables
all_terms<-c("LOMARSH","elevation","flats.8","tcwet.8","latitud")
cor(dat[,all_terms])

## Analysis Functions
#---------------------------------------------------------------
# Check for overdispersion
ODFunc<-function(x){
  chisq<-sum((resid(x, type='pearson')^2)) 
  chisq/df.residual(x)
  ##significantly greater than 1? 
  out<-1-pchisq(chisq, df.residual(x))
  print(out);print(ifelse(out<=0.05, "significant","Not significant"))
}

#Leave-One-Out LOO Model Assessments:

# Calculate Mean Squared Prediction Error (MSPE)- mean sq error between model estimate and a new observation
MSPE<-function(model,d,resp.col){#specify the model used for predictions, data, and the response column index in the data
  spe<-c() #empty list to hold individual observation results
  #for each observation in the data
  for(i in 1:nrow(d)){
    #fit the specified model without that observation
    fit_mod <- update(model,data=d[-i,])
    #use this model to predict that observation, 
    #take the difference from the observed value and square it.
    spe[i]<-(predict(fit_mod,newdata=d[i,],type="response")-dat[i,resp.col])^2
  }
    #take the mean of the squared differences for every observation in the data
  return(mean(spe))
}

# Calculate Mean Squared Error (MSE)- the difference between each response and the model estimated mean.
MSE<-function(model,d,resp.col){#specify the model used for predictions, data, and the response column index in the data
  mse<-c()#empty list to hold individual observation results
  #for each observation in the data
  for(i in 1:nrow(d)){
    #use the specified model to predict that observation, take the squared difference from the observed mean.
    mse[i]<-(predict(model,newdata=d[i,],type="response")-d[i,resp.col])^2
  }
    #take the mean of these squared differences for all observations in the data
  return(mean(mse))
}


## Test different error distribution fits
#---------------------------------------------------------------------------
# 1 . start with a simple glmm without flood metrics
####
binom.mod<-glmer(cbind(Nfld_fl,maxeggs-Nfld_fl) ~ (1|site)+prd_prp, data=dat, family=binomial(link="logit"))

  # look at diagnostic plots to see if they meet assumptions of non-dependence etc
bmod_simres <- simulateResiduals(binom.mod)
plot(bmod_simres)
  #looks like some dependence between residuals and fitted values in the data

  # look at estimated mean and compare with histogram of data
plogis(fixef(binom.mod)[1])
ggplot(dat)+
  geom_histogram(aes(x=fld_prp))+
  xlab("Proportion Nest Contents Flooded")+
  theme_bw(base_size = 12)
  #looks approximately right if fully accounting for the inflated zeros...

  #over dispersed?
ODFunc(binom.mod)
  #Yes super over dispersed

  #add fitted values to dataframe
fit_obs<-data.frame(binom.mod=predict(binom.mod,newdata=dat,type="response"),obs=dat$fld_prp)


# 2. try a distribution that accounts for overdispersion - betabinomial
####
bb.mod<-glmmTMB(cbind(Nfld_fl,maxeggs-Nfld_fl) ~ (1|site)+prd_prp, data=dat, family=betabinomial(link="logit"))

  #look at estimated mean and compare with histogram of data
plogis(bb.mod$fit$par[1])
  # even more skewed towards 0's

  #diagnostic plots with simulated residuals
bbmod_simres <- simulateResiduals(bb.mod)
plot(bbmod_simres)
  #look pretty good

  #add fitted values to dataframe
fit_obs$betabinom.mod<-predict(bb.mod,newdata=dat,type="response")


# 3. to address zero inflation, try ZI binomial distribution 
####
# (use elevation and proportion eggs eaten as drivers of whether a nest is exposed to flooding at all)
zi.binom.mod<-glmmTMB(cbind(Nfld_fl,maxeggs-Nfld_fl) ~ (1|site), data=dat, 
                      ziformula= ~prd_prp+elevation, family=binomial(link="logit"))

  # look at estimated mean and compare with histogram of data
plogis(zi.binom.mod$fit$par[1])
  # Looks more appropriate, discounting the excess zeros

  #diagnostic plots with simulated residuals
zi_binom_mod_simres <- simulateResiduals(zi.binom.mod)
plot(zi_binom_mod_simres)
ODFunc(zi.binom.mod)
# still over dispersed

  #add fitted values to dataframe
fit_obs$zibinom.mod<-predict(zi.binom.mod,newdata=dat,type="response")


# 4. try a distribution that accounts for overdispersion AND zero inflation
####
zi.bb.mod<-glmmTMB(cbind(Nfld_fl,maxeggs-Nfld_fl) ~ (1|site), data=dat, 
                       ziformula= ~prd_prp+elevation, family=betabinomial(link="logit"))

# and try a different distribution like beta or gamma that doesn't make assumptions about variance AND zero inflation
zi.beta.mod<-glmmTMB(fld_prp ~ (1|site), data=dat, 
                         ziformula= ~prd_prp+elevation, family=ordbeta(link = "logit"))


# look at estimated mean and compare with histogram of data
plogis(zi.bb.mod$fit$par[1])
plogis(zi.beta.mod$fit$par[1])


#diagnostic plots with simulated residuals
zi_bb_mod_simres <- simulateResiduals(zi.bb.mod)
plot(zi_bb_mod_simres)
# zi betabinomial has good diagnostics


zi_beta_mod_simres <- simulateResiduals(zi.beta.mod)
plot(zi_beta_mod_simres)
# problems!

#add fitted values to dataframe
fit_obs$zibetabinom.mod<-predict(zi.bb.mod,newdata=dat,type="response")
fit_obs$zibeta.mod<-predict(zi.beta.mod,newdata=dat,type="response")


#try plotting the predicted value distributions with the original observations
ggplot(fit_obs%>%
  pivot_longer(1:ncol(fit_obs),names_to = "model",values_to = "fit"))+
  geom_density(aes(x=fit))+
  facet_wrap(~model)





## Test different deterministic fits using flood indicators
#---------------------------------------------------------------------------
#based on model diagnostics, try a zi betabinomial 
#account for nuisance variables of site, flood conditions might be overall different in different locations, and number of eggs eaten by predators
# don't expect much variation over years, plus we standardized over years by taking flood freq over time. 

#list to contain all the models
mod_list<-list()

#FLATS index model - test different binary flood thresholds to see which maxes predictive performance
mod_list[[1]]<-glmmTMB(cbind(Nfld_fl,maxeggs-Nfld_fl) ~ (1|site)+flats.1, data=dat, 
                               ziformula= ~prd_prp+elevation, family=betabinomial(link="logit"))
mod_list[[2]]<-update(mod_list[[1]],~-flats.1+flats.2+(1|site))
mod_list[[3]]<-update(mod_list[[1]],~-flats.1+flats.3+(1|site))
mod_list[[4]]<-update(mod_list[[1]],~-flats.1+flats.4+(1|site))
mod_list[[5]]<-update(mod_list[[1]],~-flats.1+flats.5+(1|site))
mod_list[[6]]<-update(mod_list[[1]],~-flats.1+flats.6+(1|site))
mod_list[[7]]<-update(mod_list[[1]],~-flats.1+flats.7+(1|site))#convergence problems past 0.7
mod_list[[8]]<-update(mod_list[[1]],~-flats.1+flats.8+(1|site))
mod_list[[9]]<-update(mod_list[[1]],~-flats.1+flats.9+(1|site))

#Tasseled cap model - test different binary flood thresholds to see which maxes predictive performance
mod_list[[10]]<-glmmTMB(cbind(Nfld_fl,maxeggs-Nfld_fl) ~ (1|site)+tcwet.1, data=dat, 
                     ziformula= ~prd_prp+elevation, family=betabinomial(link="logit"))
mod_list[[11]]<-update(mod_list[[10]],~-tcwet.1+tcwet.2+(1|site))
mod_list[[12]]<-update(mod_list[[10]],~-tcwet.1+tcwet.3+(1|site))
mod_list[[13]]<-update(mod_list[[10]],~-tcwet.1+tcwet.4+(1|site))#convergence problems
mod_list[[14]]<-update(mod_list[[10]],~-tcwet.1+tcwet.5+(1|site))
mod_list[[15]]<-update(mod_list[[10]],~-tcwet.1+tcwet.6+(1|site))
mod_list[[16]]<-update(mod_list[[10]],~-tcwet.1+tcwet.7+(1|site))
mod_list[[17]]<-update(mod_list[[10]],~-tcwet.1+tcwet.8+(1|site))
mod_list[[18]]<-update(mod_list[[10]],~-tcwet.1+tcwet.9+(1|site))

#Indirect flood indicators - low marsh vegetation
mod_list[[19]]<-glmmTMB(cbind(Nfld_fl,maxeggs-Nfld_fl) ~ (1|site)+LOMARSH, data=dat, 
                     ziformula= ~prd_prp+elevation, family=betabinomial(link="logit"))





## Model Comparison
#---------------------------------------------------------------------
# compared models using MSPE, RMSE, Pseudo-R2 - more focused on model prediction, but also want to make sure the model fits the data well

# calculate null deviance for calculations 
# is difference of each observed response from the mean observed response.
null.dev<-mean(unlist(lapply(dat$fld_prp, function(x)(x-mean(dat$fld_prp,na.rm=T))^2)))

#table to hold model evaluations
eval<-data.frame(model.name=c("Flats0.1","Flats0.2","Flats0.3",'Flats0.4',"Flats0.5",
                              "Flats0.6","Flats0.7","Flats0.8",'Flats0.9',
                              "TC0.1","TC0.2","TC0.3",'TC0.4',"TC0.5",
                              "TC0.6","TC0.7","TC0.8",'TC0.9',
                              "Indirect"),
                 MSPE=NA,
                 MSE=NA)

# Take the list of models and apply the MSPE function to them, put results into the model evaluation table
eval$MSPE<-unlist(lapply(mod_list,function(x) MSPE(x,d=dat,resp.col = 42)))

# Repeat for MSE
eval$MSE<-unlist(lapply(mod_list,function(x) MSE(x,d=dat,resp.col = 42)))

# Calculate RMSE and R2 from MSE
eval2<-eval%>%
  mutate(RMSE=round(sqrt(MSE),4),#sqrt the MSE
         Pseudo_R2= 1-(round(MSE,4)/round(null.dev,4)))#take the inverse ratio of unexplained deviance from model (MSE) to unexplained deviance in a null model (mean in the observations)

# round everything
eval2[,-1]<-round(eval2[,-1],4)

#table formatting
eval2<-eval2%>%
  mutate(model.type=substr(model.name,1,nchar(model.name)-3))%>%
  arrange(model.type,MSPE)
  

write.csv(eval2,paste(path_out,"model_eval.csv"),row.names = F)



## Model Inference and visualize results
#--------------------------------------------------------------------------
# Check the model summaries of the models using the optimal threshold
summary(mod_list[[2]])#flats 0.2 (don't use the ones with model convergence issues)
summary(mod_list[[18]])#tc 0.9
summary(mod_list[[19]])#indirect

# per unit increase in flooding frequency or proportion low marsh results in how much of an increase in flooded proportion?
plogis(mod_list[[19]]$fit$par[2])#indirect
plogis(mod_list[[18]]$fit$par[2])#tc
plogis(mod_list[[2]]$fit$par[2])#flats


# 95% confidence intervals
plogis(confint(mod_list[[19]],method="wald")) #use wald,also profile, uniroot
plogis(confint(mod_list[[18]],method="wald"))
plogis(confint(mod_list[[2]],method="wald"))

#test individual variable contributions
Anova(mod_list[[19]],type="III") #indirect explains a meaningful amount of variation in the data
Anova(mod_list[[18]],type="III")# flood frequency indicators do not
Anova(mod_list[[2]],type="III")


#format the coefficients and confidence intervals and plot
#for indirect
t.ind <- broom.mixed::tidy(mod_list[[19]], conf.int = TRUE)
t.ind <- transform(t.ind,
                term=sprintf("%s.%s", component, term))%>%
  filter(effect!="ran_pars")
t.ind$term<-str_replace(t.ind$term,"\\(","")
t.ind$term<-str_replace(t.ind$term,"\\)","")

ggplot(t.ind, aes(x=estimate, y=term, color=term)) + 
  geom_vline(xintercept = 0)+
  geom_errorbar(aes(xmin=conf.low, xmax=conf.high), width=.1) +
  geom_point()+
  theme_bw(base_size = 12)+
  theme(legend.position = "none")


#for flats
t.flats <- broom.mixed::tidy(mod_list[[2]], conf.int = TRUE)
t.flats <- transform(t.flats,
                term=sprintf("%s.%s", component, term))%>%
  filter(effect!="ran_pars")
t.flats$term<-str_replace(t.flats$term,"\\(","")
t.flats$term<-str_replace(t.flats$term,"\\)","")

ggplot(t.flats, aes(x=estimate, y=term, color=term)) + 
  geom_vline(xintercept = 0)+
  geom_errorbar(aes(xmin=conf.low, xmax=conf.high), width=.1) +
  geom_point()+
  theme_bw(base_size = 12)+
  theme(legend.position = "none")


#for tc
t.tc <- broom.mixed::tidy(mod_list[[18]], conf.int = TRUE)
t.tc <- transform(t.tc,
                     term=sprintf("%s.%s", component, term))%>%
  filter(effect!="ran_pars")
t.tc$term<-str_replace(t.tc$term,"\\(","")
t.tc$term<-str_replace(t.tc$term,"\\)","")


ggplot(t.tc, aes(x=estimate, y=term, color=term)) + 
  geom_vline(xintercept = 0)+
  geom_errorbar(aes(xmin=conf.low, xmax=conf.high), width=.1) +
  geom_point()+
  theme_bw(base_size = 12)+
  theme(legend.position = "none")

