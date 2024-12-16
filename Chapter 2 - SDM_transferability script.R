#SDM Analyses 2023---- 
#Workspace setup----
rm(list=ls(all=TRUE))# Clear workspace
# Load packages
library(terra)
library(raster)
library(sf)
library(MuMIn)
library(car)
library(mgcv)
library(dplyr)
library(MASS)
library(pROC)

#install.packages("rstudioapi")

#Set wd to source location of R script. Needs to contains data.
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 

#Data----
#Read in data and remove values that are not complete
dat<-read.csv("FinalData.csv")
dat<-dat[complete.cases(dat),]

#Split data into sites
BP.data<-subset(dat,dat$site=="Banks Peninsula")
TIM.data<-subset(dat,dat$site=="Timaru")
OTA.data<-subset(dat,dat$site=="Otago")

#"Regional"
R1.data <- rbind(BP.data, TIM.data)
R2.data <-rbind(BP.data, OTA.data)
R3.data <- rbind(TIM.data, OTA.data)

#Concurvity----
##Check concurvity. Concurved variables were fit into univariate models and variable that had the highest deviance explained removed - Not included in this code. 
##Banks Peninsula----
BP.full<-gam(response.pa ~ s(combined.depth,k=4)+s(slope,k=4)+s(aspect,bs="cc")+s(mud250,k=4)+s(sand250,k=4)+s(gravel250,k=4)+s(bed.dist250,k=4)+s(tidal250,k=4)+s(sst,k=4)+s(sal,k=4)+s(do,k=4)+s(secchi,k=4)+s(dist2river,k=4)+s(dist2coast,k=4)+s(dist50m,k=4)+s(dist100m,k=4),family=binomial(link="logit"),data=BP.data)
summary(BP.full)
concurvity(BP.full, full=F)

##Timaru----
TIM.full<-gam(response.pa ~ s(combined.depth,k=4)+s(aspect,bs="cc")+s(slope,k=4)+s(mud250,k=4)+s(sand250,k=4)+s(gravel250,k=4)+s(bed.dist250,k=4)+s(tidal250,k=4)+s(sst,k=4)+s(sal,k=4)+s(do,k=4)+s(secchi,k=4)+s(dist2river,k=4)+s(dist2coast,k=4)+s(dist50m,k=4)+s(dist100m,k=4),family=binomial(link="logit"),data=TIM.data)
summary(TIM.full)
concurvity(TIM.full, full=F)

##Otago----
OTA.full<-gam(response.pa ~ s(combined.depth,k=4)+s(slope,k=4)+s(aspect,bs="cc")+s(mud250,k=4)+s(sand250,k=4)+s(gravel250,k=4)+s(bed.dist250,k=4)+s(tidal250,k=4)+s(sst,k=4)+s(sal,k=4)+s(do,k=4)+s(secchi,k=4)+s(dist2river,k=4)+s(dist2coast,k=4)+s(dist50m,k=4)+s(dist100m,k=4),family=binomial(link="logit"),data=OTA.data)
summary(OTA.full)
concurvity(OTA.full, full=F)


#Model selection and interpolation----
##Banks Peninsula ----
###Data setup ----
#Creating random subsets of the complete dataset to have an equal weight of presences:absence
BP.absence <- subset(BP.data,BP.data$response.pa == 0)
BP.presence<-subset(BP.data,BP.data$response.pa == 1)
BP.data.list<-list()
seed <- 20230511

for(i in 1:10) {
  seed <- seed+1
  set.seed(seed)
  temp<-BP.presence[sample(nrow(BP.presence),size = length(BP.absence$response.pa), replace = FALSE),]
  BP.data.list[[i]] <- rbind(BP.absence,temp)
}

rm(BP.presence,BP.absence,temp)

###Multifold cross validation----
##The first for loop reads in one of the random samples of BP data and then splits it into training and testing datasets. The second loop runs the multifold validation

#Creation of empty lists that will be populated by for loops.
model.summaries.BP<-list()
summaries<-list()
models.BP<-list()
temp.models<-list()
bp.train.data<-list()
bp.test.data<-list()

for(i in 1:10) {
  set.seed(20221126)
  temp.data<-BP.data.list[[i]]
  sample <-sample(c(TRUE,FALSE),nrow(temp.data),replace = TRUE, prob = c(0.3331,0.6669))
  test1<-temp.data[sample,]#Take 1/3rd of data forward
  data.rest<-temp.data[!sample,] #Save the rest in scratch file
  sample<-sample(c(TRUE,FALSE),nrow(data.rest),replace = TRUE, prob = c(0.5,0.5))
  test2<-data.rest[sample,] 
  test3<-data.rest[!sample,]
  
  train1<-as.data.frame(rbind(test2,test3))
  train2<-as.data.frame(rbind(test1,test3))
  train3<-as.data.frame(rbind(test2,test1))
  
  bp.train.data[[i]]<-list(train1,train2,train3)
  bp.test.data[[i]]<-list(test1,test2,test3)
  
}

rm(data.rest,train1,train2,train3,test1,test2,test3)

for(i in 1:length(bp.train.data)){
  for(j in 1:length(bp.train.data[[i]])) {
    temp.models[[j]]<-gam(response.pa ~ s(combined.depth,k=4) + s(slope,k=4) + s(mud250,k=4) + s(sal,k=4) + s(do,k=4),family=binomial(link="logit"),data=bp.train.data[[i]][[j]],select = TRUE) #Need to specify gamma
    summaries[[j]]<-summary(temp.models[[j]])
  }
  models.BP[[i]] <- temp.models
  model.summaries.BP[[i]]<-summaries
}


rm(temp.models,summaries)

##Top model results. 
#Create an empty dataframe to store all model outputs from the three-fold validation and each dataset.

summary.stats.BP <- data.frame(depth.chi2=as.numeric(),
                               slope.chi2=as.numeric(),
                               mud.chi2=as.numeric(),
                               sal.chi2=as.numeric(),
                               do.chi2=as.numeric(),
                               depth.p=as.numeric(),
                               slope.p=as.numeric(),
                               mud.p=as.numeric(),
                               sal.p=as.numeric(),
                               do.p=as.numeric(),
                               deviance.explained=as.numeric(),
                               r.sq=as.numeric(),
                               interp.Prop.correct=as.numeric(),
                               interp.CI.auc.low = as.numeric(),
                               interp.AUC = as.numeric(),
                               interp.CI.auc.high = as.numeric(),
                               interp.tss = as.numeric(),
                               Sensitivity = as.numeric(),
                               Specificity = as.numeric(),
                               transfer.prop.correct=as.numeric(),
                               transfer.CI.auc.low = as.numeric(),
                               transfer.AUC = as.numeric(),
                               transfer.CI.auc.high = as.numeric(),
                               transfer.tss = as.numeric(),
                               AUC.difference = as.numeric(),
                               TSS.difference = as.numeric(),
                               stringsAsFactors=FALSE)

#Record all interpolation results within dataframe
iteration <- 0

for(i in 1:length(models.BP)) {
  for(j in 1:length(models.BP[[i]])) {
    iteration <- iteration + 1
    val.data<-as.data.frame(bp.test.data[[i]][[j]])
    val.data$Prediction <- predict.gam(models.BP[[i]][[j]],newdata = val.data,type="response")
    summary.stats.BP[iteration,1:5]<-model.summaries.BP[[i]][[j]]$chi.sq #Chi2 results for each predictor
    summary.stats.BP[iteration,6:10]<-model.summaries.BP[[i]][[j]]$s.pv #p-value results for each predictor
    summary.stats.BP[iteration,11]<-model.summaries.BP[[i]][[j]]$dev.expl #Deviance explained of model
    summary.stats.BP[iteration,12]<-model.summaries.BP[[i]][[j]]$r.sq #R2 of model
    val.data$Pred.response<-ifelse(val.data$Prediction>0.5,1,0)
    val.data$Testpvo<-ifelse(val.data$Pred.response==val.data$response.pa,1,0)
    summary.stats.BP[iteration,13]<-sum(val.data$Testpvo)/length(val.data$Testpvo) #Proportion of correctly predicted response
    summary.stats.BP[iteration,14:16]<-ci.auc(roc(data=val.data,response=response.pa,predictor = Prediction)) #AUC with confidence intervals
    presence<-subset(val.data,response.pa==1)
    absence<-subset(val.data,response.pa==0)                                                     
    summary.stats.BP[iteration,17]<-(length(subset(presence$Testpvo, presence$Testpvo == 1))/length(presence$Testpvo))+(length(subset(absence$Testpvo, absence$Testpvo == 1))/length(absence$Testpvo))-1 ## code for TSS: Sensitivity + Specificity -1
    summary.stats.BP[iteration,18]<-length(subset(presence$Testpvo, presence$Testpvo == 1))/length(presence$Testpvo) #Sensitivity
    summary.stats.BP[iteration,19]<-length(subset(absence$Testpvo, absence$Testpvo == 1))/length(absence$Testpvo) #Specificty
  }
}

rm(presence,absence,val.data)



##Timaru----
###Data setup----
#All code as been repeated from BP model and with changes made to be Timaru specific.
TIM.absence <- subset(TIM.data,TIM.data$response.pa == 0)
TIM.presence<-subset(TIM.data,TIM.data$response.pa == 1)
TIM.data.list<-list()
seed <- 20230511

for(i in 1:5) {
  seed <- seed+1
  set.seed(seed)
  temp<-TIM.presence[sample(nrow(TIM.presence),size = length(TIM.absence$response.pa), replace = FALSE),]
  TIM.data.list[[i]] <- rbind(TIM.absence,temp)
}

rm(TIM.presence,TIM.absence,temp)

#Randomly split the data into 5 different datasets
model.summaries.TIM<-list()
summaries<-list()
models.TIM<-list()
temp.models<-list()
TIM.train.data<-list()
TIM.test.data<-list()


for(i in 1:5) {
  set.seed(20221126)
  temp.data<-TIM.data.list[[i]]
  sample<-sample(c(TRUE,FALSE),nrow(temp.data),replace = TRUE, prob = c(0.3331,0.6669))
  test1<-temp.data[sample,]#Take 1/3rd of data forward
  data.rest<-temp.data[!sample,] #Save the rest in scratch file
  sample<-sample(c(TRUE,FALSE),nrow(data.rest),replace = TRUE, prob = c(0.5,0.5))
  test2<-data.rest[sample,] 
  test3<-data.rest[!sample,]
  
  train1<-as.data.frame(rbind(test2,test3))
  train2<-as.data.frame(rbind(test1,test3))
  train3<-as.data.frame(rbind(test2,test1))
  
  TIM.train.data[[i]]<-list(train1,train2,train3)
  TIM.test.data[[i]]<-list(test1,test2,test3)
  
}

rm(data.rest,train1,train2,train3,test1,test2,test3)

###Multifold cross validation----

for(i in 1:length(TIM.train.data)){
  for(j in 1:length(TIM.train.data[[i]])) {
    temp.models[[j]]<-gam(response.pa ~ s(bed.dist250,k=4) + s(dist50m,k=4) + s(sst,k=4) + s(dist2river,k=4) + s(sal,k=4),family=binomial(link="logit"),data=TIM.train.data[[i]][[j]],select = TRUE) #Need to specify gamma
    summaries[[j]]<-summary(temp.models[[j]])
  }
  models.TIM[[i]] <- temp.models
  model.summaries.TIM[[i]]<-summaries
}


rm(temp.models,summaries)

summary.stats.TIM <- data.frame(bed.dist.chi2=as.numeric(),
                                dist50m.chi2=as.numeric(),
                                sst.chi2=as.numeric(),
                                dist2river.chi2=as.numeric(),
                                sal.chi2=as.numeric(),
                                bed.dist.p=as.numeric(),
                                dist50m.p=as.numeric(),
                                sst.p=as.numeric(),
                                dist2river.p=as.numeric(),
                                sal.p=as.numeric(),
                                deviance.explained=as.numeric(),
                                r.sq=as.numeric(),
                                interp.Prop.correct=as.numeric(),
                                interp.CI.auc.low = as.numeric(),
                                interp.AUC = as.numeric(),
                                interp.CI.auc.high = as.numeric(),
                                interp.tss = as.numeric(),
                                Sensitivity = as.numeric(),
                                Specificity = as.numeric(),
                                transfer.prop.correct=as.numeric(),
                                transfer.CI.auc.low = as.numeric(),
                                transfer.AUC = as.numeric(),
                                transfer.CI.auc.high = as.numeric(),
                                transfer.tss = as.numeric(),
                                AUC.difference = as.numeric(),
                                TSS.difference = as.numeric(),
                                stringsAsFactors=FALSE)
iteration <- 0

for(i in 1:length(models.TIM)) {
  for(j in 1:length(models.TIM[[i]])) {
    iteration <- iteration + 1
    val.data<-as.data.frame(TIM.test.data[[i]][[j]])
    val.data$Prediction <- predict.gam(models.TIM[[i]][[j]],newdata = val.data,type="response")
    summary.stats.TIM[iteration,1:5]<-model.summaries.TIM[[i]][[j]]$chi.sq
    summary.stats.TIM[iteration,6:10]<-model.summaries.TIM[[i]][[j]]$s.pv
    summary.stats.TIM[iteration,11]<-model.summaries.TIM[[i]][[j]]$dev.expl
    summary.stats.TIM[iteration,12]<-model.summaries.TIM[[i]][[j]]$r.sq
    val.data$Pred.response<-ifelse(val.data$Prediction>0.5,1,0)
    val.data$Testpvo<-ifelse(val.data$Pred.response==val.data$response.pa,1,0)
    summary.stats.TIM[iteration,13]<-sum(val.data$Testpvo)/length(val.data$Testpvo)
    summary.stats.TIM[iteration,14:16]<-ci.auc(roc(data=val.data,response=response.pa,predictor = Prediction))
    presence<-subset(val.data,response.pa==1)
    absence<-subset(val.data,response.pa==0)                                                     
    summary.stats.TIM[iteration,17]<-(length(subset(presence$Testpvo, presence$Testpvo == 1))/length(presence$Testpvo))+(length(subset(absence$Testpvo, absence$Testpvo == 1))/length(absence$Testpvo))-1 ## code for TSS: Sensitivity + Specificity -1
    summary.stats.TIM[iteration,18]<-length(subset(presence$Testpvo, presence$Testpvo == 1))/length(presence$Testpvo)
    summary.stats.TIM[iteration,19]<-length(subset(absence$Testpvo, absence$Testpvo == 1))/length(absence$Testpvo)
  }
}

rm(presence,absence,val.data)


##Otago----
###Data setup----
#All code has been repeated from BP code with changes made to be Otago specific.
OTA.absence <- subset(OTA.data,OTA.data$response.pa == 0)
OTA.presence<-subset(OTA.data,OTA.data$response.pa == 1)
OTA.data.list<-list()
seed <- 20230511

for(i in 1:5) {
  seed <- seed + 1
  set.seed(seed)
  temp<-OTA.absence[sample(nrow(OTA.absence),size = length(OTA.presence$response.pa), replace = FALSE),]
  OTA.data.list[[i]] <- rbind(OTA.presence,temp)
}

rm(OTA.presence,OTA.absence,temp)

#Randomly split the data into 5 different datasets
model.summaries.OTA<-list()
summaries<-list()
models.OTA<-list()
temp.models<-list()
OTA.train.data<-list()
OTA.test.data<-list()

for(i in 1:5) {
  set.seed(20221126)
  temp.data<-OTA.data.list[[i]]
  sample<-sample(c(TRUE,FALSE),nrow(temp.data),replace = TRUE, prob = c(0.3331,0.6669))
  test1<-temp.data[sample,]#Take 1/3rd of data forward
  data.rest<-temp.data[!sample,] #Save the rest in scratch file
  sample<-sample(c(TRUE,FALSE),nrow(data.rest),replace = TRUE, prob = c(0.5,0.5))
  test2<-data.rest[sample,] 
  test3<-data.rest[!sample,]
  
  train1<-as.data.frame(rbind(test2,test3))
  train2<-as.data.frame(rbind(test1,test3))
  train3<-as.data.frame(rbind(test2,test1))
  
  OTA.train.data[[i]]<-list(train1,train2,train3)
  OTA.test.data[[i]]<-list(test1,test2,test3)
  
}

rm(data.rest,train1,train2,train3,test1,test2,test3)

###Multifold cross validation----
for(i in 1:length(OTA.train.data)){
  for(j in 1:length(OTA.train.data[[i]])) {
    temp.models[[j]]<-gam(response.pa ~ s(sand250,k=4) + s(slope,k=4) + s(dist50m,k=4) + s(sst,k=4) + s(sal,k=4) + s(do,k=4),family=binomial(link="logit"),data=OTA.train.data[[i]][[j]],select = TRUE) #Need to specify gamma
    summaries[[j]]<-summary(temp.models[[j]])
  }
  models.OTA[[i]] <- temp.models
  model.summaries.OTA[[i]]<-summaries
}

rm(temp.models,summaries,temp.data)

summary.stats.OTA <- data.frame(sand.chi2=as.numeric(),
                                slope.chi2=as.numeric(),
                                dist50m.chi2=as.numeric(),
                                sst.chi2=as.numeric(),
                                sal.chi2=as.numeric(),
                                do.chi2=as.numeric(),
                                sand.p=as.numeric(),
                                slope.p=as.numeric(),
                                dist50m.p=as.numeric(),
                                sst.p=as.numeric(),
                                sal.p=as.numeric(),
                                do.p=as.numeric(),
                                deviance.explained=as.numeric(),
                                r.sq=as.numeric(),
                                interp.Prop.correct=as.numeric(),
                                interp.CI.auc.low = as.numeric(),
                                interp.AUC = as.numeric(),
                                interp.CI.auc.high = as.numeric(),
                                interp.tss = as.numeric(),
                                Sensitivity = as.numeric(),
                                Specificity = as.numeric(),
                                transfer.prop.correct=as.numeric(),
                                transfer.CI.auc.low = as.numeric(),
                                transfer.AUC = as.numeric(),
                                transfer.CI.auc.high = as.numeric(),
                                transfer.tss = as.numeric(),
                                AUC.difference = as.numeric(),
                                TSS.difference = as.numeric(),
                                stringsAsFactors=FALSE)
iteration <- 0

for(i in 1:length(models.OTA)) {
  for(j in 1:length(models.OTA[[i]])) {
    iteration <- iteration + 1
    val.data<-as.data.frame(OTA.test.data[[i]][[j]])
    val.data$Prediction <- predict.gam(models.OTA[[i]][[j]],newdata = val.data,type="response")
    summary.stats.OTA[iteration,1:6]<-model.summaries.OTA[[i]][[j]]$chi.sq
    summary.stats.OTA[iteration,7:12]<-model.summaries.OTA[[i]][[j]]$s.pv
    summary.stats.OTA[iteration,13]<-model.summaries.OTA[[i]][[j]]$dev.expl
    summary.stats.OTA[iteration,14]<-model.summaries.OTA[[i]][[j]]$r.sq
    val.data$Pred.response<-ifelse(val.data$Prediction>0.5,1,0)
    val.data$Testpvo<-ifelse(val.data$Pred.response==val.data$response.pa,1,0)
    summary.stats.OTA[iteration,15]<-sum(val.data$Testpvo)/length(val.data$Testpvo)
    summary.stats.OTA[iteration,16:18]<-ci.auc(roc(data=val.data,response=response.pa,predictor = Prediction))
    presence<-subset(val.data,response.pa==1)
    absence<-subset(val.data,response.pa==0)                                                     
    summary.stats.OTA[iteration,19]<-(length(subset(presence$Testpvo, presence$Testpvo == 1))/length(presence$Testpvo))+(length(subset(absence$Testpvo, absence$Testpvo == 1))/length(absence$Testpvo))-1 ## code for TSS: Sensitivity + Specificity -1
    summary.stats.OTA[iteration,20]<-length(subset(presence$Testpvo, presence$Testpvo == 1))/length(presence$Testpvo)
    summary.stats.OTA[iteration,21]<-length(subset(absence$Testpvo, absence$Testpvo == 1))/length(absence$Testpvo)
  }
}

rm(presence,absence,val.data)


#Transferability----
##BP to TIM and OTA----
#Duplicate summary statistics data for each transferred location
summary.stats.BP.TIM <- summary.stats.BP
summary.stats.BP.OTA <- summary.stats.BP


#Generate model predictions for the new areas (BP model predictions from Timaru data)
iteration<-0

for(i in 1:length(models.BP)) {
  for(j in 1:length(models.BP[[i]])) {
    iteration<- iteration + 1
    transfer.data<-TIM.data
    transfer.data$Prediction <- predict.gam(models.BP[[i]][[j]],newdata = transfer.data,type="response")
    transfer.data$Pred.response<-ifelse(transfer.data$Prediction>0.5,1,0) #Setting the level for predicted response: lower then 0.5 is considered an absence, higher is considered a presence
    transfer.data$Testpvo<-ifelse(transfer.data$Pred.response==transfer.data$response.pa,1,0)
    summary.stats.BP.TIM[iteration,20]<-sum(transfer.data$Testpvo)/length(transfer.data$Testpvo)
    summary.stats.BP.TIM[iteration,21:23]<-ci.auc(roc(data=transfer.data,response=response.pa,predictor = Prediction)) #extracting the AUC and CI from each model
    presence<-subset(transfer.data,response.pa==1)
    absence<-subset(transfer.data,response.pa==0)
    summary.stats.BP.TIM[iteration,24]<-(length(subset(presence$Testpvo, presence$Testpvo == 1))/length(presence$Testpvo))+(length(subset(absence$Testpvo, absence$Testpvo == 1))/length(absence$Testpvo))-1 ## code for TSS: Sensitivity + Specificity -1
    summary.stats.BP.TIM[iteration,25]<- summary.stats.BP.TIM[iteration,22] - summary.stats.BP.TIM[iteration,15]
    summary.stats.BP.TIM[iteration,26]<-summary.stats.BP.TIM[iteration,24] - summary.stats.BP.TIM[iteration,17]
  }
  summary.stats.BP.TIM$transfer.data.source<-"TIM"
}

#Generate model predictions for the new areas (BP model predictions from Otago data)
iteration<-0

for(i in 1:length(models.BP)) {
  for(j in 1:length(models.BP[[i]])) {
    iteration<- iteration + 1
    transfer.data<-OTA.data
    transfer.data$Prediction <- predict.gam(models.BP[[i]][[j]],newdata = transfer.data,type="response")
    transfer.data$Pred.response<-ifelse(transfer.data$Prediction>0.5,1,0) #Setting the level for predicted response: lower then 0.5 is considered an absence, higher is considered a presence
    transfer.data$Testpvo<-ifelse(transfer.data$Pred.response==transfer.data$response.pa,1,0)
    summary.stats.BP.OTA[iteration,20]<-sum(transfer.data$Testpvo)/length(transfer.data$Testpvo)
    summary.stats.BP.OTA[iteration,21:23]<-ci.auc(roc(data=transfer.data,response=response.pa,predictor = Prediction)) #extracting the AUC and CI from each model
    presence<-subset(transfer.data,response.pa==1)
    absence<-subset(transfer.data,response.pa==0)
    summary.stats.BP.OTA[iteration,24]<-(length(subset(presence$Testpvo, presence$Testpvo == 1))/length(presence$Testpvo))+(length(subset(absence$Testpvo, absence$Testpvo == 1))/length(absence$Testpvo))-1 ## code for TSS: Sensitivity + Specificity -1
    summary.stats.BP.OTA[iteration,25]<- summary.stats.BP.OTA[iteration,22] - summary.stats.BP.OTA[iteration,15]
    summary.stats.BP.OTA[iteration,26]<-summary.stats.BP.OTA[iteration,24] - summary.stats.BP.OTA[iteration,17]
  }
  summary.stats.BP.OTA$transfer.data.source<-"OTA"
}

summary.stats.BP.OTA<-summary.stats.BP.OTA[,18:27]
summary.stats.BP.OTA<-rename(summary.stats.BP.OTA, transfer.prop.correct.2 = transfer.prop.correct
                             ,transfer.CI.auc.low.2 =transfer.CI.auc.low
                             ,transfer.AUC.2=transfer.AUC
                             ,transfer.CI.auc.high.2 = transfer.CI.auc.high
                             ,transfer.tss.2 = transfer.tss
                             ,transfer.data.source.2 = transfer.data.source
                             ,AUC.difference.2 = AUC.difference
                             ,TSS.difference.2 = TSS.difference)

summary.stats.BP<-cbind(summary.stats.BP.TIM,summary.stats.BP.OTA)
rm(summary.stats.BP.OTA,summary.stats.BP.TIM)

summary.stats.BP$model.run<-c(1.1, 1.7, 2.3, 3.1, 3.7, 4.3, 5.1, 5.7, 6.3, 7.1, 7.7, 8.3, 9.1, 9.7, 10.3, 11.1, 11.7, 12.3, 13.1, 13.7, 14.3, 15.1, 15.7, 16.3, 17.1, 17.7, 18.3, 19.1, 19.7, 20.3)

summary.stats.BP$model.run.TIM<-summary.stats.BP$model.run+0.1
summary.stats.BP$model.run.OTA<-summary.stats.BP$model.run+0.2

##TIM to BP and OTA----
summary.stats.TIM.BP <- summary.stats.TIM
summary.stats.TIM.OTA <- summary.stats.TIM

iteration<-0

for(i in 1:length(models.TIM)) {
  for(j in 1:length(models.TIM[[i]])) {
    iteration<- iteration + 1
    transfer.data<-BP.data
    transfer.data$Prediction <- predict.gam(models.TIM[[i]][[j]],newdata = transfer.data,type="response")
    transfer.data$Pred.response<-ifelse(transfer.data$Prediction>0.5,1,0) #Setting the level for predicted response: lower then 0.5 is considered an absence, higher is considered a presence
    transfer.data$Testpvo<-ifelse(transfer.data$Pred.response==transfer.data$response.pa,1,0)
    summary.stats.TIM.BP[iteration,20]<-sum(transfer.data$Testpvo)/length(transfer.data$Testpvo)
    summary.stats.TIM.BP[iteration,21:23]<-ci.auc(roc(data=transfer.data,response=response.pa,predictor = Prediction)) #extracting the AUC and CI from each model
    presence<-subset(transfer.data,response.pa==1)
    absence<-subset(transfer.data,response.pa==0)
    summary.stats.TIM.BP[iteration,24]<-(length(subset(presence$Testpvo, presence$Testpvo == 1))/length(presence$Testpvo))+(length(subset(absence$Testpvo, absence$Testpvo == 1))/length(absence$Testpvo))-1 ## code for TSS: Sensitivity + Specificity -1
    summary.stats.TIM.BP[iteration,25]<- summary.stats.TIM.BP[iteration,22] - summary.stats.TIM.BP[iteration,15]
    summary.stats.TIM.BP[iteration,26]<-summary.stats.TIM.BP[iteration,24] /- summary.stats.TIM.BP[iteration,17]
  }
  summary.stats.TIM.BP$transfer.data.source<-"BP"
}

iteration<-0

for(i in 1:length(models.TIM)) {
  for(j in 1:length(models.TIM[[i]])) {
    iteration<- iteration + 1
    transfer.data<-OTA.data
    transfer.data$Prediction <- predict.gam(models.TIM[[i]][[j]],newdata = transfer.data,type="response")
    transfer.data$Pred.response<-ifelse(transfer.data$Prediction>0.5,1,0) #Setting the level for predicted response: lower then 0.5 is considered an absence, higher is considered a presence
    transfer.data$Testpvo<-ifelse(transfer.data$Pred.response==transfer.data$response.pa,1,0)
    summary.stats.TIM.OTA[iteration,20]<-sum(transfer.data$Testpvo)/length(transfer.data$Testpvo)
    summary.stats.TIM.OTA[iteration,21:23]<-ci.auc(roc(data=transfer.data,response=response.pa,predictor = Prediction)) #extracting the AUC and CI from each model
    presence<-subset(transfer.data,response.pa==1)
    absence<-subset(transfer.data,response.pa==0)
    summary.stats.TIM.OTA[iteration,24]<-(length(subset(presence$Testpvo, presence$Testpvo == 1))/length(presence$Testpvo))+(length(subset(absence$Testpvo, absence$Testpvo == 1))/length(absence$Testpvo))-1 ## code for TSS: Sensitivity + Specificity -1
    summary.stats.TIM.OTA[iteration,25]<- summary.stats.TIM.OTA[iteration,22] - summary.stats.TIM.OTA[iteration,15]
    summary.stats.TIM.OTA[iteration,26]<-summary.stats.TIM.OTA[iteration,24] - summary.stats.TIM.OTA[iteration,17]
  }
  summary.stats.TIM.OTA$transfer.data.source<-"OTA"
}

summary.stats.TIM.OTA<-summary.stats.TIM.OTA[,18:27]
summary.stats.TIM.OTA<-rename(summary.stats.TIM.OTA, transfer.prop.correct.2 = transfer.prop.correct
                              ,transfer.CI.auc.low.2 =transfer.CI.auc.low
                              ,transfer.AUC.2=transfer.AUC
                              ,transfer.CI.auc.high.2 = transfer.CI.auc.high
                              ,transfer.tss.2 = transfer.tss
                              ,transfer.data.source.2 = transfer.data.source
                              ,AUC.difference.2 = AUC.difference
                              ,TSS.difference.2 = TSS.difference)

summary.stats.TIM<-cbind(summary.stats.TIM.BP,summary.stats.TIM.OTA)
rm(summary.stats.TIM.OTA,summary.stats.TIM.BP)

summary.stats.TIM$model.run<-c(1.1, 1.7, 2.3, 3.1, 3.7, 4.3, 5.1, 5.7, 6.3, 7.1, 7.7, 8.3, 9.1, 9.7, 10.3)

summary.stats.TIM$model.run.BP<-summary.stats.TIM$model.run+0.1
summary.stats.TIM$model.run.OTA<-summary.stats.TIM$model.run+0.2


##OTA to BP and TIM----
summary.stats.OTA.BP <- summary.stats.OTA
summary.stats.OTA.TIM <- summary.stats.OTA

iteration<-0

for(i in 1:length(models.OTA)) {
  for(j in 1:length(models.OTA[[i]])) {
    iteration<- iteration + 1
    transfer.data<-BP.data
    transfer.data$Prediction <- predict.gam(models.OTA[[i]][[j]],newdata = transfer.data,type="response")
    transfer.data$Pred.response<-ifelse(transfer.data$Prediction>0.5,1,0) #Setting the level for predicted response: lower then 0.5 is considered an absence, higher is considered a presence
    transfer.data$Testpvo<-ifelse(transfer.data$Pred.response==transfer.data$response.pa,1,0)
    summary.stats.OTA.BP[iteration,22]<-sum(transfer.data$Testpvo)/length(transfer.data$Testpvo)
    summary.stats.OTA.BP[iteration,23:25]<-ci.auc(roc(data=transfer.data,response=response.pa,predictor = Prediction)) #extracting the AUC and CI from each model
    presence<-subset(transfer.data,response.pa==1)
    absence<-subset(transfer.data,response.pa==0)
    summary.stats.OTA.BP[iteration,26]<-(length(subset(presence$Testpvo, presence$Testpvo == 1))/length(presence$Testpvo))+(length(subset(absence$Testpvo, absence$Testpvo == 1))/length(absence$Testpvo))-1 ## code for TSS: Sensitivity + Specificity -1
    summary.stats.OTA.BP[iteration,27]<- summary.stats.OTA.BP[iteration,24] - summary.stats.OTA.BP[iteration,17]
    summary.stats.OTA.BP[iteration,28]<-summary.stats.OTA.BP[iteration,26] - summary.stats.OTA.BP[iteration,19]
  }
  summary.stats.OTA.BP$transfer.data.source<-"BP"
}

iteration<-0

for(i in 1:length(models.OTA)) {
  for(j in 1:length(models.OTA[[i]])) {
    iteration<- iteration + 1
    transfer.data<-TIM.data
    transfer.data$Prediction <- predict.gam(models.OTA[[i]][[j]],newdata = transfer.data,type="response")
    transfer.data$Pred.response<-ifelse(transfer.data$Prediction>0.5,1,0) #Setting the level for predicted response: lower then 0.5 is considered an absence, higher is considered a presence
    transfer.data$Testpvo<-ifelse(transfer.data$Pred.response==transfer.data$response.pa,1,0)
    summary.stats.OTA.TIM[iteration,22]<-sum(transfer.data$Testpvo)/length(transfer.data$Testpvo)
    summary.stats.OTA.TIM[iteration,23:25]<-ci.auc(roc(data=transfer.data,response=response.pa,predictor = Prediction)) #extracting the AUC and CI from each model
    presence<-subset(transfer.data,response.pa==1)
    absence<-subset(transfer.data,response.pa==0)
    summary.stats.OTA.TIM[iteration,26]<-(length(subset(presence$Testpvo, presence$Testpvo == 1))/length(presence$Testpvo))+(length(subset(absence$Testpvo, absence$Testpvo == 1))/length(absence$Testpvo))-1 ## code for TSS: Sensitivity + Specificity -1
    summary.stats.OTA.TIM[iteration,27]<- summary.stats.OTA.TIM[iteration,24] - summary.stats.OTA.TIM[iteration,17]
    summary.stats.OTA.TIM[iteration,28]<-summary.stats.OTA.TIM[iteration,26] - summary.stats.OTA.TIM[iteration,19]
  }
  summary.stats.OTA.TIM$transfer.data.source<-"TIM"
}

summary.stats.OTA.TIM<-summary.stats.OTA.TIM[,20:29]
summary.stats.OTA.TIM<-rename(summary.stats.OTA.TIM, transfer.prop.correct.2 = transfer.prop.correct
                              ,transfer.CI.auc.low.2 =transfer.CI.auc.low
                              ,transfer.AUC.2=transfer.AUC
                              ,transfer.CI.auc.high.2 = transfer.CI.auc.high
                              ,transfer.tss.2 = transfer.tss
                              ,transfer.data.source.2 = transfer.data.source
                              ,AUC.difference.2 = AUC.difference
                              ,TSS.difference.2 = TSS.difference)

summary.stats.OTA<-cbind(summary.stats.OTA.BP,summary.stats.OTA.TIM)
rm(summary.stats.OTA.TIM,summary.stats.OTA.BP)

summary.stats.OTA$model.run<-c(1.1, 1.7, 2.3, 3.1, 3.7, 4.3, 5.1, 5.7, 6.3, 7.1, 7.7, 8.3, 9.1, 9.7, 10.3)

summary.stats.OTA$model.run.BP<-summary.stats.OTA$model.run+0.1
summary.stats.OTA$model.run.TIM<-summary.stats.OTA$model.run+0.2


#Semivariogram----
library(geoR)

coordinates<-cbind(bp.train.data[[5]][[3]]$lat,bp.train.data[[5]][[3]]$long)
resids<- models.BP[[5]][[3]]$residuals
v1 <- variog(coords = coordinates, data = resids)
plot(v1)


coordinates<-cbind(TIM.train.data[[2]][[2]]$lat,TIM.train.data[[2]][[2]]$long)
resids<- models.TIM[[2]][[2]]$residuals
v1 <- variog(coords = coordinates, data = resids)
plot(v1)

coordinates<-cbind(OTA.train.data[[2]][[1]]$lat,OTA.train.data[[2]][[1]]$long)
resids<- models.OTA[[2]][[1]]$residuals
v1 <- variog(coords = coordinates, data = resids)
plot(v1)



#Regional----
##Concurvity----
R1.full<-gam(response.pa ~ s(combined.depth,k=4)+s(slope,k=4)+s(aspect,bs="cc")+s(mud250,k=4)+s(sand250,k=4)+s(gravel250,k=4)+s(bed.dist250,k=4)+s(tidal250,k=4)+s(sst,k=4)+s(sal,k=4)+s(do,k=4)+s(secchi,k=4)+s(dist2river,k=4)+s(dist2coast,k=4)+s(dist50m,k=4)+s(dist100m,k=4),family=binomial(link="logit"),data=R1.data)
summary(R1.full)
concurvity(R1.full, full=F)

R2.full<-gam(response.pa ~ s(combined.depth,k=4)+s(slope,k=4)+s(aspect,bs="cc")+s(mud250,k=4)+s(sand250,k=4)+s(gravel250,k=4)+s(bed.dist250,k=4)+s(tidal250,k=4)+s(sst,k=4)+s(sal,k=4)+s(do,k=4)+s(secchi,k=4)+s(dist2river,k=4)+s(dist2coast,k=4)+s(dist50m,k=4)+s(dist100m,k=4),family=binomial(link="logit"),data=R2.data)
summary(R2.full)
concurvity(R2.full, full=F)

R3.full<-gam(response.pa ~ s(combined.depth,k=4)+s(slope,k=4)+s(aspect,bs="cc")+s(mud250,k=4)+s(sand250,k=4)+s(gravel250,k=4)+s(bed.dist250,k=4)+s(tidal250,k=4)+s(sst,k=4)+s(sal,k=4)+s(do,k=4)+s(secchi,k=4)+s(dist2river,k=4)+s(dist2coast,k=4)+s(dist50m,k=4)+s(dist100m,k=4),family=binomial(link="logit"),data=R3.data)
summary(R3.full)
concurvity(R3.full, full=F)

rm(R1.full,R2.full,R3.full)

##Interpolation----
###Region 1: Banks Peninsula and Timaru interpolation----
R1.absence <- subset(R1.data,R1.data$response.pa == 0)
R1.presence<-subset(R1.data,R1.data$response.pa == 1)
R1.data.list<-list()
seed <- 20230511

for(i in 1:10) {
  seed <- seed+1
  set.seed(seed)
  temp<-R1.presence[sample(nrow(R1.presence),size = length(R1.absence$response.pa), replace = FALSE),]
  R1.data.list[[i]] <- rbind(R1.absence,temp)
}

rm(R1.presence,R1.absence,temp)
model.summaries.R1<-list()
summaries<-list()
models.R1<-list()
temp.models<-list()
R1.train.data<-list()
R1.test.data<-list()

for(i in 1:10) {
  set.seed(20221126)
  temp.data<-R1.data.list[[i]]
  sample <-sample(c(TRUE,FALSE),nrow(temp.data),replace = TRUE, prob = c(0.3331,0.6669))
  test1<-temp.data[sample,]#Take 1/3rd of data forward
  data.rest<-temp.data[!sample,] #Save the rest in scratch file
  sample<-sample(c(TRUE,FALSE),nrow(data.rest),replace = TRUE, prob = c(0.5,0.5))
  test2<-data.rest[sample,] 
  test3<-data.rest[!sample,]
  
  train1<-as.data.frame(rbind(test2,test3))
  train2<-as.data.frame(rbind(test1,test3))
  train3<-as.data.frame(rbind(test2,test1))
  
  R1.train.data[[i]]<-list(train1,train2,train3)
  R1.test.data[[i]]<-list(test1,test2,test3)
  
}

rm(data.rest,train1,train2,train3,test1,test2,test3)

for(i in 1:length(R1.train.data)){
  for(j in 1:length(R1.train.data[[i]])) {
    temp.models[[j]]<-gam(response.pa ~ s(slope,k=4) + s(gravel250,k=4) + s(bed.dist250,k=4) + s(sst,k=4) + s(do,k=4) + s(dist100m,k=4),family=binomial(link="logit"),data=R1.train.data[[i]][[j]],select = TRUE) #Need to specify gamma
    summaries[[j]]<-summary(temp.models[[j]])
  }
  models.R1[[i]] <- temp.models
  model.summaries.R1[[i]]<-summaries
}


rm(temp.models,summaries)

summary.stats.R1 <- data.frame(slope.chi2=as.numeric(),
                               gravel.chi2=as.numeric(),
                               beddist.chi2=as.numeric(),
                               sst.chi2=as.numeric(),
                               do.chi2=as.numeric(),
                               dist100m.chi2=as.numeric(),
                               slope.p=as.numeric(),
                               gravel.p=as.numeric(),
                               beddist.p=as.numeric(),
                               sst.p=as.numeric(),
                               do.p=as.numeric(),
                               dist100m.p=as.numeric(),
                               deviance.explained=as.numeric(),
                               r.sq=as.numeric(),
                               interp.Prop.correct=as.numeric(),
                               interp.CI.auc.low = as.numeric(),
                               interp.AUC = as.numeric(),
                               interp.CI.auc.high = as.numeric(),
                               interp.tss = as.numeric(),
                               Sensitivity = as.numeric(),
                               Specificity = as.numeric(),
                               transfer.prop.correct=as.numeric(),
                               transfer.CI.auc.low = as.numeric(),
                               transfer.AUC = as.numeric(),
                               transfer.CI.auc.high = as.numeric(),
                               transfer.tss = as.numeric(),
                               AUC.difference = as.numeric(),
                               TSS.difference = as.numeric(),
                               stringsAsFactors=FALSE)

#Record all interpolation results within dataframe
iteration <- 0

for(i in 1:length(models.R1)) {
  for(j in 1:length(models.R1[[i]])) {
    iteration <- iteration + 1
    val.data<-as.data.frame(R1.test.data[[i]][[j]])
    val.data$Prediction <- predict.gam(models.R1[[i]][[j]],newdata = val.data,type="response")
    summary.stats.R1[iteration,1:6]<-model.summaries.R1[[i]][[j]]$chi.sq #Chi2 results for each predictor
    summary.stats.R1[iteration,7:12]<-model.summaries.R1[[i]][[j]]$s.pv #p-value results for each predictor
    summary.stats.R1[iteration,13]<-model.summaries.R1[[i]][[j]]$dev.expl #Deviance explained of model
    summary.stats.R1[iteration,14]<-model.summaries.R1[[i]][[j]]$r.sq #R2 of model
    val.data$Pred.response<-ifelse(val.data$Prediction>0.5,1,0)
    val.data$Testpvo<-ifelse(val.data$Pred.response==val.data$response.pa,1,0)
    summary.stats.R1[iteration,15]<-sum(val.data$Testpvo)/length(val.data$Testpvo) #Proportion of correctly predicted response
    summary.stats.R1[iteration,16:18]<-ci.auc(roc(data=val.data,response=response.pa,predictor = Prediction)) #AUC with confidence intervals
    presence<-subset(val.data,response.pa==1)
    absence<-subset(val.data,response.pa==0)                                                     
    summary.stats.R1[iteration,19]<-(length(subset(presence$Testpvo, presence$Testpvo == 1))/length(presence$Testpvo))+(length(subset(absence$Testpvo, absence$Testpvo == 1))/length(absence$Testpvo))-1 ## code for TSS: Sensitivity + Specificity -1
    summary.stats.R1[iteration,20]<-length(subset(presence$Testpvo, presence$Testpvo == 1))/length(presence$Testpvo) #Sensitivity
    summary.stats.R1[iteration,21]<-length(subset(absence$Testpvo, absence$Testpvo == 1))/length(absence$Testpvo) #Specificty
  }
}

rm(presence,absence,val.data)


###Region 2: Banks Peninsula and Otago Interpolation----
R2.absence <- subset(R2.data,R2.data$response.pa == 0)
R2.presence<-subset(R2.data,R2.data$response.pa == 1)
R2.data.list<-list()
seed <- 20230511

for(i in 1:10) {
  seed <- seed+1
  set.seed(seed)
  temp<-R2.absence[sample(nrow(R2.absence),size = nrow(R2.presence), replace = FALSE),]
  R2.data.list[[i]] <- rbind(R2.presence,temp)
}

rm(R2.presence,R2.absence,temp.a,temp.p)
model.summaries.R2<-list()
summaries<-list()
models.R2<-list()
temp.models<-list()
R2.train.data<-list()
R2.test.data<-list()

for(i in 1:10) {
  set.seed(20221126)
  temp.data<-R2.data.list[[i]]
  sample <-sample(c(TRUE,FALSE),nrow(temp.data),replace = TRUE, prob = c(0.3331,0.6669))
  test1<-temp.data[sample,]#Take 1/3rd of data forward
  data.rest<-temp.data[!sample,] #Save the rest in scratch file
  sample<-sample(c(TRUE,FALSE),nrow(data.rest),replace = TRUE, prob = c(0.5,0.5))
  test2<-data.rest[sample,] 
  test3<-data.rest[!sample,]
  
  train1<-as.data.frame(rbind(test2,test3))
  train2<-as.data.frame(rbind(test1,test3))
  train3<-as.data.frame(rbind(test2,test1))
  
  R2.train.data[[i]]<-list(train1,train2,train3)
  R2.test.data[[i]]<-list(test1,test2,test3)
  
}

rm(data.rest,train1,train2,train3,test1,test2,test3)

for(i in 1:length(R2.train.data)){
  for(j in 1:length(R2.train.data[[i]])) {
    temp.models[[j]]<-gam(response.pa ~ s(combined.depth,k=4) + s(slope,k=4) + s(gravel250,k=4) + s(sst,k=4) + s(do,k=4) + s(dist100m,k=4),family=binomial(link="logit"),data=R2.train.data[[i]][[j]],select = TRUE) #Need to specify gamma
    summaries[[j]]<-summary(temp.models[[j]])
  }
  models.R2[[i]] <- temp.models
  model.summaries.R2[[i]]<-summaries
}


rm(temp.models,summaries)

summary.stats.R2 <- data.frame(depth.chi2=as.numeric(),
                               slope.chi2=as.numeric(),
                               aspect.chi2=as.numeric(),
                               gravel.chi2=as.numeric(),
                               sst.chi2=as.numeric(),
                               do.chi2=as.numeric(),
                               dist100m=as.numeric(),
                               depth.p=as.numeric(),
                               slope.p=as.numeric(),
                               aspect.p=as.numeric(),
                               gravel.p=as.numeric(),
                               sst.p=as.numeric(),
                               do.p=as.numeric(),
                               dist100m.p=as.numeric(),
                               deviance.explained=as.numeric(),
                               r.sq=as.numeric(),
                               interp.Prop.correct=as.numeric(),
                               interp.CI.auc.low = as.numeric(),
                               interp.AUC = as.numeric(),
                               interp.CI.auc.high = as.numeric(),
                               interp.tss = as.numeric(),
                               Sensitivity = as.numeric(),
                               Specificity = as.numeric(),
                               transfer.prop.correct=as.numeric(),
                               transfer.CI.auc.low = as.numeric(),
                               transfer.AUC = as.numeric(),
                               transfer.CI.auc.high = as.numeric(),
                               transfer.tss = as.numeric(),
                               AUC.difference = as.numeric(),
                               TSS.difference = as.numeric(),
                               stringsAsFactors=FALSE)

#Record all interpolation results within dataframe
iteration <- 0

for(i in 1:length(models.R2)) {
  for(j in 1:length(models.R2[[i]])) {
    iteration <- iteration + 1
    val.data<-as.data.frame(R2.test.data[[i]][[j]])
    val.data$Prediction <- predict.gam(models.R2[[i]][[j]],newdata = val.data,type="response")
    summary.stats.R2[iteration,1:7]<-model.summaries.R2[[i]][[j]]$chi.sq #Chi2 results for each predictor
    summary.stats.R2[iteration,8:14]<-model.summaries.R2[[i]][[j]]$s.pv #p-value results for each predictor
    summary.stats.R2[iteration,15]<-model.summaries.R2[[i]][[j]]$dev.expl #Deviance explained of model
    summary.stats.R2[iteration,16]<-model.summaries.R2[[i]][[j]]$r.sq #R2 of model
    val.data$Pred.response<-ifelse(val.data$Prediction>0.5,1,0)
    val.data$Testpvo<-ifelse(val.data$Pred.response==val.data$response.pa,1,0)
    summary.stats.R2[iteration,17]<-sum(val.data$Testpvo)/length(val.data$Testpvo) #Proportion of correctly predicted response
    summary.stats.R2[iteration,18:20]<-ci.auc(roc(data=val.data,response=response.pa,predictor = Prediction)) #AUC with confidence intervals
    presence<-subset(val.data,response.pa==1)
    absence<-subset(val.data,response.pa==0)                                                     
    summary.stats.R2[iteration,21]<-(length(subset(presence$Testpvo, presence$Testpvo == 1))/length(presence$Testpvo))+(length(subset(absence$Testpvo, absence$Testpvo == 1))/length(absence$Testpvo))-1 ## code for TSS: Sensitivity + Specificity -1
    summary.stats.R2[iteration,22]<-length(subset(presence$Testpvo, presence$Testpvo == 1))/length(presence$Testpvo) #Sensitivity
    summary.stats.R2[iteration,23]<-length(subset(absence$Testpvo, absence$Testpvo == 1))/length(absence$Testpvo) #Specificty
  }
}

rm(presence,absence,val.data)

###Region 3: Otago and Timaru----
R3.absence <- subset(R3.data,R3.data$response.pa == 0)
R3.presence<-subset(R3.data,R3.data$response.pa == 1)
R3.data.list<-list()
seed <- 20230511

for(i in 1:10) {
  seed <- seed+1
  set.seed(seed)
  temp<-R3.absence[sample(nrow(R3.absence),size = nrow(R3.presence), replace = FALSE),]
  R3.data.list[[i]] <- rbind(R3.presence,temp)
}

rm(R3.presence,R3.absence,temp.a,temp.p)
model.summaries.R3<-list()
summaries<-list()
models.R3<-list()
temp.models<-list()
R3.train.data<-list()
R3.test.data<-list()

for(i in 1:10) {
  set.seed(20221126)
  temp.data<-R3.data.list[[i]]
  sample <-sample(c(TRUE,FALSE),nrow(temp.data),replace = TRUE, prob = c(0.3331,0.6669))
  test1<-temp.data[sample,]#Take 1/3rd of data forward
  data.rest<-temp.data[!sample,] #Save the rest in scratch file
  sample<-sample(c(TRUE,FALSE),nrow(data.rest),replace = TRUE, prob = c(0.5,0.5))
  test2<-data.rest[sample,] 
  test3<-data.rest[!sample,]
  
  train1<-as.data.frame(rbind(test2,test3))
  train2<-as.data.frame(rbind(test1,test3))
  train3<-as.data.frame(rbind(test2,test1))
  
  R3.train.data[[i]]<-list(train1,train2,train3)
  R3.test.data[[i]]<-list(test1,test2,test3)
  
}

rm(data.rest,train1,train2,train3,test1,test2,test3)
for(i in 1:length(R3.train.data)){
  for(j in 1:length(R3.train.data[[i]])) {
    temp.models[[j]]<-gam(response.pa ~ s(slope,k=4) + s(mud250,k=4) + s(sst,k=4) + s(do,k=4) +s(dist50m,k=4),family=binomial(link="logit"),data=R3.train.data[[i]][[j]],select = TRUE) #Need to specify gamma
    summaries[[j]]<-summary(temp.models[[j]])
  }
  models.R3[[i]] <- temp.models
  model.summaries.R3[[i]]<-summaries
}


rm(temp.models,summaries)

summary.stats.R3 <- data.frame(slope.chi2=as.numeric(),
                               mud.chi2=as.numeric(),
                               sst.chi2=as.numeric(),
                               do.chi2=as.numeric(),
                               dist50m.chi2=as.numeric(),
                               slope.p=as.numeric(),
                               mud.p=as.numeric(),
                               sst.p=as.numeric(),
                               do.p=as.numeric(),
                               dist50m.p=as.numeric(),
                               deviance.explained=as.numeric(),
                               r.sq=as.numeric(),
                               interp.Prop.correct=as.numeric(),
                               interp.CI.auc.low = as.numeric(),
                               interp.AUC = as.numeric(),
                               interp.CI.auc.high = as.numeric(),
                               interp.tss = as.numeric(),
                               Sensitivity = as.numeric(),
                               Specificity = as.numeric(),
                               transfer.prop.correct=as.numeric(),
                               transfer.CI.auc.low = as.numeric(),
                               transfer.AUC = as.numeric(),
                               transfer.CI.auc.high = as.numeric(),
                               transfer.tss = as.numeric(),
                               AUC.difference = as.numeric(),
                               TSS.difference = as.numeric(),
                               stringsAsFactors=FALSE)

#Record all interpolation results within dataframe
iteration <- 0

for(i in 1:length(models.R3)) {
  for(j in 1:length(models.R3[[i]])) {
    iteration <- iteration + 1
    val.data<-as.data.frame(R3.test.data[[i]][[j]])
    val.data$Prediction <- predict.gam(models.R3[[i]][[j]],newdata = val.data,type="response")
    summary.stats.R3[iteration,1:5]<-model.summaries.R3[[i]][[j]]$chi.sq #Chi2 results for each predictor
    summary.stats.R3[iteration,6:10]<-model.summaries.R3[[i]][[j]]$s.pv #p-value results for each predictor
    summary.stats.R3[iteration,11]<-model.summaries.R3[[i]][[j]]$dev.expl #Deviance explained of model
    summary.stats.R3[iteration,12]<-model.summaries.R3[[i]][[j]]$r.sq #R3 of model
    val.data$Pred.response<-ifelse(val.data$Prediction>0.5,1,0)
    val.data$Testpvo<-ifelse(val.data$Pred.response==val.data$response.pa,1,0)
    summary.stats.R3[iteration,13]<-sum(val.data$Testpvo)/length(val.data$Testpvo) #Proportion of correctly predicted response
    summary.stats.R3[iteration,14:16]<-ci.auc(roc(data=val.data,response=response.pa,predictor = Prediction)) #AUC with confidence intervals
    presence<-subset(val.data,response.pa==1)
    absence<-subset(val.data,response.pa==0)                                                     
    summary.stats.R3[iteration,17]<-(length(subset(presence$Testpvo, presence$Testpvo == 1))/length(presence$Testpvo))+(length(subset(absence$Testpvo, absence$Testpvo == 1))/length(absence$Testpvo))-1 ## code for TSS: Sensitivity + Specificity -1
    summary.stats.R3[iteration,18]<-length(subset(presence$Testpvo, presence$Testpvo == 1))/length(presence$Testpvo) #Sensitivity
    summary.stats.R3[iteration,19]<-length(subset(absence$Testpvo, absence$Testpvo == 1))/length(absence$Testpvo) #Specificty
  }
}

rm(presence,absence,val.data)

##Transferability----
###Region 1 -> OTA----
iteration<-0

for(i in 1:length(models.R1)) {
  for(j in 1:length(models.R1[[i]])){
    iteration<- iteration + 1
    transfer.data<-OTA.data
    transfer.data$Prediction <- predict.gam(models.R1[[i]][[j]],newdata = transfer.data,type="response")
    transfer.data$Pred.response<-ifelse(transfer.data$Prediction>0.5,1,0) #Setting the level for predicted response: lower then 0.5 is considered an absence, higher is considered a presence
    transfer.data$Testpvo<-ifelse(transfer.data$Pred.response==transfer.data$response.pa,1,0)
    summary.stats.R1[iteration,22]<-sum(transfer.data$Testpvo)/length(transfer.data$Testpvo)
    summary.stats.R1[iteration,23:25]<-ci.auc(roc(data=transfer.data,response=response.pa,predictor = Prediction)) #extracting the AUC and CI from each model
    presence<-subset(transfer.data,response.pa==1)
    absence<-subset(transfer.data,response.pa==0)
    summary.stats.R1[iteration,26]<-(length(subset(presence$Testpvo, presence$Testpvo == 1))/length(presence$Testpvo))+(length(subset(absence$Testpvo, absence$Testpvo == 1))/length(absence$Testpvo))-1 ## code for TSS: Sensitivity + Specificity -1
    summary.stats.R1[iteration,27]<- summary.stats.R1[iteration,24] - summary.stats.R1[iteration,17]
    summary.stats.R1[iteration,28]<-summary.stats.R1[iteration,26] - summary.stats.R1[iteration,19]
  }
}
summary.stats.R1$transfer.data.source<-"OTA"
summary.stats.R1$model.run<-c(1.1, 1.7, 2.3, 3.1, 3.7, 4.3, 5.1, 5.7, 6.3, 7.1, 7.7, 8.3, 9.1, 9.7, 10.3, 11.1, 11.7, 12.3, 13.1, 13.7, 14.3, 15.1, 15.7, 16.3, 17.1, 17.7, 18.3, 19.1, 19.7, 20.3)
summary.stats.R1$model.run.OTA<-summary.stats.R1$model.run+0.1


###Region 2 -> TIM----
iteration<-0

for(i in 1:length(models.R2)) {
  for(j in 1:length(models.R2[[i]])){
    iteration<- iteration + 1
    transfer.data<-OTA.data
    transfer.data$Prediction <- predict.gam(models.R2[[i]][[j]],newdata = transfer.data,type="response")
    transfer.data$Pred.response<-ifelse(transfer.data$Prediction>0.5,1,0) #Setting the level for predicted response: lower then 0.5 is considered an absence, higher is considered a presence
    transfer.data$Testpvo<-ifelse(transfer.data$Pred.response==transfer.data$response.pa,1,0)
    summary.stats.R2[iteration,24]<-sum(transfer.data$Testpvo)/length(transfer.data$Testpvo)
    summary.stats.R2[iteration,25:27]<-ci.auc(roc(data=transfer.data,response=response.pa,predictor = Prediction)) #extracting the AUC and CI from each model
    presence<-subset(transfer.data,response.pa==1)
    absence<-subset(transfer.data,response.pa==0)
    summary.stats.R2[iteration,28]<-(length(subset(presence$Testpvo, presence$Testpvo == 1))/length(presence$Testpvo))+(length(subset(absence$Testpvo, absence$Testpvo == 1))/length(absence$Testpvo))-1 ## code for TSS: Sensitivity + Specificity -1
    summary.stats.R2[iteration,29]<- summary.stats.R2[iteration,26] - summary.stats.R2[iteration,19]
    summary.stats.R2[iteration,30]<-summary.stats.R2[iteration,28] - summary.stats.R2[iteration,21]
  }
}
summary.stats.R2$transfer.data.source<-"TIM"
summary.stats.R2$model.run<-c(1.1, 1.7, 2.3, 3.1, 3.7, 4.3, 5.1, 5.7, 6.3, 7.1, 7.7, 8.3, 9.1, 9.7, 10.3, 11.1, 11.7, 12.3, 13.1, 13.7, 14.3, 15.1, 15.7, 16.3, 17.1, 17.7, 18.3, 19.1, 19.7, 20.3)
summary.stats.R2$model.run.TIM<-summary.stats.R2$model.run+0.1

###Region 3 -> BP----
iteration<-0

for(i in 1:length(models.R3)) {
  for(j in 1:length(models.R3[[i]])){
    iteration<- iteration + 1
    transfer.data<-OTA.data
    transfer.data$Prediction <- predict.gam(models.R3[[i]][[j]],newdata = transfer.data,type="response")
    transfer.data$Pred.response<-ifelse(transfer.data$Prediction>0.5,1,0) #Setting the level for predicted response: lower then 0.5 is considered an absence, higher is considered a presence
    transfer.data$Testpvo<-ifelse(transfer.data$Pred.response==transfer.data$response.pa,1,0)
    summary.stats.R3[iteration,20]<-sum(transfer.data$Testpvo)/length(transfer.data$Testpvo)
    summary.stats.R3[iteration,21:23]<-ci.auc(roc(data=transfer.data,response=response.pa,predictor = Prediction)) #extracting the AUC and CI from each model
    presence<-subset(transfer.data,response.pa==1)
    absence<-subset(transfer.data,response.pa==0)
    summary.stats.R3[iteration,24]<-(length(subset(presence$Testpvo, presence$Testpvo == 1))/length(presence$Testpvo))+(length(subset(absence$Testpvo, absence$Testpvo == 1))/length(absence$Testpvo))-1 ## code for TSS: Sensitivity + Specificity -1
    summary.stats.R3[iteration,25]<- summary.stats.R3[iteration,22] - summary.stats.R3[iteration,15]
    summary.stats.R3[iteration,26]<-summary.stats.R3[iteration,24] - summary.stats.R3[iteration,17]
  }
}
summary.stats.R3$transfer.data.source<-"TIM"
summary.stats.R3$model.run<-c(1.1, 1.7, 2.3, 3.1, 3.7, 4.3, 5.1, 5.7, 6.3, 7.1, 7.7, 8.3, 9.1, 9.7, 10.3, 11.1, 11.7, 12.3, 13.1, 13.7, 14.3, 15.1, 15.7, 16.3, 17.1, 17.7, 18.3, 19.1, 19.7, 20.3)
summary.stats.R3$model.run.TIM<-summary.stats.R3$model.run+0.1


coordinates<-cbind(R1.train.data[[1]][[1]]$lat,R1.train.data[[1]][[1]]$long)
resids<- models.R1[[1]][[1]]$residuals
v1 <- variog(coords = coordinates, data = resids)
plot(v1)


coordinates<-cbind(R2.train.data[[2]][[2]]$lat,R2.train.data[[2]][[2]]$long)
resids<- models.R2[[2]][[2]]$residuals
v1 <- variog(coords = coordinates, data = resids)
plot(v1)

coordinates<-cbind(R3.train.data[[2]][[1]]$lat,R3.train.data[[2]][[1]]$long)
resids<- models.R3[[2]][[1]]$residuals
v1 <- variog(coords = coordinates, data = resids)
plot(v1)

#Figures----
#Figure 1. Map of study area----

BP.data$colours = ifelse(BP.data$response.pa == 1, "darkgreen", "darkred")
TIM.data$colours = ifelse(TIM.data$response.pa == 1, "darkgreen", "darkred")
OTA.data$colours = ifelse(OTA.data$response.pa == 1, "darkgreen", "darkred")
nzcoast<-shapefile("./Shapefiles/nzcoast.shp")
study.area <- crop(nzcoast, extent(170.2,178,-46,-43))
bathy <- shapefile("./Shapefiles/CroppedBathy.shp")
study.extent<-shapefile("./Shapefiles/StudyExtent.shp")
study.extent<-spTransform(study.extent,crs(nzcoast))
BP.area<-crop(nzcoast, extent(c(172.5,173.8,-44.37,-43.52)))
BP.extent <- crop(study.extent, extent(c(172.5,173.8,-44.37,-43.52)))
TIM.area<-crop(nzcoast, extent(c(171, 172.2, -44.6, -44.1)))
TIM.extent <- crop(study.extent, extent(c(171,172.2,-44.6,-44.1)))
OTA.area<-crop(nzcoast, extent(c(170.3, 174, -46.1, -45.2))) 
OTA.extent <- shapefile("./Shapefiles/OTA.extent.shp")
ex.survey <- shapefile("./Shapefiles/ExampleSurvey.shp")

#inset.limits<- c(0.1309,0.35,0.71,0.9602)#L,R,B,T
inset.limits<- c(0.2445,0.42,0.65,0.96);inset.bp<- c(0.684,0.862,0.75,0.959);inset.tim<- c(0.60,0.78,0.355,0.655);inset.ota<- c(0.455,0.655,0.065,0.365)

tiff("C:/Users/steph/Desktop/Publishing/Transferability of SDM/Figures/Figure 1. Map of study area.tiff", width = 300, height = 200, res=300, units = "mm")
par(mar=c(0.1,0.1,0.1,5))
plot(study.area,col="gray",lwd = 0.05)
plot(bathy,add=TRUE, col = "gray35")
plot(ex.survey,add=TRUE, col = "orange")
plot(study.extent,add=TRUE,lwd = 1.2)

par(xpd = NA)
lines(x=c(174.5,174.5), y=c(-46,-43),lwd=1.2);lines(x=c(170.2,174.5),y=c(-43,-43),lwd=1.2);lines(x=c(170.2,174.5),y=c(-46,-46),lwd=1.2);lines(x=c(170.2,170.2),y=c(-43,-46),lwd=1.2)

text(x=172.5,y=-43.73,labels = "Banks Peninsula",cex = 0.9);text(x=171.05,y=-44.4,labels = "Timaru",cex = 0.9);text(x=170.4,y=-45.78,labels = "Otago",cex=0.9)
scalebar(92.6,xy=c(173.2,-45.95),type="bar",divs = 4, lonlat = TRUE, label = NA,cex = 1); text(x=174.25,y=-45.85,"50nm",cex=1)
rect(173.27,-43.775,174.45,-43.3, col="white", border = NA)


par(fig=inset.limits, new=TRUE, mar=c(0,0,0,0) )
plot(nzcoast,col="gray",lwd=0.001)
lines(x=c(173.5,173.5), y=c(-46,-43),lwd=0.8);lines(x=c(170.2,173.5),y=c(-43,-43),lwd=0.8);lines(x=c(170.2,173.5),y=c(-46,-46),lwd=0.8);lines(x=c(170.2,170.2),y=c(-43,-46),lwd=0.8)
north(xy = c(166,-38),type=2,cex=1.5)
box(lwd=0.9,col="black",lty = "solid")

par(fig=inset.bp, new=TRUE, mar=c(0,0,0,0))
plot(BP.area,col="gray",lwd=0.001);plot(BP.extent,add=TRUE,lwd = 1.2)
lines(x=c(172.5,172.5), y=c(-43.97,-43.52),lwd=0.8);lines(x=c(172.5,173.215),y=c(-43.52,-43.52),lwd=0.8);lines(x=c(173.215,173.215),y=c(-43.52,-43.97),lwd=0.8);lines(x=c(172.5,173.215),y=c(-43.97,-43.97),lwd=0.8)
points(x = BP.data$long, y = BP.data$lat, col = BP.data$colours, pch = 16, cex = 0.5)
scalebar(9.26, xy = c(172.52,-43.93),type = "line", label=NA); text(x=172.7,-43.93,"5nm",cex = 0.8)

par(fig=inset.tim, new=TRUE, mar=c(0,0,0,0) )
plot(TIM.area,col="gray",lwd=0.001);plot(TIM.extent,add=TRUE,lwd = 1.2)
lines(x=c(171,171), y=c(-44.6,-44.1),lwd=0.8);lines(x=c(171,171.8),y=c(-44.1,-44.1),lwd=0.8);lines(x=c(171.8,171.8),y=c(-44.1,-44.6),lwd=0.8);lines(x=c(171,171.8),y=c(-44.6,-44.6),lwd=0.8)
points(x = TIM.data$long, y = TIM.data$lat, col = TIM.data$colours, pch = 16, cex = 0.5)
scalebar(9.26, xy = c(171.54,-44.58),type = "line", label=NA); text(x=171.72,-44.58,"5nm",cex = 0.8)

par(fig=inset.ota, new=TRUE, mar=c(0,0,0,0) )
plot(OTA.area,col="gray",lwd=0.001);plot(OTA.extent,add=TRUE,lwd = 1.2)
lines(x=c(170.3,170.3), y=c(-46.,-45.2),lwd=0.8);lines(x=c(170.3,171.25),y=c(-46.,-46.),lwd=0.8);lines(x=c(171.25,171.25),y=c(-46.,-45.2),lwd=0.8);lines(x=c(170.3,171.25),y=c(-45.2,-45.2),lwd=0.8)
points(x = OTA.data$long, y = OTA.data$lat, col = OTA.data$colours, pch = 16, cex = 0.5)
scalebar(9.26, xy = c(170.9,-45.95),type = "line", label=NA); text(x=171.13,-45.95,"5nm",cex = 0.8)

rect(171.6,-45.57,171.68,-45.62, col="white") ; text(171.7,-45.6,"Study extent", cex = 0.8, pos = 4)
lines(x=c(171.6,171.68), y=c(-45.425,-45.425),, col = "orange"); text(171.7,-45.425,"Survey route", cex = 0.8, pos = 4)
lines(x=c(171.6,171.68), y=c(-45.5,-45.5),, col = "gray35"); text(171.7,-45.5,"Depth Contour", cex = 0.8, pos = 4)
points(171.63,-45.68, pch = 16, col = "darkgreen"); text(171.7,-45.68,"Presence", cex = 0.8, pos = 4)
points(171.63,-45.75, pch = 16, col = "darkred"); text(171.7,-45.75,"Absence", cex = 0.8, pos = 4)
rect(171.58,-45.8,172.3,-45.35)

dev.off()







##Figure 2----
#Banks P example model (4 covariates, dev.e > 0.2 tss + and AUC>0.7) = models.BP[[5]][[3]]
#TIM example model = highest deviance explained
#OTA example modelgood de auc and tss

colnames(bp.train.data[[5]][[3]])[colnames(bp.train.data[[5]][[3]]) == 'combined.depth'] <- 'Depth'
colnames(bp.train.data[[5]][[3]])[colnames(bp.train.data[[5]][[3]]) == 'slope'] <- 'Slope'
colnames(bp.train.data[[5]][[3]])[colnames(bp.train.data[[5]][[3]]) == 'sal'] <- 'Salinity'
colnames(bp.train.data[[5]][[3]])[colnames(bp.train.data[[5]][[3]]) == 'mud250'] <- 'Mud'
colnames(TIM.train.data[[2]][[2]])[colnames(TIM.train.data[[2]][[2]]) == 'dist50m'] <- 'Dist50m'
colnames(TIM.train.data[[2]][[2]])[colnames(TIM.train.data[[2]][[2]]) == 'sst'] <- 'SST'
colnames(TIM.train.data[[2]][[2]])[colnames(TIM.train.data[[2]][[2]]) == 'sal'] <- 'Salinity'
colnames(TIM.train.data[[2]][[2]])[colnames(TIM.train.data[[2]][[2]]) == 'bed.dist250'] <- 'Bed.dist'
colnames(TIM.train.data[[2]][[2]])[colnames(TIM.train.data[[2]][[2]]) == 'dist2river'] <- 'Dist.river'
colnames(OTA.train.data[[2]][[1]])[colnames(OTA.train.data[[2]][[1]]) == 'dist50m'] <- 'Dist50m'
colnames(OTA.train.data[[2]][[1]])[colnames(OTA.train.data[[2]][[1]]) == 'sand250'] <- 'Sand'
colnames(OTA.train.data[[2]][[1]])[colnames(OTA.train.data[[2]][[1]]) == 'do'] <- 'DO'
colnames(OTA.train.data[[2]][[1]])[colnames(OTA.train.data[[2]][[1]]) == 'sal'] <- 'Salinity'
colnames(OTA.train.data[[2]][[1]])[colnames(OTA.train.data[[2]][[1]]) == 'sst'] <- 'SST'
colnames(OTA.train.data[[2]][[1]])[colnames(OTA.train.data[[2]][[1]]) == 'slope'] <- 'Slope'

plot.model.BP<-gam(response.pa ~ s(Depth,k=4) + s(Salinity,k=4) + s(Slope,k=4)+s(Mud,k=4), family=binomial(link="logit"), data = bp.train.data[[5]][[3]],select=TRUE)
plot.model.TIM<-gam(response.pa ~ s(Dist50m,k=4) + s(Salinity,k=4)  + s(SST,k=4) + s(Dist.river,k=4) + s(Bed.dist,k=4), family=binomial(link="logit"), data = TIM.train.data[[2]][[2]],select=TRUE)
plot.model.OTA<-gam(response.pa ~ s(Dist50m,k=4) + s(Salinity,k=4) + s(Slope,k=4) + s(Sand,k=4) + s(DO,k=4), family=binomial(link="logit"), data = OTA.train.data[[2]][[1]],select=TRUE)

tiff("Figure 2. Example models.tiff",width=160, height = 200, units = "mm", res = 300)
layout(matrix(c(1,2,3,4,0,5,6,7,8,9,10,11,12,13,14), 5, 3, byrow=FALSE))
par(mai = c(0.32,0.4,0.15,0.05),mgp = c(1.5,0.6,0))
plot(plot.model.BP, shade = T, cex.axis = 0.8, cex.lab = 0.9, las = 1, ylim=c(-10,10))
plot(plot.model.TIM, shade = T, cex.axis = 0.8,cex.lab = 0.9, las = 1, ylim=c(-10,10))
plot(plot.model.OTA, shade = T, cex.axis = 0.8,cex.lab = 0.9, las = 1, ylim=c(-10,10))
text(2.25,135,"(a) Banks Peninsula",xpd=NA,adj = c(0,0))
text(5.1,135,"(b) Timaru",xpd=NA,adj = c(0,0))
text(7.75,135,"(c) Otago",xpd=NA,adj = c(0,0))
dev.off()

## Figure 3 & 4 data setup----
colour<-list("black", "darkred", "darkblue")
list.data.summary<-list(summary.stats.BP,summary.stats.TIM,summary.stats.OTA)
col.iter <- 0
data.source<-list("Banks Peninsula", "Timaru", "Otago ")
labs<-c("a)","b)","c)","d)","e)","f)")


#Calculate means and CI
list.data.summary[[1]]$dataset<-c(1,1,1,2,2,2,3,3,3,4,4,4,5,5,5,6,6,6,7,7,7,8,8,8,9,9,9,10,10,10)
list.data.summary[[2]]$dataset<-c(1,1,1,2,2,2,3,3,3,4,4,4,5,5,5)
list.data.summary[[3]]$dataset<-c(1,1,1,2,2,2,3,3,3,4,4,4,5,5,5)

CI.low <- function(xbar, n, sdev) {
  xbar - qt(0.975,df = n - 1)*sdev/sqrt(n)
}

CI.high<-function(xbar, n, sdev) {
  xbar + qt(0.975,df = n - 1)*sdev/sqrt(n)
}

averageBP<-aggregate(cbind(interp.AUC, transfer.AUC, transfer.AUC.2, interp.tss, transfer.tss, transfer.tss.2) ~ dataset, data = list.data.summary[[1]],FUN = mean)
averageBP$sd<-aggregate(cbind(interp.AUC, transfer.AUC, transfer.CI.auc.low, transfer.CI.auc.high, transfer.AUC.2, transfer.CI.auc.low.2, transfer.CI.auc.high.2, interp.tss, transfer.tss, transfer.tss.2) ~ dataset, data = list.data.summary[[1]],FUN = sd)
averageBP$n<-3

averageTIM<-aggregate(cbind(interp.AUC, transfer.AUC, transfer.AUC.2, interp.tss, transfer.tss, transfer.tss.2) ~ dataset, data = list.data.summary[[2]],FUN = mean)
averageTIM$sd<-aggregate(cbind(interp.AUC, transfer.AUC, transfer.CI.auc.low, transfer.CI.auc.high, transfer.AUC.2, transfer.CI.auc.low.2, transfer.CI.auc.high.2, interp.tss, transfer.tss, transfer.tss.2) ~ dataset, data = list.data.summary[[2]],FUN = sd)
averageTIM$n<-3

averageOTA<-aggregate(cbind(interp.AUC, transfer.AUC, transfer.AUC.2, interp.tss, transfer.tss, transfer.tss.2) ~ dataset, data = list.data.summary[[3]],FUN = mean)
averageOTA$sd<-aggregate(cbind(interp.AUC, transfer.AUC, transfer.CI.auc.low, transfer.CI.auc.high, transfer.AUC.2, transfer.CI.auc.low.2, transfer.CI.auc.high.2, interp.tss, transfer.tss, transfer.tss.2) ~ dataset, data = list.data.summary[[3]],FUN = sd)
averageOTA$n<-3

data.mean<-list(averageBP,averageTIM,averageOTA)
rm(averageBP,averageOTA,averageTIM)

for(i in 1:3) {
  data.mean[[i]]$iAUC.95CIlow <- CI.low(xbar = data.mean[[i]]$interp.AUC, n = data.mean[[i]]$n, sdev = data.mean[[i]]$sd$interp.AUC)
  data.mean[[i]]$iAUC.95CIhigh <- CI.high(xbar = data.mean[[i]]$interp.AUC, n = data.mean[[i]]$n, sdev = data.mean[[i]]$sd$interp.AUC)
  data.mean[[i]]$tAUC.95CIlow <- CI.low(xbar = data.mean[[i]]$transfer.AUC, n = data.mean[[i]]$n, sdev = data.mean[[i]]$sd$transfer.AUC)
  data.mean[[i]]$tAUC.95CIhigh <- CI.high(xbar = data.mean[[i]]$transfer.AUC, n = data.mean[[i]]$n, sdev = data.mean[[i]]$sd$transfer.AUC)
  data.mean[[i]]$t2AUC.95CIlow <- CI.low(xbar = data.mean[[i]]$transfer.AUC.2, n = data.mean[[i]]$n, sdev = data.mean[[i]]$sd$transfer.AUC.2)
  data.mean[[i]]$t2AUC.95CIhigh <- CI.high(xbar = data.mean[[i]]$transfer.AUC.2, n = data.mean[[i]]$n, sdev = data.mean[[i]]$sd$transfer.AUC.2)
  data.mean[[i]]$itss.95CIlow <- CI.low(xbar = data.mean[[i]]$interp.tss, n = data.mean[[i]]$n, sdev = data.mean[[i]]$sd$interp.tss)
  data.mean[[i]]$itss.95CIhigh <- CI.high(xbar = data.mean[[i]]$interp.tss, n = data.mean[[i]]$n, sdev = data.mean[[i]]$sd$interp.tss)
  data.mean[[i]]$ttss.95CIlow <- CI.low(xbar = data.mean[[i]]$transfer.tss, n = data.mean[[i]]$n, sdev = data.mean[[i]]$sd$transfer.tss)
  data.mean[[i]]$ttss.95CIhigh <- CI.high(xbar = data.mean[[i]]$transfer.tss, n = data.mean[[i]]$n, sdev = data.mean[[i]]$sd$transfer.tss)
  data.mean[[i]]$t2tss.95CIlow <- CI.low(xbar = data.mean[[i]]$transfer.tss.2, n = data.mean[[i]]$n, sdev = data.mean[[i]]$sd$transfer.tss.2)
  data.mean[[i]]$t2tss.95CIhigh <- CI.high(xbar = data.mean[[i]]$transfer.tss.2, n = data.mean[[i]]$n, sdev = data.mean[[i]]$sd$transfer.tss.2)
  data.mean[[i]]$auc.diff<-data.mean[[i]]$transfer.AUC - data.mean[[i]]$interp.AUC
  data.mean[[i]]$auc.diff2<-data.mean[[i]]$transfer.AUC.2 - data.mean[[i]]$interp.AUC
  data.mean[[i]]$tss.diff<-data.mean[[i]]$transfer.tss - data.mean[[i]]$interp.tss
  data.mean[[i]]$tss.diff2<-data.mean[[i]]$transfer.tss.2 - data.mean[[i]]$interp.tss
}

##Figure 3----
tiff("Figure 3. Mean TSS-AUC combined.tiff", width = 160, height = 190, res = 300, unit = "mm")
par(mfrow=c(3,2),mai = c(0.6,0.6,0.3,0.3))

for(i in 1:3) {
  col.iter <- i
  plot(interp.tss ~ dataset, data=data.mean[[i]],pch = 19, col =colour[[col.iter]], ylim = c(-1,1),xlim = c(0.75,max(data.mean[[i]]$dataset)+0.25), ylab = "Mean TSS", xlab = "Dataset", axes = F, yaxs="i")
  arrows(x0 = data.mean[[i]]$dataset, y0 = data.mean[[i]]$itss.95CIlow, y1 = data.mean[[i]]$itss.95CIhigh,code = 3, angle = 90, length = 0.01, col =colour[[col.iter]])
  axis(1, at = seq(1, max(data.mean[[i]]$dataset), by= 1),cex.axis = 0.8)
  axis(2, at = seq(-1,1, by = 0.2), las = 1,cex.axis = 0.8)
  abline(h=-1,lwd = 1.3)
  abline(h=0, lty="dotted")
  
  col2<-ifelse(col.iter+1 > 2, 1, 2)
  
  points(transfer.tss ~ c(dataset+0.1), data=data.mean[[i]], pch=19, col = colour[[col2]])
  arrows(x0 = c(data.mean[[i]]$dataset + 0.1), y0 = data.mean[[i]]$ttss.95CIlow, y1 = data.mean[[i]]$ttss.95CIhigh, code = 3, angle = 90, length = 0.01, col = colour[[col2]])
  
  col3<-ifelse(col.iter<=2, 3, 2)
  points(transfer.tss.2~c(dataset+0.2), data=data.mean[[i]], pch=19, col = colour[[col3]])
  arrows(x0 = c(data.mean[[i]]$dataset+0.2), y0 = data.mean[[i]]$t2tss.95CIlow, y1 = data.mean[[i]]$t2tss.95CIhigh, code = 3, angle = 90, length = 0.01, col = colour[[col3]])
  if(i == 1){
    text(x = -1, y = 1.2, labels = labs[[i]], xpd = NA)
  }else{
    text(x = -0.1, y = 1.2, labels = labs[[i]], xpd = NA)
  }
  
  plot(interp.AUC ~ dataset, data=data.mean[[i]],pch = 19, col =colour[[col.iter]], ylim = c(0,1),xlim = c(0.75,max(data.mean[[i]]$dataset)+0.25), ylab = "Mean AUC", xlab = "Dataset", axes = F, yaxs="i")
  arrows(x0 = data.mean[[i]]$dataset, y0 = data.mean[[i]]$iAUC.95CIlow, y1 = data.mean[[i]]$iAUC.95CIhigh,code = 3, angle = 90, length = 0.01, col =colour[[col.iter]])
  axis(1, at = seq(1, max(data.mean[[i]]$dataset), by= 1),cex.axis = 0.8)
  axis(2, at = seq(0,1, by = 0.1), las = 1,cex.axis = 0.8)
  abline(h=0,lwd = 1.3)
  abline(h=0.5, lty="dotted")
  abline(h=0.7,lty = "dashed")
  
  col2<-ifelse(col.iter+1 > 2, 1, 2)
  
  points(transfer.AUC ~ c(dataset+0.1), data=data.mean[[i]], pch=19, col = colour[[col2]])
  arrows(x0 = c(data.mean[[i]]$dataset + 0.1), y0 = data.mean[[i]]$tAUC.95CIlow, y1 = data.mean[[i]]$tAUC.95CIhigh, code = 3, angle = 90, length = 0.01, col = colour[[col2]])
  col3<-ifelse(col.iter<=2, 3, 2)
  points(transfer.AUC.2~c(dataset+0.2), data=data.mean[[i]], pch=19, col = colour[[col3]])
  arrows(x0 = c(data.mean[[i]]$dataset+0.2), y0 = data.mean[[i]]$t2AUC.95CIlow, y1 = data.mean[[i]]$t2AUC.95CIhigh,code = 3, angle = 90, length = 0.01, col = colour[[col3]])
  if(i == 1){
    text(x = -1, y = 1.1, labels = labs[[i+3]], xpd = NA)
  }else{
    text(x = -0.1, y = 1.1, labels = labs[[i+3]], xpd = NA)
  }
  
  
  if(i == 1) {
    legend(x=6.5,y=0.3,legend = c("Banks Peninsula","Timaru","Otago"),pch=19,col=c("black","darkred","darkblue"), cex=0.8)
  }
}

dev.off()


##Figure 4----
list.Rdata.summary<-list(summary.stats.R1,summary.stats.R2,summary.stats.R3)
colour<-list("black", "grey60")
data.source<-list("Region 1: BP & TIM", "Region 2: BP & OTA", "Region 3: TIM & OTA")
col.iter <- 0

rm(summary.stats.R1,summary.stats.R2,summary.stats.R3)


#Calculate means and CI
list.Rdata.summary[[1]]$dataset<-c(1,1,1,2,2,2,3,3,3,4,4,4,5,5,5,6,6,6,7,7,7,8,8,8,9,9,9,10,10,10)
list.Rdata.summary[[2]]$dataset<-c(1,1,1,2,2,2,3,3,3,4,4,4,5,5,5,6,6,6,7,7,7,8,8,8,9,9,9,10,10,10)
list.Rdata.summary[[3]]$dataset<-c(1,1,1,2,2,2,3,3,3,4,4,4,5,5,5,6,6,6,7,7,7,8,8,8,9,9,9,10,10,10)

CI.low <- function(xbar, n, sdev) {
  xbar - qt(0.975,df = n - 1)*sdev/sqrt(n)
}

CI.high<-function(xbar, n, sdev) {
  xbar + qt(0.975,df = n - 1)*sdev/sqrt(n)
}

averageR1<-aggregate(cbind(interp.AUC, transfer.AUC, interp.tss, transfer.tss) ~ dataset, data = list.Rdata.summary[[1]],FUN = mean)
averageR1$sd<-aggregate(cbind(interp.AUC, transfer.AUC, transfer.CI.auc.low, transfer.CI.auc.high, interp.tss, transfer.tss) ~ dataset, data = list.Rdata.summary[[1]],FUN = sd)
averageR1$n<-3

averageR2<-aggregate(cbind(interp.AUC, transfer.AUC, interp.tss, transfer.tss) ~ dataset, data = list.Rdata.summary[[2]],FUN = mean)
averageR2$sd<-aggregate(cbind(interp.AUC, transfer.AUC, transfer.CI.auc.low, transfer.CI.auc.high, interp.tss, transfer.tss) ~ dataset, data = list.Rdata.summary[[2]],FUN = sd)
averageR2$n<-3

averageR3<-aggregate(cbind(interp.AUC, transfer.AUC, interp.tss, transfer.tss) ~ dataset, data = list.Rdata.summary[[3]],FUN = mean)
averageR3$sd<-aggregate(cbind(interp.AUC, transfer.AUC, transfer.CI.auc.low, transfer.CI.auc.high,  interp.tss, transfer.tss) ~ dataset, data = list.Rdata.summary[[3]],FUN = sd)
averageR3$n<-3

data.mean<-list(averageR1,averageR2,averageR3)
rm(averageR1,averageR3,averageR2)

for(i in 1:3) {
  data.mean[[i]]$iAUC.95CIlow <- CI.low(xbar = data.mean[[i]]$interp.AUC, n = data.mean[[i]]$n, sdev = data.mean[[i]]$sd$interp.AUC)
  data.mean[[i]]$iAUC.95CIhigh <- CI.high(xbar = data.mean[[i]]$interp.AUC, n = data.mean[[i]]$n, sdev = data.mean[[i]]$sd$interp.AUC)
  data.mean[[i]]$tAUC.95CIlow <- CI.low(xbar = data.mean[[i]]$transfer.AUC, n = data.mean[[i]]$n, sdev = data.mean[[i]]$sd$transfer.AUC)
  data.mean[[i]]$tAUC.95CIhigh <- CI.high(xbar = data.mean[[i]]$transfer.AUC, n = data.mean[[i]]$n, sdev = data.mean[[i]]$sd$transfer.AUC)
  data.mean[[i]]$itss.95CIlow <- CI.low(xbar = data.mean[[i]]$interp.tss, n = data.mean[[i]]$n, sdev = data.mean[[i]]$sd$interp.tss)
  data.mean[[i]]$itss.95CIhigh <- CI.high(xbar = data.mean[[i]]$interp.tss, n = data.mean[[i]]$n, sdev = data.mean[[i]]$sd$interp.tss)
  data.mean[[i]]$ttss.95CIlow <- CI.low(xbar = data.mean[[i]]$transfer.tss, n = data.mean[[i]]$n, sdev = data.mean[[i]]$sd$transfer.tss)
  data.mean[[i]]$ttss.95CIhigh <- CI.high(xbar = data.mean[[i]]$transfer.tss, n = data.mean[[i]]$n, sdev = data.mean[[i]]$sd$transfer.tss)
  data.mean[[i]]$auc.diff<-data.mean[[i]]$transfer.AUC - data.mean[[i]]$interp.AUC
  data.mean[[i]]$tss.diff<-data.mean[[i]]$transfer.tss - data.mean[[i]]$interp.tss
}


tiff("Figure 4. Regional mean TSS-AUC combined.tiff", width = 160, height = 190, res = 300, unit = "mm")
par(mfrow=c(3,2),mai = c(0.6,0.6,0.3,0.3))

for(i in 1:3) {
  plot(interp.tss ~ dataset, data=data.mean[[i]],pch = 19, col =colour[[1]], ylim = c(-1,1),xlim = c(0.75,max(data.mean[[i]]$dataset)+0.25), ylab = "Mean TSS", xlab = "Dataset", axes = F, yaxs="i")
  arrows(x0 = data.mean[[i]]$dataset, y0 = data.mean[[i]]$itss.95CIlow, y1 = data.mean[[i]]$itss.95CIhigh,code = 3, angle = 90, length = 0.01, col =colour[[1]])
  axis(1, at = seq(1, max(data.mean[[i]]$dataset), by= 1),cex.axis = 0.8)
  axis(2, at = seq(-1,1, by = 0.2), las = 1,cex.axis = 0.8)
  abline(h=-1,lwd = 1.3)
  abline(h=0, lty="dotted")
  
  points(transfer.tss ~ c(dataset+0.1), data=data.mean[[i]], pch=19, col = colour[[2]])
  arrows(x0 = c(data.mean[[i]]$dataset + 0.1), y0 = data.mean[[i]]$ttss.95CIlow, y1 = data.mean[[i]]$ttss.95CIhigh, code = 3, angle = 90, length = 0.01, col = colour[[2]])
  
  if(i == 1){
    text(x = -1, y = 1.2, labels = labs[[i]], xpd = NA)
  }else{
    text(x = -1, y = 1.2, labels = labs[[i]], xpd = NA)
  }
  
  plot(interp.AUC ~ dataset, data=data.mean[[i]],pch = 19, col =colour[[1]], ylim = c(0,1),xlim = c(0.75,max(data.mean[[i]]$dataset)+0.25), ylab = "Mean AUC", xlab = "Dataset", axes = F, yaxs="i")
  arrows(x0 = data.mean[[i]]$dataset, y0 = data.mean[[i]]$iAUC.95CIlow, y1 = data.mean[[i]]$iAUC.95CIhigh,code = 3, angle = 90, length = 0.01, col =colour[[1]])
  axis(1, at = seq(1, max(data.mean[[i]]$dataset), by= 1),cex.axis = 0.8)
  axis(2, at = seq(0,1, by = 0.1), las = 1,cex.axis = 0.8)
  abline(h=0,lwd = 1.3)
  abline(h=0.5, lty="dotted")
  abline(h=0.7,lty = "dashed")
  
  col2<-ifelse(col.iter+1 > 2, 1, 2)
  
  points(transfer.AUC ~ c(dataset+0.1), data=data.mean[[i]], pch=19, col = colour[[2]])
  arrows(x0 = c(data.mean[[i]]$dataset + 0.1), y0 = data.mean[[i]]$tAUC.95CIlow, y1 = data.mean[[i]]$tAUC.95CIhigh, code = 3, angle = 90, length = 0.01, col = colour[[2]])
  if(i == 1){
    text(x = -1, y = 1.1, labels = labs[[i+3]], xpd = NA)
  }else{
    text(x = -1, y = 1.1, labels = labs[[i+3]], xpd = NA)
  }
  
  
  if(i == 1) {
    legend(x=6.5,y=0.3,legend = c("Interpolation","Transferability"),pch=19,col=c("black","gray"), cex=0.8)
  }
}

dev.off()


##Table data----
tab2.bp<-apply(summary.stats.BP[,c(1:26,28:36)],2,mean)
tab2.tim<-apply(summary.stats.TIM[,c(1:26,28:36)],2,mean)
tab2.ota<-apply(summary.stats.OTA[,c(1:28,30:38)],2,mean)

mean(data.mean[[1]]$interp.tss)
mean(data.mean[[2]]$interp.tss)
mean(data.mean[[3]]$interp.tss)
mean(data.mean[[1]]$interp.AUC)
mean(data.mean[[2]]$interp.AUC)
mean(data.mean[[3]]$interp.AUC)

mean(data.mean[[1]]$itss.95CIlow)
mean(data.mean[[1]]$itss.95CIhigh)
mean(data.mean[[1]]$iAUC.95CIlow)
mean(data.mean[[1]]$iAUC.95CIhigh)
mean(data.mean[[2]]$itss.95CIlow)
mean(data.mean[[2]]$itss.95CIhigh)
mean(data.mean[[2]]$iAUC.95CIlow)
mean(data.mean[[2]]$iAUC.95CIhigh)
mean(data.mean[[3]]$itss.95CIlow)
mean(data.mean[[3]]$itss.95CIhigh)
mean(data.mean[[3]]$iAUC.95CIlow)
mean(data.mean[[3]]$iAUC.95CIhigh)

mean(c(data.mean[[1]]$transfer.tss,data.mean[[1]]$transfer.tss.2))
mean(c(data.mean[[2]]$transfer.tss,data.mean[[2]]$transfer.tss.2))
mean(c(data.mean[[3]]$transfer.tss,data.mean[[3]]$transfer.tss.2))
mean(c(data.mean[[1]]$transfer.AUC,data.mean[[1]]$transfer.AUC.2))
mean(c(data.mean[[2]]$transfer.AUC,data.mean[[2]]$transfer.AUC.2))
mean(c(data.mean[[3]]$transfer.AUC,data.mean[[3]]$transfer.AUC.2))

mean(c(data.mean[[1]]$ttss.95CIlow,data.mean[[1]]$t2tss.95CIlow))
mean(c(data.mean[[1]]$ttss.95CIhigh,data.mean[[1]]$t2tss.95CIhigh))
mean(c(data.mean[[1]]$tAUC.95CIlow,data.mean[[1]]$t2AUC.95CIlow))
mean(c(data.mean[[1]]$tAUC.95CIhigh,data.mean[[1]]$t2AUC.95CIhigh))
mean(c(data.mean[[2]]$ttss.95CIlow,data.mean[[2]]$t2tss.95CIlow))
mean(c(data.mean[[2]]$ttss.95CIhigh,data.mean[[2]]$t2tss.95CIhigh))
mean(c(data.mean[[2]]$tAUC.95CIlow,data.mean[[2]]$t2AUC.95CIlow))
mean(c(data.mean[[2]]$tAUC.95CIhigh,data.mean[[2]]$t2AUC.95CIhigh))
mean(c(data.mean[[3]]$ttss.95CIlow,data.mean[[3]]$t2tss.95CIlow))
mean(c(data.mean[[3]]$ttss.95CIhigh,data.mean[[3]]$t2tss.95CIhigh))
mean(c(data.mean[[3]]$tAUC.95CIlow,data.mean[[3]]$t2AUC.95CIlow))
mean(c(data.mean[[3]]$tAUC.95CIhigh,data.mean[[3]]$t2AUC.95CIhigh))




#Supplementary Information----
##Appendix S1----
y.label<-list("Depth (m)","Secchi depth (m)", "Sea surface temperature \n(°celcius)",expression("Dissolved Oxygen " ~ (mg.L^{-1})),"Salinity (PSU)","Slope (°)","% Mud cover","% Sand cover","% Gravel cover",
              expression("Benthic disturbance "~ (m.s^{-1})),expression("Tidal current " ~ (m.s^{-1})),"Distance to \ncoastline (m)","Distance to \nriver mouth (m)","Distance to 50m \ndepth contour (m)","Distance to 100m \ndepth contour (m)")
predictor<-list(dat$combined.depth,dat$secchi,dat$sst,dat$do,dat$sal,dat$slope,dat$mud250,dat$sand250,dat$gravel250,dat$bed.dist250,dat$tidal250,dat$dist2coast,dat$dist2river,dat$dist50m,dat$dist100m)

tiff("Appendix S1. Covariate range.tiff",width = 160, height = 210, units = "mm", res = 300)
par(mai = c(0.45,0.4,0.02,0.1),mfrow = c(5,3),mgp = c(1.2,0.3,0))

for(i in 1:length(y.label)) {
  boxplot(predictor[[i]] ~ dat$site, xlab = "Location", ylab = y.label[[i]], lwd=0.5,cex.lab=0.8, axes=FALSE,cex=0.5,pch=19)
  axis(1, lwd=0.5, lwd.ticks = 0.5, cex.axis = 0.7, labels = c("Banks Peninsula", "Otago","Timaru"), at = c(1,2,3),tck=-0.03)
  axis(2, lwd=0.5, lwd.ticks = 0.2, cex.axis = 0.7, tck=-0.03)
  box(lwd=0.5)
}

dev.off()

rm(y.label,predictor)

