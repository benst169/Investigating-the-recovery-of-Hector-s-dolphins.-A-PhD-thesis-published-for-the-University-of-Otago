#===================
#Setup Workspace----
#===================
rm(list = ls(all=TRUE))
library(raster)

setwd("C:/Users/steph/Desktop/PhD/Research/IndividualBasedModelling/1. Scenario 1. Pop growth")

#=============
#FUNCTIONS----
#=============

##Biological----
###Supporting functions----
maturity <- function(dataframe){
  for(i in 1:nrow(dataframe)){
    if(dataframe[i,"Mature"] == 1){
      dataframe[i,"Mature"] = 1
    }else{
      if(dataframe[i,"Mature"] == 0 & dataframe[i,"Age"] < 6){
        dataframe[i,"Mature"] = 0
      }else{
        if(dataframe[i,"Mature"] == 0 & dataframe[i,"Age"] >= 6 & dataframe[i,"Age"] <= 8){
          dataframe[i,"Mature"] = sample(c(0,1),1,prob = c(0.7,0.3))
        }else{
          dataframe[i,"Mature"] = 1
        }
      }
    }
  }
  return(dataframe[,"Mature"])
} ##NOTE: Should figure out the actual probability of maturing at 6, 7 or 8 yo

#At the end of the March all mature females are assessed for whether they became pregnant during the summer. If they did, they will give birth the following season
pregnancy <- function(dataframe,fertility.mean,fertility.sd){

  fertility.rate = rnorm(1,mean = fertility.mean,sd=fertility.sd)
  fertility.rate = ifelse(fertility.rate < 0, 0, ifelse(fertility.rate > 1,1,fertility.rate))
  
  for(i in 1:nrow(dataframe)){
    
    #Three conditions to giving birth in the coming season ("SuccessfulPregnant"): Has to be a female, mature, and not currently giving parental care to a previous offspring at the start of the season, or the age of the previous offspring needs to be >1.
    if(dataframe[i,"Sex"] == "F" & dataframe[i,"Mature"] == 1 & dataframe[i,"ParentalCare"] == FALSE){
      dataframe[i,"SuccessfulPregnant"] =  sample(c(TRUE, FALSE),size = 1,replace = TRUE,c(fertility.rate,1-fertility.rate))
    }else{
      if(is.na(dataframe[i,"CurrentCalf"]) == FALSE | length(dataframe[which(dataframe[i,"CurrentCalf"] == dataframe$IndividualID),]) == 0){
        if(dataframe[which(dataframe[i,"CurrentCalf"] == dataframe$IndividualID),"Age"] >= 1){
          dataframe[i,"SuccessfulPregnant"] =  sample(c(TRUE, FALSE),size = 1,replace = TRUE,c(fertility.rate,1-fertility.rate))
        }
      }else{
        dataframe[i,"SuccessfulPregnant"] = dataframe[i,"SuccessfulPregnant"]
      }
    }
  }
  
  return(dataframe)
}

weaning<- function(dataframe){
  for(i in 1:nrow(dataframe)){
    if(dataframe[i,"Mother"] != "X" & length(which(dataframe[i,"Mother"] == dataframe$IndividualID)) > 0)
      if(dataframe[i,"Weaned"] == FALSE & dataframe[which(dataframe[i,"Mother"] == dataframe$IndividualID),"SuccessfulPregnant"] == TRUE){
        dataframe[i,c("Weaned","FirstYearOnOwn")] = TRUE
      }else{
        if(dataframe[i,"Weaned"] == FALSE & dataframe[i,"Age"] >= 1.5){
          dataframe[i,"Weaned"] = sample(c(TRUE,FALSE),1,replace = TRUE,prob=c(0.3,0.7))
          dataframe[i,"FirstYearOnOwn"] = dataframe[i,"Weaned"]
          
        }
      }else if(dataframe[i,"Weaned"] == FALSE & dataframe[i,"Age"] > 6){
        dataframe[i,"Weaned"] = TRUE
      }
    # Give calves which spent longer with their mums the ability to survive better
    if(dataframe[i,"Weaned"]==TRUE & dataframe[i,"FirstYearOnOwn"]==TRUE & dataframe[i,"Age"] > 2.5){
      dataframe[i,"FirstYearOnOwn"] = FALSE
    }
    
    #If the current calf weaned then the mother has no current calf.
    if(dataframe[i,"Weaned"]==TRUE & length(which(dataframe[i,"Mother"] == dataframe$IndividualID)) > 0){
      if(is.na(dataframe[which(dataframe[i,"Mother"] == dataframe$IndividualID),"CurrentCalf"]) == FALSE){
        if(dataframe[which(dataframe[i,"Mother"] == dataframe$IndividualID),"CurrentCalf"] == dataframe[i,"IndividualID"]){
          dataframe[which(dataframe[i,"Mother"] == dataframe$IndividualID),"CurrentCalf"] = NA
        }
      }
    }
  }
  return(dataframe)
}

###Primary functions----

reproduction <- function(dataframe){
  ProbCalve = 0.006 * ifelse(d.o.y > 289, d.o.y-289,76+d.o.y) #Probability that a pregnant female gives birth on a specific day increases to 100% throughout the season
  for(i in 1:nrow(dataframe)){
    if(dataframe[i,"SuccessfulPregnant"] == TRUE){
      new.calf = sample(c(TRUE, FALSE),1,replace=TRUE,prob=c(ProbCalve,1-ProbCalve)) 
      
      if(new.calf == TRUE){
        #Generate new calf row, assign ID 
        dataframe[nrow(dataframe)+1,] = NA
        dataframe[nrow(dataframe),"IndividualID"] = paste(year,rownames(dataframe[nrow(dataframe),]), sep="_")
        dataframe[nrow(dataframe),"Sex"] = sample(c("M","F"),1,replace=TRUE,prob=c(0.5,0.5))
        dataframe[nrow(dataframe),c("Age","Mature","DaysClosestHRC")] = 0 #Removed Proximity value
        dataframe[nrow(dataframe),c("Mother","x_loc","y_loc","Depth","GroupID","OrigPopulation","HomeRangeCentre")] = dataframe[i,c("IndividualID","x_loc","y_loc","Depth","GroupID","OrigPopulation","HomeRangeCentre")]
        dataframe[nrow(dataframe),"GroupSize"] = dataframe[i,"GroupSize"] + 1
        dataframe[nrow(dataframe),"Alive"] = 1
        dataframe[nrow(dataframe),c("ParentalCare","SuccessfulPregnant","Weaned","FirstYearOnOwn")] = FALSE
        dataframe[nrow(dataframe),"NoCalves"] = ifelse(dataframe[nrow(dataframe),"Sex"] == "F",0,NA)
        
        
        
        #Update the mothers data to reflect new calf
        
        dataframe[i,"CurrentCalf"] = dataframe[nrow(dataframe),"IndividualID"]
        
        if(dataframe[i,"NoCalves"] == 0 & is.na(dataframe[i,"NoCalves"])==FALSE){
          dataframe[i,"CalfIDs"] = dataframe[nrow(dataframe),"IndividualID"]
        }else{
          dataframe[i,"CalfIDs"] = paste(dataframe[i,"CalfIDs"], dataframe[nrow(dataframe),1],sep = ",")
        }
        dataframe[i,"NoCalves"] = dataframe[i,"NoCalves"]+1
        dataframe[i,"ParentalCare"] = TRUE
        dataframe[i,"SuccessfulPregnant"] = FALSE
        dataframe[i,"GroupSize"] = dataframe[i,"GroupSize"] + 1
        
      }
    }
    
  }
  return(dataframe)
}

survival <- function(dataframe, protection, survival.ip, survival.op){
  inside.protection <- extract(protection,dataframe[,c("x_loc","y_loc")])
  
  for(i in 1:nrow(dataframe)){
    #Survival rate based protection is based on estimates from Gormley et al. (2012) - should consider updating to Lindsays SR
    if(inside.protection[i] == 1){
      adult.surv.mean = survival.ip #0.917^(1/365) #To calculate daily survival, take the 365th root of annual survival
      adult.surv.sd = 0.0001 #May need to adjust the SD
    }else{
      adult.surv.mean = survival.op #0.89^(1/365)
      adult.surv.sd = 0.0002 #May need to adjust the SD
    }
    
    #Age based survival is calculated based on the discrepancy in survival of bottlenose dolphin calves compared with an adult as calculated by Arso-Civil et al. (2019).  
    ##The more parental investment an individual receives will reduce the discrepancy, so that the survival rate of any individual >5 will be drawn from the same distribution based on adult survival.
    if(dataframe[i,"Age"] >= 5 & dataframe[i,"Weaned"] == TRUE | dataframe[i,"Mother"] == "X"){
      survived.rate <- rnorm(1, adult.surv.mean, adult.surv.sd) 
    }else{
      
      #The first calf an individual has and the first year of life will result in the lowest survival
      if(length(which(dataframe[i,"Mother"] == dataframe$IndividualID)) > 0){      
        if(dataframe[which(dataframe[i,"Mother"] == dataframe$IndividualID),"NoCalves"] == 1){
          if(dataframe[i,"Age"] <= 1){
            survived.rate <- rnorm(1,((0.882^(1/365))*adult.surv.mean), adult.surv.sd) 
          }else{
            if(dataframe[i,"Age"] > 1 & dataframe[i,"Weaned"] == FALSE){
              survived.rate <- rnorm(1,((1.030^(1/365))*adult.surv.mean), adult.surv.sd) 
            }else{
              if(dataframe[i,"Age"] > 1 & dataframe[i,"Weaned"] == TRUE & dataframe[i,"FirstYearOnOwn"] == TRUE){
                survived.rate <- rnorm(1,((0.900^(1/365))*adult.surv.mean), adult.surv.sd) 
              }else{
                survived.rate <- rnorm(1, adult.surv.mean, adult.surv.sd) 
              }
            }
          }
        }else{
          if(dataframe[i,"Age"] <= 1){
            survived.rate <- rnorm(1,((0.928^(1/365))*adult.surv.mean), adult.surv.sd) 
          }else{
            if(dataframe[i,"Age"] > 1 & dataframe[i,"Weaned"] == FALSE){
              survived.rate <- rnorm(1,((1.031^(1/365))*adult.surv.mean), adult.surv.sd) 
            }else{
              if(dataframe[i,"Age"] > 1 & dataframe[i,"Weaned"] == TRUE & dataframe[i,"FirstYearOnOwn"] == TRUE){
                survived.rate <- rnorm(1,((0.943^(1/365))*adult.surv.mean), adult.surv.sd) 
              }else{
                survived.rate <- rnorm(1, adult.surv.mean, adult.surv.sd) 
              }
            }
          }
        }
      }else{
        if(length(which(dataframe[i,"Mother"] == dataframe$IndividualID)) == 0){
          if(dataframe[i,"FirstYearOnOwn"] == TRUE){
            survived.rate <- rnorm(1,((0.943^(1/365))*adult.surv.mean), adult.surv.sd)
          }else{
            if(dataframe[i,"FirstYearOnOwn"] == FALSE){
              survived.rate <- rnorm(1, adult.surv.mean, adult.surv.sd)
            }
          }
        }
      }
    }
    #As an individual ages it will become more likely to die. Iteratively (and arbitrarily?) lower survival rate of dolphins over a certain age.
    if(dataframe[i,"Age"] > 40){
      survived.rate = survived.rate-(0.0005*(dataframe[i,"Age"]-40))
    }
    
    #A probability >1 does not exist, so if the survival rate exceeds 1, it need to be reduced to 1.
    survived.rate<-ifelse(survived.rate > 1, 1, survived.rate)
    dataframe[i,"Alive"] = sample(c("1","0"),1,replace=TRUE,prob=c(survived.rate,1-survived.rate))
  }
  
  #Finally I will go through the dataframe one final time to determine if the mother of a young individual died they will simply also die and update the mothers info to reflect if calf died.
  for(i in 1:nrow(dataframe)){
    if(dataframe[i,"Weaned"] == FALSE & dataframe[i,"Mother"] != "X" & length(which(dataframe[i,"Mother"]==dataframe$IndividualID)) > 0){
      if(dataframe[i,"Age"] < 1.5 & dataframe[which(dataframe[i,"Mother"]==dataframe$IndividualID),"Alive"] == 0){
        dataframe[i,"Alive"] = 0
      }else{
        if(dataframe[i,"Weaned"] == FALSE & dataframe[i,"Age"] > 1.5 & dataframe[which(dataframe[i,"Mother"]==dataframe$IndividualID),"Alive"] == 0 ){
          dataframe[i,c("Weaned","FirstYearOnOwn")] = TRUE
        }
      }
    }else{
      if(is.na(dataframe[i,"CurrentCalf"])==FALSE){
        if(dataframe[which(dataframe[i,"CurrentCalf"] == dataframe$IndividualID),"Alive"] == 0){
          dataframe[i,"CurrentCalf"] = NA
          dataframe[i,"ParentalCare"] = FALSE
        }
      }
    }
  }
  return(dataframe[!dataframe$Alive == 0,])
}


#===================
#Initialise data----
#===================
#===================
#Initialise data----
#===================
##Load landscape----
#poly100m<-shapefile("./Landscape features/100mPolygonWGS.shp")
depth<-raster("./Landscape features/SI_depth.tif")
setnet.protect2022 <- raster("./Landscape features/setnet2022.tif")
protect100m <- raster("./Landscape features/protect100m.tif") ##Might need a slightly bigger raster - needs to extend outside 200m deep
no.protect <- raster("./Landscape features/NoProtect.tif")
HomeRangeCentres <- shapefile("./Landscape features/HD_HR_centre.shp")
BP_initial <- shapefile("./Landscape features/BP_initial.shp") 


##Initialize individuals----
##BanksPeninsula data initial population size = 3500
inds_data<-data.frame(array(data=0,dim=c(500,20)))
colnames(inds_data)<-c("IndividualID","Sex","Age","Mature","Mother","CalfIDs","x_loc","y_loc","Depth","GroupID","GroupSize","Alive","ParentalCare","NoCalves","CurrentCalf","Weaned","FirstYearOnOwn","SuccessfulPregnant","OrigPopulation","HomeRangeCentre") 

###Basic information----
inds_data[,"IndividualID"]<-seq(1,nrow(inds_data),by=1)
inds_data[,"Sex"]<-sample(c("M","F"),nrow(inds_data),replace=TRUE)
inds_data[,"Age"]<-round(rnorm(nrow(inds_data),15,sd=8)) #Could use the database to get actual values for mean and SD, and understand what the distribution of ages looks like. This won't be perfect as the dolphins may be older when they get marked.
inds_data[,"Age"]<-ifelse(inds_data[,3]<3,3,inds_data[,3])
inds_data[,"Mature"] <- maturity(inds_data)
inds_data[,"Mother"] <- "X"
inds_data[,"Alive"] <- 1

InitialGroupNo <- nrow(inds_data)/5
inds_data[,"GroupID"] <- sample(1:InitialGroupNo) #Randomly assign an individuals group 
for(i in 1:nrow(inds_data)){
  inds_data[i,"GroupSize"] = length(which(inds_data[,"GroupID"] == inds_data[i,"GroupID"]))
}

InitialLocations <- spsample(BP_initial, n=InitialGroupNo, type="random") #Make sure to generate within appropriate extent
for(i in 1:length(InitialLocations)){
  for(j in 1:nrow(inds_data)){
    if(inds_data[j,"GroupID"] == i){
      inds_data[j,"x_loc"]<-InitialLocations$x[i]
      inds_data[j,"y_loc"]<-InitialLocations$y[i]
    }else{
      inds_data[j,"x_loc"]<-inds_data[j,"x_loc"]
      inds_data[j,"y_loc"]<-inds_data[j,"y_loc"]
    }
  }
}

inds_data[,"Depth"] <- extract(depth,inds_data[,7:8])

for(i in 1:nrow(inds_data)) {
  alldistance <- pointDistance(inds_data[i,c("x_loc","y_loc")],coordinates(HomeRangeCentres),lonlat = TRUE)
  closestcentre <- min(alldistance)
  
  inds_data[i,c("HomeRangeCentre")] <- HomeRangeCentres$HR_Center[which(closestcentre == alldistance)]
  inds_data[i,"OrigPopulation"] <- ifelse(inds_data[i,"HomeRangeCentre"] == "BP_BirdlingsFlat" | inds_data[i,"HomeRangeCentre"] == "BP_AkaroaHarbour" | inds_data[i,"HomeRangeCentre"] == "BP_SteepHead" | inds_data[i,"HomeRangeCentre"] == "BP_PigeonBay", "BanksPeninsula", ifelse(inds_data[i,"HomeRangeCentre"] == "TIM_port" | inds_data[i,"HomeRangeCentre"] == "TIM_RakaiaRiver", "CanterburyBight", inds_data[i,"HomeRangeCentre"]))
}



###Calf info----
inds_data[,"ParentalCare"]<-ifelse(inds_data[,2] == "M" | inds_data[,4] == 0, FALSE, sample(c(TRUE,FALSE),nrow(inds_data), replace = TRUE, prob=c(0.33,0.67))) 
inds_data[,"SuccessfulPregnant"] = FALSE
inds_data[,"CurrentCalf"] = NA
inds_data[,"Weaned"] = TRUE
inds_data[,"FirstYearOnOwn"] = FALSE


#Generate a prior calving history
for(i in 1:nrow(inds_data)){
  if(inds_data[i,"Sex"] == "F" & inds_data[i,"Mature"] == 1){
    inds_data[i,"NoCalves"] = round((inds_data[i,"Age"]-sample(c(6,7,8),1,replace = TRUE))/sample(c(2,3),1,replace = TRUE))
    inds_data[i,"NoCalves"] = ifelse(inds_data[i,"NoCalves"]<0,0,inds_data[i,"NoCalves"])
    if(inds_data[i,"ParentalCare"] == TRUE & inds_data[i,"NoCalves"] == 0){
      inds_data[i,"NoCalves"] = 1
    }else{
      inds_data[i,"NoCalves"] = inds_data[i,"NoCalves"]
    }
  }else{
    inds_data[i,"NoCalves"] = ifelse(inds_data[i,"Sex"] == "F",0,NA)
  }
  
}

#Add the calves to the dataframe:
for(i in 1:nrow(inds_data)){
  if(inds_data[i,"ParentalCare"] == TRUE){
    
    #Generate the associated offspring (0, 1, 2 years old)
    inds_data[nrow(inds_data)+1,"IndividualID"] = max(inds_data$IndividualID)+1
    inds_data[nrow(inds_data),"Sex"] = sample(c("M","F"),1,replace=TRUE,prob=c(0.5,0.5))
    inds_data[nrow(inds_data),"Age"] = sample(c(0,1,2),1,replace=TRUE)
    inds_data[nrow(inds_data),c("Mature")] = 0 
    inds_data[nrow(inds_data),c("Mother","x_loc","y_loc","Depth","GroupID","OrigPopulation","HomeRangeCentre")] = inds_data[i,c("IndividualID","x_loc","y_loc","Depth","GroupID","OrigPopulation","HomeRangeCentre")]
    inds_data[nrow(inds_data),"GroupSize"] = inds_data[i,"GroupSize"] + 1
    inds_data[nrow(inds_data),"Alive"] = 1
    inds_data[nrow(inds_data),c("ParentalCare","SuccessfulPregnant","Weaned","FirstYearOnOwn")] = FALSE
    inds_data[nrow(inds_data),"NoCalves"] = ifelse(inds_data[nrow(inds_data),"Sex"] == "F",0,NA)
    
    
    #Mother's info
    inds_data[i,"CurrentCalf"] = inds_data[nrow(inds_data),"IndividualID"]
    inds_data[i,"GroupSize"] = inds_data[i,"GroupSize"] + 1
    
    if(inds_data[i,"NoCalves"] == 1){
      inds_data[i,"CalfIDs"] = inds_data[nrow(inds_data),"IndividualID"]
    }else{
      UnknownCalfIDspaste = paste(rep(NA,ifelse(inds_data[i,"NoCalves"]-1 >= 0,inds_data[i,"NoCalves"]-1,0)),collapse = " ")
      inds_data[i,"CalfIDs"] = paste(UnknownCalfIDspaste, inds_data[nrow(inds_data),1])
      inds_data[i,"CalfIDs"] = gsub(" ",", ", inds_data[i,"CalfIDs"])
    }
  }else{
    if(inds_data[i,"ParentalCare"] == FALSE & is.na(inds_data[i,"NoCalves"]) == FALSE){
      if(inds_data[i,"NoCalves"] == 0){
        inds_data[i,"CalfIDs"] = 0
      }else{
        if(inds_data[i,"NoCalves"] == 1){
          inds_data[i,"CalfIDs"] = paste(NA)
        }else{
          inds_data[i,"CalfIDs"] = paste(rep(NA,inds_data[i,"NoCalves"]),collapse = " ")
          inds_data[i,"CalfIDs"] = gsub(" ",", ", inds_data[i,"CalfIDs"])
        }
      }
    }
  }
}


rm(InitialGroupNo,InitialLocations,UnknownCalfIDspaste,alldistance,closestcentre)


#==============
#Simulation----
#==============
survival.values = rep(c(0.88^(1/365),0.89^(1/365),0.90^(1/365),0.91^(1/365),0.92^(1/365),0.93^(1/365),0.94^(1/365),0.95^(1/365),0.96^(1/365),0.97^(1/365),0.98^(1/365),0.99^(1/365)),6)
fertility.values = c(0.4,0.5,0.6,0.7,0.8,0.9,0.4,0.5,0.6,0.7,0.8,0.9,0.4,0.5,0.6,0.7,0.8,0.9,0.4,0.5,0.6,0.7,0.8,0.9,0.4,0.5,0.6,0.7,0.8,0.9,0.4,0.5,0.6,0.7,0.8,0.9,0.4,0.5,0.6,0.7,0.8,0.9,0.4,0.5,0.6,0.7,0.8,0.9,0.4,0.5,0.6,0.7,0.8,0.9,0.4,0.5,0.6,0.7,0.8,0.9,0.4,0.5,0.6,0.7,0.8,0.9,0.4,0.5,0.6,0.7,0.8,0.9)

demographics = as.data.frame(fertility.values[order(fertility.values)])
demographics$Survival = survival.values

Dem.tests = nrow(demographics)


##Define simulation parameters----
time.steps <- 365*30
ts <- 0
d.o.y <- 90
season = NA
year = 1
trial = 69 ##Can start here for next run. And will continue to add to the list

##Set up record objects to keep track of population over time ----
list.all.inds <- NULL #Keep track of individuals that were added at any stage

annual_repro_hist <- NULL
AllTrials_PopHist <- NULL
load("./Model outputs/PopHist_DemographicTrial.rda")
AllTrials_PopHist = PopHist_DemographicTrial

#Run simulation----
while(trial < Dem.tests){
  trial <- trial+1
  inds = inds_data
  list.all.inds[[trial]] <- inds_data$IndividualID
  ts <- 0
  d.o.y <- 90
  year = 1
  
  while(ts < time.steps){
    
    #Temporal variation in the environment
    ts <- ts + 1
    d.o.y <- if(d.o.y == 365){
               d.o.y = 1
             }else{
              d.o.y = d.o.y + 1
             }
    year <- if(d.o.y == 1){
              year = year+1
            }else{
              year = year
            }
    season <- ifelse(d.o.y < 46 | d.o.y > 319, "Summer",ifelse(d.o.y < 137, "Autumn", ifelse(d.o.y < 228, "Winter","Spring")))
    
    
    #Dolphin simulation
    inds[,"Age"] <- inds[,"Age"] + (1/365) #Update age by time step, in this case daily increments. 
    inds <- survival(dataframe = inds, protection = no.protect, survival.ip = demographics[trial,2], survival.op = demographics[trial,2]) #Every day an individual is given a chance of surviving
    ##From the middle to October to the end of March individuals are able to reproduce, if in the previous year they became successfully pregnant
    if(nrow(inds) > 0){  
    
      if(d.o.y > 289 | d.o.y < 91){
        inds <- reproduction(dataframe = inds)
        list.all.inds[[trial]] <- append(list.all.inds[[trial]],inds$IndividualID); list.all.inds[[trial]] <- unique(list.all.inds[[trial]]) #Keep track of all individuals that have ever been added to the dataframe
      }
      ##At the end of the summer season we will determine if an individual has reached sexual maturity over the past year and which sexually mature females had successful pregnancies.
      if(d.o.y == 91){
        inds[,"Mature"]<-maturity(inds)
        inds <- pregnancy(inds,fertility.mean = demographics[trial,1], fertility.sd = 0.01)
      }
      ##At the beginning of Spring all Juveniles that naturally wean this year will be determined//Weaning is forced on older individuals throughout the year IF their mother dies
      if(d.o.y == 245){
       inds <- weaning(dataframe = inds)
      }
      
      #Simulation records
      #inds_move_hist[[ts]] <- inds[,7:9]
      if(d.o.y == 91){
      annual_repro_hist[[year]] <- inds[,c(1:6,14:18)]
      }

    }else {
      ts = time.steps
    }
    print(c(trial, year, d.o.y, nrow(inds)))
  }
  
  AllTrials_PopHist[[trial]] <- annual_repro_hist
  
}


PopHist_DemographicTrial <- AllTrials_PopHist
save(PopHist_DemographicTrial, file = "./Model outputs/PopHist_DemographicTrial.rda")


#===========================
#Evaluation-----------------
#===========================

load(file = "./PopHist_DemographicTrial.rda")

SurvivalFecundityPopSizes<-NULL

##Population trajectories with no protection

for(i in 1:length(PopHist_DemographicTrial)){
  annual_pop_size<-data.frame(Fertility = as.numeric(),
                              Survival = as.numeric(),
                              Year = as.integer(),
                              PopulationSize = as.integer())
  
  for(j in 1:length(PopHist_DemographicTrial[[i]])){
    annual_pop_size[j,1] = demographics[i,1]
    annual_pop_size[j,2] = demographics[i,2]
    annual_pop_size[j,3] = j
    annual_pop_size[j,4] = nrow(PopHist_DemographicTrial[[i]][[j]])
  }
  SurvivalFecundityPopSizes[[i]] <- annual_pop_size
}

for(i in 1:length(SurvivalFecundityPopSizes)){
  for(j in 7:nrow(SurvivalFecundityPopSizes[[i]])){
    SurvivalFecundityPopSizes[[i]][j,"PopGrowthRate"] = (SurvivalFecundityPopSizes[[i]][j,"PopulationSize"] - SurvivalFecundityPopSizes[[i]][j-1,"PopulationSize"])/SurvivalFecundityPopSizes[[i]][j-1,"PopulationSize"]
    
  }
}

for(i in 1:length(SurvivalFecundityPopSizes)){
  demographics[i,"MeanGrowthRate"] = mean(SurvivalFecundityPopSizes[[i]]$PopGrowthRate,na.rm = TRUE)
  demographics[i,"SDGrowthRate"] = sd(SurvivalFecundityPopSizes[[i]]$PopGrowthRate,na.rm = TRUE)
  demographics[i,"TotalGrowthRate"] = ((SurvivalFecundityPopSizes[[i]][nrow(SurvivalFecundityPopSizes[[i]]),"PopulationSize"] - SurvivalFecundityPopSizes[[i]][1,"PopulationSize"])/SurvivalFecundityPopSizes[[i]][6,"PopulationSize"])/25
  demographics[i,"FinalPopulationSize"] = SurvivalFecundityPopSizes[[i]][nrow(SurvivalFecundityPopSizes[[i]]),"PopulationSize"]
}



demographics$Survival = demographics$Survival^365


for(j in 1:length(PopSizeAllScenarios)){
  for(i in 1:length(PopSizeAllScenarios[[j]])){
    if(j == 1 & i == 1){
      plot(PopulationSize~Year,data=PopSizeAllScenarios[[j]][[i]], type = "l", ylim = c(0,130), xlim = c(0,110),col = list.colours[[j]])
      rect(-10,-10,10,150,col=rgb(red=.217,green =.217,blue=.214 ,alpha=0.05),lty="dotted")
      text(3,120,"Burn-in")
    }else{
      lines(PopSizeAllScenarios[[j]][[i]]$PopulationSize~PopSizeAllScenarios[[j]][[i]]$Year, col = list.colours[[j]])
    }
    
    
  }
  
}

write.csv(demographics,"C:/Users/steph/Desktop/PhD/Research/IndividualBasedModelling/1. Scenario 1. Pop growth/SurvivalFecundityPopulationGrowth.csv")

library(RColorBrewer)

pal=colorRampPalette(c("red","darkgreen"))

demographics$order = findInterval(demographics$MeanGrowthRate, sort(demographics$MeanGrowthRate))

demographics$Trend = ifelse(demographics$MeanGrowthRate<(-0.01),"Decline",ifelse(demographics$MeanGrowthRate>0.01,"Increase","Stable"))
""

plot(demographics$`fertility.values[order(fertility.values)]`~demographics$Survival,pch=22, bg=pal(nrow(demographics))[demographics$order],cex=5,ylab = "Pregnancy rate \n(Available females that concieve per year)", xlab="Annual adult survival rate")


palette(default)

heatmap(a$MeanGrowthRate)

library('ggplot2')
demographics$Trend = ifelse(demographics$MeanGrowthRate<(-0.01),"Decline",ifelse(demographics$MeanGrowthRate>0.01,"Increase","Stable"))


a=demographics
a$`fertility.values[order(fertility.values)]`
b<-c(0.4,0.5,0.6,0.7,0.8,0.9)
c<-c("0.88","0.90","0.92","0.94","0.96","0.98")

library('viridis')
tiff("C:/Users/steph/Desktop/PhD/Writing/Chapter 6 - Predicting recovery through agent-based models/Figures/Figure 5.X. Scenario 1.tiff", width = 200, height = 100, res=300, units = "mm")
ggplot(data=a, aes(y=Survival, x=`fertility.values[order(fertility.values)]`,fill=TotalGrowthRate)) + 
  geom_tile()+
  scale_fill_viridis()+
  theme_classic() +
  ylab("Survival")+
  xlab("Pregnancy Rate") +
  labs(fill="λ")+
  scale_y_continuous(expand = c(0,0),breaks = c(0.88,0.90,0.92,0.94,0.96,0.98),labels = c) +
  scale_x_continuous(expand = c(0,0),limits = c(0.3,1),n.breaks = 6) +
  coord_cartesian(xlim = c(0.35, 0.95),ylim=c(0.875,0.995))+
  theme(axis.text.x = element_text(colour = "black",size = 12))+
  theme(axis.text.y = element_text( colour = "black",size = 12))+
  theme(axis.title = element_text( colour = "black",size = 12))+
  theme(legend.text = element_text( colour = "black",size = 12))+
  theme(legend.title = element_text( colour = "black",size = 12))+
  annotate("segment", x=0.45, xend=0.55, y=0.975, yend=0.975, colour="black",size=1)+
  annotate("segment", x=0.45, xend=0.45, y=0.975, yend=0.995, colour="black",size=1)+
  annotate("segment", x=0.55, xend=0.55, y=0.975, yend=0.955, colour="black",size=1)+
  annotate("segment", x=0.55, xend=0.75, y=0.955, yend=0.955, colour="black",size=1)+
  annotate("segment", x=0.75, xend=0.75, y=0.955, yend=0.945, colour="black",size=1)+
  annotate("segment", x=0.75, xend=0.85, y=0.945, yend=0.945, colour="black",size=1)+
  annotate("segment", x=0.85, xend=0.85, y=0.945, yend=0.935, colour="black",size=1)+
  annotate("segment", x=0.85, xend=0.95, y=0.935, yend=0.935, colour="black",size=1)+
  
  annotate("segment", x=0.35, xend=0.45, y=0.965, yend=0.965, colour="black",size=1)+
  annotate("segment", x=0.45, xend=0.45, y=0.965, yend=0.955, colour="black",size=1)+
  annotate("segment", x=0.45, xend=0.55, y=0.955, yend=0.955, colour="black",size=1)+
  annotate("segment", x=0.55, xend=0.55, y=0.955, yend=0.935, colour="black",size=1)+
  annotate("segment", x=0.55, xend=0.65, y=0.935, yend=0.935, colour="black",size=1)+
  annotate("segment", x=0.65, xend=0.65, y=0.935, yend=0.925, colour="black",size=1)+
  annotate("segment", x=0.65, xend=0.85, y=0.925, yend=0.925, colour="black",size=1)+
  annotate("segment", x=0.85, xend=0.85, y=0.925, yend=0.905, colour="black",size=1)+
  annotate("segment", x=0.85, xend=0.95, y=0.905, yend=0.905, colour="black",size=1)



dev.off()
