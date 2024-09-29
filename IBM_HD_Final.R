#===================
#Setup Workspace----
#===================
rm(list = ls(all=TRUE))
library(raster)

setwd("C:/Users/steph/Desktop/PhD/Research/IndividualBasedModelling/3. Scenario 3. Final Model Runs/")

#=============
#FUNCTIONS----
#=============

##Biological----
###Supporting functions----
maturity <- function(dataframe){
  #iterate through each row in the dataframe
  for(i in 1:nrow(dataframe)){
    #If an individual is already mature, they remain mature
    if(dataframe[i,"Mature"] == 1){
      dataframe[i,"Mature"] = 1
    }else{
      #if an individual is not mature but less than 6 years old - they remain immature
      if(dataframe[i,"Mature"] == 0 & dataframe[i,"Age"] < 6){
        dataframe[i,"Mature"] = 0
      }else{
        #if an individual is not mature and between the ages of 6 and 8, they will have a 70% probability of maturing
        if(dataframe[i,"Mature"] == 0 & dataframe[i,"Age"] >= 6 & dataframe[i,"Age"] <= 8){
          dataframe[i,"Mature"] = sample(c(0,1),1,prob = c(0.7,0.3))
          #All individuals older than 8, will have become mature
        }else{
          dataframe[i,"Mature"] = 1
        }
      }
    }
  }
  #Updated dataframe is returned
  return(dataframe[,"Mature"])
} 


pregnancy <- function(dataframe){
  #Set the mean and sd that the fertility rate can be drawn from. 
  fertility.mean = 0.6 
  fertility.sd = 0.002
  fertility.rate = rnorm(1,mean = fertility.mean,sd=fertility.sd)
  fertility.rate = ifelse(fertility.rate < 0, 0, ifelse(fertility.rate > 1,1,fertility.rate))
  
  #Iterate through every row of the dataframe
  for(i in 1:nrow(dataframe)){
    
    #If a dolphin is female, mature, and not currently giving parental care to a previous offspring at the start of the season:
    if(dataframe[i,"Sex"] == "F" & dataframe[i,"Mature"] == 1 & dataframe[i,"ParentalCare"] == FALSE){
      
      #Determine if she became pregnant based on the fertility rate.
      dataframe[i,"SuccessfulPregnant"] =  sample(c(TRUE, FALSE),size = 1,replace = TRUE,c(fertility.rate,1-fertility.rate))
    }else{
      #If a mature female is currently giving parental care, but their offspring are at least one year old
      if(is.na(dataframe[i,"CurrentCalf"]) == FALSE){
        if(dataframe[which(dataframe[i,"CurrentCalf"] == dataframe$IndividualID),"Age"] >= 1){
          
          #Determine if she became pregnant based on the fertility rate.
          dataframe[i,"SuccessfulPregnant"] =  sample(c(TRUE, FALSE),size = 1,replace = TRUE,c(fertility.rate,1-fertility.rate))
        }
      }else{
        
        #Otherwise she will not become pregnant
        dataframe[i,"SuccessfulPregnant"] = dataframe[i,"SuccessfulPregnant"]
      }
    }
  }
  
  #Update the dataframe with new pregnancy values
  return(dataframe)
}

weaning<- function(dataframe){
  for(i in 1:nrow(dataframe)){
    
    #If is originally in the data do nothing.
    if(dataframe[i,"Mother"] != "X" & length(which(dataframe[i,"Mother"] == dataframe$IndividualID)) > 0){
      
      #If the mother is pregnant with a new offspring the current calf will wean
      if(dataframe[i,"Weaned"] == FALSE & dataframe[which(dataframe[i,"Mother"] == dataframe$IndividualID),"SuccessfulPregnant"] == TRUE){
        dataframe[i,c("Weaned","FirstYearOnOwn")] = TRUE
        dataframe[which(dataframe[i,"Mother"] == dataframe$IndividualID),"ParentalCare"] == FALSE
        dataframe[which(dataframe[i,"Mother"] == dataframe$IndividualID),"CurrentCalf"] = NA
        
      }else{
        
        #If the current calf is greater than 1 and a half years old, then a 30% chance it will wean
        if(dataframe[i,"Weaned"] == FALSE & dataframe[i,"Age"] >= 1.5){
          dataframe[i,"Weaned"] = sample(c(TRUE,FALSE),1,replace = TRUE,prob=c(0.3,0.7))
          dataframe[i,"FirstYearOnOwn"] = dataframe[i,"Weaned"]
          
          #Update the mothers information as well
          if(dataframe[i,"Weaned"] == TRUE)
          dataframe[which(dataframe[i,"Mother"] == dataframe$IndividualID),"ParentalCare"] = !dataframe[i,"Weaned"]
          dataframe[which(dataframe[i,"Mother"] == dataframe$IndividualID),"CurrentCalf"] = NA
        }
      }
      
      #Separate to weaning, for older calves (that stayed with their mums for longer) the probability that they survive is higher.
      if(dataframe[i,"Weaned"]==TRUE & dataframe[i,"FirstYearOnOwn"]==TRUE & dataframe[i,"Age"] > 3){
        dataframe[i,"FirstYearOnOwn"] = FALSE
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
        dataframe[nrow(dataframe),c("Age","Mature")] = 0 #Removed Proximity value
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
      adult.surv.sd = 0.0001 #May need to adjust the SD
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
    #As an individual ages it will become more likely to die. Iteratively lower survival rate of dolphins over a certain age.
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
        if(dataframe[which(dataframe[i,"CurrentCalf"]==dataframe$IndividualID),"Alive"] == 0){
          dataframe[i,"CurrentCalf"] = NA
          dataframe[i,"ParentalCare"] = FALSE
        }
      }
    }
  }
  return(dataframe[!dataframe$Alive == 0,])
}

#Movement----
##In Decimal degrees (WGS84) a .01 degree change in Longitude = 777.558m and Latitude = 1111.477m. Slight distortions are noted but the general pattern holds true.


movement.HR<-function(dataframe, x.move = seq(-0.064,0.064,by=0.0001), y.move = seq(-0.045,0.045,by=0.0001)){
  for(i in 1:nrow(dataframe)){
    
    #Step 1: Determine how social an individual is feeling by generating a group tolerance parameter, with a seasonal basis.
    if(season == "Summer" | season == "Spring"){
      GroupTolerance = ifelse((x=rnorm(1,6,8)) <= 2, 2, round(x))
    }else{
      GroupTolerance = ifelse((x=rnorm(1,4,5)) <= 2, 2, round(x))
    }
    
    #Step 2: Determine a group decision. Does a dolphin remain in the same group, leave and join a different group, or leave and form a new group?
    
    if(dataframe[i,"Weaned"] == TRUE){
      
      ##A) For individuals who are currently on their own, or only with a calf:
      if(dataframe[i,"GroupSize"] == 1 | dataframe[i,"GroupSize"] == 2 & dataframe[i,"ParentalCare"] == TRUE){
        dist.matrix = pointDistance(p1 = cbind(dataframe[i,"x_loc"],dataframe[i,"y_loc"]),p2 = cbind(dataframe[,"x_loc"],dataframe[,"y_loc"]),lonlat = TRUE)
        ClosestGroupSize <- dataframe[min(which(pointDistance(p1 = cbind(dataframe[i,"x_loc"],dataframe[i,"y_loc"]),p2 = cbind(dataframe[,"x_loc"],dataframe[,"y_loc"]),lonlat = TRUE) > 0)), "GroupSize"]
        
        ###If the individual is within 1km of a different group and is smaller than the group tolerance limit they will join the new group. Otherwise sample a TRUE or FALSE allowing group tolerance limit to be overcome
        if(min(dist.matrix[dist.matrix > 0]) < 1000 & ClosestGroupSize < GroupTolerance){
          Join.Group = TRUE
        }else if(min(dist.matrix[dist.matrix > 0]) < 1000){
          Join.Group = sample(c(TRUE,FALSE),1,replace=TRUE,prob = c(0.2,0.8))
        }else{
          Join.Group = FALSE
        }
        
        if(Join.Group == TRUE){
          dataframe[i,"GroupID"] = dataframe[min(which(pointDistance(p1 = cbind(dataframe[i,"x_loc"],dataframe[i,"y_loc"]),p2 = cbind(dataframe[,"x_loc"],dataframe[,"y_loc"]),lonlat = TRUE) > 0)), "GroupID"]
          for(j in 1:nrow(dataframe)){
            dataframe[j,"GroupSize"] = length(which(dataframe[,"GroupID"] == dataframe[j,"GroupID"]))
          }
        }else{
          dataframe[i,"GroupID"] = dataframe[i,"GroupID"]
          #Recalculate group size for every individual
          for(j in 1:nrow(dataframe)){
            dataframe[j,"GroupSize"] = length(which(dataframe[,"GroupID"] == dataframe[j,"GroupID"]))
          }
        }
      }else{
        
        ##B) If an individual is already part of a group, they need to make a decision to leave or stay based on their current group tolerance:
        if(dataframe[i,"GroupSize"] > 1 | dataframe[i,"GroupSize"] > 2 & dataframe[i,"ParentalCare"] == TRUE){
          Leave.Group = ifelse(dataframe[i,"GroupSize"] > GroupTolerance,TRUE,FALSE)
          
          if(Leave.Group == TRUE){
            dist.matrix = pointDistance(p1 = cbind(dataframe[i,"x_loc"],dataframe[i,"y_loc"]),p2 = cbind(dataframe[,"x_loc"],dataframe[,"y_loc"]),lonlat = TRUE)
            ClosestGroupSize <- dataframe[min(which(pointDistance(p1 = cbind(dataframe[i,"x_loc"],dataframe[i,"y_loc"]),p2 = cbind(dataframe[,"x_loc"],dataframe[,"y_loc"]),lonlat = TRUE) > 0)), "GroupSize"]
            
            if(min(dist.matrix[dist.matrix > 0]) < 1000 & ClosestGroupSize < GroupTolerance){
              Join.Group = TRUE
            }else if(min(dist.matrix[dist.matrix > 0]) < 1000){
              Join.Group = sample(c(TRUE,FALSE),1,replace=TRUE,prob = c(0.2,0.8))
            }else{
              Join.Group = FALSE
            }
            
            #In this instance if there is another group nearby, an individual will join them otherwise they will leave their current group and start a new one.
            if(Join.Group == TRUE){
              dataframe[i,c("GroupID","x_loc","y_loc")] = dataframe[min(which(pointDistance(p1 = cbind(dataframe[i,"x_loc"],dataframe[i,"y_loc"]),p2 = cbind(dataframe[,"x_loc"],dataframe[,"y_loc"]),lonlat = TRUE) > 0)), c("GroupID","x_loc","y_loc")]
              for(j in 1:nrow(dataframe)){
                #After an individual leaves or joins a group the group size needs to be updated.
                dataframe[j,"GroupSize"] = length(which(dataframe[,"GroupID"] == dataframe[j,"GroupID"]))
              }
            }else{
              dataframe[i,"GroupID"] = max(dataframe[,"GroupID"])+1
              
              for(j in 1:nrow(dataframe)){
                #After an individual leaves or joins a group the group size needs to be updated.
                dataframe[j,"GroupSize"] = length(which(dataframe[,"GroupID"] == dataframe[j,"GroupID"]))
              }
            }
          }else{
            dataframe[i,"GroupID"] = dataframe[i,"GroupID"]
            
          }
        }
      }
    }else{
      if(dataframe[i,"Weaned"] == FALSE){
        
        ##C) If the individual is not yet weaned, it will move with it's mum
        dataframe[i,c("x_loc","y_loc","Depth","GroupID")] = dataframe[which(dataframe[i,"Mother"] == dataframe$IndividualID),c("x_loc","y_loc","Depth","GroupID")]
        for(j in 1:nrow(dataframe)){
          dataframe[j,"GroupSize"] = length(which(dataframe[,"GroupID"] == dataframe[j,"GroupID"]))
        }
      }
    }
  }
  
  #Step 3: Generate new locations for every group.
  ##A) Extract all current locations and generate a proposed location.
  orig.locations = dataframe[!duplicated(dataframe[,"GroupID"]),c("x_loc","y_loc","Depth","GroupID")]
  prop.locations = dataframe[!duplicated(dataframe[,"GroupID"]),c("x_loc","y_loc","Depth","GroupID")]
  
  #Depending on the season, individuals will be more or less likely to use different water depths
  if(season == "Summer" | season == "Spring"){
    preferred.depth.shallow = 20
    preferred.depth.deep = 50
    
  }else{
    preferred.depth.shallow = 100
    preferred.depth.deep = 100
  }
  
  for(i in 1:nrow(prop.locations)){

    iteration <- 0
    
    repeat{
      iteration <- iteration + 1
      prop.locations[i,"x_loc"] <- orig.locations[i,"x_loc"] + sample(x.move, 1, replace=TRUE)
      prop.locations[i,"y_loc"] <- orig.locations[i,"y_loc"] + sample(y.move, 1, replace=TRUE)
      prop.locations[i,"Depth"] <- extract(depth,cbind(prop.locations[i,"x_loc"],prop.locations[i,"y_loc"]))
      
      
      if(prop.locations[i,"Depth"] > preferred.depth.shallow){
        prop.locations[i,"Depth.decision"] = "Accept"
      }else{
        if(prop.locations[i,"Depth"] > preferred.depth.deep){
          prop.locations[i,"Depth.decision"] = sample(c("Accept","Reject"),1,replace=TRUE,prob = c(0.50,0.50))
        }else{
          prop.locations[i,"Depth.decision"] = sample(c("Accept","Reject"),1,replace=TRUE,prob = c(0.20,0.80))
        }
      }
      
      LeadersHRC <- min(dataframe[which(dataframe[,"GroupID"]==prop.locations[i,"GroupID"]),"HomeRangeCentre"])
      Dist2HRC <- pointDistance(orig.locations[i,c("x_loc","y_loc")], coordinates(HomeRangeCentres[which(LeadersHRC == HomeRangeCentres$HR_Center),]),lonlat=TRUE) 
      Dist2propHRC <- pointDistance(prop.locations[i,c("x_loc","y_loc")], coordinates(HomeRangeCentres[which(LeadersHRC == HomeRangeCentres$HR_Center),]),lonlat=TRUE)
      
      if(Dist2propHRC < Dist2HRC){
        #If the proposed distance is closer to the HRC then the previous, the new location will be accepted
        prop.locations[i,"HR.decision"] = "Accept"
        
      }else if(Dist2propHRC > Dist2HRC){
        #Otherwise if the proposed location takes a dolphin further from their HRC...
        if(Dist2HRC < 10000){ 
          #But is within 25km of the HRC... the point will be accepted
          prop.locations[i,"HR.decision"] = "Accept"
        }else if(Dist2HRC < 25000 ){
          #Further than 25km and rejection of the point begins, starting with low levels of rejection and building
          prop.locations[i,"HR.decision"] = sample(c("Accept","Reject"),1,replace=TRUE, prob = c(0.7,0.3))
        }else if(Dist2HRC < 50000){
          prop.locations[i,"HR.decision"] = sample(c("Accept","Reject"),1,replace=TRUE, prob = c(0.5,0.5))
        }else if(Dist2HRC > 50000){
          prop.locations[i,"HR.decision"] = sample(c("Accept","Reject"),1,replace=TRUE, prob = c(0.3,0.7))
        }
      }
      
      if(!is.na(prop.locations[i,"Depth"]) & prop.locations[i,"Depth"] < 0 & prop.locations[i,"Depth"] > (-200) & prop.locations[i,"Depth.decision"] == "Accept" & prop.locations[i,"HR.decision"] == "Accept" ){
        
        break
      }else{
        if(iteration == 50){
          prop.locations[i,c("x_loc","y_loc","Depth")] <- orig.locations[i,c("x_loc","y_loc","Depth")]
          prop.locations[i,c("Depth.decision","HR.decision")] = "Accept"
          break
        }
      }
      
    }
  }
  
  ##Finally assign all new locations, and update HRC information for each individual in the dataframe.
  for(i in 1:nrow(dataframe)){
    dataframe[i,c("x_loc","y_loc","Depth")] = prop.locations[which(dataframe[i,"GroupID"] == prop.locations$GroupID),c("x_loc","y_loc","Depth")]
  }
  return(dataframe)
}


#===================
#Initialise data----
#===================
##Load landscape----

depth<-raster("./Landscape features/SI_depth.tif")
protect100<- raster("./Landscape features/protect100m.tif")
setnet.protect2022 <- raster("./Landscape features/setnet2022.tif")
no.protect <- raster("./Landscape features/noprotect.tif")
HomeRangeCentres <- shapefile("./Landscape features/HD_HR_centre.shp")


##Load individuals----

BP_data = read.csv("BP_Trial_data.csv")
Tim_data = read.csv("Tim_Trial_data.csv")
Dun_data = read.csv("Dun_Trial_data.csv")


#==============
#Simulation----
#==============

##Define simulation parameters----
time.steps <- 365*110 #The total length of time the simulation will run for. Days x years
season = "Autumn" #Start the simulation at the end of summer
inds = inds_data
ts <- 0 #Keep track of the total number of ts that have occurred
d.o.y <- 55 #Set the start day to be the end of Summer Keeps track of where in the year we are up to.
year = 1 #Simulation starts at year 1.
trial = 0


list.all.inds <- NULL
inds_move_hist <- NULL #Keep track of movements
annual_pop_hist <- NULL #Annual snapshot of the population (to be used for survival and fecundity data)
AllTrials_PopHist <- NULL
AllTrials_MoveHist <- NULL

#Run simulation----
while(trial < 10){
  trial <- trial+1
  inds = inds_data
  list.all.inds[[trial]] <- inds_data$IndividualID
  ts <- 0
  d.o.y <- 90
  year = 1
  
  while(ts < time.steps){
    
    #Temporal variation in the environment
    ts <- ts + 10 #10 day increments
    d.o.y <- if(d.o.y >= 365){
               d.o.y = d.o.y-365
             }else{
              d.o.y = d.o.y + 10
             }
    year <- if(d.o.y >= 365){
              year = year+1
            }else{
              year = year
            }
    season <- ifelse(d.o.y < 46 | d.o.y > 319, "Summer",ifelse(d.o.y < 137, "Autumn", ifelse(d.o.y < 228, "Winter","Spring")))
    
    
    #Dolphin simulation
    ##Functions that occur at every time step (move/age/survive):
    inds <- movement.HR(dataframe = inds)
    inds[,"Age"] <- inds[,"Age"] + (10/365) 
    
    ##A "burn-in" period, where the initial population is allowed to stabilise. They can still move/reproduce/survive etc. but the affect of the protection will not occur until year 10.
    if(year < 11){
      inds <- survival(dataframe = inds, protection = no.protect, survival.ip = 0.92^(10/365), survival.op = 0.92^(10/365))
    }else{
      inds <- survival(dataframe = inds, protection = no.protect, survival.ip = 0.94^(10/365), survival.op = 0.89^(10/365))
    }
     
    ##Functions that occur within seasonal boundaries (reproduce/mature/pregnancy/weaning):
    ###From the start of November to the end of March individuals are able to reproduce, if in the previous year they became successfully pregnant
    if(nrow(inds) > 0){  
    
      if(d.o.y > 305 | d.o.y < 91){
        inds <- reproduction(dataframe = inds)
        list.all.inds[[trial]] <- append(list.all.inds[[trial]],inds$IndividualID); list.all.inds[[trial]] <- unique(list.all.inds[[trial]]) #Keep track of all individuals that have ever been added to the dataframe
      }
      ##At the end of the summer season we will determine if an individual has reached sexual maturity over the past year and which sexually mature females had successful pregnancies.
      if(d.o.y >= 90 & d.o.y <= 99){
        inds[,"Mature"]<-maturity(inds)
        inds <- pregnancy(inds)
      }
      ##At the beginning of Spring all Juveniles that naturally wean this year will be determined//Weaning is forced on older individuals throughout the year IF their mother dies
      if(d.o.y >= 245 & d.o.y <= 254){
       inds <- weaning(dataframe = inds)
      }
      
      #Simulation records
      if(d.o.y == 60 | d.o.y == 65){
        annual_pop_hist[[year]] <- inds[,c(1:6,14:20)] #A snapshot of the population early March
      }
      if(ts %% 10 == 0){
        inds_move_hist[[ts/10]] <- inds[,c(1,7:9,11,19,20)]
      }
    }else {
      ts = time.steps
    }
    print(c(trial, ts, year, d.o.y, nrow(inds)))
  }
  AllTrials_PopHist[[trial]] = annual_pop_hist
  AllTrials_MoveHist[[trial]] = inds_move_hist
  
  assign(paste0("POPHIST_DUN_Y30_TS1_S95_T", trial,sep=""), AllTrials_PopHist[[trial]])
  assign(paste0("MOVEHIST_DUN_Y30_TS1_S95_T", trial,sep=""), AllTrials_MoveHist[[trial]])
  
  save(list=paste0("POPHIST_DUN_Y30_TS1_S95_T", trial,sep=""), file = "./Model outputs/Dun_y30_ts1_surv_06_NP_PopHist.rda")
  save(list=paste0("MOVEHIST_DUN_Y30_TS1_S95_T", trial,sep=""), file = "./Model outputs/Laptop_94_06_NP_MoveHist.rda")
}

Laptop_94_06_NP_PopHist <- AllTrials_PopHist
Laptop_94_06_NP_MoveHist <- AllTrials_MoveHist


#==============
#Save data----
#==============

save(Laptop_94_06_NP_PopHist, file = "./Model outputs/Laptop_94_06_NP_PopHist.rda")
save(Laptop_94_06_NP_MoveHist, file = "./Model outputs/Laptop_94_06_NP_MoveHist.rda")

