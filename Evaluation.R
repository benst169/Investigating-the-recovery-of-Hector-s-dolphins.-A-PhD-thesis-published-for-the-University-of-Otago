#==============================
#Set up workspace and load data
#==============================
rm(list = ls(all=TRUE))

library(RColorBrewer)
library(raster)
setwd("C:/Users/steph/Desktop/PhD/Research/IndividualBasedModelling/2. Scenario 2. Dun demographics/Model outputs/")

load("./all_NP_PopHist.rda")
load("./all_CP_PopHist.rda")
load("./all_100P_PopHist.rda")

all_CP_PopHist[[3]] = all_CP_PopHist[[3]][-10]

# ===============
#Evaluation-----
#===============


##Demographic parameters and rates-----
###Annual Population Size and Population growth rates----
PopSizeDataFrameAll_NP = NULL
no.protect.PopSize<-NULL
PopSizeDataFrameAll_CP = NULL
current.protect.PopSize<-NULL
PopSizeDataFrameAll_100P = NULL
protect100.PopSize<-NULL

##Population trajectories with no protection
for(h in 1:length(all_NP_PopHist)){
  
  for(i in 1:length(all_NP_PopHist[[h]])){
    annual_pop_size<-data.frame(Year = as.integer(),
                                PopulationSize = as.integer())
    
    for(j in 2:length(all_NP_PopHist[[h]][[i]])){
      annual_pop_size[j,1] = j
      annual_pop_size[j,2] = nrow(all_NP_PopHist[[h]][[i]][[j]])
    }
    no.protect.PopSize[[i]] <- annual_pop_size
  }
  PopSizeDataFrameAll_NP[[h]] = no.protect.PopSize
}



##Population trajectories with current protection
for(h in 1:length(all_CP_PopHist)){
  for(i in 1:length(all_CP_PopHist[[h]])){
    annual_pop_size<-data.frame(Year = as.integer(),
                                PopulationSize = as.integer())
    
    for(j in 2:length(all_CP_PopHist[[h]][[i]])){
      annual_pop_size[j,1] = j
      annual_pop_size[j,2] = nrow(all_CP_PopHist[[h]][[i]][[j]])
    }
    current.protect.PopSize[[i]] <- annual_pop_size
  }
  PopSizeDataFrameAll_CP[[h]] = current.protect.PopSize
}


##Population trajectories with 100m protection
for(h in 1:length(all_100P_PopHist)){
  for(i in 1:length(all_100P_PopHist[[h]])){
    annual_pop_size<-data.frame(Year = as.integer(),
                                PopulationSize = as.integer())
    
    for(j in 2:length(all_100P_PopHist[[h]][[i]])){
      annual_pop_size[j,1] = j
      annual_pop_size[j,2] = nrow(all_100P_PopHist[[h]][[i]][[j]])
    }
    protect100.PopSize[[i]] <- annual_pop_size
  }
  PopSizeDataFrameAll_100P[[h]] = protect100.PopSize

}


PopSizeAllScenarios <- list(PopSizeDataFrameAll_NP,PopSizeDataFrameAll_CP,PopSizeDataFrameAll_100P)
rm(current.protect.PopSize,no.protect.PopSize,protect100.PopSize,annual_pop_size,PopSizeDataFrameAll_NP,PopSizeDataFrameAll_CP,PopSizeDataFrameAll_100P,h,i,j)
list.colours<-c("red","black","blue")




#abline(h=10)
#abline(h=50, lty = "dashed")
#abline(h=25, lty = "dotted")


PopulationGrowthMeans = NULL
Protection = c("NoProtection","CurrentProtection","100mProtection")
SurvivalTrials = c("Surv95_Fec08","Surv95_Fec07","Surv95_Fec06","Surv94_Fec06","Surv93_Fec06","Surv92_Fec06","Surv91_Fec06","Surv96_Fec06")

for(h in 1:length(PopSizeAllScenarios)){
  PopSizeDataframe =data.frame(Protection = as.character(),
                               SurvivalTrial = as.character(),
                             Trial = as.integer(),
                             Year = as.integer(),
                             PopulationSize = as.integer())
  for(i in 1:length(PopSizeAllScenarios[[h]])){
    for(j in 1:length(PopSizeAllScenarios[[h]][[i]])){
      if(i==1 & j == 1){
        data = as.data.frame(cbind(rep(Protection[[h]],nrow(PopSizeAllScenarios[[h]][[i]][[j]])),rep(SurvivalTrials[[i]],nrow(PopSizeAllScenarios[[h]][[i]][[j]])),rep(j,nrow(PopSizeAllScenarios[[h]][[i]][[j]])),PopSizeAllScenarios[[h]][[i]][[j]]$Year,PopSizeAllScenarios[[h]][[i]][[j]]$PopulationSize))
        names(data) = names(PopSizeDataframe)
        PopSizeDataframe <- data
      }else{
        data = as.data.frame(cbind(rep(Protection[[h]],nrow(PopSizeAllScenarios[[h]][[i]][[j]])),rep(SurvivalTrials[[i]],nrow(PopSizeAllScenarios[[h]][[i]][[j]])),rep(j,nrow(PopSizeAllScenarios[[h]][[i]][[j]])),PopSizeAllScenarios[[h]][[i]][[j]]$Year,PopSizeAllScenarios[[h]][[i]][[j]]$PopulationSize))
        names(data) = names(PopSizeDataframe)
        PopSizeDataframe = rbind(PopSizeDataframe,data)
        
      }
    }
  }
  PopulationGrowthMeans[[h]] = PopSizeDataframe
}

PopGrowthMeanSurvivalTrials = NULL
PopGrowthMeanFecundTrials = NULL
PopGrowthAll = NULL
PopGrowthAll_order = NULL

for(h in 1:length(PopulationGrowthMeans)){

  PopulationGrowthMeans[[h]]$Protection = as.factor(PopulationGrowthMeans[[h]]$Protection)
  PopulationGrowthMeans[[h]]$SurvivalTrial = as.factor(PopulationGrowthMeans[[h]]$SurvivalTrial)
  PopulationGrowthMeans[[h]]$Trial = as.factor(PopulationGrowthMeans[[h]]$Trial)
  PopulationGrowthMeans[[h]]$Year = as.integer(PopulationGrowthMeans[[h]]$Year)
  PopulationGrowthMeans[[h]]$PopulationSize = as.integer(PopulationGrowthMeans[[h]]$PopulationSize)
  
  MeanPopSize <- aggregate(PopulationSize ~  Year + SurvivalTrial, data = PopulationGrowthMeans[[h]], FUN = mean)

  mps1<-subset(MeanPopSize,MeanPopSize$SurvivalTrial == "Surv95_Fec08")
  mps2<-subset(MeanPopSize,MeanPopSize$SurvivalTrial == "Surv95_Fec07")
  mps3<-subset(MeanPopSize,MeanPopSize$SurvivalTrial == "Surv91_Fec06")
  mps4<-subset(MeanPopSize,MeanPopSize$SurvivalTrial == "Surv92_Fec06")
  mps5<-subset(MeanPopSize,MeanPopSize$SurvivalTrial == "Surv93_Fec06")
  mps6<-subset(MeanPopSize,MeanPopSize$SurvivalTrial == "Surv94_Fec06")
  mps7<-subset(MeanPopSize,MeanPopSize$SurvivalTrial == "Surv95_Fec06")
  mps8<-subset(MeanPopSize,MeanPopSize$SurvivalTrial == "Surv96_Fec06")
  
  survive = list(mps3,mps4,mps5,mps6,mps7,mps8)
  fecund = list(mps7,mps2,mps1)
  
  PopGrowthMeanSurvivalTrials[[h]] = survive
  PopGrowthMeanFecundTrials[[h]]=fecund
  
  PopGrowthAll[[h]] = list(mps3,mps4,mps5,mps6,mps1,mps2,mps7,mps8)
  PopGrowthAll_order[[h]] = list(mps1,mps2,mps7,mps6,mps5,mps4,mps3,mps8)
  
}

rm(mps1,mps2,mps3,mps4,mps5,mps6,mps7,mps8,data,MeanPopSize,survive,fecund)

#Figures -----

tiff("C:/Users/steph/Desktop/PhD/Writing/Chapter 6 - Predicting recovery through agent-based models/Figures/Survival trial.tiff",res = 300, units = "mm",width = 160, height=200)

reds =  brewer.pal(n=6,name = "Reds")
greys = brewer.pal(n=6,name = "Greys")
blues = brewer.pal(n=6,name = "Blues")
colour.list = list(reds,greys,blues)

par(mfrow =c(3,1),mai=c(0.6,0.9,0.1,0.9))
for(h in 1:length(PopGrowthMeanSurvivalTrials)){
  for(i in 1:length(PopGrowthMeanSurvivalTrials[[h]])){
    if(i == 1){
      par(xpd = FALSE)
      plot(PopGrowthMeanSurvivalTrials[[h]][[i]]$PopulationSize~PopGrowthMeanSurvivalTrials[[h]][[i]]$Year, type = "l", ylim = c(0,100), xlim = c(8,45),xaxs = "i",yaxs = "i",axes = FALSE,xlab = "",ylab = "",lwd = 3,col = colour.list[[h]][i])
 
      }else{
        lines(PopGrowthMeanSurvivalTrials[[h]][[i]]$PopulationSize~PopGrowthMeanSurvivalTrials[[h]][[i]]$Year,col = colour.list[[h]][[i]],lwd=3)
        
      }
    
  }
  
  par(xpd = NA)
  rect(8.05,40,10,60,col="white",border = NA)
  rect(40,-10,46,100,col="white",border = NA)
  axis(1,at = seq(10,40, by = 10),labels = c(0,10,20,30))
  axis(2)
  legend(41,80,legend = c("91%","92%","93%","94%","95%","96%"),fill = colour.list[[h]])
}
text(3,190,"Mean population size",srt=90,cex=1.3)
text(25,-20,"Years",cex=1.3)
text(10,372,"a)",cex=1.4)
text(10,235,"b)",cex=1.4)
text(10,100,"c)",cex=1.5)
dev.off()




tiff("C:/Users/steph/Desktop/PhD/Writing/Chapter 6 - Predicting recovery through agent-based models/Figures/Fecundity trial.tiff",res = 300, units = "mm",width = 160, height=200)

reds =  brewer.pal(n=3,name = "Reds")
greys = brewer.pal(n=3,name = "Greys")
blues = brewer.pal(n=3,name = "Blues")
colour.list = list(reds,greys,blues)

par(mfrow =c(3,1),mai=c(0.6,0.9,0.1,0.9))
for(h in 1:length(PopGrowthMeanFecundTrials)){
  for(i in 1:length(PopGrowthMeanFecundTrials[[h]])){
    if(i == 1){
      par(xpd = FALSE)
      plot(PopGrowthMeanFecundTrials[[h]][[i]]$PopulationSize~PopGrowthMeanFecundTrials[[h]][[i]]$Year, type = "l", ylim = c(0,200), xlim = c(8,45),xaxs = "i",yaxs = "i",axes = FALSE,xlab = "",ylab = "",lwd = 3,col = colour.list[[h]][i])
      
    }else{
      lines(PopGrowthMeanFecundTrials[[h]][[i]]$PopulationSize~PopGrowthMeanFecundTrials[[h]][[i]]$Year,col = colour.list[[h]][[i]],lwd=3)
      
    }
    
  }
  
  par(xpd = NA)
  rect(8.05,40,10,80,col="white",border = NA)
  rect(40,-10,46,230,col="white",border = NA)
  axis(1,at = seq(10,40, by = 10),labels = c(0,10,20,30))
  axis(2)
  legend(41,140,legend = c("60%","70%","80%"),fill = colour.list[[h]])
}
text(3,380,"Mean population size",srt=90,cex=1.3)
text(25,-40,"Years",cex=1.3)
text(10,745,"a)",cex=1.4)
text(10,475,"b)",cex=1.4)
text(10,200,"c)",cex=1.5)

dev.off()





tiff("C:/Users/steph/Desktop/PhD/Writing/Chapter 6 - Predicting recovery through agent-based models/Figures/SurvivalFecundCombined trial.tiff",res = 300, units = "mm",width = 160, height=200)

reds =  brewer.pal(n=6,name = "Reds")
greys = brewer.pal(n=6,name = "Greys")
blues = brewer.pal(n=6,name = "Blues")
colour.list = list(reds,greys,blues)

par(mfrow =c(3,2),mai=c(0.6,0.6,0.2,0))
for(h in 1:length(PopGrowthMeanSurvivalTrials)){
  for(i in 1:length(PopGrowthMeanSurvivalTrials[[h]])){
    if(i == 1){
      par(xpd = FALSE)
      plot(PopGrowthMeanSurvivalTrials[[h]][[i]]$PopulationSize~PopGrowthMeanSurvivalTrials[[h]][[i]]$Year, type = "l", ylim = c(0,200), xlim = c(8,45),xaxs = "i",yaxs = "i",axes = FALSE,xlab = "",ylab = "",lwd = 3,col = colour.list[[h]][i])
      
    }else{
      lines(PopGrowthMeanSurvivalTrials[[h]][[i]]$PopulationSize~PopGrowthMeanSurvivalTrials[[h]][[i]]$Year,col = colour.list[[h]][[i]],lwd=3)
      
    }
    
  }
  
  par(xpd = NA)
  rect(8.05,40,10,60,col="white",border = NA)
  rect(40,-10,46,100,col="white",border = NA)
  axis(1,at = seq(10,40, by = 10),labels = c(0,10,20,30))
  axis(2)
  legend(12,180,legend = c("91%","92%","93%","94%","95%","96%"),fill = colour.list[[h]])


  reds =  brewer.pal(n=3,name = "Reds")
  greys = brewer.pal(n=3,name = "Greys")
  blues = brewer.pal(n=3,name = "Blues")
  colour.list2 = list(reds,greys,blues)

  for(j in 1:length(PopGrowthMeanFecundTrials[[h]])){
    if(j == 1){
      par(xpd = FALSE)
      plot(PopGrowthMeanFecundTrials[[h]][[j]]$PopulationSize~PopGrowthMeanFecundTrials[[h]][[j]]$Year, type = "l", ylim = c(0,200), xlim = c(8,45),xaxs = "i",yaxs = "i",axes = FALSE,xlab = "",ylab = "",lwd = 3,col = colour.list2[[h]][j])
      
    }else{
      lines(PopGrowthMeanFecundTrials[[h]][[j]]$PopulationSize~PopGrowthMeanFecundTrials[[h]][[j]]$Year,col = colour.list2[[h]][[j]],lwd=3)
      
    }
    
  }
  par(xpd = NA)
  rect(8.05,40,10,80,col="white",border = NA)
  rect(40,-10,46,230,col="white",border = NA)
  axis(1,at = seq(10,40, by = 10),labels = c(0,10,20,30))
  axis(2)
  legend(12,180,legend = c("60%","70%","80%"),fill = colour.list2[[h]])
}

text(-44,350,"Mean population \nsize",srt=90,cex=1.2)
text(1,-40,"Years",cex=1.3)
text(-35,785,"a)",cex=1.3)
text(11,785,"d)",cex=1.3)
text(-35,495,"b)",cex=1.3)
text(11,495,"e)",cex=1.3)
text(-35,205,"c)",cex=1.3)
text(11,205,"f)",cex=1.3)

dev.off()

tiff("C:/Users/steph/Desktop/PhD/Writing/Chapter 6 - Predicting recovery through agent-based models/Figures/Figure5.4_SurvFecCP.tiff",res = 300, units = "mm",width = 200, height=100)

greys = brewer.pal(n=6,name = "Greys")


par(mfrow =c(1,2),mai=c(0.6,0.9,0.2,0))
for(h in 2:2){
  for(i in 1:length(PopGrowthMeanSurvivalTrials[[h]])){
    if(i == 1){
      par(xpd = FALSE)
      plot(PopGrowthMeanSurvivalTrials[[2]][[i]]$PopulationSize~PopGrowthMeanSurvivalTrials[[2]][[i]]$Year, type = "l", ylim = c(0,200), xlim = c(8,45),xaxs = "i",yaxs = "i",axes = FALSE,xlab = "",ylab = "",lwd = 3,col = greys[[i]])
      
    }else{
      lines(PopGrowthMeanSurvivalTrials[[2]][[i]]$PopulationSize~PopGrowthMeanSurvivalTrials[[2]][[i]]$Year,col = greys[[i]],lwd=3)
      
    }
    
  }
  
  par(xpd = NA)
  rect(8.05,40,10,60,col="white",border = NA)
  rect(40,-10,46,100,col="white",border = NA)
  axis(1,at = seq(10,40, by = 10),labels = c(0,10,20,30))
  axis(2)
  legend(12,180,legend = c("91%","92%","93%","94%","95%","96%"),fill = greys)
  
  greys2 = brewer.pal(n=3,name = "Greys")

  
  for(j in 1:length(PopGrowthMeanFecundTrials[[h]])){
    if(j == 1){
      par(xpd = FALSE,mai = c(0.6,0.6,0.2,0))
      plot(PopGrowthMeanFecundTrials[[2]][[j]]$PopulationSize~PopGrowthMeanFecundTrials[[2]][[j]]$Year, type = "l", ylim = c(0,200), xlim = c(8,45),xaxs = "i",yaxs = "i",axes = FALSE,xlab = "",ylab = "",lwd = 3,col = greys2[[j]])
      
    }else{
      lines(PopGrowthMeanFecundTrials[[2]][[j]]$PopulationSize~PopGrowthMeanFecundTrials[[2]][[j]]$Year,col = greys2[[j]],lwd=3)
      
    }
    
  }
  par(xpd = NA)
  rect(8.05,40,10,80,col="white",border = NA)
  rect(40,-10,46,230,col="white",border = NA)
  axis(1,at = seq(10,40, by = 10),labels = c(0,10,20,30))
  axis(2)
  legend(12,180,legend = c("60%","70%","80%"),fill = greys2)
}

text(-40,100,"Mean population \nsize",srt=90,cex=1.2)
text(2,-30,"Years",cex=1.3)
text(-29,205,"a)",cex=1.3)
text(11,205,"b)",cex=1.3)

dev.off()

rm(blues,greys,reds,colour.list,colour.list2)




##Growth rate-----


for(i in 1:length(PopGrowthAll)){
  for(j in 1:length(PopGrowthAll[[i]])){
    for(k in 2:nrow(PopGrowthAll[[i]][[j]])){
      PopGrowthAll[[i]][[j]][k,"PopGrowthRate"] = (PopGrowthAll[[i]][[j]][k,"PopulationSize"] - PopGrowthAll[[i]][[j]][k-1,"PopulationSize"])/PopGrowthAll[[i]][[j]][k-1,"PopulationSize"]
      
    }
  }
  
}

Demographics = data.frame(SurvivalTrial = as.character(),
                          Protection = as.character(),
                          MeanGrowthRate = as.numeric())


Protection = c("NoProtect","CurrentProtect","100mProtect")

meangrowthrate = NULL
iteration = 0

for(i in 1:length(PopGrowthAll)){
  for(j in 1:length(PopGrowthAll[[i]])){
    iteration = iteration +1
    Demographics[iteration,"SurvivalTrial"] = paste0(PopGrowthAll[[i]][[j]][1,"SurvivalTrial"])
    Demographics[iteration,"Protection"] = Protection[i]
    Demographics[iteration,"MeanGrowthRate"] = mean(PopGrowthAll[[i]][[j]][10:nrow(PopGrowthAll[[i]][[j]]),"PopGrowthRate"],na.rm=TRUE)
    Demographics[iteration,"SDGrowthRate"] = sd(PopGrowthAll[[i]][[j]][10:nrow(PopGrowthAll[[i]][[j]]),"PopGrowthRate"],na.rm=TRUE)
  }
}

rm(Protection,iteration)

##Survival and fecundity -----
###Setting up Capture histories ---------
Order.Trials = c("Surv95_Fec08","Surv95_Fec07","Surv95_Fec06","Surv94_Fec06","Surv93_Fec06","Surv92_Fec06","Surv91_Fec06","Surv96_Fec06")

#Dunedin No Protection
IndTrial_CH = NULL
Dun_NP_CH = NULL

for(i in 1:length(all_NP_PopHist)){
  for(j in 1:length(all_NP_PopHist[[i]])){
    for(k in 2:length(all_NP_PopHist[[i]][[j]])){
      if(k == 2){
        AllInds = all_NP_PopHist[[i]][[j]][[k]][,c("IndividualID","Sex","Age","Mature","CurrentCalf")]
        AllInds$Year = k
        AllInds$SurvivalTrial = Order.Trials[[i]]
      }else{
        temp <- all_NP_PopHist[[i]][[j]][[k]][,c("IndividualID","Sex","Age","Mature","CurrentCalf")]
        temp$Year = k
        temp$SurvivalTrial =  Order.Trials[[i]]
        AllInds = rbind(AllInds,temp)
      }
    }
    AllInds$CaptureState = NA
    
    for(l in 1:nrow(AllInds)){
      if(AllInds[l,"Sex"] == "F" & AllInds[l,"Mature"] == 1 & is.na(AllInds[l,"CurrentCalf"])==FALSE){
        if(AllInds[which(AllInds[l,"CurrentCalf"]==AllInds[,"IndividualID"] & AllInds[l,"Year"]==AllInds[,"Year"]),"Age"] < 1){
          AllInds[l,"CaptureState"] = "B"
        }else{
          AllInds[l,"CaptureState"] = "NB"
        }
      }else if(AllInds[l,"Sex"] == "M" & AllInds[l,"Mature"] == 1){
        AllInds[l,"CaptureState"] = "A"
      }else if(AllInds[l,"Sex"] == "F" & AllInds[l,"Mature"] == 1){
        AllInds[l,"CaptureState"] = "NB"
      }else if(AllInds[l,"Age"] > 3){
        AllInds[l,"CaptureState"] = "SA"
      }else if(AllInds[l,"Age"] >= 1){
        AllInds[l,"CaptureState"] = "J"
      }else if(AllInds[l,"Age"] < 1){
        AllInds[l,"CaptureState"] = "C"
      }
      AllInds[l,"MaxAge"] = max(AllInds[which(AllInds[l,"IndividualID"]==AllInds[,"IndividualID"]),"Age"])
    }

    IndTrial_CH[[j]] = reshape(AllInds, direction = "wide", timevar = "Year", idvar = c("IndividualID","Sex","MaxAge","SurvivalTrial"), drop = c("Age","Mature","CurrentCalf"))
  }
  Dun_NP_CH[[i]] = IndTrial_CH
  
}


#Dunedin Current Protection
Dun_CP_CH = NULL
IndTrial_CH = NULL

# i = survival trials =8
# j = replicates =10
for(i in 1:length(all_CP_PopHist)){
  for(j in 1:length(all_CP_PopHist[[i]])){
    for(k in 2:length(all_CP_PopHist[[i]][[j]])){
      if(k == 2){
        AllInds = all_CP_PopHist[[i]][[j]][[k]][,c("IndividualID","Sex","Age","Mature","CurrentCalf")]
        AllInds$Year = k
        AllInds$SurvivalTrial = Order.Trials[[i]]
      }else{
        temp <- all_CP_PopHist[[i]][[j]][[k]][,c("IndividualID","Sex","Age","Mature","CurrentCalf")]
        temp$Year = k
        temp$SurvivalTrial =  Order.Trials[[i]]
        AllInds = rbind(AllInds,temp)
      }
    }
    AllInds$CaptureState = NA
    
    for(l in 1:nrow(AllInds)){
      if(AllInds[l,"Sex"] == "F" & AllInds[l,"Mature"] == 1 & is.na(AllInds[l,"CurrentCalf"])==FALSE){
        if(AllInds[which(AllInds[l,"CurrentCalf"]==AllInds[,"IndividualID"] & AllInds[l,"Year"]==AllInds[,"Year"]),"Age"] < 1){
          AllInds[l,"CaptureState"] = "B"
        }else{
          AllInds[l,"CaptureState"] = "NB"
        }
      }else if(AllInds[l,"Sex"] == "M" & AllInds[l,"Mature"] == 1){
        AllInds[l,"CaptureState"] = "A"
      }else if(AllInds[l,"Sex"] == "F" & AllInds[l,"Mature"] == 1){
        AllInds[l,"CaptureState"] = "NB"
      }else if(AllInds[l,"Age"] > 3){
        AllInds[l,"CaptureState"] = "SA"
      }else if(AllInds[l,"Age"] >= 1){
        AllInds[l,"CaptureState"] = "J"
      }else if(AllInds[l,"Age"] < 1){
        AllInds[l,"CaptureState"] = "C"
      }
      AllInds[l,"MaxAge"] = max(AllInds[which(AllInds[l,"IndividualID"]==AllInds[,"IndividualID"]),"Age"])
    }
    
    IndTrial_CH[[j]] = reshape(AllInds, direction = "wide", timevar = "Year", idvar = c("IndividualID","Sex","MaxAge","SurvivalTrial"), drop = c("Age","Mature","CurrentCalf"))
  }
  Dun_CP_CH[[i]] = IndTrial_CH
  
}

#Dunedin 100m Protection
Dun_100P_CH = NULL
IndTrial_CH = NULL

# i = survival trials =8
# j = replicates =10
for(i in 1:length(all_100P_PopHist)){
  for(j in 1:length(all_100P_PopHist[[i]])){
    for(k in 2:length(all_100P_PopHist[[i]][[j]])){
      if(k == 2){
        AllInds = all_100P_PopHist[[i]][[j]][[k]][,c("IndividualID","Sex","Age","Mature","CurrentCalf")]
        AllInds$Year = k
        AllInds$SurvivalTrial = Order.Trials[[i]]
      }else{
        temp <- all_100P_PopHist[[i]][[j]][[k]][,c("IndividualID","Sex","Age","Mature","CurrentCalf")]
        temp$Year = k
        temp$SurvivalTrial =  Order.Trials[[i]]
        AllInds = rbind(AllInds,temp)
      }
    }
    AllInds$CaptureState = NA
    
    for(l in 1:nrow(AllInds)){
      if(AllInds[l,"Sex"] == "F" & AllInds[l,"Mature"] == 1 & is.na(AllInds[l,"CurrentCalf"])==FALSE){
        if(AllInds[which(AllInds[l,"CurrentCalf"]==AllInds[,"IndividualID"] & AllInds[l,"Year"]==AllInds[,"Year"]),"Age"] < 1){
          AllInds[l,"CaptureState"] = "B"
        }else{
          AllInds[l,"CaptureState"] = "NB"
        }
      }else if(AllInds[l,"Sex"] == "M" & AllInds[l,"Mature"] == 1){
        AllInds[l,"CaptureState"] = "A"
      }else if(AllInds[l,"Sex"] == "F" & AllInds[l,"Mature"] == 1){
        AllInds[l,"CaptureState"] = "NB"
      }else if(AllInds[l,"Age"] > 3){
        AllInds[l,"CaptureState"] = "SA"
      }else if(AllInds[l,"Age"] >= 1){
        AllInds[l,"CaptureState"] = "J"
      }else if(AllInds[l,"Age"] < 1){
        AllInds[l,"CaptureState"] = "C"
      }
      AllInds[l,"MaxAge"] = max(AllInds[which(AllInds[l,"IndividualID"]==AllInds[,"IndividualID"]),"Age"])
    }
    
    IndTrial_CH[[j]] = reshape(AllInds, direction = "wide", timevar = "Year", idvar = c("IndividualID","Sex","MaxAge","SurvivalTrial"), drop = c("Age","Mature","CurrentCalf"))
  }
  Dun_100P_CH[[i]] = IndTrial_CH
  
}


All_Dun_CH = list(Dun_NP_CH,Dun_CP_CH,Dun_100P_CH)
rm(IndTrial_CH,temp,AllInds,all_100P_PopHist,all_CP_PopHist,all_NP_PopHist,Dun_CP_CH,Dun_NP_CH,Dun_100P_CH)

###Survival rates age structured CJS models-----

Protection = c("NoProtect","CurrentProtect","100mProtect")
iteration = 0
mean.survival.trial= data.frame(Trial = as.integer(),
                                TotalSurvival = as.numeric(),
                                AdultSurvival = as.numeric())

#h=protect
#i=survivalTrial
#k=nrows
for(h in 1: length(PopSizeAllScenarios)){
  for(i in 1:length(PopSizeAllScenarios[[h]])){
    for(j in 1:length(PopSizeAllScenarios[[h]][[i]])){
      for(k in 2:nrow(PopSizeAllScenarios[[h]][[i]][[j]])){
        PopSizeAllScenarios[[h]][[i]][[j]][k,"TotCalves"] = length(which(All_Dun_CH[[h]][[i]][[j]][,k+3] == "C"))
        PopSizeAllScenarios[[h]][[i]][[j]][k,"TotJuveniles"] = length(which(All_Dun_CH[[h]][[i]][[j]][,k+3] == "J"))
        PopSizeAllScenarios[[h]][[i]][[j]][k,"TotSubAdults"] = length(which(All_Dun_CH[[h]][[i]][[j]][,k+3] == "SA"))
        PopSizeAllScenarios[[h]][[i]][[j]][k,"TotAdults"] = length(which(All_Dun_CH[[h]][[i]][[j]][,k+3] == "A" | All_Dun_CH[[h]][[i]][[j]][,k+3] == "NB" | All_Dun_CH[[h]][[i]][[j]][,k+3] == "B"))
        PopSizeAllScenarios[[h]][[i]][[j]][k,"TotMatFem"] = length(which(All_Dun_CH[[h]][[i]][[j]][,k+3] == "NB" | All_Dun_CH[[h]][[i]][[j]][,k+3] == "B"))
        
        
        for(l in 3:nrow(PopSizeAllScenarios[[h]][[i]][[j]])){
          PopSizeAllScenarios[[h]][[i]][[j]][l,"AdultsSurvivedt-t1"] = length(which(All_Dun_CH[[h]][[i]][[j]][,l+3] == "NB" & All_Dun_CH[[h]][[i]][[j]][,l+2] == "NB"  | All_Dun_CH[[h]][[i]][[j]][,l+3] == "NB" & All_Dun_CH[[h]][[i]][[j]][,l+2] == "B" | All_Dun_CH[[h]][[i]][[j]][,l+3] == "B" & All_Dun_CH[[h]][[i]][[j]][,l+2] == "NB" | All_Dun_CH[[h]][[i]][[j]][,l+3] == "NB" & All_Dun_CH[[h]][[i]][[j]][,l+2] == "B"  | All_Dun_CH[[h]][[i]][[j]][,l+3] == "A" & All_Dun_CH[[h]][[i]][[j]][,l+2] == "A"))
          PopSizeAllScenarios[[h]][[i]][[j]][l,"TotSurvival"] = (PopSizeAllScenarios[[h]][[i]][[j]][l,"PopulationSize"] - PopSizeAllScenarios[[h]][[i]][[j]][l,"TotCalves"]) / PopSizeAllScenarios[[h]][[i]][[j]][l-1,"PopulationSize"] #Total individuals that survived minus the addition of new calves
          PopSizeAllScenarios[[h]][[i]][[j]][l,"AdultSurvival"] = (PopSizeAllScenarios[[h]][[i]][[j]][l,"AdultsSurvivedt-t1"]) / PopSizeAllScenarios[[h]][[i]][[j]][l-1,"TotAdults"] 
          
        }
      }
      
      mean.survival.trial[j,"SurvivalTrial"] = Order.Trials[[i]]
      mean.survival.trial[j,"Protection"] = Protection[[h]]
      mean.survival.trial[j,"TotalSurvival"] = mean(PopSizeAllScenarios[[h]][[i]][[j]][10:nrow(PopSizeAllScenarios[[h]][[i]][[j]]),"TotSurvival"],na.rm=TRUE)
      mean.survival.trial[j,"AdultSurvival"] = mean(PopSizeAllScenarios[[h]][[i]][[j]][10:nrow(PopSizeAllScenarios[[h]][[i]][[j]]),"AdultSurvival"],na.rm=TRUE)
      
    }
    
    row = which(mean.survival.trial$SurvivalTrial == Demographics$SurvivalTrial & mean.survival.trial$Protection == Demographics$Protection)
    Demographics[row,"TotalSurvival"] = mean(mean.survival.trial$TotalSurvival,na.rm=TRUE)
    Demographics[row,"SDTotalSurvival"] = sd(mean.survival.trial$TotalSurvival,na.rm=TRUE)
    Demographics[row,"AdultSurvival"] = mean(mean.survival.trial$AdultSurvival,na.rm=TRUE)
    Demographics[row,"SDAdultSurvival"] = sd(mean.survival.trial$AdultSurvival,na.rm=TRUE)

  }
}





###Fecundity chapter 4 models
Fecundity_trials = NULL
fecundity = NA

for(g in 1:length(All_Dun_CH)){
  for(h in 1:length(All_Dun_CH[[g]])){
    for(i in 1:length(All_Dun_CH[[g]][[h]])){
      female.data = All_Dun_CH[[g]][[h]][[i]]
      female.data = subset(female.data,female.data$Sex == "F")
      
      fec_dataframe = data.frame(IndividualID =as.character(),
                                 SurvivalTrial = as.character(),
                                 Protection = as.character(),
                                 MaxAge = as.numeric(),
                                 Average.ICI = as.numeric(),
                                 No.Calves = as.numeric())
      
      for(j in 1:nrow(female.data)){
        BreedingYears = as.vector(which(female.data[j,]=="B"))
        if(length(BreedingYears) > 1){
          ICI = rep(NA,length(BreedingYears)-1)
          
          for(k in 1:length(ICI)){
            ICI[k] = BreedingYears[k+1]-BreedingYears[k]
          }
        }else{
          ICI = NA
        }
        fec_dataframe[j,"Protection"] = Protection[[g]]
        fec_dataframe[j,"SurvivalTrial"] = Order.Trials[[h]]
        fec_dataframe[j,"IndividualID"] = female.data[j,"IndividualID"] 
        fec_dataframe[j,"MaxAge"] = female.data[j,"MaxAge"]
        fec_dataframe[j,"Average.ICI"] = mean(ICI,na.rm=TRUE)
        fec_dataframe[j,"No.Calves"] = length(BreedingYears)
    }
    fecundity[i] = mean(fec_dataframe$Average.ICI,na.rm = TRUE)
  }
 
  row = which(unique(fec_dataframe$SurvivalTrial) == Demographics$SurvivalTrial & unique(fec_dataframe$Protection) == Demographics$Protection)
  Demographics[row,"MeanICI"] = mean(fecundity,na.rm=TRUE)
  Demographics[row,"SD_ICI"] = sd(fecundity,na.rm=TRUE)
  Demographics[row,"MeanFecundity"] = 1/mean(fecundity,na.rm=TRUE)
  
  }
}






##Age-----

Age.data.rep = data.frame(SurvivalTrials = as.character(),
                            Protection = as.character(),
                            MeanAll = as.numeric(),
                            MinAll = as.numeric(),
                            MaxAll = as.numeric(),
                            MeanAdult = as.numeric(),
                            MinAdult = as.numeric(),
                            MaxAdult = as.numeric())

for(h in 1:length(All_Dun_CH)){
  for(i in 1:length(All_Dun_CH[[h]])){
    for(j in 1:length(All_Dun_CH[[h]][[i]])){
      Age.data.rep[j,"SurvivalTrials"] = Order.Trials[[i]]
      Age.data.rep[j,"Protection"] = Protection[[h]]
      Age.data.rep[j,"MeanAll"] = mean(All_Dun_CH[[h]][[i]][[j]]$MaxAge)
      Age.data.rep[j,"MinAll"] = min(All_Dun_CH[[h]][[i]][[j]]$MaxAge)
      Age.data.rep[j,"MaxAll"] = max(All_Dun_CH[[h]][[i]][[j]]$MaxAge)
      
      temp = subset(All_Dun_CH[[h]][[i]][[j]],All_Dun_CH[[h]][[i]][[j]]$MaxAge >3)
      Age.data.rep[j,"MeanAdult"] = mean(temp$MaxAge)
      Age.data.rep[j,"MinAdult"] = min(temp$MaxAge)
      Age.data.rep[j,"MaxAdult"] = max(temp$MaxAge)
    }
    row = which(unique(Age.data.rep$SurvivalTrials) == Demographics$SurvivalTrial & unique(Age.data.rep$Protection) == Demographics$Protection)
    Demographics[row,"MeanAgeAll"] = mean(Age.data.rep[,"MeanAll"])
    Demographics[row,"MinAge3All"]= min(Age.data.rep[,"MinAll"])
    Demographics[row,"MaxAge3All"]= max(Age.data.rep[,"MaxAll"])
    Demographics[row,"MeanAge3plus"]= mean(Age.data.rep[,"MeanAdult"])
    Demographics[row,"MinAge3plus"]= min(Age.data.rep[,"MinAdult"])
    Demographics[row,"MaxAge3plus"]= max(Age.data.rep[,"MaxAdult"])
  }
}

rm(temp,fec_dataframe,Fecundity_trials,mean.survival.trial,data,Age.data.trial,BreedingYears,fecundity,female.data,g,h,i,j,k,l,Protection)

write.csv(Demographics,"C:/Users/steph/Desktop/PhD/Writing/Chapter 6 - Predicting recovery through agent-based models/Figures/Simulation2_Demographic.csv",row.names = FALSE)



#Movements -----

load("all_NP_MoveHist.rda")
load("all_CP_MoveHist.rda")
load("all_100P_MoveHist.rda")

all_CP_MoveHist[[3]] = all_CP_MoveHist[[3]][-10]
all_100P_MoveHist[[1]] = all_100P_MoveHist[[1]][-10]

Dun_initial <- shapefile("C:/Users/steph/Desktop/PhD/Research/IndividualBasedModelling/2. Scenario 2. Dun demographics/Landscape features/Dun_initial.shp")

all_MoveHist = list(all_NP_MoveHist,all_CP_MoveHist,all_100P_MoveHist)
rm(all_NP_MoveHist,all_CP_MoveHist,all_100P_MoveHist)
#Structure = protection, trials, replicates, days


for(h in 1:length(all_MoveHist)){
  for(i in 1:length(all_MoveHist[[h]])){
    for(j in 1:length(all_MoveHist[[h]][[i]])){
      for(k in 1:length(all_MoveHist[[h]][[i]][[j]])){
        
        
      }
    }
    
  }
}



#Emergent Home Range ----
#Transform list into individual moves.

list.all.inds = c(1:54)

Locs_by_ind = NULL

for(i in 1:length(list.all.inds[[1]])){
  for(j in 1:length(Dun_CP_Y100_Pop50_movement[[1]])){
    ind.x <- subset(Dun_CP_Y100_Pop50_movement[[1]][[j]], Dun_CP_Y100_Pop50_movement[[1]][[j]]$X == list.all.inds[[1]][i])
    if(nrow(ind.x) > 0 & j == 1){
      ind.x$time.step = j
      Locs_by_ind[[i]] = ind.x
    }else{
      if(nrow(ind.x) > 0 & j > 1){
        ind.x$time.step = j
        Locs_by_ind[[i]] = rbind(Locs_by_ind[[i]],ind.x)
      }else{
        if(nrow(ind.x) == 0 & j == 1){
          ind.x[1,1:5] = NA
          ind.x$time.step = j
          Locs_by_ind[[i]] = ind.x
        }else{
          ind.x[1,1:5] = NA
          ind.x$time.step = j
          Locs_by_ind[[i]] = rbind(Locs_by_ind[[i]],ind.x)
        }
      }
    }
  }
}










ind_locs<-NULL
for(j in 1:50){
  for(i in 1:365){
    ts <- i
    x_loc <- inds_move_hist[[i]][j,2]
    y_loc <- inds_move_hist[[i]][j,3]
    ts.depth <- inds_move_hist[[i]][j,4]
    
    ind.x <- cbind(ts,x_loc,y_loc,ts.depth) 
    if(i > 1){
      ind_locs[[j]] <- rbind(ind_locs[[j]],ind.x)
    }else{
      ind_locs[[j]]<-ind.x
    }
  }
}


movement.features <- data.frame(Individual = as.integer(),
                                max.distance = as.numeric(),
                                mean.distance = as.numeric(),
                                max.depth = as.numeric(),
                                min.depth = as.numeric(),
                                mean.depth = as.numeric())



for(i in 1:length(ind_locs)){
  dist_matrix <- pointDistance(p1 = cbind(ind_locs[[i]][,2],ind_locs[[i]][,3]), p2 = cbind(ind_locs[[i]][,2],ind_locs[[i]][,3]), lonlat = TRUE, allpairs = TRUE)
  max.dist <- max(dist_matrix,na.rm = TRUE)*0.001
  mean.dist <- mean(dist_matrix,na.rm = TRUE)*0.001
  max.depth<-min(ind_locs[[i]][,4],na.rm=TRUE)
  min.depth<-max(ind_locs[[i]][,4],na.rm=TRUE)
  mean.depth<-mean(ind_locs[[i]][,4],na.rm=TRUE)
  
  movement.features[i,]<-cbind(i,max.dist,mean.dist,max.depth,min.depth,mean.depth) 
}




#emergent_HR<-NULL ##Ideally I would calculate a individual home range for each data point to test and see if my emergent home ranges are approximately 50km alongshore...

plot(Otago_coast, col = "gray")

for(i in 1:20){
  points(ind_locs[[i]][,2],ind_locs[[i]][,3],col=cols[i],pch=20)
}






#Figures --------

library(raster)
library(terra)

