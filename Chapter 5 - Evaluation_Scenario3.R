#==================================
#Set up workspace and load data----
#==================================
#Setup environment
rm(list = ls(all=TRUE))
library(RColorBrewer)
setwd("C:/Users/steph/Desktop/PhD/Research/IndividualBasedModelling/3. Scenario 3. Final Model Runs/Model outputs")

file_names=as.list(dir())
lapply(file_names,load,.GlobalEnv)

#pop = grep("PopHist",names(.GlobalEnv),value=TRUE);pop_np = grep("NP",pop,value=TRUE);pop_cp = grep("CP",pop,value=TRUE);pop_100p = grep("100P",pop,value=TRUE)
#tim_pop_np = grep("Tim",pop_np,value=TRUE);tim_pop_cp = grep("Tim",pop_cp,value=TRUE);tim_pop_100p = grep("Tim",pop_100p,value=TRUE);dun_pop_np = grep("Dun",pop_np,value=TRUE);dun_pop_cp = grep("Dun",pop_cp,value=TRUE);dun_pop_100p = grep("Dun",pop_100p,value=TRUE);bp_pop_np = grep("BP",pop_np,value=TRUE);bp_pop_cp = grep("BP",pop_cp,value=TRUE);bp_pop_100p = grep("BP",pop_100p,value=TRUE)
#Tim_Pop_NP<-do.call("list",mget(tim_pop_np));Tim_Pop_CP<-do.call("list",mget(tim_pop_cp));Tim_Pop_100P<-do.call("list",mget(tim_pop_100p));Dun_Pop_NP<-do.call("list",mget(dun_pop_np));Dun_Pop_CP<-do.call("list",mget(dun_pop_cp));Dun_Pop_100P<-do.call("list",mget(dun_pop_100p));BP_Pop_NP<-do.call("list",mget(bp_pop_np));BP_Pop_CP<-do.call("list",mget(bp_pop_cp));BP_Pop_100P<-do.call("list",mget(bp_pop_100p))

#rm(list=setdiff(ls(), c("Tim_Pop_NP","Tim_Pop_CP","Tim_Pop_100P","BP_Pop_NP","BP_Pop_CP","BP_Pop_100P","Dun_Pop_NP","Dun_Pop_CP","Dun_Pop_100P")))

#Dun_Pop_All = list(Dun_Pop_NP,Dun_Pop_CP,Dun_Pop_100P)
#Tim_Pop_All = list(Tim_Pop_NP,Tim_Pop_CP,Tim_Pop_100P)
#BP_Pop_All = list(BP_Pop_NP,BP_Pop_CP,BP_Pop_100P)

#rm(list=setdiff(ls(), c("Dun_Pop_All","Tim_Pop_All","BP_Pop_All","Dun_Move_All","Tim_Move_All","BP_Move_All")))

#save(BP_Pop_All,file="./BP_Pop_All.rda")
#save(Tim_Pop_All,file="./Tim_Pop_All.rda")
#save(Dun_Pop_All,file="./Dun_Pop_All.rda")



#===============
#Evaluation-----
#===============

##Age distribution emergent property-----


randnumber = round(runif(1,0,length(BP_Pop_All[[2]])))

BP_Pop1 = as.data.frame(BP_Pop_All[[2]][[randnumber]][[1]]$Age)
BP_Pop1$ageclass = ifelse(BP_Pop1[,1] < 5, "<05",ifelse(BP_Pop1[,1] < 10, "<10",ifelse(BP_Pop1[,1] < 15,"<15",ifelse(BP_Pop1[,1] < 20,"<20",ifelse(BP_Pop1[,1] < 25,"<25",ifelse(BP_Pop1[,1] < 30, "<30",ifelse(BP_Pop1[,1] < 35,"<35",ifelse(BP_Pop1[,1] < 40,"<40",">40"))))))))
BP_Pop2 = as.data.frame(BP_Pop_All[[2]][[randnumber]][[10]]$Age)
BP_Pop2$ageclass = ifelse(BP_Pop2[,1] < 5, "<05",ifelse(BP_Pop2[,1] < 10, "<10",ifelse(BP_Pop2[,1] < 15,"<15",ifelse(BP_Pop2[,1] < 20,"<20",ifelse(BP_Pop2[,1] < 25,"<25",ifelse(BP_Pop2[,1] < 30, "<30",ifelse(BP_Pop2[,1] < 35,"<35",ifelse(BP_Pop2[,1] < 40,"<40",">40"))))))))
BP_Pop3 = as.data.frame(BP_Pop_All[[2]][[randnumber]][[20]]$Age)
BP_Pop3$ageclass = ifelse(BP_Pop3[,1] < 5, "<05",ifelse(BP_Pop3[,1] < 10, "<10",ifelse(BP_Pop3[,1] < 15,"<15",ifelse(BP_Pop3[,1] < 20,"<20",ifelse(BP_Pop3[,1] < 25,"<25",ifelse(BP_Pop3[,1] < 30, "<30",ifelse(BP_Pop3[,1] < 35,"<35",ifelse(BP_Pop3[,1] < 40,"<40",">40"))))))))
BP_Pop4 = as.data.frame(BP_Pop_All[[2]][[randnumber]][[30]]$Age)
BP_Pop4$ageclass = ifelse(BP_Pop4[,1] < 5, "<05",ifelse(BP_Pop4[,1] < 10, "<10",ifelse(BP_Pop4[,1] < 15,"<15",ifelse(BP_Pop4[,1] < 20,"<20",ifelse(BP_Pop4[,1] < 25,"<25",ifelse(BP_Pop4[,1] < 30, "<30",ifelse(BP_Pop4[,1] < 35,"<35",ifelse(BP_Pop4[,1] < 40,"<40",">40"))))))))
BP_Pop5 = as.data.frame(BP_Pop_All[[2]][[randnumber]][[40]]$Age)
BP_Pop5$ageclass = ifelse(BP_Pop5[,1] < 5, "<05",ifelse(BP_Pop5[,1] < 10, "<10",ifelse(BP_Pop5[,1] < 15,"<15",ifelse(BP_Pop5[,1] < 20,"<20",ifelse(BP_Pop5[,1] < 25,"<25",ifelse(BP_Pop5[,1] < 30, "<30",ifelse(BP_Pop5[,1] < 35,"<35",ifelse(BP_Pop5[,1] < 40,"<40",">40"))))))))
BP_Pop6 = as.data.frame(BP_Pop_All[[2]][[randnumber]][[48]]$Age)
BP_Pop6$ageclass = ifelse(BP_Pop6[,1] < 5, "<05",ifelse(BP_Pop6[,1] < 10, "<10",ifelse(BP_Pop6[,1] < 15,"<15",ifelse(BP_Pop6[,1] < 20,"<20",ifelse(BP_Pop6[,1] < 25,"<25",ifelse(BP_Pop6[,1] < 30, "<30",ifelse(BP_Pop6[,1] < 35,"<35",ifelse(BP_Pop6[,1] < 40,"<40",">40"))))))))


p1 = aggregate(BP_Pop1[,1]~ageclass,data=BP_Pop1,length)
p2 = aggregate(BP_Pop2[,1]~ageclass,data=BP_Pop2,length)
p3 = aggregate(BP_Pop3[,1]~ageclass,data=BP_Pop3,length)
p4 = aggregate(BP_Pop4[,1]~ageclass,data=BP_Pop4,length)
p5 = aggregate(BP_Pop5[,1]~ageclass,data=BP_Pop5,length)
p6 = aggregate(BP_Pop6[,1]~ageclass,data=BP_Pop6,length)

BP_Age_dist = merge(p1,p2,all.x = TRUE,all.y=TRUE)
BP_Age_dist = merge(BP_Age_dist,p3,all.x = TRUE,all.y=TRUE)
BP_Age_dist = merge(BP_Age_dist,p4,all.x = TRUE,all.y=TRUE)
BP_Age_dist = merge(BP_Age_dist,p5,all.x = TRUE,all.y=TRUE)
BP_Age_dist = merge(BP_Age_dist,p6,all.x = TRUE,all.y=TRUE)

BP_Age_dist$InitialStand = BP_Age_dist$`BP_Pop1[, 1]`/sum(BP_Age_dist$`BP_Pop1[, 1]`,na.rm = TRUE)
BP_Age_dist$PostBI = BP_Age_dist$`BP_Pop2[, 1]`/sum(BP_Age_dist$`BP_Pop2[, 1]`,na.rm = TRUE)
BP_Age_dist$Final = BP_Age_dist$`BP_Pop6[, 1]`/sum(BP_Age_dist$`BP_Pop6[, 1]`,na.rm = TRUE)



barplot(as.matrix(t(BP_Age_dist[,c(2,3,7)])),beside = TRUE, ylab = "Number of individuals",xlab = "Age class (years old)",ylim = c(0,60))
axis(1,labels = BP_Age_dist$ageclass, at = seq(2.5,38,by=4.01),pos=0)
legend(x = 27,y=29,legend = c("Initial population","Post burn-in","Final population"), fill = c("gray35","gray70","gray90"))
abline(h=0)

tiff("C:/Users/steph/Desktop/PhD/Writing/Chapter 6 - Predicting recovery through agent-based models/Figures/Area based Age distribution.tiff",res = 300, units = "mm",width = 160, height=150)

barplot(as.matrix(t(BP_Age_dist[,c(8,9,10)])),beside = TRUE, ylab = "Proportion of individuals",xlab = "Age class (years old)",ylim = c(0,0.5))
axis(1,labels = BP_Age_dist$ageclass, at = seq(2.5,38,by=4.01),pos=0)
legend(x = 24,y=0.48,legend = c("Initial population","Post burn-in","Final population"), fill = c("gray35","gray70","gray90"))
abline(h=0)

dev.off()

rm(BP_Pop1,BP_Pop2,BP_Pop3,BP_Pop4,BP_Pop5,BP_Pop6,p1,p2,p3,p4,p5,p6,BP_Age_dist,randnumber)


##Growth rate----
##Setup annual population sizes and growth rates
#Structure=Protections (NP,CP,100P), Replicates, Days
PopSizeDataFrameAll_BP = NULL
BP.PopSize<-NULL
PopSizeDataFrameAll_Tim = NULL
Tim.PopSize<-NULL
PopSizeDataFrameAll_Dun = NULL
Dun.PopSize<-NULL

##Population trajectories in Banks Peninsula
for(h in 1:length(BP_Pop_All)){
  BP.PopSize <- NULL
  for(i in 1:length(BP_Pop_All[[h]])){
    annual_pop_size<-data.frame(Year = as.integer(),
                                PopulationSize = as.integer())
    
    for(j in 1:length(BP_Pop_All[[h]][[i]])){
      annual_pop_size[j,1] = j
      annual_pop_size[j,2] = nrow(BP_Pop_All[[h]][[i]][[j]])
    }
    BP.PopSize[[i]] <- annual_pop_size
  }
  PopSizeDataFrameAll_BP[[h]] = BP.PopSize
}



##Population trajectories in Timaru
for(h in 1:length(Tim_Pop_All)){
  Tim.PopSize <- NULL
  for(i in 1:length(Tim_Pop_All[[h]])){
    annual_pop_size<-data.frame(Year = as.integer(),
                                PopulationSize = as.integer())
    
    for(j in 1:length(Tim_Pop_All[[h]][[i]])){
      annual_pop_size[j,1] = j
      annual_pop_size[j,2] = nrow(Tim_Pop_All[[h]][[i]][[j]])
    }
    Tim.PopSize[[i]] <- annual_pop_size
  }
  PopSizeDataFrameAll_Tim[[h]] = Tim.PopSize
}


##Population trajectories in Dunedin
for(h in 1:length(Dun_Pop_All)){
  Dun.PopSize <- NULL
  for(i in 1:length(Dun_Pop_All[[h]])){
    annual_pop_size<-data.frame(Year = as.integer(),
                                PopulationSize = as.integer())
    
    for(j in 1:length(Dun_Pop_All[[h]][[i]])){
      annual_pop_size[j,1] = j
      annual_pop_size[j,2] = nrow(Dun_Pop_All[[h]][[i]][[j]])
    }
    Dun.PopSize[[i]] <- annual_pop_size
  }
  PopSizeDataFrameAll_Dun[[h]] = Dun.PopSize
}



PopSizeAllScenarios_All <- list(PopSizeDataFrameAll_BP,PopSizeDataFrameAll_Tim,PopSizeDataFrameAll_Dun)
rm(Tim.PopSize,Dun.PopSize,BP.PopSize,annual_pop_size,PopSizeDataFrameAll_BP,PopSizeDataFrameAll_Tim,PopSizeDataFrameAll_Dun,h,i,j)





PopulationGrowthMeans = NULL
Site = c("BanksPeninsula","Timaru","Dunedin")
Protection = c("NoProtection","CurrentProtection","100mProtection")
iteration = 0
#Site -> Protection -> Reps -> Days?

for(h in 1:length(PopSizeAllScenarios_All)){
  PopSizeDataframe =data.frame(Site = as.character(),
                               Protection = as.character(),
                               Trial = as.integer(),
                               Year = as.integer(),
                               PopulationSize = as.integer())
  for(i in 1:length(PopSizeAllScenarios_All[[h]])){
    for(j in 1:length(PopSizeAllScenarios_All[[h]][[i]])){
      if(i==1 & j == 1){
        data = as.data.frame(cbind(rep(Site[[h]],nrow(PopSizeAllScenarios_All[[h]][[i]][[j]])),rep(Protection[[i]],nrow(PopSizeAllScenarios_All[[h]][[i]][[j]])),rep(j,nrow(PopSizeAllScenarios_All[[h]][[i]][[j]])),PopSizeAllScenarios_All[[h]][[i]][[j]]$Year,PopSizeAllScenarios_All[[h]][[i]][[j]]$PopulationSize))
        names(data) = names(PopSizeDataframe)
        PopSizeDataframe <- data
      }else{
        data = as.data.frame(cbind(rep(Site[[h]],nrow(PopSizeAllScenarios_All[[h]][[i]][[j]])),rep(Protection[[i]],nrow(PopSizeAllScenarios_All[[h]][[i]][[j]])),rep(j,nrow(PopSizeAllScenarios_All[[h]][[i]][[j]])),PopSizeAllScenarios_All[[h]][[i]][[j]]$Year,PopSizeAllScenarios_All[[h]][[i]][[j]]$PopulationSize))
        names(data) = names(PopSizeDataframe)
        PopSizeDataframe = rbind(PopSizeDataframe,data)
        
      }
      
    }
  }
  PopulationGrowthMeans[[h]] = PopSizeDataframe
}

PopGrowthAll = NULL

for(h in 1:length(PopulationGrowthMeans)){

  PopulationGrowthMeans[[h]]$Protection = as.factor(PopulationGrowthMeans[[h]]$Protection)
  PopulationGrowthMeans[[h]]$Site = as.factor(PopulationGrowthMeans[[h]]$Site)
  PopulationGrowthMeans[[h]]$Trial = as.factor(PopulationGrowthMeans[[h]]$Trial)
  PopulationGrowthMeans[[h]]$Year = as.integer(PopulationGrowthMeans[[h]]$Year)
  PopulationGrowthMeans[[h]]$PopulationSize = as.integer(PopulationGrowthMeans[[h]]$PopulationSize)
  
  MeanPopSize <- aggregate(PopulationSize ~  Year + Site + Protection, data = PopulationGrowthMeans[[h]], FUN = mean)
  
  mps1<-subset(MeanPopSize, MeanPopSize$Protection == "NoProtection")
  mps2<-subset(MeanPopSize, MeanPopSize$Protection == "CurrentProtection")
  mps3<-subset(MeanPopSize, MeanPopSize$Protection == "100mProtection")

  PopGrowthAll[[h]] = list(mps1,mps2,mps3)
}


rm(mps1,mps2,mps3,data)



reds =  brewer.pal(n=6,name = "Reds")
greys = brewer.pal(n=6,name = "Greys")
blues = brewer.pal(n=6,name = "Blues")
list.colours1<-c( rgb(255, 0, 0, max = 255, alpha = 50), rgb(0, 0, 0, max = 255, alpha = 50),rgb(0, 0, 255, max = 255, alpha = 50))
list.colours2= c(reds[[5]],greys[[6]],blues[[5]])



tiff("C:/Users/steph/Desktop/PhD/Writing/Chapter 6 - Predicting recovery through agent-based models/Figures/Area based Population growth.tiff",res = 300, units = "mm",width = 160, height=200)

par(mfrow = c(3,1),mai = c(0.5,0.7,0.3,0.3))
for(h in 1:length(PopSizeAllScenarios_All)){
  for(i in 1: length(PopSizeAllScenarios_All[[h]])){
    for(j in 1:length(PopSizeAllScenarios_All[[h]][[i]])){
      if(i == 1 & j == 1){
        plot(PopSizeAllScenarios_All[[h]][[i]][[j]]$PopulationSize~PopSizeAllScenarios_All[[h]][[i]][[j]]$Year, type = "l", ylim = c(0,300), xlim = c(8,60),xaxs = "i",yaxs = "i",axes = FALSE,xlab = "",ylab = "",lwd = 1,col = list.colours1[[i]])
      }else{
        lines(PopSizeAllScenarios_All[[h]][[i]][[j]]$PopulationSize~PopSizeAllScenarios_All[[h]][[i]][[j]]$Year,col = list.colours1[[i]])
      }
    }
    
    lines(PopGrowthAll[[h]][[i]]$PopulationSize~PopGrowthAll[[h]][[i]]$Year,lwd=3, col = list.colours2[[i]])
    rect(8.05,60,10,150,col="white",border = NA)
    axis(1,at = seq(10,50, by = 10),labels = c(0,10,20,30,40))
    axis(2)
  }
}
par(xpd= NA)
legend(45,1200, fill = list.colours2,legend = c("No Protection","Current Protection","100m Protection"),cex=1.2)
text(3,600,"Population size",srt=90,cex=1.5)
text(31,-60,"Years",cex=1.5)
text(10,1180,"a)",cex=1.4)
text(10,750,"b)",cex=1.4)
text(10,320,"c)",cex=1.5)
dev.off()



rm(blues,greys,reds,list.colours1,list.colours2,Site,iteration,MeanPopSize,PopSizeDataframe,Protection)




##Demographic parameters set up - growth rate-----


for(i in 1:length(PopGrowthAll)){
  for(j in 1:length(PopGrowthAll[[i]])){
    for(k in 2:nrow(PopGrowthAll[[i]][[j]])){
      PopGrowthAll[[i]][[j]][k,"PopGrowthRate"] = (PopGrowthAll[[i]][[j]][k,"PopulationSize"] - PopGrowthAll[[i]][[j]][k-1,"PopulationSize"])/PopGrowthAll[[i]][[j]][k-1,"PopulationSize"]
      
    }
  }
  
}

Demographics = data.frame(Site = as.character(),
                          Protection = as.character(),
                          MeanGrowthRate = as.numeric(),
                          SDGrowthRate = as.numeric())




meangrowthrate = NULL
iteration = 0

for(i in 1:length(PopGrowthAll)){
  for(j in 1:length(PopGrowthAll[[i]])){
    iteration = iteration +1
    Demographics[iteration,"Site"] = paste0(PopGrowthAll[[i]][[j]][1,"Site"])
    Demographics[iteration,"Protection"] = paste0(PopGrowthAll[[i]][[j]][1,"Protection"])
    Demographics[iteration,"MeanGrowthRate"] = mean(PopGrowthAll[[i]][[j]][10:nrow(PopGrowthAll[[i]][[j]]),"PopGrowthRate"],na.rm=TRUE)
    Demographics[iteration,"SDGrowthRate"] = sd(PopGrowthAll[[i]][[j]][10:nrow(PopGrowthAll[[i]][[j]]),"PopGrowthRate"],na.rm=TRUE)
  }
}

rm(iteration)


###Setting up Capture histories ---------
##Banks Peninsula capture histories
#Protection = c("NoProtect","CurrentProtect","100mProtect")
#IndTrial_CH = NULL
#BP_all_CH = NULL

#for(i in 1:length(BP_Pop_All)){
#  for(j in 1:length(BP_Pop_All[[i]])){
 #   for(k in 2:length(BP_Pop_All[[i]][[j]])){
  #    if(k == 2){
   #     AllInds = BP_Pop_All[[i]][[j]][[k]][,c("IndividualID","Sex","Age","Mature","CurrentCalf")]
    #    AllInds$Year = k
     #   AllInds$Protection = Protection[[i]]
      #  AllInds$Site = "BanksPeninsula"
#      }else{
 #       temp <- BP_Pop_All[[i]][[j]][[k]][,c("IndividualID","Sex","Age","Mature","CurrentCalf")]
  #      temp$Year = k
   #     temp$Protection = Protection[[i]]
    #    temp$Site = "BanksPeninsula"
     #   AllInds = rbind(AllInds,temp)
      #}
#    }
 #   AllInds$CaptureState = NA
    
  #  for(l in 1:nrow(AllInds)){
   #   if(AllInds[l,"Sex"] == "F" & AllInds[l,"Mature"] == 1 & is.na(AllInds[l,"CurrentCalf"])==FALSE){
    #    if(AllInds[which(AllInds[l,"CurrentCalf"]==AllInds[,"IndividualID"] & AllInds[l,"Year"]==AllInds[,"Year"]),"Age"] < 1){
     #     AllInds[l,"CaptureState"] = "B"
      #  }else{
       #   AllInds[l,"CaptureState"] = "NB"
        #}
#      }else if(AllInds[l,"Sex"] == "M" & AllInds[l,"Mature"] == 1){
 #       AllInds[l,"CaptureState"] = "A"
  #    }else if(AllInds[l,"Sex"] == "F" & AllInds[l,"Mature"] == 1){
   #     AllInds[l,"CaptureState"] = "NB"
    #  }else if(AllInds[l,"Age"] > 3){
     #   AllInds[l,"CaptureState"] = "SA"
      #}else if(AllInds[l,"Age"] >= 1){
#        AllInds[l,"CaptureState"] = "J"
 #     }else if(AllInds[l,"Age"] < 1){
  #      AllInds[l,"CaptureState"] = "C"
   #   }
    #  AllInds[l,"MaxAge"] = max(AllInds[which(AllInds[l,"IndividualID"]==AllInds[,"IndividualID"]),"Age"])
#    }

#    IndTrial_CH[[j]] = reshape(AllInds, direction = "wide", timevar = "Year", idvar = c("IndividualID","Sex","MaxAge","Protection","Site"), drop = c("Age","Mature","CurrentCalf"))
#  }
#  BP_all_CH[[i]] = IndTrial_CH
#}


##Timaru CH
#Tim_all_CH = NULL
#IndTrial_CH = NULL

#for(i in 1:length(Tim_Pop_All)){
 # for(j in 1:length(Tim_Pop_All[[i]])){
  #  for(k in 2:length(Tim_Pop_All[[i]][[j]])){
   #   if(k == 2){
    #    AllInds = Tim_Pop_All[[i]][[j]][[k]][,c("IndividualID","Sex","Age","Mature","CurrentCalf")]
     #   AllInds$Year = k
      #  AllInds$Protection = Protection[[i]]
       # AllInds$Site = "Timaru"
#      }else{
 #       temp <- Tim_Pop_All[[i]][[j]][[k]][,c("IndividualID","Sex","Age","Mature","CurrentCalf")]
  #      temp$Year = k
   #     temp$Protection = Protection[[i]]
    #    temp$Site = "Timaru"
     #   AllInds = rbind(AllInds,temp)
      #}
#    }
 #   AllInds$CaptureState = NA
    
  #  for(l in 1:nrow(AllInds)){
   #   if(AllInds[l,"Sex"] == "F" & AllInds[l,"Mature"] == 1 & is.na(AllInds[l,"CurrentCalf"])==FALSE){
    #    if(AllInds[which(AllInds[l,"CurrentCalf"]==AllInds[,"IndividualID"] & AllInds[l,"Year"]==AllInds[,"Year"]),"Age"] < 1){
     #     AllInds[l,"CaptureState"] = "B"
      #  }else{
       #   AllInds[l,"CaptureState"] = "NB"
        #}
#      }else if(AllInds[l,"Sex"] == "M" & AllInds[l,"Mature"] == 1){
 #       AllInds[l,"CaptureState"] = "A"
  #    }else if(AllInds[l,"Sex"] == "F" & AllInds[l,"Mature"] == 1){
   #     AllInds[l,"CaptureState"] = "NB"
    #  }else if(AllInds[l,"Age"] > 3){
     #   AllInds[l,"CaptureState"] = "SA"
      #}else if(AllInds[l,"Age"] >= 1){
       # AllInds[l,"CaptureState"] = "J"
#      }else if(AllInds[l,"Age"] < 1){
 #       AllInds[l,"CaptureState"] = "C"
  #    }
   #   AllInds[l,"MaxAge"] = max(AllInds[which(AllInds[l,"IndividualID"]==AllInds[,"IndividualID"]),"Age"])
    #}
    
#    IndTrial_CH[[j]] = reshape(AllInds, direction = "wide", timevar = "Year", idvar = c("IndividualID","Sex","MaxAge","Protection","Site"), drop = c("Age","Mature","CurrentCalf"))
 # }
  #Tim_all_CH[[i]] = IndTrial_CH
#}

##Dunedin 100m Protection
#Dun_all_CH = NULL
#IndTrial_CH = NULL

#for(i in 1:length(Dun_Pop_All)){
 # for(j in 1:length(Dun_Pop_All[[i]])){
  #  for(k in 2:length(Dun_Pop_All[[i]][[j]])){
   #   if(k == 2){
    #    AllInds = Dun_Pop_All[[i]][[j]][[k]][,c("IndividualID","Sex","Age","Mature","CurrentCalf")]
     #   AllInds$Year = k
      #  AllInds$Protection = Protection[[i]]
       # AllInds$Site = "Dunedin"
#      }else{
 #       temp <- Dun_Pop_All[[i]][[j]][[k]][,c("IndividualID","Sex","Age","Mature","CurrentCalf")]
  #      temp$Year = k
   #     temp$Protection = Protection[[i]]
    #    temp$Site = "Dunedin"
     #   AllInds = rbind(AllInds,temp)
      #}
#    }
 #   AllInds$CaptureState = NA
    
  #  for(l in 1:nrow(AllInds)){
   #   if(AllInds[l,"Sex"] == "F" & AllInds[l,"Mature"] == 1 & is.na(AllInds[l,"CurrentCalf"])==FALSE){
    #    if(AllInds[which(AllInds[l,"CurrentCalf"]==AllInds[,"IndividualID"] & AllInds[l,"Year"]==AllInds[,"Year"]),"Age"] < 1){
     #     AllInds[l,"CaptureState"] = "B"
      #  }else{
       #   AllInds[l,"CaptureState"] = "NB"
        #}
#      }else if(AllInds[l,"Sex"] == "M" & AllInds[l,"Mature"] == 1){
 #       AllInds[l,"CaptureState"] = "A"
  #    }else if(AllInds[l,"Sex"] == "F" & AllInds[l,"Mature"] == 1){
   #     AllInds[l,"CaptureState"] = "NB"
    #  }else if(AllInds[l,"Age"] > 3){
     #   AllInds[l,"CaptureState"] = "SA"
      #}else if(AllInds[l,"Age"] >= 1){
       # AllInds[l,"CaptureState"] = "J"
#      }else if(AllInds[l,"Age"] < 1){
 #       AllInds[l,"CaptureState"] = "C"
  #    }
   #   AllInds[l,"MaxAge"] = max(AllInds[which(AllInds[l,"IndividualID"]==AllInds[,"IndividualID"]),"Age"])
    #}
    
#    IndTrial_CH[[j]] = reshape(AllInds, direction = "wide", timevar = "Year", idvar = c("IndividualID","Sex","MaxAge","Protection","Site"), drop = c("Age","Mature","CurrentCalf"))
 # }
  #Dun_all_CH[[i]] = IndTrial_CH
#}


#All_sites_CH = list(BP_all_CH,Tim_all_CH,Dun_all_CH)
#save(All_sites_CH,file="./All_sites_CH.rda")

rm(IndTrial_CH,temp,AllInds,Dun_Pop_All,Tim_Pop_All,BP_Pop_All,Tim_all_CH,BP_all_CH,Dun_all_CH)

###Survival rates age structured CJS models-----

Protection = c("NoProtection","CurrentProtection","100mProtection")
Sites = c("BanksPeninsula","Timaru","Dunedin")
iteration = 0
mean.survival.trial= data.frame(Trial = as.integer(),
                                TotalSurvival = as.numeric(),
                                AdultSurvival = as.numeric())

#h=protect
#i=survivalTrial
#k=nrows
for(h in 1:length(PopSizeAllScenarios_All)){
  for(i in 1:length(PopSizeAllScenarios_All[[h]])){
    mean.survival.trial = data.frame(Trial = as.integer(),TotalSurvival = as.numeric(),AdultSurvival = as.numeric())
    
    for(j in 1:length(PopSizeAllScenarios_All[[h]][[i]])){
      for(k in 2:nrow(PopSizeAllScenarios_All[[h]][[i]][[j]])){
        PopSizeAllScenarios_All[[h]][[i]][[j]][k,"TotCalves"] = length(which(All_sites_CH[[h]][[i]][[j]][,k+4] == "C"))
        PopSizeAllScenarios_All[[h]][[i]][[j]][k,"TotJuveniles"] = length(which(All_sites_CH[[h]][[i]][[j]][,k+4] == "J"))
        PopSizeAllScenarios_All[[h]][[i]][[j]][k,"TotSubAdults"] = length(which(All_sites_CH[[h]][[i]][[j]][,k+4] == "SA"))
        PopSizeAllScenarios_All[[h]][[i]][[j]][k,"TotAdults"] = length(which(All_sites_CH[[h]][[i]][[j]][,k+4] == "A" | All_sites_CH[[h]][[i]][[j]][,k+4] == "NB" | All_sites_CH[[h]][[i]][[j]][,k+4] == "B"))
        PopSizeAllScenarios_All[[h]][[i]][[j]][k,"TotMatFem"] = length(which(All_sites_CH[[h]][[i]][[j]][,k+4] == "NB" | All_sites_CH[[h]][[i]][[j]][,k+4] == "B"))
        
        
        for(l in 3:nrow(PopSizeAllScenarios_All[[h]][[i]][[j]])){
          PopSizeAllScenarios_All[[h]][[i]][[j]][l,"AdultsSurvivedt-t1"] = length(which(All_sites_CH[[h]][[i]][[j]][,l+4] == "NB" & All_sites_CH[[h]][[i]][[j]][,l+3] == "NB"  | All_sites_CH[[h]][[i]][[j]][,l+4] == "NB" & All_sites_CH[[h]][[i]][[j]][,l+3] == "B" | All_sites_CH[[h]][[i]][[j]][,l+4] == "B" & All_sites_CH[[h]][[i]][[j]][,l+3] == "NB" | All_sites_CH[[h]][[i]][[j]][,l+4] == "NB" & All_sites_CH[[h]][[i]][[j]][,l+3] == "B"  | All_sites_CH[[h]][[i]][[j]][,l+4] == "A" & All_sites_CH[[h]][[i]][[j]][,l+3] == "A"))
          PopSizeAllScenarios_All[[h]][[i]][[j]][l,"TotSurvival"] = (PopSizeAllScenarios_All[[h]][[i]][[j]][l,"PopulationSize"] - PopSizeAllScenarios_All[[h]][[i]][[j]][l,"TotCalves"]) / PopSizeAllScenarios_All[[h]][[i]][[j]][l-1,"PopulationSize"] #Total individuals that survived minus the addition of new calves
          PopSizeAllScenarios_All[[h]][[i]][[j]][l,"AdultSurvival"] = (PopSizeAllScenarios_All[[h]][[i]][[j]][l,"AdultsSurvivedt-t1"]) / PopSizeAllScenarios_All[[h]][[i]][[j]][l-1,"TotAdults"] 
          
        }
      }
      
      mean.survival.trial[j,"Site"] = Sites[[h]]
      mean.survival.trial[j,"Protection"] = Protection[[i]]
      mean.survival.trial[j,"TotalSurvival"] = mean(PopSizeAllScenarios_All[[h]][[i]][[j]][10:nrow(PopSizeAllScenarios_All[[h]][[i]][[j]]),"TotSurvival"],na.rm=TRUE)
      mean.survival.trial[j,"AdultSurvival"] = mean(PopSizeAllScenarios_All[[h]][[i]][[j]][10:nrow(PopSizeAllScenarios_All[[h]][[i]][[j]]),"AdultSurvival"],na.rm=TRUE)
      
    }
    
    row = which(unique(mean.survival.trial$Site) == Demographics$Site & unique(mean.survival.trial$Protection) == Demographics$Protection)
    Demographics[row,"TotalSurvival"] = mean(mean.survival.trial$TotalSurvival,na.rm=TRUE)
    Demographics[row,"SDTotalSurvival"] = sd(mean.survival.trial$TotalSurvival,na.rm=TRUE)
    Demographics[row,"AdultSurvival"] = mean(mean.survival.trial$AdultSurvival,na.rm=TRUE)
    Demographics[row,"SDAdultSurvival"] = sd(mean.survival.trial$AdultSurvival,na.rm=TRUE)

  }
}



###Fecundity-----
Fecundity_trials = NULL
fecundity = NA

for(g in 1:length(All_sites_CH)){
  for(h in 1:length(All_sites_CH[[g]])){
    for(i in 1:length(All_sites_CH[[g]][[h]])){
      female.data = All_sites_CH[[g]][[h]][[i]]
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
        fec_dataframe[j,"Protection"] = Protection[[h]]
        fec_dataframe[j,"Site"] = Sites[[g]]
        fec_dataframe[j,"IndividualID"] = female.data[j,"IndividualID"] 
        fec_dataframe[j,"MaxAge"] = female.data[j,"MaxAge"]
        fec_dataframe[j,"Average.ICI"] = mean(ICI,na.rm=TRUE)
        fec_dataframe[j,"No.Calves"] = length(BreedingYears)
    }
    fecundity[i] = mean(fec_dataframe$Average.ICI,na.rm = TRUE)
  }
 
  row = which(unique(fec_dataframe$Site) == Demographics$Site & unique(fec_dataframe$Protection) == Demographics$Protection)
  Demographics[row,"MeanICI"] = mean(fecundity,na.rm=TRUE)
  Demographics[row,"SD_ICI"] = sd(fecundity,na.rm=TRUE)
  Demographics[row,"MeanFecundity"] = 1/mean(fecundity,na.rm=TRUE)
  
  }
}


###Age-----

Age.data.rep = data.frame(Site = as.character(),
                            Protection = as.character(),
                            MeanAll = as.numeric(),
                            MinAll = as.numeric(),
                            MaxAll = as.numeric(),
                            MeanAdult = as.numeric(),
                            MinAdult = as.numeric(),
                            MaxAdult = as.numeric())

for(h in 1:length(All_sites_CH)){
  for(i in 1:length(All_sites_CH[[h]])){
    for(j in 1:length(All_sites_CH[[h]][[i]])){
      Age.data.rep[j,"Site"] = Sites[[h]]
      Age.data.rep[j,"Protection"] = Protection[[i]]
      Age.data.rep[j,"MeanAll"] = mean(All_sites_CH[[h]][[i]][[j]]$MaxAge)
      Age.data.rep[j,"MinAll"] = min(All_sites_CH[[h]][[i]][[j]]$MaxAge)
      Age.data.rep[j,"MaxAll"] = max(All_sites_CH[[h]][[i]][[j]]$MaxAge)
      
      temp = subset(All_sites_CH[[h]][[i]][[j]],All_sites_CH[[h]][[i]][[j]]$MaxAge >3)
      Age.data.rep[j,"MeanAdult"] = mean(temp$MaxAge)
      Age.data.rep[j,"MinAdult"] = min(temp$MaxAge)
      Age.data.rep[j,"MaxAdult"] = max(temp$MaxAge)
    }
    row = which(unique(Age.data.rep$Site) == Demographics$Site & unique(Age.data.rep$Protection) == Demographics$Protection)
    Demographics[row,"MeanAgeAll"] = mean(Age.data.rep[,"MeanAll"])
    Demographics[row,"MinAge3All"]= min(Age.data.rep[,"MinAll"])
    Demographics[row,"MaxAge3All"]= max(Age.data.rep[,"MaxAll"])
    Demographics[row,"MeanAge3plus"]= mean(Age.data.rep[,"MeanAdult"])
    Demographics[row,"MinAge3plus"]= min(Age.data.rep[,"MinAdult"])
    Demographics[row,"MaxAge3plus"]= max(Age.data.rep[,"MaxAdult"])
  }
}

##Example frequency distribution of max age

tiff("C:/Users/steph/Desktop/PhD/Writing/Chapter 6 - Predicting recovery through agent-based models/Figures/Appendices. Histogram of the maximum age that individuals reached.tiff",res = 300, units = "mm",width = 160, height=150)
par(mgp = c(2,0.5,0))
hist(All_sites_CH[[1]][[2]][[round(runif(1,1,length(All_sites_CH[[1]][[2]])))]]$MaxAge,breaks = seq(0,61,by=1),main = "", xlab = "Age (years)", ylab = "Frequency \n(maximum age of individuals)",ylim= c(0,100),axes = F)
axis(1,pos=0,tck = -0.015)
axis(2,tck = -0.015,las=2)
abline(h=0)
dev.off()

rm(temp,fec_dataframe,Fecundity_trials,mean.survival.trial,data,Age.data.trial,BreedingYears,fecundity,female.data,g,h,i,j,k,l,Protection)

write.csv(Demographics,"C:/Users/steph/Desktop/PhD/Writing/Chapter 6 - Predicting recovery through agent-based models/Figures/Simulation3_SiteDemogrphic.csv",row.names = FALSE)








#Movements -----


#file_names=as.list(dir())
#lapply(file_names,load,.GlobalEnv)

#move = grep("MoveHist",names(.GlobalEnv),value=TRUE);move_np = grep("NP",move,value=TRUE);move_cp = grep("CP",move,value=TRUE);move_100p = grep("100P",move,value=TRUE)
#tim_move_np = grep("Tim",move_np,value=TRUE);tim_move_cp = grep("Tim",move_cp,value=TRUE);tim_move_100p = grep("Tim",move_100p,value=TRUE);dun_move_np = grep("Dun",move_np,value=TRUE);dun_move_cp = grep("Dun",move_cp,value=TRUE);dun_move_100p = grep("Dun",move_100p,value=TRUE);bp_move_np = grep("BP",move_np,value=TRUE);bp_move_cp = grep("BP",move_cp,value=TRUE);bp_move_100p = grep("BP",move_100p,value=TRUE)

#Tim_Move_NP<-do.call("list",mget(tim_move_np));Tim_Move_CP<-do.call("list",mget(tim_move_cp));Tim_Move_100P<-do.call("list",mget(tim_move_100p));Dun_Move_NP<-do.call("list",mget(dun_move_np));Dun_Move_CP<-do.call("list",mget(dun_move_cp));Dun_Move_100P<-do.call("list",mget(dun_move_100p));BP_Move_NP<-do.call("list",mget(bp_move_np));BP_Move_CP<-do.call("list",mget(bp_move_cp));BP_Move_100P<-do.call("list",mget(bp_move_100p))



#rm(list=setdiff(ls(), c("Tim_Move_NP","Tim_Move_CP","Tim_Move_100P","BP_Move_NP","BP_Move_CP","BP_Move_100P","Dun_Move_NP","Dun_Move_CP","Dun_Move_100P")))

#Dun_Move_All = list(Dun_Move_NP,Dun_Move_CP,Dun_Move_100P)
#Tim_Move_All = list(Tim_Move_NP,Tim_Move_CP,Tim_Move_100P)
#BP_Move_All = list(BP_Move_NP,BP_Move_CP,BP_Move_100P)

#rm(list=setdiff(ls(), c("Dun_Move_All","Tim_Move_All","BP_Move_All")))

#MoveHist_all = list(BP_Move_All,Tim_Move_All,Dun_Move_All)
#rm(BP_Move_All,Tim_Move_All,Dun_Move_All);gc()
#save(MoveHist_all, file="./MoveHist_All.rda")

##Link movements to timesteps: Structure g = Populations, h = protection, i = replicate trial, j = timestep, k = rows in timesteps

for(g in 1:length(MoveHist_all)){
  for(h in 1:length(MoveHist_all[[g]])){
    for(i in 1:length(MoveHist_all[[g]][[h]])){
      for(j in 1:length(MoveHist_all[[g]][[h]][[i]])){
        MoveHist_all[[g]][[h]][[i]][[j]]$Timestep = j*10
        MoveHist_all[[g]][[h]][[i]][[j]]$DaysSinceStart = 45 + (j*10)
        MoveHist_all[[g]][[h]][[i]][[j]]$DayOfTheYear =  MoveHist_all[[g]][[h]][[i]][[j]]$DaysSinceStart %% 365
        if(j==1){
          MoveHist_all[[g]][[h]][[i]][[j]]$Year = 1
        }else if(MoveHist_all[[g]][[h]][[i]][[j]][1,"DaysSinceStart"] %% 365 == 0 | MoveHist_all[[g]][[h]][[i]][[j]][1,"DaysSinceStart"] %% 365 == 5){
          MoveHist_all[[g]][[h]][[i]][[j]]$Year =  MoveHist_all[[g]][[h]][[i]][[j-1]][1,"Year"] + 1
        }else{
          MoveHist_all[[g]][[h]][[i]][[j]]$Year = MoveHist_all[[g]][[h]][[i]][[j-1]][1,"Year"]
        }
      }
    }
  }
}

BP1 = MoveHist_all[[1]][[2]][runif(1,min=1,max=20)]

bp_all_moves = data.frame(IndividualID = as.character(),
                    x_loc = as.numeric(),
                    y_loc = as.numeric(),
                    Depth = as.numeric(),
                    GroupSize = as.integer(),
                    OrigPopulation = as.character(),
                    HomeRangeCentre = as.character(),
                    Timestep = as.integer(),
                    DaysSinceStart = as.integer(),
                    DayOfTheYear = as.integer(),
                    Year = as.integer())

for(i in 1:length(BP1[[1]])){
  if(i ==1){
  bp_all_moves = BP1[[1]][[i]]
  }else{
    temp = BP1[[1]][[i]]
    bp_all_moves = rbind(bp_all_moves,temp)
  }
}
  


summer=subset(bp_all_moves, bp_all_moves$DayOfTheYear < 30 & bp_all_moves$Year > 10 | bp_all_moves$DayOfTheYear > 350 & bp_all_moves$Year > 10)
winter=subset(bp_all_moves, bp_all_moves$DayOfTheYear > 155 & bp_all_moves$Year > 10 & bp_all_moves$DayOfTheYear < 200)

summer_subset = summer[runif(100,1,nrow(summer)),]
winter_subset = winter[runif(100,1,nrow(winter)),]
summer_subset$Groups = cut(summer_subset$Depth, breaks = c(0,-10,-20,-30,-40,-50,-60,-70,-80,-90,-100,-200))
bp.s=aggregate(Depth~Groups,data=summer_subset,length)
winter_subset$Groups = cut(winter_subset$Depth, breaks = c(0,-10,-20,-30,-40,-50,-60,-70,-80,-90,-100,-200))
bp.w=aggregate(Depth~Groups,data=winter_subset,length)


Tim1 = MoveHist_all[[2]][[2]][runif(1,min=1,max=20)]

tim_all_moves = data.frame(IndividualID = as.character(),
                       x_loc = as.numeric(),
                       y_loc = as.numeric(),
                       Depth = as.numeric(),
                       GroupSize = as.integer(),
                       OrigPopulation = as.character(),
                       HomeRangeCentre = as.character(),
                       Timestep = as.integer(),
                       DaysSinceStart = as.integer(),
                       DayOfTheYear = as.integer(),
                       Year = as.integer())

for(i in 1:length(Tim1[[1]])){
  if(i ==1){
    tim_all_moves = Tim1[[1]][[i]]
  }else{
    temp = Tim1[[1]][[i]]
    tim_all_moves = rbind(tim_all_moves,temp)
  }
}

summer.tim=subset(tim_all_moves, tim_all_moves$DayOfTheYear < 40 & tim_all_moves$Year > 10 | tim_all_moves$DayOfTheYear > 330 & tim_all_moves$Year > 10)
winter.tim=subset(tim_all_moves, tim_all_moves$DayOfTheYear > 145 & tim_all_moves$Year > 10 & tim_all_moves$DayOfTheYear < 220)

summer.tim_subset = summer.tim[runif(5000,1,nrow(summer.tim)),]
winter.tim_subset = winter.tim[runif(5000,1,nrow(winter.tim)),]
summer.tim_subset$Groups = cut(summer.tim_subset$Depth, breaks = c(0,-10,-20,-30,-40,-50,-60,-70,-80,-90,-100,-200))
tim.s=aggregate(Depth~Groups,data=summer.tim_subset,length)
winter.tim_subset$Groups = cut(winter.tim_subset$Depth, breaks = c(0,-10,-20,-30,-40,-50,-60,-70,-80,-90,-100,-200))
tim.w=aggregate(Depth~Groups,data=winter.tim_subset,length)


Dun1 = MoveHist_all[[3]][[2]][runif(1,min=1,max=20)]

dun_all_moves = data.frame(IndividualID = as.character(),
                       x_loc = as.numeric(),
                       y_loc = as.numeric(),
                       Depth = as.numeric(),
                       GroupSize = as.integer(),
                       OrigPopulation = as.character(),
                       HomeRangeCentre = as.character(),
                       Timestep = as.integer(),
                       DaysSinceStart = as.integer(),
                       DayOfTheYear = as.integer(),
                       Year = as.integer())

for(i in 1:length(Dun1[[1]])){
  if(i ==1){
    dun_all_moves = Dun1[[1]][[i]]
  }else{
    temp = Dun1[[1]][[i]]
    dun_all_moves = rbind(dun_all_moves,temp)
  }
}

summer.dun=subset(dun_all_moves, dun_all_moves$DayOfTheYear < 40 & dun_all_moves$Year > 10 | dun_all_moves$DayOfTheYear > 330 & dun_all_moves$Year > 10)
winter.dun=subset(dun_all_moves, dun_all_moves$DayOfTheYear > 145 & dun_all_moves$Year > 10 & dun_all_moves$DayOfTheYear < 220)

summer.dun_subset = summer.dun[runif(5000,1,nrow(summer.dun)),]
winter.dun_subset = winter.dun[runif(5000,1,nrow(winter.dun)),]
summer.dun_subset$Groups = cut(summer.dun_subset$Depth, breaks = c(0,-10,-20,-30,-40,-50,-60,-70,-80,-90,-100,-200))
dun.s=aggregate(Depth~Groups,data=summer.dun_subset,length)
winter.dun_subset$Groups = cut(winter.dun_subset$Depth, breaks = c(0,-10,-20,-30,-40,-50,-60,-70,-80,-90,-100,-200))
dun.w=aggregate(Depth~Groups,data=winter.dun_subset,length)


AreaSeasonDepths = bp.s
colnames(AreaSeasonDepths) = c("bins","BP.summer")
AreaSeasonDepths$BP.winter = bp.w$Depth
AreaSeasonDepths = merge(AreaSeasonDepths,tim.s, by.x = "bins", by.y = "Groups", all.x = TRUE, all.y=FALSE)
AreaSeasonDepths = merge(AreaSeasonDepths,tim.w, by.x = "bins", by.y = "Groups", all.x = TRUE, all.y=FALSE)
AreaSeasonDepths = merge(AreaSeasonDepths,dun.s, by.x = "bins", by.y = "Groups", all.x = TRUE, all.y=FALSE)
AreaSeasonDepths = merge(AreaSeasonDepths,dun.w, by.x = "bins", by.y = "Groups", all.x = TRUE, all.y=FALSE)

AreaSeasonDepths = AreaSeasonDepths[order(AreaSeasonDepths$bins,decreasing = T),]


tiff("C:/Users/steph/Desktop/PhD/Writing/Chapter 6 - Predicting recovery through agent-based models/Figures/Distance distribution.tiff",res = 300, units = "mm",width = 100, height=200)
par(mfrow=c(3,1),mai= c(.6,.55,.3,0.05))
barplot(as.matrix(t(AreaSeasonDepths[,2:3])),beside = T, ylim = c(0,1600),xaxt = "n",cex.lab = 1.2)
axis(1, at = seq(2,33,by=3),labels = c(10,20,30,40,50,60,70,80,90,100,200),cex.lab = 1.4)
text(2,1500,"a)",cex = 1.4)
abline(h=0)
legend(x=26, y=1500, legend = c("Summer","Winter"), fill = c("gray35","gray90"))
barplot(as.matrix(t(AreaSeasonDepths[,4:5])),beside = T, xaxt = "n",ylim = c(0,1600),cex.lab = 1.2,ylab = "Group locations")
axis(1, at = seq(2,33,by=3),labels = c(10,20,30,40,50,60,70,80,90,100,200),pos=0,cex.lab = 1.4)
text(2,1500,"b)",cex = 1.4)
abline(h=0)

barplot(as.matrix(t(AreaSeasonDepths[,6:7])),beside = T,xaxt = "n",ylim = c(0,1600),cex.lab = 1.2,xlab = "Depth categories (m)")
axis(1, at = seq(2,33,by=3),labels = c(10,20,30,40,50,60,70,80,90,100,200),pos=0,cex.lab = 1.4)
abline(h=0)
text(2,1500,"c)",cex = 1.4)

dev.off()


write.csv(summer_subset,"C:/Users/steph/Desktop/PhD/Writing/Chapter 6 - Predicting recovery through agent-based models/Figures/bp_summer.csv")
write.csv(summer.tim_subset,"C:/Users/steph/Desktop/PhD/Writing/Chapter 6 - Predicting recovery through agent-based models/Figures/tim_summer.csv")
write.csv(summer.dun_subset,"C:/Users/steph/Desktop/PhD/Writing/Chapter 6 - Predicting recovery through agent-based models/Figures/dun_summer.csv")

rm(bp.s,bp.w,summer,winter,summer_subset,winter_subset)


all.inds.bp = unique(bp_all_moves$IndividualID)
random.inds.bp = all.inds.bp[sample(1:length(all.inds.bp),300,replace = FALSE)]

bp.individuals = subset(bp_all_moves,bp_all_moves$IndividualID == random.inds.bp)

MaxDistance.bp = data.frame(IDs = as.character(unique(bp.individuals$IndividualID)),
                            MaxDist = rep(NA,length(unique(bp.individuals$IndividualID))))


library(raster)
library(terra)


for(i in 1:nrow(MaxDistance.bp)){
  MaxDistance.bp[i,2] = max(pointDistance(bp.individuals[which(bp.individuals$IndividualID == random.inds.bp[[1]]),c("x_loc","y_loc")],bp.individuals[which(bp.individuals$IndividualID == MaxDistance.bp[i,1]),c("x_loc","y_loc")],lonlat = TRUE,allpairs = TRUE))
  
}


tiff("C:/Users/steph/Desktop/PhD/Writing/Chapter 6 - Predicting recovery through agent-based models/Figures/Distance homerange.tiff",res = 300, units = "mm",width = 160, height=130)
hist(MaxDistance.bp$MaxDist,breaks = c(0,10000,20000,30000,40000,50000,60000,70000,80000,90000,100000,110000), axes = FALSE,ylim = c(0,50),main = "",xlab = "Maximum distance (km)")
axis(1, labels = c(0,10,20,30,40,50,60,70,80,90,100,110), at= seq(0,110000,10000),pos=0)
axis(2)
abline(h=0)
dev.off()


#Emergent Home Range ----
#Transform list into individual moves.

##seq(55,length(MoveHist[[eachtimestep]])*10,by=10)

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
coast = shapefile("C:/Users/steph/Desktop/PhD/Research/GIS/nzcoast.shp")
depth = shapefile("C:/Users/steph/Desktop/PhD/Research/GIS/USeful shapefiles/50 & 100m depth contour.shp")


BP_summer_comp = subset(bp_all_moves,bp_all_moves$Year == 30 & bp_all_moves$DayOfTheYear >340 & bp_all_moves$DayOfTheYear < 20)
BP_winter_comp = subset(bp_all_moves,bp_all_moves$Year == 30 & bp_all_moves$DayOfTheYear > 180 &bp_all_moves$DayOfTheYear < 200)

tiff("C:/Users/steph/Desktop/PhD/Writing/Chapter 6 - Predicting recovery through agent-based models/Figures/Appendix X. summer vs. winter.tiff", width = 160, height = 220, res=300, units = "mm")
par(mfrow = c(1,2))
plot(coast, xlim = c(172.4,173.4),ylim = c(-44.2,-43.4),col="gray")
plot(depth, add= TRUE, col = "#2A788EFF")
points(BP_summer_comp$y_loc~BP_summer_comp$x_loc,pch=19,cex = 0.8)
plot(coast, xlim = c(172.4,173.4),ylim = c(-44.2,-43.4),col="gray")
plot(depth, add= TRUE, col = "#2A788EFF")
points(BP_winter_comp$y_loc~BP_winter_comp$x_loc,pch=19,cex = 0.8)

dev.off()

