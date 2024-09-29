#Setup workspace----
rm(list = ls(all = TRUE))

#library(rjags)
#library(bayesplot)
#library(posterior)

setwd("C:/Users/steph/Desktop/PhD/Research/Fecundity")

surv = read.csv("AllSurveys2024.csv")
enc = read.csv("AllEncounters2024.csv")
data = read.csv("AllFemalePictureHistory2024.csv")

#Data setup----
##Surveys ----
surv$SurveyDate = as.Date(surv$SurveyDate, format = "%d/%m/%Y")
surv$Month = as.integer(format(surv$SurveyDate, "%m"))
surv$Day = as.integer(format(surv$SurveyDate, "%d"))
surv$Week = NA
for(i in 1:nrow(surv)){
  if(surv[i,"Month"] == 11){
    if(surv[i,"Day"] <= 7){
      surv[i,"Week"] = 1
    }else if(surv[i,"Day"] <= 14){
      surv[i,"Week"] = 2
    }else if(surv[i,"Day"] <= 21){
      surv[i,"Week"] = 3
    }else if(surv[i,"Day"] <= 28){
      surv[i,"Week"] = 4
    }else{
        surv[i,"Week"] = 5
    }
  }else if (surv[i,"Month"] == 12){
    if(surv[i,"Day"] <= 5){
      surv[i,"Week"] = 5
    }else if(surv[i,"Day"] <= 12){
      surv[i,"Week"] = 6
    }else if(surv[i,"Day"] <= 19){
      surv[i,"Week"] = 7
    }else if(surv[i,"Day"] <= 26){
      surv[i,"Week"] = 8
    }else{
      surv[i,"Week"] = 9
    }
  }else if (surv[i,"Month"] == 1){
    if(surv[i,"Day"] <= 2){
      surv[i,"Week"] = 9
    }else if(surv[i,"Day"] <= 9){
      surv[i,"Week"] = 10
    }else if(surv[i,"Day"] <= 16){
      surv[i,"Week"] = 11
    }else if(surv[i,"Day"] <= 23){
      surv[i,"Week"] = 12
    }else if(surv[i,"Day"] <= 30){
      surv[i,"Week"] = 13
    }else{
      surv[i,"Week"] = 14
    }
  }else if(surv[i,"Month"] == 2){
    if(surv[i,"Day"] <= 6){
      surv[i,"Week"] = 14
    }else if(surv[i,"Day"] <= 13){
      surv[i,"Week"] = 15
    }else if(surv[i,"Day"] <= 20){
      surv[i,"Week"] = 16
    }else if(surv[i,"Day"] <= 27){
      surv[i,"Week"] = 17
    }else{
      surv[i,"Week"] = 18
    }
  }else{
    if(surv[i,"Day"] <= 6){
      surv[i,"Week"] = 18
    }else if(surv[i,"Day"] <= 13){
      surv[i,"Week"] = 19
    }else if(surv[i,"Day"] <= 20){
      surv[i,"Week"] = 20
    }else if(surv[i,"Day"] <= 27){
      surv[i,"Week"] = 21
    }else{
      surv[i,"Week"] = 22
    }
  }
}


surv$Season <- ifelse(surv$Month >= 7, as.integer(format(surv$SurveyDate, "%Y")) + 1, as.integer(format(surv$SurveyDate, "%Y")))

surv[is.na(surv)] = 0
surv$BanksAreas = NA #Right now surveys are included from all areas (including Rakaia, Timaru, Dunedin etc.) that are in the catalogue. I only want surveys that occurred in BP. Challenge is that the surveys are not always filled in. Assumption is that all surveys are BP UNLESS otherwise specified, so if I include all surveys that have BP effort or and are not specified outside BP I should be good.

for(i in 1:nrow(surv)){
  if(sum(surv[i,c(8:12,29:61)]) > 0 & sum(surv[i,13:28]) == 0){
    surv[i,"BanksAreas"] = 0
  }else{
    surv[i,"BanksAreas"] = 1
  }
}

surv <- subset(surv, surv$BanksAreas == 1 & surv$Month >= 11 | surv$BanksAreas == 1 & surv$Month <= 3)

surv = surv[,c(1:3,6,62:66)]

AnnSurvEff = aggregate(SurveyDate~Season,data = surv,length) #Number of surveys conducted annually, from November to March.
WeekSurvEff = aggregate(SurveyDate~Week,data = surv,length) #Number of surveys conducted weekly, from November to March.

rm(surv)




##Encounters ----
enc$StartDate = as.Date(enc$StartDate, "%d/%m/%Y")
enc$SurveyArea = as.factor(enc$SurveyArea)
enc$Month = as.integer(format(enc$StartDate, "%m"))
enc$Day = as.integer(format(enc$StartDate, "%d"))
enc$Season = ifelse(enc$Month >= 7, as.integer(format(enc$StartDate, "%Y"))+1, as.integer(format(enc$StartDate, "%Y")))
enc$Week = NA

for(i in 1:nrow(enc)){
  if(enc[i,"Month"] == 11){
    if(enc[i,"Day"] <= 7){
      enc[i,"Week"] = 1
    }else if(enc[i,"Day"] <= 14){
      enc[i,"Week"] = 2
    }else if(enc[i,"Day"] <= 21){
      enc[i,"Week"] = 3
    }else if(enc[i,"Day"] <= 28){
      enc[i,"Week"] = 4
    }else{
      enc[i,"Week"] = 5
    }
  }else if (enc[i,"Month"] == 12){
    if(enc[i,"Day"] <= 5){
      enc[i,"Week"] = 5
    }else if(enc[i,"Day"] <= 12){
      enc[i,"Week"] = 6
    }else if(enc[i,"Day"] <= 19){
      enc[i,"Week"] = 7
    }else if(enc[i,"Day"] <= 26){
      enc[i,"Week"] = 8
    }else{
      enc[i,"Week"] = 9
    }
  }else if (enc[i,"Month"] == 1){
    if(enc[i,"Day"] <= 2){
      enc[i,"Week"] = 9
    }else if(enc[i,"Day"] <= 9){
      enc[i,"Week"] = 10
    }else if(enc[i,"Day"] <= 16){
      enc[i,"Week"] = 11
    }else if(enc[i,"Day"] <= 23){
      enc[i,"Week"] = 12
    }else if(enc[i,"Day"] <= 30){
      enc[i,"Week"] = 13
    }else{
      enc[i,"Week"] = 14
    }
  }else if(enc[i,"Month"] == 2){
    if(enc[i,"Day"] <= 6){
      enc[i,"Week"] = 14
    }else if(enc[i,"Day"] <= 13){
      enc[i,"Week"] = 15
    }else if(enc[i,"Day"] <= 20){
      enc[i,"Week"] = 16
    }else if(enc[i,"Day"] <= 27){
      enc[i,"Week"] = 17
    }else{
      enc[i,"Week"] = 18
    }
  }else{
    if(enc[i,"Day"] <= 6){
      enc[i,"Week"] = 18
    }else if(enc[i,"Day"] <= 13){
      enc[i,"Week"] = 19
    }else if(enc[i,"Day"] <= 20){
      enc[i,"Week"] = 20
    }else if(enc[i,"Day"] <= 27){
      enc[i,"Week"] = 21
    }else{
      enc[i,"Week"] = 22
    }
  }
}


head(enc)

WeeklyCalves <- aggregate(cbind(Calves, GroupSize) ~ StartDate, data = enc, sum)
WeeklyCalves$CpGS = WeeklyCalves$Calves/WeeklyCalves$GroupSize
WeeklyCalves$Week = enc[match(WeeklyCalves[,"StartDate"],enc[,"StartDate"]),"Week"]

WeeklyCalves <- aggregate(cbind(CpGS, Calves, GroupSize) ~ Week, data = WeeklyCalves, mean)


SEWeeklyCalves = aggregate(cbind(Calves, GroupSize)~StartDate,data = enc,sum)
SEWeeklyCalves$Week = enc[match(SEWeeklyCalves[,"StartDate"],enc[,"StartDate"]),"Week"]
SEWeeklyCalves <- aggregate(cbind(Calves, GroupSize) ~ Week, data = SEWeeklyCalves, function(x) {sd(x)/sqrt(length(x))})


SeasonalCalves <- aggregate(cbind(Calves, GroupSize) ~ Season, data = enc, sum)

##Known female sighting records -----
colnames(data)[5] = "CalfEvidence"
data$Sighted = ifelse(data$CalfEvidence == "Y", 2, 1)
data$Sighted = ifelse(data$NotesOn1. == "Y", 3, data$Sighted)

DateList = unique(data$DateTaken)
IndList = unique(data$IndividualName)
for(d in 1:length(DateList)){
  for(I in 1:length(IndList)){
    del = subset(data,data[,"IndividualName"] == IndList[I] & data[,"DateTaken"] == DateList[d])
    
    if(nrow(del)>1){
      
      for(i in 1:nrow(data)){
        if(data[i,"IndividualName"] == IndList[I] & data[i,"DateTaken"] == DateList[d]){
          data[i,"Sighted"] = max(del$Sighted)
        }
      }
    }
  }
}

rm(I,d,DateList,IndList)

dat1 = reshape(data[,c("DateTaken", "IndividualName","Sighted")],direction = "wide", timevar = "DateTaken", idvar = "IndividualName", drop = c("MarkCategory","Sex","X0._PhotoEvidence","CalfEvidence","NotesOn1.","Photoquality"))
dat1[is.na(dat1)] = 0


dat2<-as.data.frame(t(dat1))
colnames(dat2) = dat2[1,]
dat2 = dat2[-1,]

dat2$Date = rownames(dat2)
dat2$Date = gsub("Sighted.","",dat2$Date)
dat2$Date = as.Date(dat2$Date, "%d/%m/%Y")
dat2 = dat2[order(dat2$Date),]
dat2$Year = format(as.Date(dat2$Date, format="%d/%m/%Y"),"%Y"); dat2$Year = as.integer(dat2$Year)
dat2$month = format(as.Date(dat2$Date, format="%d/%m/%Y"),"%m"); dat2$month = as.integer(dat2$month)
dat2$day = format(as.Date(dat2$Date, format="%d/%m/%Y"),"%d"); dat2$day = as.integer(dat2$day)
dat2$Season = ifelse(dat2$month <= 6, dat2$Year, dat2$Year + 1)  

##Use only sightings that were during primary season: Nov - Mar
dat3<-subset(dat2,dat2$month > 10 | dat2$month < 4)

YearData<-as.data.frame(matrix(NA,nrow=length(unique(dat3$Season)),ncol = ncol(dat3)-4))
colnames(YearData)<-c("Season",colnames(dat3)[1:(ncol(dat3)-5)])
YearData$Season<-unique(dat3$Season)

#Transistion the sightings by day into annual sightings. 

for(i in 1:nrow(dat3)){
  for(j in 1:(ncol(dat3)-5)){
    new.column = j + 1
    new.row = match(dat3[i,"Season"],YearData$Season)
    if(is.na(YearData[new.row,new.column]) == TRUE){
      YearData[new.row,new.column] = dat3[i,j]
    }else if(YearData[new.row,new.column] > dat3[i,j]){
        YearData[new.row,new.column] = YearData[new.row,new.column]
    }else{
      YearData[new.row,new.column] = dat3[i,j]
    }
  }
}


YearSightingsMatrix <- as.data.frame(t(YearData[,-1]))
colnames(YearSightingsMatrix) <- YearData$Season

YearSightingsMatrix$'1998' = "0"
YearSightingsMatrix$'1999' = "0"

YearSightingsMatrix = YearSightingsMatrix[,c(1:12,38,39,13:37)]

#YearSightingsMatrix$TotalAnnualSightings <-NA
#for(i in 1:nrow(YearSightingsMatrix)){
#  YearSightingsMatrix[i,ncol(YearSightingsMatrix)] = length(which(YearSightingsMatrix[i,] > 0))
#}

#YearSightingsMatrix<- subset(YearSightingsMatrix,YearSightingsMatrix$TotalAnnualSightings >= 5)

#YearSightingsMatrix$TotalCalves <-NA
#for(i in 1:nrow(YearSightingsMatrix)){
#  YearSightingsMatrix[i,ncol(YearSightingsMatrix)] = length(which(YearSightingsMatrix[i,] == 2))
#}


#TopDolphins = as.data.frame(rownames(YearSightingsMatrix))
#colnames(TopDolphins) <- "IndividualID"


### Partition season into early mid and late


dat3$s.partition = ifelse(dat3$month == 11 | dat3$month == 12 & dat3$day <= 20, "early",
                          ifelse(dat3$month == 12 & dat3$day > 20 | dat3$month == 1 | dat3$month == 2 & dat3$day <= 7, "mid","late"))

for(i in 1:nrow(dat3)){
  dat3[i,"s.partition"] = paste(c(dat3[i,ncol(dat3)-1],dat3[i,ncol(dat3)]), collapse =".")
}

PartYearData <- as.data.frame(matrix(NA,nrow=length(unique(dat3$s.partition)),ncol = ncol(dat3)-5))
colnames(PartYearData)<-c("Season",colnames(dat3)[1:(ncol(dat3)-6)])
PartYearData$Season<-unique(dat3$s.partition)

for(i in 1:nrow(dat3)){
  for(j in 1:(ncol(dat3)-6)){
    new.column = j + 1
    new.row = match(dat3[i,"s.partition"],PartYearData$Season)
    if(is.na(PartYearData[new.row,new.column]) == TRUE){
      PartYearData[new.row,new.column] = dat3[i,j]
    }else if(PartYearData[new.row,new.column] > dat3[i,j]){
      PartYearData[new.row,new.column] = PartYearData[new.row,new.column]
    }else{
      PartYearData[new.row,new.column] = dat3[i,j]
    }
  }
}

PartYearSightingsMatrix <- as.data.frame(t(PartYearData[,-1]))
colnames(PartYearSightingsMatrix) <- PartYearData$Season
PartYearSightingsMatrix$IndividualID <- rownames(PartYearSightingsMatrix)

#PartYearSightingsMatrix <-merge(TopDolphins,PartYearSightingsMatrix,by="IndividualID",all.x = TRUE, all.y = FALSE)
rownames(PartYearSightingsMatrix)= PartYearSightingsMatrix$IndividualID
PartYearSightingsMatrix = PartYearSightingsMatrix[,-1]

PartYearSightingsMatrix$'1990.early' = "0";PartYearSightingsMatrix$'1990.late' = "0";PartYearSightingsMatrix$'1991.early' = "0";PartYearSightingsMatrix$'1992.early' = "0";PartYearSightingsMatrix$'1993.early' = "0";PartYearSightingsMatrix$'1998.early' = "0";PartYearSightingsMatrix$'1998.mid' = "0";PartYearSightingsMatrix$'1998.late' = "0";PartYearSightingsMatrix$'1999.early' = "0";PartYearSightingsMatrix$'1999.mid' = "0";PartYearSightingsMatrix$'1999.late' = "0";PartYearSightingsMatrix$'2000.early' = "0";PartYearSightingsMatrix$'2009.late' = "0";PartYearSightingsMatrix$'2011.late' = "0";PartYearSightingsMatrix$'2012.late' = "0";PartYearSightingsMatrix$'2017.early' = "0";PartYearSightingsMatrix$'2019.early' = "0";PartYearSightingsMatrix$'2021.early' = "0"

PartYearSightingsMatrix = PartYearSightingsMatrix[,c(1:12,100,13,101,102,14,15,103,16,17,104,18:31,105:111,32:59,112,60:64,113,65,66,114,67:78,115,79:83,116,84:88,117,89:99)]

RS.data = array(unlist(PartYearSightingsMatrix), dim = c(nrow(PartYearSightingsMatrix),3,39)) #Data set up for a robust structure design.





#write.csv(PartYearSightingsMatrix,"capturehistory.csv", row.names = FALSE)

#Summary statistics ----
##BP survey areas-----
library(raster)
library(terra)
coast = shapefile("C:/Users/steph/Desktop/PhD/Research/GIS/nzcoast.shp")
BP <-crop(coast, extent(c(172.3,173.8,-44.6,-43.3)))
area.division = shapefile("C:/Users/steph/Desktop/PhD/Research/GIS/BP_Survey_edited.shp")
area.division2 = shapefile("C:/Users/steph/Desktop/PhD/Research/GIS/BP_generalSurveyAreas.shp")
ex.Survey = shapefile("C:/Users/steph/Desktop/PhD/Research/GIS/examplesurveys.shp")

inset.limits<- c(0.071,0.37,0.6,0.9674)#L,R,B,T

tiff("C:/Users/steph/Desktop/PhD/Writing/Chapter 5 - Potential and realised fecundity of Hector's dolphins/Figures/Figure 1. BP study area.tiff", width = 200, height = 200, res=300, units = "mm")
par(mar=c(7,0.1,0.1,4),xpd=NA)
plot(BP,col="gray",lwd = 0.05)
plot(area.division,add=TRUE,lwd=2)
plot(ex.Survey,col="maroon", add=TRUE)

text(172.90,-43.50,"NW",cex = 1.5);text(173.15,-43.59,"NE",cex = 1.5);text(173.2,-43.9,"E",cex = 1.5);text(172.93,-43.73,"AKH",cex = 1.5);text(172.8,-43.97,"S",cex = 1.5)

text(172.785,-43.55,"1",cex = 0.9);text(172.838,-43.58,"2",cex = 0.9);text(172.905,-43.60,"3",cex = 0.9);text(172.965,-43.615,"4",cex = 0.9);text(173.035,-43.63,"5",cex = 0.9);text(173.10,-43.66,"6",cex = 0.9);text(173.145,-43.72,"7",cex = 0.9);text(173.15,-43.785,"8",cex = 0.9);text(173.133,-43.84,"9",cex = 0.9);text(173.07,-43.88,"10",cex = 0.9);text(173.015,-43.9,"11",cex = 0.9);text(172.972,-43.915,"12",cex = 0.9);text(172.92,-43.92,"13",cex = 0.9);text(172.85,-43.909,"14",cex = 0.9);text(172.78,-43.885,"15",cex = 0.9);text(172.732,-43.864,"16",cex = 0.9)

lines(x=c(172.3,172.3), y=c(-44.05,-43.3),lwd=0.8);lines(x=c(172.3,173.3),y=c(-43.3,-43.3),lwd=0.8);lines(x=c(172.3,173.3),y=c(-44.05,-44.05),lwd=0.8);lines(x=c(173.3,173.3),y=c(-43.3,-44.05))
scalebar(18.52,xy=c(172.35,-44.03), type = "line",lonlat=TRUE,label = "10nm",cex = 1.5,lwd=5)

par(fig=inset.limits, new=TRUE, mar=c(0,0,0,0) )
plot(coast,col="gray",lwd=0.001)
lines(x=c(172.3,172.3), y=c(-44.2,-43.3),lwd=0.8);lines(x=c(172.3,173.8),y=c(-43.3,-43.3),lwd=0.8);lines(x=c(172.3,173.8),y=c(-44.2,-44.2),lwd=0.8);lines(x=c(173.8,173.8),y=c(-43.3,-44.2),lwd=0.8)
north(xy = c(166,-37),type=2,cex=1.5)



box(lwd=0.9,col="black",lty = "solid")

dev.off()

tiff("C:/Users/steph/Desktop/PhD/Writing/Chapter 5 - Potential and realised fecundity of Hector's dolphins/Figures/Figure 1. BP study area2.tiff", width = 200, height = 200, res=300, units = "mm")

par(mar=c(7,0.1,0.1,4),xpd=NA)
plot(BP,col="gray",lwd = 0.05)
plot(area.division2,add=TRUE,lwd=2)
plot(ex.Survey,col="maroon", add=TRUE)

text(172.87,-43.55,"NW",cex = 1.5);text(173.11,-43.63,"NE",cex = 1.5);text(173.15,-43.87,"E",cex = 1.5);text(172.93,-43.73,"AKH",cex = 1.5);text(172.82,-43.93,"S",cex = 1.5)

lines(x=c(172.3,172.3), y=c(-44.05,-43.3),lwd=0.8);lines(x=c(172.3,173.3),y=c(-43.3,-43.3),lwd=0.8);lines(x=c(172.3,173.3),y=c(-44.05,-44.05),lwd=0.8);lines(x=c(173.3,173.3),y=c(-43.3,-44.05))
scalebar(18.52,xy=c(172.35,-44.03), type = "line",lonlat=TRUE,label = "10nm",cex = 1.5,lwd=5)

par(fig=inset.limits, new=TRUE, mar=c(0,0,0,0),mgp = c(3,0.5,0))
plot(coast,col="gray",lwd=0.001)
lines(x=c(172.3,172.3), y=c(-44.2,-43.3),lwd=0.8);lines(x=c(172.3,173.8),y=c(-43.3,-43.3),lwd=0.8);lines(x=c(172.3,173.8),y=c(-44.2,-44.2),lwd=0.8);lines(x=c(173.8,173.8),y=c(-43.3,-44.2),lwd=0.8)
north(xy = c(166,-37),type=2,cex=1.5)
box(lwd=0.9,col="black",lty = "solid")
axis(1,at = seq(165,180,by=5),labels = c("165°E","170°E","175°E","180°E"),tck=-0.02,lwd = 0.9)
axis(4,at = seq(-50,-35,by=5),labels=c("35°S","40°S","45°S","50°S"),las=2,tck=-0.02,lwd = 0.9)

dev.off()




rm(coast,BP,area.division,ex.Survey,inset.limits)



##Effort related ----
get.first  <- function(x) min(which(x!=0))
first.capture.all = apply(YearData[,2:123],2,get.first)
first.capture.all = as.data.frame(first.capture.all)

t.mature <- function(x) {
  mature.calf = min(which(x == 2))
  mature.juv = min(which(x == 3)) - 1
  mature.age = min(which(x!=0)) + 6
  x = min(c(mature.calf,mature.juv,mature.age))
  if(x == 0){
    x = 1
  }
  return(x)
}

fmc = apply(YearData[,2:123],2,FUN = t.mature)


NoFemales = data.frame(Season = as.integer(),
                       TotalFemales = as.integer(),
                       NoRecaptures = as.integer(),
                       NumberFemalesAdded = as.integer())

for(i in 1:37){
  if(i == 1){
    NoFemales[i,"Season"] = YearData[i,"Season"]
    NoFemales[i,"TotalFemales"] = length(which(first.capture.all == 1))
    NoFemales[i,"NoRecaptures"] = 0
    NoFemales[i,"NumberFemalesAdded"] = length(which(first.capture.all[,1] == i))
  }else{
    NoFemales[i,"Season"] = YearData[i,"Season"]
    NoFemales[i,"TotalFemales"] = length(which(first.capture.all[,1] == i)) + NoFemales[i-1,"TotalFemales"]
    NoFemales[i,"NoRecaptures"] = length(which(YearData[i,2:123] > 0)) - length(which(first.capture.all[,1] == i))
    NoFemales[i,"NumberFemalesAdded"] = length(which(first.capture.all[,1] == i))
  }
}

NoFemales[nrow(NoFemales)+1,"Season"] =1998
NoFemales[nrow(NoFemales)+1,"Season"] =1999

NoFemales = merge(NoFemales,AnnSurvEff,by = "Season", all = TRUE)
NoFemales = NoFemales[order(NoFemales$Season),]
NoFemales$plotpos <- seq(0.8,46.8, by = 1.2)




#tiff("C:/Users/steph/Desktop/PhD/Writing/Chapter 5 - Potential and realised fecundity of Hector's dolphins/Figures/SurveyAndCatalogueEffortThroughTime.tiff", width = 220, height = 150, res = 300, units = "mm")

par(xpd = NA,mgp = c(3,0.5,0),mai=c(1.3,1,0.7,1))
barplot(as.matrix(t(NoFemales[,3:4])),axes = FALSE, ylim=c(1,150),names.arg = c(rep("",nrow(NoFemales))),ylab = "Number of females", xlab = "Season", cex.lab = 1)
axis(1, at = seq(0.8,46.8, by = 1.2), labels = NoFemales$Season, las= 2, cex.axis = 0.9,pos=0, tck = -0.02)
axis(2, at = seq(0,150,by=50),las=2,cex.axis = 0.9,tck = -0.02)
lines(x=c(-1.65,49),y=c(0,0))
lines(NoFemales$TotalFemales~NoFemales$plotpos, lwd = 2, col = "gray50")
points(NoFemales$TotalFemales~NoFemales$plotpos, pch = 19, col = "gray50")
text(x= 53,y=75,"Number of Surveys",srt=90,cex = 1)

lines(NoFemales$plotpos, NoFemales$SurveyDate, lwd = 2)
axis(4, at = seq(0,150,50), las = 2, cex.axis = 0.9, tck = -0.02)
points(NoFemales$plotpos, NoFemales$SurveyDate,pch = 19)

rect(1,-37,2,-40, col = "gray94");text(x= 6.9, y = -39, "Initial observation \nof females",cex = 0.9)
rect(13,-37,14,-40, col = "gray24");text(x= 18.7, y = -39, "Number of \nrecaptured females",cex = 0.9)
lines(x=c(28,30), y= c(-39.5,-39.5),lwd = 2, col="gray50");points(29,-39.5,pch=19,col="gray50");text(x= 33.7, y = -39, "Total females \nin catalogue",cex = 0.9)
lines(x=c(40,42), y= c(-39.5,-39.5),lwd = 2, col="black");points(41,-39.5,pch=19,col="black");text(x= 42.2, y = -39, "Surveys",cex = 0.9, pos = 4)
#dev.off()

##Calving season----
WeekCalfHist<-merge(WeekSurvEff,WeeklyCalves,by = "Week",all=TRUE)
WeekCalfHist<-merge(WeekCalfHist,SEWeeklyCalves[,1:2],by = "Week",all=TRUE)

rm(WeekSurvEff,WeeklyCalves)
WeekCalfHist = WeekCalfHist[-nrow(WeekCalfHist),] # Remove the last row as their are fewer days in this week due to Total Number of days not being divisible by 7.

#WeekCalfHist$NoCalves.Eff = WeekCalfHist$Calves / WeekCalfHist$SurveyDate #Need to get the SUM back if want to recreate original graph
WeekCalfHist$Plot.pos<-seq(0.7,25.2, by = 1.2)


#tiff("C:/Users/steph/Desktop/PhD/Writing/Chapter 5 - Potential and realised fecundity of Hector's dolphins/Figures/Calving season2.tiff", width = 200, height = 120, res = 300, units = "mm")
par(xpd = FALSE,mgp = c(1.9,0.5,0))
barplot(WeekCalfHist$Calves.x, axes = FALSE, ylim= c(0,6), ylab = "Mean number of Calves", xlab = "Week",cex.lab = 1)


axis(1,at = seq(0.7,25.2, by = 1.2),labels = WeekCalfHist$Week,cex.axis = 0.9,tck = -0.025)
axis(2, at = seq(0,5, by = 1), tck = -0.025, cex.axis = 0.9, las = 2)

lines(x=c(0.2,5.5), y = c(5, 5)); lines(x=c(5.5,10.7), y = c(5.2, 5.2)); lines(x=c(10.7,16.1), y = c(5, 5)); lines(x=c(16.1,21), y = c(5.2, 5.2)); lines(x=c(21,25.5), y = c(5, 5))
text(2.8,5.3,"Nov", cex = 0.9); text(7.9,5.5,"Dec", cex = 0.9); text(13.4,5.3,"Jan", cex = 0.9); text(18.3,5.5,"Feb", cex = 0.9); text(23.2,5.3,"Mar", cex = 0.9)

abline(h=0)
#dev.off()

WeekCalfHist$ConfInt = qt(0.975,length(WeekCalfHist$SurveyDate)-1)*WeekCalfHist$Calves.y


tiff("C:/Users/steph/Desktop/PhD/Writing/Chapter 5 - Potential and realised fecundity of Hector's dolphins/Figures/Calving season2.tiff", width = 200, height = 120, res = 300, units = "mm")
par(xpd = FALSE,mgp = c(1.9,0.5,0))
barplot(WeekCalfHist$Calves.x, axes = FALSE, ylim= c(0,6), ylab = "Mean number of calves", xlab = "Week",cex.lab = 1)
arrows(x0 = seq(0.7,25.2, by = 1.2), y0 = WeekCalfHist$Calves.x, x1 = seq(0.7,25.2, by = 1.2), y1 = WeekCalfHist$Calves.x + WeekCalfHist$ConfInt,angle=90,length=0.05)
arrows(x0 = seq(0.7,25.2, by = 1.2), y0 = WeekCalfHist$Calves.x, x1 = seq(0.7,25.2, by = 1.2), y1 = WeekCalfHist$Calves.x - WeekCalfHist$ConfInt,angle=90,length=0.05)

axis(1,at = seq(0.7,25.2, by = 1.2),labels = WeekCalfHist$Week,cex.axis = 0.9,tck = -0.025)
axis(2, at = seq(0,5, by = 1), tck = -0.025, cex.axis = 0.9, las = 2)

lines(x=c(0.2,5.5), y = c(5.3, 5.3)); lines(x=c(5.5,10.7), y = c(5.5, 5.5)); lines(x=c(10.7,16.1), y = c(5.3, 5.3)); lines(x=c(16.1,21), y = c(5.5, 5.5)); lines(x=c(21,25.5), y = c(5.3, 5.3))
text(2.8,5.5,"Nov", cex = 0.9); text(7.9,5.7,"Dec", cex = 0.9); text(13.4,5.5,"Jan", cex = 0.9); text(18.3,5.7,"Feb", cex = 0.9); text(23.2,5.5,"Mar", cex = 0.9)

abline(h=0)
dev.off()

##Calf sightings ----
length(which(PartYearData[,2:123] == 2))
length(which(YearData[,2:123] == 2))


#Fecundity estimate 1: Proportion of calves to total individuals sighted ----
##Data ----




AnnualCalvingHist = merge(AnnSurvEff,SeasonalCalves,by="Season",all = TRUE)
colnames(AnnualCalvingHist) = c("Season","NoSurveys","NoCalves","NoDolphins")
rm(AnnSurvEff,SeasonalCalves)

AnnualCalvingHist[nrow(AnnualCalvingHist)+1,"Season"] = 1998
AnnualCalvingHist[nrow(AnnualCalvingHist)+1,"Season"] = 1999
AnnualCalvingHist = AnnualCalvingHist[order(AnnualCalvingHist$Season),]

##Calving indices ----
AnnualCalvingHist$CalvingIndex = (AnnualCalvingHist$NoCalves)/(AnnualCalvingHist$NoDolphins)
AnnualCalvingHist$CalvingIndex.SurveyEff = (AnnualCalvingHist$NoCalves/AnnualCalvingHist$NoDolphins)/AnnualCalvingHist$NoSurveys
AnnualCalvingHist$CalvesPer100Dolphins <- AnnualCalvingHist$CalvingIndex*100

avg.calvingindex = mean(AnnualCalvingHist$CalvingIndex,na.rm =TRUE)
upper95.calving = avg.calvingindex + (qt(0.975,36)*(sd(AnnualCalvingHist$CalvingIndex, na.rm = TRUE)/sqrt(37)))
lower95.calving = avg.calvingindex - (qt(0.975,36)*(sd(AnnualCalvingHist$CalvingIndex, na.rm = TRUE)/sqrt(37)))

#tiff("C:/Users/steph/Desktop/PhD/Writing/Chapter 5 - Potential and realised fecundity of Hector's dolphins/Figures/CalvingIndex.tiff", width = 200, height = 120, res = 300, units = "mm")

barplot(AnnualCalvingHist$CalvingIndex,ylim = c(0,0.1), ylab = "Annual Calving Index", xlab = "Season",axes = FALSE)
axis(1, at = seq(0.7,46.7,by=1.2), labels = AnnualCalvingHist$Season, las = 2,cex.axis=0.9)
axis(2, at = seq(0,0.10, by= 0.01),las=2,cex.axis=0.9)
abline(h=0)
rect(-2,lower95.calving,49,upper95.calving,col=rgb(red = 0.3, green = 0.5, blue = 0.8, alpha = 0.2), border = NA)
abline(h=avg.calvingindex,lty = "dashed")
abline(h=lower95.calving,lty = "dotted")
abline(h=upper95.calving,lty = "dotted")
#dev.off()



m1<-lm(AnnualCalvingHist$CalvingIndex~AnnualCalvingHist$Season)
summary(m1)
conf_interval = predict(m1, newdata = data.frame(AnnualCalvingHist$Season),interval="confidence",level=0.95)

#tiff("C:/Users/steph/Desktop/PhD/Writing/Chapter 5 - Potential and realised fecundity of Hector's dolphins/Figures/GrossReproductiveRate.tiff", width = 200, height = 120, res = 300, units = "mm")
par(xpd = NA)
plot(AnnualCalvingHist$CalvingIndex~AnnualCalvingHist$Season,ylim=c(0,0.1), ylab = "Annual Calving Index", xlab = "Season",axes = FALSE,pch=20,yaxs ="i")
axis(1, at = seq(1986,2024,by=1), labels = AnnualCalvingHist$Season, las = 2,cex.axis=0.9)
axis(2, at = seq(0,0.10, by= 0.01),las=2,cex.axis=0.9)
lines(x = c(1984,2025), y= c(0,0),lwd=1)
par(xpd = FALSE)
abline(m1)
matlines(AnnualCalvingHist$Season, conf_interval[,2:3], col= "black", lty = 2)


#dev.off()

#Fecundity estimate 2: Proportion of breeding females to known females -----


Mature.Females = as.data.frame(first.capture.all)

t.mature <- function(x) {
  mature.calf = min(which(x == 2))
  mature.juv = min(which(x == 3)) - 1
  mature.age = min(which(x!=0)) + 6
  x = min(c(mature.calf,mature.juv,mature.age))
  if(x == 0){
    x = 1
  }
  return(x)
}

Mature.Females$firstmature = apply(YearData[,2:123],2,FUN = t.mature)
Mature.Females$firstmature = ifelse(Mature.Females$firstmature > 37, 38,Mature.Females$firstmature)


y.dat2 = as.data.frame = t(YearData[,-1])
colnames(y.dat2) = YearData$Season

for(i in 1:nrow(y.dat2)){
  if(Mature.Females[i,"firstmature"] > Mature.Females[i,"first.capture.all"]){
    for(j in 1:Mature.Females[i,"firstmature"]){
      y.dat2[i,j-1] = 0
    }
  }
}

YearData.re <- as.data.frame(t(y.dat2))
YearData.re$Season = rownames(YearData.re)




YearData.re$No.Females = apply(YearData.re[2:123],1,function(x){length(which(x > 0))})
YearData.re$BreedingFemales = apply(YearData.re[2:123],1,function(x){length(which(x == 2))})

MatureFemales = YearData.re[,c(123,124,125)]
MatureFemales[nrow(MatureFemales)+1,"Season"] = 1998
MatureFemales[nrow(MatureFemales)+1,"Season"] = 1999
MatureFemales = MatureFemales[order(MatureFemales$Season),]

MatureFemales$PropBreeders = MatureFemales$BreedingFemales / (MatureFemales$BreedingFemales + MatureFemales$No.Females)
#MatureFemales[MatureFemales== 0] = NA

avg.PropBreeders = mean(MatureFemales$PropBreeders, na.rm = TRUE)
upper95.propB = avg.PropBreeders + (qt(0.975,36)*(sd(MatureFemales$PropBreeders, na.rm = TRUE)/sqrt(37)))
lower95.propB = avg.PropBreeders - (qt(0.975,36)*(sd(MatureFemales$PropBreeders, na.rm = TRUE)/sqrt(37)))


n=nrow(MatureFemales[-c(13,14),])
Nci=MatureFemales[-c(13,14),"BreedingFemales"]
Nmi=MatureFemales[-c(13,14),"No.Females"]

F_hat = (1/(2*n)) * (sum(Nci/Nmi))




#tiff("C:/Users/steph/Desktop/PhD/Writing/Chapter 5 - Potential and realised fecundity of Hector's dolphins/Figures/Fecundity2_BreedersToNonBreeder.tiff", width = 200, height = 120, res = 300, units = "mm")
par(mgp = c(2.5,0.5,0))
barplot(MatureFemales$PropBreeders,ylim = c(0,1), ylab = "Proportion of observed breeders \nto known females", xlab = "Season",axes = FALSE, cex.lab = 0.85)
axis(1, at = seq(0.7,46.7,by=1.2), labels = AnnualCalvingHist$Season, las = 2,cex.axis=0.7,tck = -0.025)
axis(2, at = seq(0,1, by= 0.1),las=2,cex.axis=0.7, tck = -0.025)
abline(h=0)
rect(-2,lower95.propB,49,upper95.propB,col=rgb(red = 0.3, green = 0.5, blue = 0.8, alpha = 0.2), border = NA)
abline(h=avg.PropBreeders,lty = "dashed")
abline(h=upper95.propB,lty = "dotted")
abline(h=lower95.propB,lty = "dotted")
#dev.off()

#Fecundity estimate 3: Bayesian MCMC reproductive model ----
##Matrix models----
#Using transition probabilities to determine pop. fecundity with no uncertainty in state assignment
certain.pop = matrix(NA, 10000, 30)
certain.pop[1:5000,1] = 1 #B
certain.pop[5001:10000,1] = 0 #NB


for(t in 2:ncol(certain.pop)){
  for(i in 1:nrow(certain.pop)){
    psiNB.B = 0.293
    psiB.NB = 0.935
    if(psiB.NB > 1){
      psiB.NB = 1
    }else if(psiB.NB < 0){
      psiB.NB = 0
    }
    if(psiNB.B > 1){
      psiNB.B = 1
    }else if(psiNB.B < 0){
      psiNB.B = 0
    }
    
    if(certain.pop[i,(t-1)] == 0){
      certain.pop[i,t] = sample(c(0,1),1,replace=TRUE, prob = c((1-psiNB.B),psiNB.B))
    }else if(certain.pop[i,(t-1)] == 1){
      certain.pop[i,t] = sample(c(0,1),1,replace=TRUE, prob = c(psiB.NB, (1-psiB.NB)))
    }
  }
}

PropBreedersCertain = apply(certain.pop,2,function(x) length(which(x == 1)))/10000




##Error
certain.pop = matrix(NA, 1000, 30)
certain.pop[1:500,1] = 1 #B
certain.pop[501:1000,1] = 0 #NB

PosteriorTransitionProbs = multistate.annualcaptures_I.prior.phi.constant$sims.list$mean.psi
PosteriorTransitionProbs = PosteriorTransitionProbs[sample(1:27000,1000,replace = FALSE),]

CertainFecundEst = rep(NA,1000)


for(a in 1:nrow(PosteriorTransitionProbs)){
  for(t in 2:ncol(certain.pop)){
    for(i in 1:nrow(certain.pop)){
      psiNB.B = PosteriorTransitionProbs[a,2]
      psiB.NB = PosteriorTransitionProbs[a,1]
      if(psiB.NB > 1){
        psiB.NB = 1
      }else if(psiB.NB < 0){
        psiB.NB = 0
      }
      if(psiNB.B > 1){
        psiNB.B = 1
      }else if(psiNB.B < 0){
        psiNB.B = 0
      }
      
      if(certain.pop[i,(t-1)] == 0){
        certain.pop[i,t] = sample(c(0,1),1,replace=TRUE, prob = c((1-psiNB.B),psiNB.B))
      }else if(certain.pop[i,(t-1)] == 1){
        certain.pop[i,t] = sample(c(0,1),1,replace=TRUE, prob = c(psiB.NB, (1-psiB.NB)))
      }
    }
  }
  y.var1 = apply(certain.pop,2,function(x) length(which(x == 1)))/1000
  CertainFecundEst[a] = y.var1[30]
}

#Uncertainty in state assignment
uncertain.pop = matrix(NA, 10000, 30)
uncertain.pop[1:5000,1] = 1 #B
uncertain.pop[5001:10000,1] = 0 #NB


for(t in 2:ncol(uncertain.pop)){
  for(i in 1:nrow(uncertain.pop)){
    psiNB.B = 0.377
    psiB.NB = 0.885
    if(psiB.NB > 1){
      psiB.NB = 1
    }else if(psiB.NB < 0){
      psiB.NB = 0
    }
    if(psiNB.B > 1){
      psiNB.B = 1
    }else if(psiNB.B < 0){
      psiNB.B = 0
    }
    
    if(uncertain.pop[i,(t-1)] == 0){
      uncertain.pop[i,t] = sample(c(0,1),1,replace=TRUE, prob = c((1-psiNB.B),psiNB.B))
    }else if(uncertain.pop[i,(t-1)] == 1){
      uncertain.pop[i,t] = sample(c(0,1),1,replace=TRUE, prob = c(psiB.NB, (1-psiB.NB)))
    }
  }
}

PropBreedersUncertain = apply(uncertain.pop,2,function(x) length(which(x == 1)))/10000


##Error
uncertain.pop = matrix(NA, 1000, 30)
uncertain.pop[1:500,1] = 1 #B
uncertain.pop[501:1000,1] = 0 #NB

PosteriorTransitionProbs = multievent.annualcaptures_Iprior_phi$sims.list$mean.psi
PosteriorTransitionProbs = PosteriorTransitionProbs[sample(1:27000,1000,replace = FALSE),]

UncertainFecundEst = rep(NA,1000)


for(a in 1:nrow(PosteriorTransitionProbs)){
  for(t in 2:ncol(uncertain.pop)){
    for(i in 1:nrow(uncertain.pop)){
      psiNB.B = PosteriorTransitionProbs[a,2]
      psiB.NB = PosteriorTransitionProbs[a,1]
      if(psiB.NB > 1){
        psiB.NB = 1
      }else if(psiB.NB < 0){
        psiB.NB = 0
      }
      if(psiNB.B > 1){
        psiNB.B = 1
      }else if(psiNB.B < 0){
        psiNB.B = 0
      }
      
      if(uncertain.pop[i,(t-1)] == 0){
        uncertain.pop[i,t] = sample(c(0,1),1,replace=TRUE, prob = c((1-psiNB.B),psiNB.B))
      }else if(uncertain.pop[i,(t-1)] == 1){
        uncertain.pop[i,t] = sample(c(0,1),1,replace=TRUE, prob = c(psiB.NB, (1-psiB.NB)))
      }
    }
  }
  y.var1 = apply(uncertain.pop,2,function(x) length(which(x == 1)))/1000
  UncertainFecundEst[a] = y.var1[30]
}



time = 1:ncol(certain.pop)



#tiff("C:/Users/steph/Desktop/PhD/Writing/Chapter 5 - Potential and realised fecundity of Hector's dolphins/Figures/Fecundity3_MatrixModels.tiff", width = 200, height = 100, res = 300, units = "mm")
par(mfrow=c(1,2),mgp = c(1.4,0.3,0),mai = c(1,0.7,0.8,0.1))
plot(time,PropBreedersCertain,ylim=c(0,0.6), ylab= "Estimated proportion \nof Breeders", xlab="Year",pch=16,bty="n",yaxs="i",cex.axis = 0.7, cex = 0.7, cex.lab = 0.8,tck = -.02)
abline(h=mean(PropBreedersCertain[-c(1:5)]),lty = "dotted")


#par(mai = c(0.5,1,0.5,0.5))
plot(time,PropBreedersUncertain,ylim=c(0,0.6), ylab= "", xlab="Year",pch=16,bty="n",yaxs="i",cex.axis = 0.7, cex = 0.7, cex.lab = 0.8,tck = -.02)
abline(h=mean(PropBreedersUncertain[-c(1:5)]),lty = "dotted")

par(xpd = NA)
text(x= -43.2, y = 0.63,"a)")
text(x= -4, y = 0.63,"b)")

#dev.off()

#Parameter estimates


Model.1.x <- c(0.293,0.918,0.938,0.913,0.284)
Model.1.y <- c(4.3, 3.5, 3.1, 2.3, 1.9)
Model.2.x <- c(0.338,0.921,0.935,0.886,0.374,0.847,0.249,0.076)
Model.2.y <- c(4.1,3.3,2.9,2.1,1.7,1.1,0.7,0.1)


phi.b <- expression(italic(ɸ[B]))
phi.nb <- expression(italic(ɸ[NB]))
psi.b <- expression(italic(ψ[B]))
psi.nb <- expression(italic(ψ[NB]))
delta.b <- expression(italic(δ[B]))
delta.nb <- expression(italic(δ[NB]))
pi = expression(italic(π))

#tiff("C:/Users/steph/Desktop/PhD/Writing/Chapter 5 - Potential and realised fecundity of Hector's dolphins/Figures/ParameterEstimates.tiff", width = 200, height = 130, res = 300, units = "mm")

plot(Model.1.x,Model.1.y, xlim = c(0,1),ylim=c(0,4.4),axes = FALSE, pch = 16, col = "blue",ylab = "Parameters",xlab = "Probability")
points(Model.2.x,Model.2.y,pch = 16)
lines(x = c(0.261,0.325),y = c(4.3,4.3));lines(x = c(0.307,0.371),y = c(4.1,4.1));lines(x = c(0.882,0.948),y = c(3.5,3.5));lines(x = c(0.888,0.948),y = c(3.3,3.3));lines(x = c(0.918,0.957),y = c(3.1,3.1));lines(x = c(0.913,0.955),y = c(2.9,2.9));lines(x = c(0.821,0.975),y = c(2.3,2.3));lines(x = c(0.775,0.969),y = c(2.1,2.1));lines(x = c(0.216,0.361),y = c(1.9,1.9));lines(x = c(0.257,0.538),y = c(1.7,1.7));lines(x = c(0.700,0.980),y = c(1.1,1.1));lines(x = c(0.188,0.317),y = c(0.7,0.7));lines(x = c(0.003,0.206),y = c(0.1,0.1))
axis(1,at = seq(0,1,0.1))
axis(2, at = c(4.2,3.4,3,2.2,1.8,1.1,0.7,0.1), labels = c(expression(italic(p)),phi.b,phi.nb,psi.b,psi.nb,delta.b,delta.nb,pi), las=2)
par(xpd = NA)
lines(x=c(-0.0405,-0.0405),y = c(-0.18,4.5))
lines(x=c(-0.04,0),y = c(-0.18,-0.18))
#dev.off()





confidence_interval <- function(vector, interval,vec_mean) {
  # Standard deviation of sample
  vec_sd <- sd(vector)
  # Sample size
  n <- length(vector)
  # Error according to t distribution
  error <- qt((interval + 1)/2, df = n - 1) * vec_sd / sqrt(n)
  # Confidence interval as a vector
  result <- c("lower" = vec_mean - error, "upper" = vec_mean + error)
  return(result)
}


Mean.Fecund = c(mean(MatureFemales$PropBreeders,na.rm=TRUE),PropBreedersCertain[30],PropBreedersUncertain[30])
std.dev = c(sd(MatureFemales$PropBreeders,na.rm=TRUE),sd(CertainFecundEst),sd(UncertainFecundEst))

lowCI = c(Mean.Fecund[1] - qt(0.975,df=length(MatureFemales$PropBreeders)-3)*sd(MatureFemales$PropBreeders,na.rm=TRUE)/sqrt(length(MatureFemales$PropBreeders)-2), 
          confidence_interval(CertainFecundEst,0.95,Mean.Fecund[2])[1],
          confidence_interval(UncertainFecundEst,0.95,Mean.Fecund[3])[1])

highCI = c(Mean.Fecund[1] + qt(0.975,df=length(MatureFemales$PropBreeders)-3)*sd(MatureFemales$PropBreeders,na.rm=TRUE)/sqrt(length(MatureFemales$PropBreeders)-2), 
           confidence_interval(CertainFecundEst,0.95,Mean.Fecund[2])[2],
           confidence_interval(UncertainFecundEst,0.95,Mean.Fecund[3])[2])


tiff("C:/Users/steph/Desktop/PhD/Writing/Chapter 5 - Potential and realised fecundity of Hector's dolphins/Figures/ComparisonOfFecunditySD.tiff", width = 200, height = 130, res = 300, units = "mm")

par(mai = c(0.9,1.2,0.4,0.4),mgp = c(3,1.5,0),xpd = NA)
plot(x= c(0.15,0.45,0.75), y= Mean.Fecund,xlim=c(0,0.92),ylim= c(0,0.4), axes = FALSE,yaxs ="i",ylab = "Fecundity", xlab = "Method", pch=16,cex.lab=0.9)
axis(1,at = seq(0.15,0.75, by=0.3),labels = c("Observed\n","Multi-state \nmodel","Multi-event \nmodel"),cex.axis = 0.8)
par(mgp = c(2.5,1,0))
axis(2,at = seq(0,0.4, by= 0.1),las=2,cex.axis = 0.8)
arrows(x0 = c(0.15,0.45,0.75), y0 = Mean.Fecund, x1 = c(0.15,0.45,0.75), y1 = Mean.Fecund + std.dev,angle = 90, length = 0.02)
arrows(x0 = c(0.15,0.45,0.75), y0 = Mean.Fecund, x1 = c(0.15,0.45,0.75), y1 = Mean.Fecund - std.dev,angle = 90, length = 0.02)
lines(x=c(-0.05,0.9),y=c(0,0),lwd=1)


dev.off()

tiff("C:/Users/steph/Desktop/PhD/Writing/Chapter 5 - Potential and realised fecundity of Hector's dolphins/Figures/ComparisonOfFecundityCI.tiff", width = 200, height = 130, res = 300, units = "mm")
par(mai = c(0.9,1.2,0.4,0.4),mgp = c(3,1.5,0),xpd = NA)
plot(x= c(0.15,0.45,0.75), y= Mean.Fecund,xlim=c(0,0.92),ylim= c(0.14,0.30), axes = FALSE,yaxs ="i",ylab = "Fecundity", xlab = "Method", pch=16,cex.lab=1.1,cex=0.8)
axis(1,at = seq(0.15,0.75, by=0.3),labels = c("Observed\n","Multi-state \nmodel","Multi-event \nmodel"),cex.axis = 1)
par(mgp = c(2.5,1,0))
axis(2,at = seq(0.15,0.3, by= 0.05),las=2,cex.axis = 1)
arrows(x0 = c(0.15,0.45,0.75), y0 = Mean.Fecund, x1 = c(0.15,0.45,0.75), y1 = highCI,angle = 90, length = 0.02)
arrows(x0 = c(0.15,0.45,0.75), y0 = Mean.Fecund, x1 = c(0.15,0.45,0.75), y1 = lowCI,angle = 90, length = 0.02)
lines(x=c(-0.0365,0.9),y=c(0.14,0.14),lwd=1)
lines(x=c(-0.0365,-0.0365),y=c(0.14,0.15),lwd=1)
dev.off()



#Models------------








library(jagsUI)
#library(rjags) # What is the differences between these packages?



































#Define Model ----
tmp = "
model {
  ##
  # ------------------------------------------------------------------------------
  # Parameters:
  # phi.NB: survival probability of a non-breeder
  # phi.B: survival probabilty of a breeder
  # psi.NB.B: transition probability from non-breeder to breeder
  # psi.B.NB: transition probability from breeder to non-breeder
  # p.NB: recapture probability of a non-breeder
  # p.B: recapture probability of a breeder
  # pi.B: Probability of being in initial state breeder
  # delta.B: Probability of seeing a breeder and correctly ascertaining they are a breeder
  # delta.NB: Probability of seeing a non-breeder and correctly ascertaining they are a non-breeder
  # ------------------------------------------------------------------------------
  #States (z):
  # 1 Alive & Non-Breeder
  # 2 Alive & Breeder
  # 3 Dead
  # Observations (y):
  # 1 Observed as non-breeder
  # 2 Observed as breeder
  # 3 Not observed
  # --------------------------
  
  # Constraints on priors
  for (i in 1:n){
    for (t in 1:(T-1)){
      phi.B[i,t] = mean.phi[1]
      phi.NB[i,t] = mean.phi[2]
      psi.B.NB[i,t] = mean.psi[1]
      psi.NB.B[i,t] = mean.psi[2]
      delta.B[i,t] = mean.delta[1]
      delta.NB[i,t] = mean.delta[2]

    }
    
    for (t in 1:12){
      p.B[i,t] = mean.p
      p.NB[i,t] = mean.p
    }
    for (t in 13:14){
      p.B[i,t] = 0.0001
      p.NB[i,t] = 0.0001
        }
    for (t in 15:(T-1)){
      p.B[i,t] = mean.p
      p.NB[i,t] = mean.p
    }
  }
  
  # Priors - uniform priors are uninformative
  for(u in 1:2){
    mean.phi[u] ~ dunif(0, 1) 
    mean.psi[u] ~ dunif(0, 1) 
    mean.delta[u] ~ dunif(0, 1)
  }
  mean.p ~ dunif(0, 1)
  pi.B ~ dunif(0, 1)
  
 
  # Probability matrices-----------------------------------------------------------------
  ## Gamma =  state-based processes (survival and transistion; and 
  ## Omega = observation based processes (capture)
  for(i in 1:n){
 
  # Initial States matrices
    Pi[i,1] = pi.B                                  #Pr of having initial state B, t is not relevant here as the initial state is always the first capture
    Pi[i,2] = 1 - pi.B                              #Pr of having initial state NB
    Pi[i,3] = 0                                     #Pr of having initial state Dead
    

    for(t in fc[i]:(T - 1)){
      # Probability matrix of state z(t+1) given z(t)
      gamma[1,i,t,1] = phi.NB[i,t] * (1 - psi.NB.B[i,t])  #State (z) Alive and Non-breeder at t+1, when alive and non breeder at t
      gamma[1,i,t,2] = phi.NB[i,t] * psi.NB.B[i,t]        #State (z) Alive and Breeder at t+1, when alive and non breeder at t
      gamma[1,i,t,3] = 1 - phi.NB[i,t]                    #State (z) Dead at t+1, when alive and non breeder at t
      gamma[2,i,t,1] = phi.B[i,t] * psi.B.NB[i,t]         #State (z) Alive and Non-breeder at t+1, when alive and breeder at t
      gamma[2,i,t,2] = phi.B[i,t] * (1-psi.B.NB[i,t])     #State (z) Alive and Breeder at t+1, when alive and breeder at t
      gamma[2,i,t,3] = 1 - phi.B[i,t]                     #State (z) Dead at t+1, when alive and breeder at t
      gamma[3,i,t,1] = 0                                  #State (z) Alive and Non-breeder at t+1, when Dead at t
      gamma[3,i,t,2] = 0                                  #State (z) Alive and Breeder at t+1, when Dead at t
      gamma[3,i,t,3] = 1                                  #State (z) Dead at t+1, when Dead at t
      

      # probabilities of y(t) given z(t) 
      omega[1,i,t,1] = p.NB[i,t] * delta.NB[i,t]          #Pr (Alive NB t -> observed and detected NB)
      omega[1,i,t,2] = 0                                  #Pr (Alive NB t -> observed, but are detected as a breeder)
      omega[1,i,t,3] = p.NB[i,t] * (1 - delta.NB[i,t])    #Pr (Alive NB t -> observed, but are in an unknown state)
      omega[1,i,t,4] = 1 - p.NB[i,t]                      #Pr (Alive NB t -> not detected t)
      omega[2,i,t,1] = 0                                  #Pr (Alive B t -> observed and detected NB)
      omega[2,i,t,2] = p.B[i,t] * delta.B[i,t]            #Pr (Alive B t -> observed and detected B)
      omega[2,i,t,3] = p.B[i,t] * (1- delta.B[i,t])       #Pr (Alive B t -> observed, but are in an unknown state)
      omega[2,i,t,4] = 1 - p.B[i,t]                       #Pr (Alive B t ->  not detected t)
      omega[3,i,t,1] = 0                                  #Pr (dead t -> detected B t)
      omega[3,i,t,2] = 0                                  #Pr (dead t -> detected NB t)
      omega[3,i,t,3] = 0                                  #Pr (dead t -> detected in unknown state)
      omega[3,i,t,4] = 1                                  #Pr (dead  -> not-detected t)
      
      
      # probabilities of y(t) given z(t) for the initial state
      omega.inits[1,i,t,1] = delta.NB[i,t]                #Pr (Alive NB t -> observed and detected NB)
      omega.inits[1,i,t,2] = 0                            #Pr (Alive NB t -> observed, but are detected as a breeder)
      omega.inits[1,i,t,3] = (1 - delta.NB[i,t])          #Pr (Alive NB t -> observed, but are in an unknown state)
      omega.inits[1,i,t,4] = 0                            #Pr (Alive NB t -> not detected t)
      omega.inits[2,i,t,1] = 0                            #Pr (Alive B t -> observed and detected NB)
      omega.inits[2,i,t,2] = delta.B[i,t]                 #Pr (Alive B t -> observed and detected B)
      omega.inits[2,i,t,3] = (1- delta.B[i,t])            #Pr (Alive B t -> observed, but are in an unknown state)
      omega.inits[2,i,t,4] = 0                            #Pr (Alive B t ->  not detected t)
      omega.inits[3,i,t,1] = 0                            #Pr (dead t -> detected B t)
      omega.inits[3,i,t,2] = 0                            #Pr (dead t -> detected NB t)
      omega.inits[3,i,t,3] = 0                            #Pr (dead t -> detected in unknown state)
      omega.inits[3,i,t,4] = 1                            #Pr (dead  -> not-detected t)

    } 
  }
  # Likelihood 
  for (i in 1:n){
    
    # Define latent state at first capture
    z[i,fc[i]] ~ dcat(Pi[i,1:3]) 
    y[i,fc[i]] ~ dcat(omega[z[i,fc[i]],i,fc[i],1:4])
    
    for (t in (fc[i] + 1):T){
    
      # State process using the gamma matrix (transition state probabilities), calculate the likely state of individual i at time t based on the state of individual i at t-1
      z[i,t] ~ dcat(gamma[z[i,t-1], i, t-1, 1:3]) #Can be individual or time  specific
      
      # Observation process, y(t) given z(t) -> using the omega matrix
      y[i,t] ~ dcat(omega[z[i,t], i, t-1, 1:4]) 
    }
  }
}"
cat(tmp, file = "./Multistate Model Development/Model4_MultiEvent_MatureFemalesCH_inclYearGap_UncertainDetection.txt") 

# Data for model ---- 
obs.matrix <- as.numeric(as.matrix(YearSightingsMatrix[,1:(ncol(YearSightingsMatrix)-2)]))
obs.matrix<-matrix(obs.matrix,nrow = nrow(YearSightingsMatrix),ncol = ncol(YearSightingsMatrix)-2)

#get.first <- function(x) min(which(x!=0))
# Instead of get.first, I want to use the first sighting of a dolphin from where it would be mature and therefore available to breed
t.mature <- function(x) {
  mature.calf = min(which(x == 2))
  mature.juv = min(which(x == 3)) - 1
  mature.age = min(which(x!=0)) + 6
  x = min(c(mature.calf,mature.juv,mature.age))
  if(x == 0){
    x = 1
  }
  return(x)
}

fmc = apply(obs.matrix,1,FUN = t.mature)
#fc = apply(obs.matrix,1,FUN = get.first)


## Determine all individuals where we cannot differentiate mature or immature status (seen for < 6 years | Never sighted with a calf or 1+)
notmature = rep(NA,nrow(obs.matrix)) #Determine and remove all individuals where we do not know that they are mature.
for(i in 1:nrow(obs.matrix)){
  if(length(which(obs.matrix[i,fmc[i]:39] > 0)) > 0){
    notmature[i] = 1
  }else{
    notmature[i] = 0
  }
}

obs.matrix = obs.matrix[-which(notmature == 0),]
fmc = apply(obs.matrix,1,FUN = t.mature)

#We start the model at the first instance where an individual would have been mature. For some, this means a year in which they were not observed but we know that they are a) alive and b) capable of breeding. This is the case for individuals where their maturity is dependent on age rather than the first sighting with a calf.
state.fc = rep(NA,nrow(obs.matrix))

for(i in 1:nrow(obs.matrix)){
  if(obs.matrix[i,fmc[i]] == 0){
    state.fc[i] = sample(c(1,2),1,replace = TRUE)
  }else if(obs.matrix[i,fmc[i]] == 1){ 
    state.fc[i] = 1
  }else if(obs.matrix[i,fmc[i]] == 2 | obs.matrix[i,fmc[i] + 1] == 3){
    state.fc[i] = 2
  }else if(obs.matrix[i,fmc[i]] == 3){
    state.fc[i] = 1
  }
}

#For multistate------
##Recode obs.matrix (0 is not allowed), seen NB = 1 seen B = 2, not seen = 3
#CHwJ = obs.matrix # Retain the capture history with juveniles for initial values
#obs.matrix[obs.matrix == 3] = 1 #Change juvenile sightings to 1
#obs.matrix[obs.matrix==0] = 3 # Change not sighted to 3


#jags.data = list (y = obs.matrix,
#                  fc = fmc,
#                  first.state = state.fc,
#                  n = nrow(obs.matrix),
#                  T = ncol(obs.matrix))

## Set initial values ----
##Create a function to set initial values for z

##If I do not allow for mistaken ID between states ( i.e. even if sighted with a 1+ at t+1 I do not "fix" state at t) the new CH function works.
#ms.inits.z <- function(ch, f){
#  for(i in 1:nrow(ch)){
#  }
#    for(t in 1:ncol(ch)){
#      if(ch[i,t] == 2){
#        ch[i,t] = 2
#      }else if(ch[i,t] == 3 | ch[i,t] == 1){
#        ch[i,t] = 1
#      }else{
#        unknowns = which(ch == 0)
#        ch[unknowns] = sample(c(1,2),length(unknowns),replace = TRUE) #Maybe this should be changed to NA?
#      }
#    }
#    ch[i,1:f[i]] <- NA
#  }
  
#  return(ch)
  
#}


#For multievent -----
#Recode obs.matrix (0 is not allowed), seen correctly id'd NB = 1, seen correctly id'd B = 2, seen but state not ascertained = 3, not seen = 4
obs.matrix[obs.matrix == 1] = 5 #If sighted without calf or juvenile unknown if it was a breeder that 1) lost calf, 2) had not yet calved or 3) true non-breeder. Assign unused value for differentiation
obs.matrix[obs.matrix == 3] = 1 #If seen with juvenile we can assume that the mother is a non-breeder that season.
obs.matrix[obs.matrix == 5] = 3 #Simple change from random number to the correct code
obs.matrix[obs.matrix==0] = 4 #Not observed = 4


jags.data = list (y = obs.matrix,
                  fc = fmc,
                  n = nrow(obs.matrix),
                  T = ncol(obs.matrix))

# Set initial values ----
#Create a function to set initial values for z

#Allowing for mistaken identification between observation and state.
ms.inits.z <- function(ch, f){
  for(i in 1:nrow(ch)){
    
    for(t in 1:ncol(ch)){
      if(ch[i,t] == 2){
        ch[i,t] = 2
      }else if(ch[i,t] == 1){
        ch[i,t] = 1
      }else{
        unknowns = which(ch == 4 | ch == 3)
        ch[unknowns] = sample(c(1,2),length(unknowns),replace = TRUE) 
      }
    }
    
    #Putting in more states based information around Juvenile sightings if seen with juvenile it has to be 1 in that same year and have a calf in the year prior
    #for(t in 1: ncol(ch)-1){
    #  if(ch[i,t+1] == 1){
    #    ch[i,t] = 2
    #  }
    #}
    ch[i,1:f[i]] <- NA
  }
  return(ch)
}

multievent.init.z <- function(ch, f){
  for (i in 1:nrow(ch)) {
    for (t in 1:ncol(ch)) {
      if (t >= f[i] & ch[i,t] == 4) {ch[i,t] <- which(rmultinom(1, 1, c(1/2,1/2))==1)}
      if (t >= f[i] & ch[i,t] == 3) {ch[i,t] <- which(rmultinom(1, 1, c(1/2,1/2))==1)}
      if (t < f[i]) {ch[i,t] <- NA}
    }
  }
  return(ch)
}


#------




# Actual initial values:
inits = function(){list(mean.phi = runif(2,0,1),
                        mean.psi = runif(2,0,1),
                        mean.p = runif(1,0,1),
                        mean.delta = runif(2,0,1),
                        pi.B = runif(1,0,1),
                        z = multievent.init.z(ch = obs.matrix, f=fmc))}

# Parameters to monitor ----
parameters = c("mean.phi", "mean.psi", "mean.p","mean.delta","mean.pi")


# MCMC settings ----
n.iter = 5000
n.burnin = 1000
n.chains = 3
n.thinning = 1

# Run Model ----
multistate.model1.5 <- jags(data = jags.data,
                            inits = inits,
                            parameters.to.save = parameters,
                            model.file = "./Multistate Model Development/Model4_MultiEvent_MatureFemalesCH_inclYearGap_UncertainDetection.txt",
                            n.chains = n.chains,
                            n.thin = n.thinning,
                            n.iter = n.iter,
                            n.burnin = n.burnin)




print(multistate.model1.5,digits=3)

traceplot(multistate.model1.5)




##Matrix model
#Using transition probabilities to determine pop. fecundity
pop = matrix(NA, 1000, 1000)
pop[1:900,1] = 1 #B
pop[901:1000,1] = 0 #NB


for(t in 2:ncol(pop)){
  for(i in 1:nrow(pop)){
    psiNB.B = rnorm(1,0.552,0.132)
    psiB.NB = rnorm(1,0.896,0.57)
    if(psiB.NB > 1){
      psiB.NB = 1
    }else if(psiB.NB < 0){
      psiB.NB = 0
    }
    if(psiNB.B > 1){
      psiNB.B = 1
    }else if(psiNB.B < 0){
      psiNB.B = 0
    }
    
    if(pop[i,(t-1)] == 0){
      pop[i,t] = sample(c(0,1),1,replace=TRUE, prob = c(psiNB.B, (1-psiNB.B)))
    }else if(pop[i,(t-1)] == 1){
      pop[i,t] = sample(c(0,1),1,replace=TRUE, prob = c(psiB.NB, (1-psiB.NB)))
    }
  }
}

x.var = 1:ncol(pop)
y.var = apply(pop,2,function(x) length(which(x == 1)))/1000

plot(x.var,y.var,ylim=c(0,1))
mean(y.var)
