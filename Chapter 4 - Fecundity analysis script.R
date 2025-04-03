#Setup workspace----
rm(list = ls(all = TRUE))

library(raster)
library(terra)
library(jagsUI)

setwd("Y:/Hector's dolphin/6. Researchers/Steph Bennington/3. Fecundity/Data")

surv = read.csv("AllSurveys2024.csv")
enc = read.csv("AllEncounters2024.csv")
data = read.csv("AllFemalePictureHistory2024.csv")
ConfFem = read.csv("ConfidentFemales.csv")
YearSightingsMatrix = read.csv("AllFemaleAnnualCH.csv")

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

#Define the season (rather than year) that a survey occurred
surv$Season <- ifelse(surv$Month >= 7, as.integer(format(surv$SurveyDate, "%Y")) + 1, as.integer(format(surv$SurveyDate, "%Y")))

#Remove surveys that occurred outside of Banks Peninsula or the summer season (1st Nov to 31st Mar)
surv[is.na(surv)] = 0
surv$BanksAreas = NA

for(i in 1:nrow(surv)){
  if(sum(surv[i,c(8:12,29:61)]) > 0 & sum(surv[i,13:28]) == 0){
    surv[i,"BanksAreas"] = 0
  }else{
    surv[i,"BanksAreas"] = 1
  }
}

surv <- subset(surv, surv$BanksAreas == 1 & surv$Month >= 11 | surv$BanksAreas == 1 & surv$Month <= 3)

#Refine survey data and aggregate by month and week
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

#Define the week (using first of Nov as start date) that each encounter occurred within
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

###Calving season data----
#Aggregate calf sighting histories into week and correct for group size
WeeklyCalves <- aggregate(cbind(Calves, GroupSize) ~ StartDate, data = enc, sum)
WeeklyCalves$CpGS = WeeklyCalves$Calves/WeeklyCalves$GroupSize
WeeklyCalves$Week = enc[match(WeeklyCalves[,"StartDate"],enc[,"StartDate"]),"Week"]
MeanCalf <- aggregate(cbind(CpGS, Calves, GroupSize) ~ Week, data = WeeklyCalves, mean)
SECalf <- aggregate(cbind(CpGS,Calves, GroupSize) ~ Week, data = WeeklyCalves, function(x) {sd(x)/sqrt(length(x))})

WeeklyCalfHistory <- aggregate(cbind(Calves, GroupSize) ~ Week, data = enc, sum);colnames(WeeklyCalfHistory) <- c("Week","TotalCalves","TotalDolphins")
#WeeklyCalfHistory$MeanCalvesPerSurvey = MeanCalf$Calves
#WeeklyCalfHistory$SECalvesPerSurvey = SECalf$Calves
#WeeklyCalfHistory$MeanDolphinsPerSurvey = MeanCalf$GroupSize
#WeeklyCalfHistory$SEDolphinsPerSurvey = SECalf$GroupSize
WeeklyCalfHistory$MeanCalvesPerDolphinPerSurvey = MeanCalf$CpGS
WeeklyCalfHistory$SECalvesPerDolphinPerSurvey = SECalf$CpGS
WeeklyCalfHistory<-merge(WeeklyCalfHistory,WeekSurvEff)

rm(WeekSurvEff,WeeklyCalves,MeanCalf,SECalf)
WeeklyCalfHistory = WeeklyCalfHistory[-nrow(WeeklyCalfHistory),] # Remove the last row as their are fewer days in this week due to Total Number of days not being divisible by 7.
WeeklyCalfHistory$ConfInt = qt(0.975,WeeklyCalfHistory$SurveyDate-1)*WeeklyCalfHistory$SECalvesPerDolphinPerSurvey
WeeklyCalfHistory$Plot.pos<-seq(0.7,25.2, by = 1.2)

SeasonalCalves <- aggregate(cbind(Calves, GroupSize) ~ Season, data = enc, sum)

##Capture Histories ----
YearSightingsMatrix = merge(ConfFem, YearSightingsMatrix, by.x = "IndividualID", by.y = "X",all.x =TRUE, all.y = FALSE)
rownames(YearSightingsMatrix) = YearSightingsMatrix$IndividualID
YearSightingsMatrix = YearSightingsMatrix[,-c(1,2)]

YearData = as.data.frame(t(YearSightingsMatrix))
YearData$Season = gsub("X","",rownames(YearData))

rm(ConfFem)


#Data Summary ----
##Database history ----
get.first  <- function(x) min(which(x!=0))
first.capture.all = apply(YearData[,1:105],2,get.first)
first.capture.all = as.data.frame(first.capture.all)

NoFemales = data.frame(Season = as.integer(),
                       TotalFemales = as.integer(),
                       NoRecaptures = as.integer(),
                       NumberFemalesAdded = as.integer())

for(i in 1:39){
  if(i == 1){
    NoFemales[i,"Season"] = YearData[i,"Season"]
    NoFemales[i,"TotalFemales"] = length(which(first.capture.all == 1))
    NoFemales[i,"NoRecaptures"] = 0
    NoFemales[i,"NumberFemalesAdded"] = length(which(first.capture.all[,1] == i))
  }else{
    NoFemales[i,"Season"] = YearData[i,"Season"]
    NoFemales[i,"TotalFemales"] = length(which(first.capture.all[,1] == i)) + NoFemales[i-1,"TotalFemales"]
    NoFemales[i,"NoRecaptures"] = length(which(YearData[i,2:106] > 0)) - length(which(first.capture.all[,1] == i))
    NoFemales[i,"NumberFemalesAdded"] = length(which(first.capture.all[,1] == i))
  }
}

NoFemales[13:14,"TotalFemales"] = NA
NoFemales = merge(NoFemales,AnnSurvEff,by = "Season", all = TRUE)
NoFemales = NoFemales[order(NoFemales$Season),]
NoFemales$plotpos <- seq(0.8,46.8, by = 1.2)

##Calving index ----
AnnualCalvingHist = merge(AnnSurvEff,SeasonalCalves,by="Season",all = TRUE)
colnames(AnnualCalvingHist) = c("Season","NoSurveys","NoCalves","NoDolphins")
rm(AnnSurvEff,SeasonalCalves)

AnnualCalvingHist[nrow(AnnualCalvingHist)+1,"Season"] = 1998
AnnualCalvingHist[nrow(AnnualCalvingHist)+1,"Season"] = 1999
AnnualCalvingHist = AnnualCalvingHist[order(AnnualCalvingHist$Season),]


AnnualCalvingHist$CalvingIndex = (AnnualCalvingHist$NoCalves)/(AnnualCalvingHist$NoDolphins)
m1<-lm(AnnualCalvingHist$CalvingIndex~AnnualCalvingHist$Season)
summary(m1)
conf_interval = predict(m1, newdata = data.frame(AnnualCalvingHist$Season),interval="confidence",level=0.95)



#Fecundity estimate 1: Observed proportion of breeding females to known mature females -----
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

Mature.Females = as.data.frame(first.capture.all)
Mature.Females$firstmature = apply(YearData[,1:105],2,FUN = t.mature)
Mature.Females$firstmature = ifelse(Mature.Females$firstmature > 37, 38,Mature.Females$firstmature)


y.dat2 = as.data.frame(t(YearData[,-106]))
colnames(y.dat2) = YearData$Season

for(i in 1:nrow(y.dat2)){
  if(Mature.Females[i,"firstmature"] > Mature.Females[i,"first.capture.all"]){
    for(j in 2:Mature.Females[i,"firstmature"]){
      y.dat2[i,j-1] = 0
    }
  }
}

YearData.re <- as.data.frame(t(y.dat2))
YearData.re$Season = rownames(YearData.re)


YearData.re$No.Females = apply(YearData.re[1:105],1,function(x){length(which(x > 0))})
YearData.re$BreedingFemales = apply(YearData.re[1:105],1,function(x){length(which(x == 2))})

MatureFemales = YearData.re[,c(106:108)]
MatureFemales[which(MatureFemales$Season == 1998), 2:3] = NA
MatureFemales[which(MatureFemales$Season == 1999), 2:3] = NA
MatureFemales = MatureFemales[order(MatureFemales$Season),]

MatureFemales[2:nrow(MatureFemales),"PropBreedersReal"] = MatureFemales[2:nrow(MatureFemales),"BreedingFemales"] / MatureFemales[2:nrow(MatureFemales),"No.Females"] 
#MatureFemales[MatureFemales== 0] = NA

avg.PropBreedersREAL = mean(MatureFemales$PropBreedersReal, na.rm = TRUE)
upper95.propB = avg.PropBreedersREAL + (qt(0.975,36)*(sd(MatureFemales$PropBreedersReal, na.rm = TRUE)/sqrt(37)))
lower95.propB = avg.PropBreedersREAL - (qt(0.975,36)*(sd(MatureFemales$PropBreedersReal, na.rm = TRUE)/sqrt(37)))


n=nrow(MatureFemales[-c(13,14),])
Nci=MatureFemales[-c(13,14),"BreedingFemales"]
Nmi=MatureFemales[-c(13,14),"No.Females"]

F_hat = (1/(2*n)) * (sum(Nci/Nmi))







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


#tiff("C:/Users/steph/Desktop/PhD/Writing/Chapter 5 - Potential and realised fecundity of Hector's dolphins/Figures/Calving season2.tiff", width = 200, height = 120, res = 300, units = "mm")
par(xpd = FALSE,mgp = c(1.9,0.5,0))
barplot(WeekCalfHist$Calves.x, axes = FALSE, ylim= c(0,6), ylab = "Mean number of calves", xlab = "Week",cex.lab = 1)
arrows(x0 = seq(0.7,25.2, by = 1.2), y0 = WeekCalfHist$Calves.x, x1 = seq(0.7,25.2, by = 1.2), y1 = WeekCalfHist$Calves.x + WeekCalfHist$ConfInt,angle=90,length=0.05)
arrows(x0 = seq(0.7,25.2, by = 1.2), y0 = WeekCalfHist$Calves.x, x1 = seq(0.7,25.2, by = 1.2), y1 = WeekCalfHist$Calves.x - WeekCalfHist$ConfInt,angle=90,length=0.05)

axis(1,at = seq(0.7,25.2, by = 1.2),labels = WeekCalfHist$Week,cex.axis = 0.9,tck = -0.025)
axis(2, at = seq(0,5, by = 1), tck = -0.025, cex.axis = 0.9, las = 2)

lines(x=c(0.2,5.5), y = c(5.3, 5.3)); lines(x=c(5.5,10.7), y = c(5.5, 5.5)); lines(x=c(10.7,16.1), y = c(5.3, 5.3)); lines(x=c(16.1,21), y = c(5.5, 5.5)); lines(x=c(21,25.5), y = c(5.3, 5.3))
text(2.8,5.5,"Nov", cex = 0.9); text(7.9,5.7,"Dec", cex = 0.9); text(13.4,5.5,"Jan", cex = 0.9); text(18.3,5.7,"Feb", cex = 0.9); text(23.2,5.5,"Mar", cex = 0.9)

abline(h=0)
#dev.off()

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

m1<-lm(AnnualCalvingHist$CalvingIndex~AnnualCalvingHist$Season)
summary(m1)
conf_interval = predict(m1, newdata = data.frame(AnnualCalvingHist$Season),interval="confidence",level=0.95)



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

MatureFemales$PropBreeders = MatureFemales$BreedingFemales / MatureFemales$No.Females
#MatureFemales[MatureFemales== 0] = NA

avg.PropBreeders = mean(MatureFemales$PropBreeders, na.rm = TRUE)
upper95.propB = avg.PropBreeders + (qt(0.975,36)*(sd(MatureFemales$PropBreeders, na.rm = TRUE)/sqrt(37)))
lower95.propB = avg.PropBreeders - (qt(0.975,36)*(sd(MatureFemales$PropBreeders, na.rm = TRUE)/sqrt(37)))

n=nrow(MatureFemales[-c(13,14),])
Nci=MatureFemales[-c(13,14),"BreedingFemales"]
Nmi=MatureFemales[-c(13,14),"No.Females"]

F_hat = (1/(2*n)) * (sum(Nci/Nmi))


#Fecundity estimate 3: Bayesian multi-state and event models ----
##Matrix models (requires running/loading modelling results from separate scripts)----
#Using transition probabilities to determine the mean and 95% Credible Intervals (quantiles) from the fecundity estimated from:
##Bayesian multi-state model (certainty in state assignment)
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

median(CertainFecundEst) #0.234
quantile(CertainFecundEst,probs = c(.025,.975))#0.180 - 0.287

##Bayesian multi-event model (uncertainty in state assignment)
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

median(UncertainFecundEst) #0.291
quantile(UncertainFecundEst,probs = c(.025,.975)) #0.214925 - 0.393050

time = 1:ncol(certain.pop)

Data.Fecund = c(mean(MatureFemales$PropBreedersReal,na.rm=TRUE),0.234,0.291)
Data.fecund.upp = c(mean(MatureFemales$PropBreedersReal,na.rm=TRUE) + qt(0.975,df=length(MatureFemales$PropBreedersReal)-3)*sd(MatureFemales$PropBreedersReal,na.rm=TRUE)/sqrt(length(MatureFemales$PropBreedersReal)-2),0.180,0.215)
Data.fecund.low = c(mean(MatureFemales$PropBreedersReal,na.rm=TRUE) - qt(0.975,df=length(MatureFemales$PropBreedersReal)-3)*sd(MatureFemales$PropBreedersReal,na.rm=TRUE)/sqrt(length(MatureFemales$PropBreedersReal)-2),0.287,0.393)



#Figures ----
##BP survey areas-----
coast = shapefile("C:/Users/steph/Desktop/PhD/Research/GIS/nzcoast.shp")
BP <-crop(coast, extent(c(172.3,173.8,-44.6,-43.3)))
area.division = shapefile("C:/Users/steph/Desktop/PhD/Research/GIS/BP_generalSurveyAreas.shp")
ex.Survey = shapefile("C:/Users/steph/Desktop/PhD/Research/GIS/examplesurveys.shp")

inset.limits<- c(0.071,0.37,0.6,0.9674)#L,R,B,T

tiff("./Figures/Figure 1. BP study area.tiff", width = 200, height = 200, res=300, units = "mm")
par(mar=c(7,0.1,0.1,4),xpd=NA)
plot(BP,col="gray",lwd = 0.05)
plot(area.division,add=TRUE,lwd=2)
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


##Summary of survey and CR data through time----
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


##Summary of the weekly calf observations ----
tiff("./Figures/Figure 4. Calving season.tiff", width = 200, height = 120, res = 300, units = "mm")
par(xpd = FALSE,mgp = c(2.3,0.5,0))
barplot(WeeklyCalfHistory$MeanCalvesPerDolphinPerSurvey, axes = FALSE, ylim= c(0,0.11), ylab = "Mean proportion of calves", xlab = "Week",cex.lab = 1)
arrows(x0 = seq(0.7,25.2, by = 1.2), y0 = WeeklyCalfHistory$MeanCalvesPerDolphinPerSurvey, x1 = seq(0.7,25.2, by = 1.2), y1 = WeeklyCalfHistory$MeanCalvesPerDolphinPerSurvey + WeeklyCalfHistory$ConfInt,angle=90,length=0.05)
arrows(x0 = seq(0.7,25.2, by = 1.2), y0 = WeeklyCalfHistory$MeanCalvesPerDolphinPerSurvey, x1 = seq(0.7,25.2, by = 1.2), y1 = WeeklyCalfHistory$MeanCalvesPerDolphinPerSurvey - WeeklyCalfHistory$ConfInt,angle=90,length=0.05)
axis(1,at = seq(0.7,25.2, by = 1.2),labels = WeeklyCalfHistory$Week,cex.axis = 0.9,tck = -0.025)
axis(2, at = seq(0,0.10, by = 0.02), tck = -0.025, cex.axis = 0.9, las = 2)

par(xpd=NA)
lines(x=c(0.2,5.5), y = c(0.12,0.12)); lines(x=c(5.5,10.7), y = c(0.123, 0.123)); lines(x=c(10.7,16.1), y = c(0.12, 0.12)); lines(x=c(16.1,21), y = c(0.123, 0.123)); lines(x=c(21,25.5), y = c(0.12, 0.12))
text(2.8,0.123,"Nov", cex = 0.9); text(7.9,0.126,"Dec", cex = 0.9); text(13.4,0.123,"Jan", cex = 0.9); text(18.3,0.126,"Feb", cex = 0.9); text(23.2,0.123,"Mar", cex = 0.9)
lines(x=c(-0.8,0.2),y=c(0,0))
dev.off()

##Calving index----
tiff("./Figures/CalvingIndex.tiff", width = 200, height = 120, res = 300, units = "mm")
barplot(AnnualCalvingHist$CalvingIndex,ylim = c(0,0.1), ylab = "Annual Calving Index", xlab = "Season",axes = FALSE)
axis(1, at = seq(0.7,46.7,by=1.2), labels = AnnualCalvingHist$Season, las = 2,cex.axis=0.9)
axis(2, at = seq(0,0.10, by= 0.01),las=2,cex.axis=0.9)
abline(h=0)
rect(-2,lower95.calving,49,upper95.calving,col=rgb(red = 0.3, green = 0.5, blue = 0.8, alpha = 0.2), border = NA)
abline(h=avg.calvingindex,lty = "dashed")
abline(h=lower95.calving,lty = "dotted")
abline(h=upper95.calving,lty = "dotted")
dev.off()

tiff("./Figures/Figure 5.GrossReproductiveRate.tiff", width = 200, height = 120, res = 300, units = "mm")
par(xpd = NA)
plot(AnnualCalvingHist$CalvingIndex~AnnualCalvingHist$Season,ylim=c(0,0.1), ylab = "Annual Calving Index", xlab = "Season",axes = FALSE,pch=20,yaxs ="i")
axis(1, at = seq(1986,2024,by=1), labels = AnnualCalvingHist$Season, las = 2,cex.axis=0.9)
axis(2, at = seq(0,0.10, by= 0.01),las=2,cex.axis=0.9)
lines(x = c(1984,2025), y= c(0,0),lwd=1)
par(xpd = FALSE)
abline(m1)
matlines(AnnualCalvingHist$Season, conf_interval[,2:3], col= "black", lty = 2)
dev.off()

##Breeder to non-breeder observations
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

##Matrix models
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

tiff("C:/Users/steph/Desktop/Publishing/Fecundity/Figures/MatrixModel_pub.tiff", width = 130, height = 100, res = 300, units = "mm")
par(mgp = c(1.4,0.3,0),mai = c(1,0.7,0.8,0.1))
plot(time,PropBreedersUncertain,ylim=c(0,0.6), ylab= "Estimated proportion \nof Breeders", xlab="Year",pch=16,bty="n",yaxs="i",cex.axis = 0.7, cex = 0.7, cex.lab = 0.8,tck = -.02)
abline(h=mean(PropBreedersUncertain[-c(1:5)]),lty = "dotted")
dev.off()



##Parameter estimates

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



##Fecundity estimates

#tiff("C:/Users/steph/Desktop/PhD/Writing/Chapter 5 - Potential and realised fecundity of Hector's dolphins/Figures/Fecundity2_BreedersToNonBreeder.tiff", width = 200, height = 120, res = 300, units = "mm")
par(mgp = c(2.5,0.5,0))
barplot(MatureFemales$PropBreedersReal,ylim = c(0,1), ylab = "Proportion of observed breeders \nto known females", xlab = "Season",axes = FALSE, cex.lab = 0.85)
axis(1, at = seq(0.7,46.7,by=1.2), labels = AnnualCalvingHist$Season, las = 2,cex.axis=0.7,tck = -0.025)
axis(2, at = seq(0,1, by= 0.1),las=2,cex.axis=0.7, tck = -0.025)
abline(h=0)
rect(-2,lower95.propB,49,upper95.propB,col=rgb(red = 0.3, green = 0.5, blue = 0.8, alpha = 0.2), border = NA)
abline(h=avg.PropBreedersREAL,lty = "dashed")
abline(h=upper95.propB,lty = "dotted")
abline(h=lower95.propB,lty = "dotted")
#dev.off()

tiff("C:/Users/steph/Desktop/PhD/Defence/Fecundity figures/Fecund.tiff",width = 200,height = 160,res = 300, units = "mm")
par(mai = c(1,1,1,1))
plot(Data.Fecund, xlim = c(0,4),ylim = c(0,0.5),ylab= "Estimated proportion \nof Breeders", xlab="Method",pch=19,bty="n",cex.axis = 0.9, cex = 0.7, cex.lab = 1,tck = -.02,xaxt = "n",yaxs = "i", xaxs= "i")
arrows(c(1,2,3),Data.fecund.low,c(1,2,3),Data.fecund.upp,code = 3, angle=90,length = 0.03,col=c("black","black"))

par(mgp = c(3,2,0),xpd= NA)
axis(1,at = 1:2,labels = c("Observed","Multi-state \nModel","Multi-Event \nModel"))
lines(c(0,3),c(0,0))

dev.off()

###Comparison to previous studies
Fec.ests = c(0.4,0.409,0.450,0.420,0.193,0.298)
upper.conf = c(NA, 0.635,NA,NA,0.235,0.302)
lower.conf = c(NA, 0.267,NA,NA,0.151,0.296)

tiff("C:/Users/steph/Desktop/Publishing/Fecundity/Fecundity.tiff", width = 200, height = 130, res = 300, units = "mm")
par(mfrow=c(1,2),mgp = c(1.4,0.3,0),mai = c(1.5,0.7,0.8,0.1))
plot(time,PropBreedersUncertain,ylim=c(0,0.7),ylab= "Estimated proportion \nof Breeders", xlab="Year",pch=16,bty="n",cex.axis = 0.9, cex = 0.7, cex.lab = 1,tck = -.02)
abline(h=mean(PropBreedersUncertain[-c(1:5)]),lty = "dotted")

plot(Fec.ests,bty="n",ylim = c(0,0.7),xlim = c(0.8,6.3),xlab = "",tck = -.02,cex.lab =1,cex = 0.7,cex.axis = 0.9,ylab = "Fecundity",pch=19,xaxt = "n",col=c("black","black","black","black","blue","blue"))
arrows(c(1,2,3,4,5,6),lower.conf,c(1,2,3,4,5,6),upper.conf,code = 3, angle=90,length = 0.03,col=c("black","black","black","black","blue","blue"))
axis(1,at = 1:6,labels = NA, las = 2)

par(xpd = NA)
text(1.2:6.2,-0.1, c("Slooten & Dawson 1992","Gormley 2009","Roberts et al. 2019", "Mackenzie et al. 2022", "This study - observed", "This study - modelled"), srt = 45,cex = 0.9,pos = 2)
text(x= -6.4, y = 0.73,"a)")
text(x= 1.1, y = 0.73,"b)")

dev.off()
