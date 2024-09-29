rm(list=ls(all=TRUE))# Clear workspace
library(dplyr)
library(raster)
library(sf)
library(terra)
library(mapplots)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#Original data----
ceph.seq45<-read.csv("CephSequences_n45.csv") #List of sequences names (incl. length and sequence) that were matched as Cephalorhynchus (summer 2022, n=46)
ceph.seq92 <- read.csv("CephSequences_n92.csv")
site.seq45<-read.csv("SiteSequence_45.csv") #Matrix of all sequences (summer 2022, n=46), sites and number of reads
site.seq92 <- read.csv("SiteSequence_92.csv")
hap.match<-read.csv("Haplotype_matching.csv") 
edna.meta <- read.csv("eDNA.metadata.csv",na.strings = c("NA",""))

#Merge tables and retain only sequences that are matched with Cephalorhynchus
ceph.seq45$V1<-gsub(">","",ceph.seq45$V1)
ceph.seq92$V1<-gsub(">","",ceph.seq92$V1)
site.seq45<-merge(ceph.seq45, site.seq45, by.x = "V1", by.y = "sequence_names", all.x = TRUE)
site.seq92<-merge(ceph.seq92, site.seq92, by.x = "V1", by.y = "sequence_names", all.x = TRUE)

##Cross contamination----
###LOD----
n <- 4; alpha <- 0.01; mean.neg <- mean(colMeans(site.seq45[271,84:87])); sd.neg <- sd(site.seq45[271,84:87]); t.value<-qt((1-(alpha/2)),n-1)
DT.alpha <- mean.neg + (t.value * sd.neg)

#Filter all ASVs with < DT(0.99)

for(i in 4:ncol(site.seq45)){
  for(j in 1:nrow(site.seq45)){
    if(site.seq45[j,i] <= DT.alpha){
      site.seq45[j,i] = 0
    }
  }
}



##Merge datasets----
###A and B data from duplicated PCR runs
x<-as.data.frame(t(site.seq45)); colnames(x)<-x[1,]; x<-x[-c(1:3),]; x[]<-lapply(x, as.integer); x$Sample<-row.names(x); x$Sample<-substr(x$Sample, 1, nchar(x$Sample)-1)
x<-aggregate(x[,1:507],by = list(x$Sample), FUN = mean)
site.seq45<-as.data.frame(t(x)); colnames(site.seq45)<-site.seq45[1,]; site.seq45<-site.seq45[-1,]; site.seq45[]<-lapply(site.seq45,as.integer); site.seq45$sequence_names<-row.names(site.seq45); site.seq45<-merge(ceph.seq45, site.seq45, by.x = "V1", by.y = "sequence_names", all.x = TRUE)
site.seq92[,4:98]<-lapply(site.seq92[,4:98],as.integer)

rm(ceph.seq45,ceph.seq92,alpha,DT.alpha,mean.neg,sd.neg,n,t.value,x,i,j)


### Manipulation into three forms of the same data: dat1 = Number of reads in every sequence (rows) against samples (columns); dat2 = Number of reads in every sample (rows) against sequence (columns); dat3 = as dat2 but proportions
dat1.92<-site.seq92
dat1.92[is.na(dat1.92)] <- 0
dat1.92<-merge(dat1.92,hap.match[,c(1,8:10)],by = "V1",all.x=TRUE)
dat1.92<-dat1.92[,c(1:3,99:101,4:98)]

dat1.45<-site.seq45
dat1.45[is.na(dat1.45)] <- 0
dat1.45<-merge(dat1.45,hap.match[,c(1,8:10)],by = "V1",all.x=TRUE)
dat1.45<-dat1.45[,c(1:3,50:52,4:49)]

#Change dataframe orientation
dat2.92<-t(dat1.92[,c(6:101)]); colnames(dat2.92)<-dat2.92[1,]; dat2.92<-as.data.frame(dat2.92[-1,])
dat2.92[] <- lapply(dat2.92, as.numeric)
dat2.92<-dat2.92[, order(names(dat2.92))]
dat2.92$Total.Reads<-apply(dat2.92[1:114],1,FUN=sum)

dat2.45<-t(dat1.45[,c(6:52)]); colnames(dat2.45)<-dat2.45[1,]; dat2.45<-as.data.frame(dat2.45[-1,])
dat2.45[] <- lapply(dat2.45, as.numeric)
dat2.45<-dat2.45[, order(names(dat2.45))]
dat2.45$Total.Reads<-apply(dat2.45[1:507],1,FUN=sum)

#Convert read count data to proportion of reads in a sample
dat3.92<-dat2.92[,1:114]/dat2.92$Total.Reads
dat3.92[dat3.92==0]<-NaN

dat3.45<-dat2.45[,1:507]/dat2.45$Total.Reads
dat3.45[dat3.45==0]<-NaN


##Error rate----
#Isolate all previously identified likely error ASVs, then their occurrence
pot.errors92<-dat3.92[,11:59]
pot.errors45<-dat3.45[,10:317]

vec.list<-NA
for(i in 1:ncol(pot.errors92)){
  for (j in 1:nrow(pot.errors92)) {
    if(is.na(pot.errors92[j,i])==FALSE){
      c <- pot.errors92[j,i]
      vec.list<-append(vec.list,c)
    }
  }
}
vec.list<-vec.list[-1]

## Calculate the error rate DT
n <- length(vec.list); alpha <- 0.01; mean.vec<-mean(vec.list); sd.vec <- sd(vec.list); t.value<-qt((1-(alpha/2)),n-1)
DT.mutated92 <- mean.vec +t.value*sd.vec

vec.list<-NA
for(i in 1:ncol(pot.errors45)){
  for (j in 1:nrow(pot.errors45)) {
    if(is.na(pot.errors45[j,i])==FALSE){
      c <- pot.errors45[j,i]
      vec.list<-append(vec.list,c)
    }
  }
}
vec.list<-vec.list[-1]

## Calculate the error rate DT
n <- length(vec.list); alpha <- 0.01; mean.vec<-mean(vec.list); sd.vec <- sd(vec.list); t.value<-qt((1-(alpha/2)),n-1)
DT.mutated45 <- mean.vec +t.value*sd.vec

##Remove probable errors based on the DT
dat3.92 <- as.data.frame(t(dat3.92[,1:114]))
dat3.45 <- as.data.frame(t(dat3.45[,1:507]))


for(i in 1:ncol(dat3.92)){
  for(j in 1:nrow(dat3.92)){
    if(is.na(dat3.92[j,i])==FALSE & dat3.92[j,i] <= DT.mutated92){
      dat3.92[j,i] = 0
    }
  }
}

dat3.92[is.na(dat3.92)] <- 0
dat3.92<-subset(dat3.92,apply(dat3.92,1,sum) > 0) 

dat3.92$NoSamples <- NA
for(i in 1:nrow(dat3.92)){
  dat3.92[i,96] <- sum(dat3.92[i,1:95]>0)
}

for(i in 1:ncol(dat3.45)){
  for(j in 1:nrow(dat3.45)){
    if(is.na(dat3.45[j,i])==FALSE & dat3.45[j,i] <= DT.mutated45){
      dat3.45[j,i] = 0
    }
  }
}

dat3.45[is.na(dat3.45)] <- 0
dat3.45<-subset(dat3.45,apply(dat3.45,1,sum) > 0) 

dat3.45$NoSamples <- NA
for(i in 1:nrow(dat3.45)){
  dat3.45[i,47] <- sum(dat3.45[i,1:46]>0)
}


rm(i,j,c,mean.vec,n,t.value,vec.list,alpha,sd.vec,pot.errors45,pot.errors92)

##Final Haplotype Criteria----
HapCriteria92 <- as.data.frame(as.vector(row.names(dat3.92)))
colnames(HapCriteria92)<-c("HapNames")
# Criteria 1: Does the ASV occur across multiple samples
HapCriteria92$MultipleSamples <- ifelse(dat3.92$NoSamples > 1,1,0)

# Criteria 2: Does the ASV occur in the highest proportion at least once in a sample
HighestProp<-rep(NA,95)
for(i in 1:95){
  max.row <- which.max(dat3.92[,i])
  HighestProp[i]<- row.names(dat3.92)[max.row]
}

HighestProp<-as.data.frame(unique(HighestProp))
colnames(HighestProp)<-"HapNames"
HighestProp$HighestInSample<-1
HapCriteria92 <- merge(HapCriteria92,HighestProp, by = "HapNames",all.x = TRUE)
HapCriteria92[is.na(HapCriteria92)] <- 0
HapCriteria92$NoCriteriaFit<-apply(HapCriteria92[,2:3],1,sum)


HapCriteria45 <- as.data.frame(as.vector(row.names(dat3.45)))
colnames(HapCriteria45)<-c("HapNames")
# Criteria 1: Does the ASV occur across multiple samples
HapCriteria45$MultipleSamples <- ifelse(dat3.45$NoSamples > 1,1,0)

# Criteria 2: Does the ASV occur in the highest proportion at least once in a sample
HighestProp<-rep(NA,46)
for(i in 1:46){
  max.row <- which.max(dat3.45[,i])
  HighestProp[i]<- row.names(dat3.45)[max.row]
}

HighestProp<-as.data.frame(unique(HighestProp))
colnames(HighestProp)<-"HapNames"
HighestProp$HighestInSample<-1
HapCriteria45 <- merge(HapCriteria45,HighestProp, by = "HapNames",all.x = TRUE)
HapCriteria45[is.na(HapCriteria45)] <- 0
HapCriteria45$NoCriteriaFit<-apply(HapCriteria45[,2:3],1,sum)


#Setting up final dataframes:
#If ASVs match one criteria than they are considered, if match two than more confident they are real
#Across studies
CombinedStudies = merge(HapCriteria45[,1:3],HapCriteria92[,1:3], all = TRUE, by = "HapNames")
CombinedStudies[is.na(CombinedStudies)] <- 0

CombinedStudies$NoCriteriaFit = apply(CombinedStudies[,2:5],1,sum)

FinalConfHaplotypes <- as.data.frame(CombinedStudies$HapNames[which(CombinedStudies$NoCriteriaFit >= 2)])
colnames(FinalConfHaplotypes)<-"names"
dat3.92$names<-rownames(dat3.92)
dat3.45$names<-rownames(dat3.45)

FinalConfHaplotypes92 <- merge(FinalConfHaplotypes,dat3.92[,c(1:96,97)], by= "names", all.x = TRUE, all.y = FALSE)
FinalConfHaplotypes45 <- merge(FinalConfHaplotypes,dat3.45[,c(1:46,48)], by= "names", all.x = TRUE, all.y = FALSE)

FinalConfHaplotypes92 = FinalConfHaplotypes92[complete.cases(FinalConfHaplotypes92),]
FinalConfHaplotypes45 = FinalConfHaplotypes45[complete.cases(FinalConfHaplotypes45),]

rm(i,hap.match,HapCriteria45,HapCriteria92,FinalConfHaplotypes,HighestProp,site.seq45,site.seq92,max.row)



#Rename unknown sequences:
FinalConfHaplotypes45[7,1] <- "eDNA.1"
#FinalAllHaplotypes[10:13,1] <- c("eDNA.1","eDNA.2","eDNA.3","eDNA.4") #I have removed the non-confident haplotypes

rownames(FinalConfHaplotypes92)<-FinalConfHaplotypes92$names
FinalConfHaplotypes92<-as.data.frame(t(FinalConfHaplotypes92[,2:96]))
FinalConfHaplotypes92[FinalConfHaplotypes92==0]<-NA  

rownames(FinalConfHaplotypes45)<-FinalConfHaplotypes45$names
FinalConfHaplotypes45<-as.data.frame(t(FinalConfHaplotypes45[,2:46]))
FinalConfHaplotypes45[FinalConfHaplotypes45==0]<-NA  





##Other----
edna.meta <- read.csv("eDNA.metadata.csv",na.strings = c("NA",""))


#Tables data----
se<-function(x){
  sd(x)/sqrt(length(x))
}

count2<- function(x){
  length(which(x == 2))
}

count3<- function(x){
  length(which(x == 3))
}

count1<- function(x){
  length(which(x == 1))
}

count0<- function(x){
  length(which(x == 0))
}

count1plus<- function(x){
  length(which(x > 0))
}

#Results -----
##Summary statistics----
HaplotypeSummaryReads92 <- dat2.92


for(i in 1:114){
  for(j in 1:nrow(HaplotypeSummaryReads92)){
    error <- HaplotypeSummaryReads92[j,115]*DT.mutated92
    if(HaplotypeSummaryReads92[j,i]-error < 0){
      HaplotypeSummaryReads92[j,i] <- 0
    }else{
      HaplotypeSummaryReads92[j,i]<- HaplotypeSummaryReads92[j,i]
    }
  }
}

HaplotypeSummaryReads92<-HaplotypeSummaryReads92[,which(apply(HaplotypeSummaryReads92[,1:114],2,sum) > 0)]
colnames(HaplotypeSummaryReads92) <- c("Hap_A","Hap_C+","Hap_D","Hap_H","Hap_I","Hap_K","Hap_M","Hap_S","eDNA.2","eDNA.3","eDNA.4","eDNA.5")
HaplotypeSummaryReads92$TotalReads<-apply(HaplotypeSummaryReads92[,1:8],1,sum)
HaplotypeSummaryReads92$TotalHaps<-apply(HaplotypeSummaryReads92[,1:8],1,count1plus)

HaplotypeSummaryReads92$SampleID<-rownames(HaplotypeSummaryReads92)
HaplotypeSummaryReads92$Site <- edna.meta[match(HaplotypeSummaryReads92$SampleID,edna.meta$EppendorfID),1]
HaplotypeSummaryReads92$Season <- edna.meta[match(HaplotypeSummaryReads92$SampleID,edna.meta$EppendorfID),2]
HaplotypeSummaryReads92$SampleType<-edna.meta[match(HaplotypeSummaryReads92$SampleID,edna.meta$EppendorfID),10]
HaplotypeSummaryReads92$SampleType<-ifelse(is.na(HaplotypeSummaryReads92$SampleType) == TRUE,"Lab control",HaplotypeSummaryReads92$SampleType)


HaplotypeSummaryReads45 <- dat2.45


for(i in 1:507){
  for(j in 1:nrow(HaplotypeSummaryReads45)){
    error <- HaplotypeSummaryReads45[j,508]*DT.mutated45
    if(HaplotypeSummaryReads45[j,i]-error < 0){
      HaplotypeSummaryReads45[j,i] <- 0
    }else{
      HaplotypeSummaryReads45[j,i]<- HaplotypeSummaryReads45[j,i]
    }
  }
}

HaplotypeSummaryReads45<-HaplotypeSummaryReads45[,which(apply(HaplotypeSummaryReads45[,1:507],2,sum) > 0)]
colnames(HaplotypeSummaryReads45) <- c("Hap_A","Hap_C+","Hap_D","Hap_H","Hap_I","Hap_M","Hap_Q","unk.1","unk.2","unk.3","unk.4","unk.5","unk.6","unk.7","unk.8","unk9","unk.10","eDNA.1","unk.11","unk.12","unk.13")
HaplotypeSummaryReads45$TotalReads<-apply(HaplotypeSummaryReads45[,c(1:7,18)],1,sum)
HaplotypeSummaryReads45$TotalHaps<-apply(HaplotypeSummaryReads45[,c(1:7,18)],1,count1plus)

HaplotypeSummaryReads45$SampleID<-rownames(HaplotypeSummaryReads45)
HaplotypeSummaryReads45$Site <- edna.meta[match(HaplotypeSummaryReads45$SampleID,edna.meta$EppendorfID),1]
HaplotypeSummaryReads45$Season <- edna.meta[match(HaplotypeSummaryReads45$SampleID,edna.meta$EppendorfID),2]
HaplotypeSummaryReads45$SampleType<-edna.meta[match(HaplotypeSummaryReads45$SampleID,edna.meta$EppendorfID),10]
HaplotypeSummaryReads45$SampleType<-ifelse(is.na(HaplotypeSummaryReads45$SampleType) == TRUE,"Lab control",HaplotypeSummaryReads45$SampleType)







###Table 1: Summary of sampling methods and haplotypes ----
SampleTypeSum92<-aggregate(TotalReads~SampleType,data = HaplotypeSummaryReads92,FUN = mean)
SampleTypeSE<-aggregate(TotalReads~SampleType,data = HaplotypeSummaryReads92,FUN = se)
SampleTypeN<-aggregate(TotalHaps~SampleType,data = HaplotypeSummaryReads92,FUN = length)
SampleTypeN0<-aggregate(TotalHaps~SampleType,data = HaplotypeSummaryReads92,FUN = count0)
SampleTypeN1<-aggregate(TotalHaps~SampleType,data = HaplotypeSummaryReads92,FUN = count1)
SampleTypeN2<-aggregate(TotalHaps~SampleType,data = HaplotypeSummaryReads92,FUN = count2)
SampleTypeN3<-aggregate(TotalHaps~SampleType,data = HaplotypeSummaryReads92,FUN = count3)

SampleTypeSum92$SE<-SampleTypeSE$TotalReads
SampleTypeSum92$N<-SampleTypeN$TotalHaps
SampleTypeSum92$N0<-SampleTypeN0$TotalHaps
SampleTypeSum92$N1<-SampleTypeN1$TotalHaps
SampleTypeSum92$N2<-SampleTypeN2$TotalHaps
SampleTypeSum92$N3<-SampleTypeN3$TotalHaps


SampleTypeSum45<-aggregate(TotalReads~SampleType,data = HaplotypeSummaryReads45,FUN = mean)
SampleTypeSE<-aggregate(TotalReads~SampleType,data = HaplotypeSummaryReads45,FUN = se)
SampleTypeN<-aggregate(TotalHaps~SampleType,data = HaplotypeSummaryReads45,FUN = length)
SampleTypeN0<-aggregate(TotalHaps~SampleType,data = HaplotypeSummaryReads45,FUN = count0)
SampleTypeN1<-aggregate(TotalHaps~SampleType,data = HaplotypeSummaryReads45,FUN = count1)
SampleTypeN2<-aggregate(TotalHaps~SampleType,data = HaplotypeSummaryReads45,FUN = count2)
SampleTypeN3<-aggregate(TotalHaps~SampleType,data = HaplotypeSummaryReads45,FUN = count3)


SampleTypeSum45$SE<-SampleTypeSE$TotalReads
SampleTypeSum45$N<-SampleTypeN$TotalHaps
SampleTypeSum45$N0<-SampleTypeN0$TotalHaps
SampleTypeSum45$N1<-SampleTypeN1$TotalHaps
SampleTypeSum45$N2<-SampleTypeN2$TotalHaps
SampleTypeSum45$N3<-SampleTypeN3$TotalHaps

#write.csv(SampleTypeSum45,"C:/Users/steph/Desktop/PhD/Writing/Chapter 4 - Analysis of eDNA - population connectivity + sex ratios/Figures/Summary of haplotype detections pilot.csv")
#write.csv(SampleTypeSum92,"C:/Users/steph/Desktop/PhD/Writing/Chapter 4 - Analysis of eDNA - population connectivity + sex ratios/Figures/Summary of haplotype detections 2023.csv")



#Extract the count and associated frequency for each Haplotype for each site
colnames(HaplotypeSummaryReads92)[2]<-"Hap_Cplus"
colnames(HaplotypeSummaryReads45)[2]<-"Hap_Cplus"


HapSites92<-aggregate(cbind(Hap_A,Hap_Cplus,Hap_D,Hap_H,Hap_I,Hap_K,Hap_M,Hap_S)~Site,data=HaplotypeSummaryReads92,FUN=count1plus)
HapSitesFreq92<-HapSites92
for(i in 1:nrow(HapSitesFreq92)){
  HapSitesFreq92[i,2:9]<-HapSitesFreq92[i,2:9]/sum(HapSites92[i,2:9])
}

HapProp92<-HaplotypeSummaryReads92[,1:8]/HaplotypeSummaryReads92$TotalReads
HapProp92<-HaplotypeSummaryReads92[,1:8]/HaplotypeSummaryReads92$TotalReads


HapSites45<-aggregate(cbind(Hap_A,Hap_Cplus,Hap_D,Hap_H,Hap_I,Hap_M,eDNA.1)~Site,data=HaplotypeSummaryReads45,FUN=count1plus)
HapSitesFreq45<-HapSites45
for(i in 1:nrow(HapSitesFreq45)){
  HapSitesFreq45[i,2:8]<-HapSitesFreq45[i,2:8]/sum(HapSites45[i,2:8])
}

HapProp45<-HaplotypeSummaryReads45[,c(1:7,18)]/HaplotypeSummaryReads45$TotalReads
HapProp45<-HaplotypeSummaryReads45[,c(1:7,18)]/HaplotypeSummaryReads45$TotalReads

HapSites45$Season = 2022
HapSites92$Season = 2023

HapSites = merge(HapSites45,HapSites92,all =TRUE)
#write.csv(HapSites,"HaplotypeFrequenciesSites.csv")

rm(SiteYearSUM,SampleTypeSum92,SampleTypeSum45,SiteYearN,SiteYearN0,SiteYearN1,SiteYearN2,SiteYearN3,SiteYearSE,SampleTypeSE,SampleTypeN,SampleTypeN1,SampleTypeN0,SampleTypeN2,SampleTypeN3,error,i,j,count0,count1,count2, count3, DT.mutated, count1plus,HapProp45,HapProp92,HapSites,HapSites45,HapSites92,HapSitesFreq45,HapSitesFreq92)


##Drogue
droguevsample = dat3.92[,c("BP_N_HD_057a","BP_N_HD_057b","BP_AKH_HD_114a","BP_AKH_HD_114b","BP_S_HD_115a","BP_S_HD_115b")]
#Low detections 2 of 6, one was in the drogue the other was in the sample.
rm(droguevsample)

#Figures----
nzcoast<-shapefile("./Shapefiles/nzcoast.shp")
south.island <-crop(nzcoast, extent(166,174.5,-47,-40))


##Figure 3. Map of haplotype locations three panels----
##Data----
BP.area<-crop(nzcoast, extent(c(172.5,173.8,-44.37,-43.52)))
TIM.area<-crop(nzcoast, extent(c(171, 172.2, -44.6, -44.2)))
OTA.area<-crop(nzcoast, extent(c(170.38, 174, -45.9, -45.6))) 

FinalConfHaplotypes92$SampleID<- rownames(FinalConfHaplotypes92)
FinalConfHaplotypes92[is.na(FinalConfHaplotypes92)] = 0
NoHaplotypes <- rep(NA, length = nrow(FinalConfHaplotypes92))
for(i in 1:length(NoHaplotypes)){
  NoHaplotypes[i] <- sum(FinalConfHaplotypes92[i,1:8]>0)
}

mapdata92<-merge(FinalConfHaplotypes92, edna.meta[,c(5,6,19)],by.x = "SampleID", by.y = "EppendorfID",all.x = TRUE, all.y = FALSE)
mapdata92<-mapdata92[complete.cases(mapdata92),]
mapdata92$Site<-NA
mapdata92[1:43,"Site"]<-"BP"
mapdata92[44:58,"Site"]<-"DUN"
mapdata92[59:91,"Site"]<-"TIM"
mapdata92$Site<-as.factor(mapdata92$Site)

no.occur <- function(x){
  length(which(x > 0))
}

site.presence92<-aggregate(.~Site,data=mapdata92[,c(12,2:9)],FUN = no.occur)
site.presence92<-as.data.frame(t(site.presence92[,-1]))
colnames(site.presence92)<-c("BP","OTA","TIM")

hap92.A <- subset(mapdata92[,c(2,10,11)], mapdata92$Hap_A > 0);hap92.Cplus <- subset(mapdata92[,c(3,10,11)], mapdata92$`Hap_C+` > 0);hap92.D <- subset(mapdata92[,c(4,10,11)], mapdata92$Hap_D > 0);hap92.H <- subset(mapdata92[,c(5,10,11)], mapdata92$Hap_H > 0);hap92.I <- subset(mapdata92[,c(6,10,11)], mapdata92$Hap_I > 0);hap92.K <- subset(mapdata92[,c(7,10,11)], mapdata92$Hap_K > 0);hap92.M <- subset(mapdata92[,c(8,10,11)], mapdata92$Hap_M > 0);hap92.S <- subset(mapdata92[,c(9,10,11)], mapdata92$Hap_S > 0)

hap92.A<-SpatialPointsDataFrame(coords = hap92.A[,c(3,2)],data = hap92.A,proj4string = CRS("+init=EPSG:4326"));hap92.Cplus<-SpatialPointsDataFrame(coords = hap92.Cplus[,c(3,2)],data = hap92.Cplus,proj4string = CRS("+init=EPSG:4326"));hap92.D<-SpatialPointsDataFrame(coords = hap92.D[,c(3,2)],data = hap92.D,proj4string = CRS("+init=EPSG:4326"));hap92.H<-SpatialPointsDataFrame(coords = hap92.H[,c(3,2)],data = hap92.H,proj4string = CRS("+init=EPSG:4326"));hap92.I<-SpatialPointsDataFrame(coords = hap92.I[,c(3,2)],data = hap92.I,proj4string = CRS("+init=EPSG:4326"));hap92.K<-SpatialPointsDataFrame(coords = hap92.K[,c(3,2)],data = hap92.K,proj4string = CRS("+init=EPSG:4326"));hap92.M<-SpatialPointsDataFrame(coords = hap92.M[,c(3,2)],data = hap92.M,proj4string = CRS("+init=EPSG:4326"));hap92.S<-SpatialPointsDataFrame(coords = hap92.S[,c(3,2)],data = hap92.S,proj4string = CRS("+init=EPSG:4326"))


FinalConfHaplotypes45$SampleID<- rownames(FinalConfHaplotypes45)
FinalConfHaplotypes45[is.na(FinalConfHaplotypes45)] = 0
NoHaplotypes <- rep(NA, length = nrow(FinalConfHaplotypes45))
for(i in 1:length(NoHaplotypes)){
  NoHaplotypes[i] <- sum(FinalConfHaplotypes45[i,1:7]>0)
}

mapdata45<-merge(FinalConfHaplotypes45, edna.meta[,c(5,6,19)],by.x = "SampleID", by.y = "EppendorfID",all.x = TRUE, all.y = FALSE)
mapdata45<-mapdata45[complete.cases(mapdata45),]
mapdata45$Site<-NA
mapdata45[1:37,"Site"]<-"BP"
mapdata45[38,"Site"]<-"DUN"
mapdata45[39:43,"Site"]<-"TIM"
mapdata45$Site<-as.factor(mapdata45$Site)

no.occur <- function(x){
  length(which(x > 0))
}

site.presence45<-aggregate(.~Site,data=mapdata45[,c(11,2:8)],FUN = no.occur)
site.presence45<-as.data.frame(t(site.presence45[,-1]))
colnames(site.presence45)<-c("BP","OTA","TIM")

hap45.A <- subset(mapdata45[,c(2,9,10)], mapdata45$Hap_A > 0);hap45.Cplus <- subset(mapdata45[,c(3,9,10)], mapdata45$`Hap_C+` > 0);hap45.D <- subset(mapdata45[,c(4,9,10)], mapdata45$Hap_D > 0);hap45.H <- subset(mapdata45[,c(5,9,10)], mapdata45$Hap_H > 0);hap45.I <- subset(mapdata45[,c(6,9,10)], mapdata45$Hap_I > 0);hap45.M <- subset(mapdata45[,c(7,9,10)], mapdata45$Hap_M > 0);hap45.e1 <- subset(mapdata45[,c(8,9,10)], mapdata45$eDNA.1 > 0)

hap45.A<-SpatialPointsDataFrame(coords = hap45.A[,c(3,2)],data = hap45.A,proj4string = CRS("+init=EPSG:4326"));hap45.Cplus<-SpatialPointsDataFrame(coords = hap45.Cplus[,c(3,2)],data = hap45.Cplus,proj4string = CRS("+init=EPSG:4326"));hap45.D<-SpatialPointsDataFrame(coords = hap45.D[,c(3,2)],data = hap45.D,proj4string = CRS("+init=EPSG:4326"));hap45.H<-SpatialPointsDataFrame(coords = hap45.H[,c(3,2)],data = hap45.H,proj4string = CRS("+init=EPSG:4326"));hap45.I<-SpatialPointsDataFrame(coords = hap45.I[,c(3,2)],data = hap45.I,proj4string = CRS("+init=EPSG:4326"));hap45.M<-SpatialPointsDataFrame(coords = hap45.M[,c(3,2)],data = hap45.M,proj4string = CRS("+init=EPSG:4326"));hap45.e1<-SpatialPointsDataFrame(coords = hap45.e1[,c(3,2)],data = hap45.e1,proj4string = CRS("+init=EPSG:4326"))

a<-rgb(255/255,208/255,176/255); c<-rgb(201/255,48/255,35/255); d<-rgb(240/255,208/245,250/255); e<-rgb(248/255,236/255,54/255); g<-rgb(255/255,173/255,59/255); h<-rgb(16/255,161/255,216/255); i<-rgb(176/255,84/255,210/255); j<-rgb(45/255,98/255,179/255); k<-rgb(15/255,15/255,15/255); l<-rgb(94/255,193/255,0/255); m<-rgb(26/255,111/255,39/255); n<-"darkorange"; o<-rgb(134/255,153/255,168/255); p<-rgb(202/255,142/255,112/255); q<-rgb(139/255,89/255,88/255); r<-rgb(222/255,238/255,254/255); s<-rgb(13/255,113/255,119/255); v<-rgb(254/255,253/255,174/255); e1<-rgb(98/255,232/255,216/255); e2<-rgb(222/255,80/255,230/255)

#a = "#440154FF" ; c = "#472D7BFF" ;d ="#3B528BFF";h ="#2C728EFF";i = "#21908CFF" ;k = "#27AD81FF";m = "#5DC863FF";s = "#AADC32FF";e1 ="#FDE725FF"



eDNA.92.colours<-c(a,c,d,h,i,k,m,s)
eDNA.45.colours<-c(a,c,d,h,i,m,e1)

all.colours = c(a,c,d,h,i,k,m,s,e1)

fade = rgb(1,1,1,0.3)


rm(a,c,d,e,g,h,i,j,k,l,m,n,o,p,q,r,s,v,e1,e2)

tiff("C:/Users/steph/Desktop/PhD/Writing/Chapter 4 - Analysis of eDNA - population connectivity + sex ratios/Figures/UPDATED_FIG3_pilot and 2023 studies.tiff",width = 160, height = 200,res=300, unit="mm")
par(mfrow=c(3,2),mai=c(0.01,0.2,0.01,0.5),xpd = NA)

plot(BP.area, col = "gray")
rect(172.5,-43.5,172.525,-43.9,border ="white", col = "white")
lines(x = c(172.525,172.525),y=c(-43.52,-43.843))
points(hap45.A,bg=eDNA.45.colours[1],pch=21,cex=2); points(hap45.Cplus,bg=eDNA.45.colours[2],pch=21,cex=2); points(hap45.D,bg=eDNA.45.colours[3],pch=21,cex=2); points(hap45.H,bg=eDNA.45.colours[4],pch=21,cex=2); points(hap45.I,bg=eDNA.45.colours[5],pch=21,cex=2); points(hap45.M,bg=eDNA.45.colours[6],pch=21,cex=2); points(hap45.e1,bg=eDNA.45.colours[7],pch=21,cex=2)
scalebar(1.852,xy=c(172.57,-43.81),type="line",lonlat = TRUE,label = NA); text(x=172.58,y=-43.821,"1nm")
add.pie(x=173.193, y= -43.57, radius=0.076, site.presence45$BP,labels="",clockwise=TRUE,col=eDNA.45.colours,cex=0.5,xpd=NA)
text(x=173.195, y= -43.57, labels = " BP \n(n=55)",pos=1,offset=4.3, cex = 1.2)
text(172.7,-43.49,"(a) Pilot study",cex = 1.5, pos =4)

plot(BP.area, col = "gray")
rect(172.5,-43.5,172.525,-43.9,border ="white", col = "white")
lines(x = c(172.525,172.525),y=c(-43.52,-43.843))
points(hap92.A,bg=eDNA.92.colours[1],pch=21,cex=2); points(hap92.Cplus,bg=eDNA.92.colours[2],pch=21,cex=2); points(hap92.D,bg=eDNA.92.colours[3],pch=21,cex=2); points(hap92.H,bg=eDNA.92.colours[4],pch=21,cex=2); points(hap92.I,bg=eDNA.92.colours[5],pch=21,cex=2)
scalebar(1.852,xy=c(172.57,-43.81),type="line",lonlat = TRUE,label = NA); text(x=172.58,y=-43.821,"1nm")
add.pie(x=173.191, y= -43.57, radius=0.076, site.presence92$BP,labels="",clockwise=TRUE,col=eDNA.92.colours,cex=0.5,xpd=NA)
text(x=173.195, y= -43.57, labels = " BP \n(n=34)",pos=1,offset=4.3, cex = 1.2)
text(172.7,-43.49,"(b) Main study",cex = 1.5,pos = 4)

plot(TIM.area,col = "gray")
points(hap45.Cplus,bg=eDNA.45.colours[2],pch=21,cex=2); points(hap45.A,bg=eDNA.45.colours[1],pch=21,cex=2); 
scalebar(1.852,xy=c(171.029,-44.575),type="line",lonlat = TRUE, label = NA); text(x=171.039,y=-44.585,"1nm")
add.pie(x=171.555, y= -44.4, radius=0.062, site.presence45$TIM,labels="",clockwise=TRUE,col=eDNA.45.colours,cex=0.5)
text(x=171.555, y= -44.4, labels = "  TIM \n(n=4)",pos=1,offset=4.3, cex = 1.2)


plot(TIM.area,col = "gray")
points(hap92.Cplus,bg=eDNA.92.colours[2],pch=21,cex=2); points(hap92.S,bg=eDNA.92.colours[7],pch=21,cex=2.5); points(hap92.H,bg=eDNA.92.colours[4],pch=21,cex=1.5); points(hap92.A,bg=eDNA.92.colours[1],pch=21,cex=2); points(hap92.K,bg=eDNA.92.colours[6],pch=21,cex=2)
scalebar(1.852,xy=c(171.029,-44.575),type="line",lonlat = TRUE, label = NA); text(x=171.039,y=-44.585,"1nm")
add.pie(x=171.555, y= -44.4, radius=0.062, site.presence92$TIM,labels="",clockwise=TRUE,col=eDNA.92.colours,cex=0.5)
text(x=171.555, y= -44.4, labels = "  TIM \n(n=32)",pos=1,offset=4.3, cex = 1.2)



plot(OTA.area,col="gray")
scalebar(1.852,xy=c(170.400,-45.884),type="line",lonlat = TRUE, label = NA); text(x=170.412,y=-45.892,"1nm")
text(x=170.808, y= -45.65, labels = " DUN \n(n=0)",pos=1,offset=4.3, cex=1.2)
rect(169.8,-45.91,170.9,-44.95,col = fade,border =NA)
legend(y=-45.603,x=170.385,legend=c("A","C+","D","H","I","K","M","S","eDNA1"),fill =all.colours,bty="o",bg="white",cex=1.3)
par(xpd = NA)


plot(OTA.area,col="gray")
points(hap92.S,bg=eDNA.92.colours[8],pch=21,cex=2,xpd = NA);points(hap92.Cplus,bg=eDNA.92.colours[2],pch=21,cex=1.25)
scalebar(1.852,xy=c(170.400,-45.884),type="line",lonlat = TRUE, label = NA); text(x=170.412,y=-45.892,"1nm")

add.pie(x=170.805, y= -45.65, radius=0.047, site.presence92$OTA,labels="",clockwise=TRUE,col=eDNA.92.colours,cex=0.5)
text(x=170.808, y= -45.65, labels = " DUN \n(n=15)",pos=1,offset=4.3, cex=1.2)
north(xy = c(170.42,-45.67),type=2,cex=1.7)



dev.off()



rm(hap45.A,hap45.Cplus,hap45.D,hap45.e1,hap45.H,hap45.I,hap45.M,hap92.A,hap92.Cplus,hap92.D,hap92.H,hap92.I,hap92.K,hap92.M,hap92.S,BP.area,OTA.area,TIM.area)



#Figure 1. Map of study area----
#nzcoast<-shapefile("./Shapefiles/nzcoast.shp")
#south.island <-crop(nzcoast, extent(166,174.5,-47,-40))

#study.area<-shapefile("./Shapefiles/StudyArea.shp")
study.area <- crop(nzcoast, extent(c(168, 175, -47, -43))) 
study.extent<-shapefile("./Shapefiles/StudyExtent.shp")
study.extent<-study.extent[study.extent$id != 3,]
study.extent<-spTransform(study.extent,crs(nzcoast))

#inset.limits<- c(0.1309,0.35,0.71,0.9602)
inset.limits<- c(0.05,0.4,0.6,0.96)

tiff("C:/Users/steph/Desktop/PhD/Writing/Chapter 4 - Analysis of eDNA - population connectivity + sex ratios/Figures/Figure 1. Map of study area.tiff", width = 200, height = 200, res=300, units = "mm")
par(mar=c(0.1,0.1,0.1,0.1))
plot(south.island,col="gray",lwd = 0.05)
#plot(bathy,add=TRUE)
plot(study.extent,add=TRUE,col = "#FDE725FF")
#lines(x=c(173.5,173.5), y=c(-46,-43.4),lwd=1.2);lines(x=c(170.2,173.5),y=c(-43.4,-43.4),lwd=1.2);lines(x=c(170.2,173.5),y=c(-46,-46),lwd=1.2);lines(x=c(170.2,170.2),y=c(-43.4,-46),lwd=1.2) #Lines for original study area map
lines(x=c(174.5,174.5), y=c(-47,-40.5),lwd=1.2);lines(x=c(166.035,174.5),y=c(-40.5,-40.5),lwd=1.2);lines(x=c(166.035,174.5),y=c(-47,-47),lwd=1.2);lines(x=c(166.035,166.035),y=c(-40.5,-47),lwd=1.2)

text(x=172,y=-43.73,labels = "Banks Peninsula",cex = 0.9);text(x=170.9,y=-44.4,labels = "Timaru",cex = 0.9);text(x=170.2,y=-45.78,labels = "Dunedin",cex=0.9);text(x=173.25,y=-42.35,labels = "Kaikōura",cex = 0.8);text(x=173.5,y=-41.65,labels = "Cloudy/Clifford \nBay",cex = 0.8);text(x=170.55,y=-45.1,labels = "Oamaru",cex = 0.8);text(x=170.45,y=-45.35,labels = "Moeraki",cex = 0.8);text(x=169.35,y=-46.3,labels = "Kaka Point",cex = 0.8);text(x=168.85,y=-46.46,labels = "Toetoe Bay",cex = 0.8);text(x=167.6,y=-46.05,labels = "Te Waewae Bay",cex = 0.8)
scalebar(92.6,xy=c(172.5,-46.95),type="bar",divs = 4, lonlat = TRUE, label = NA,cex = 1); text(x=174,y=-46.92,"50nm",cex=1)


par(fig=inset.limits, new=TRUE, mar=c(0,0,0,0) )
plot(nzcoast,col="gray",lwd=0.001)
lines(x=c(174.5,174.5), y=c(-47,-40.5),lwd=0.8);lines(x=c(166.04,174.5),y=c(-40.5,-40.5),lwd=0.8);lines(x=c(166.04,174.5),y=c(-47,-47),lwd=0.8);lines(x=c(166.04,166.04),y=c(-40.5,-47),lwd=0.8)
north(xy = c(165,-38),type=2,cex=1.5)
axis(1,at = seq(165,180,by=5),labels = c("165ºE","170ºE","175ºE","180ºE"),cex.axis = 0.8)
axis(4, at = seq(-50,-35, by = 5),labels = c("-50ºS","-45ºS","-40ºS","-35ºS"),las=2,cex.axis = 0.8)


box(lwd=0.9,col="black",lty = "solid")
dev.off()

rm(south.island,study.extent,inset.limits,nzcoast,study.area)




#Appendices----


##Appendices X. Discovery curve----
discovery<-mapdata45[,1:10]
for(i in 2:10){
  for(j in 1:nrow(discovery)){
    if(discovery[j,i] > 0){
      discovery[j,i] <- names(discovery[i])
    }
  }
}

discovery$Date<-as.Date(edna.meta[match(discovery$SampleID,edna.meta$EppendorfID),3], format = "%d/%m/%Y")
discovery$Site<-edna.meta[match(discovery$SampleID,edna.meta$EppendorfID),1]
discovery<-discovery[order(discovery$Date),]

discovery.data<-as.data.frame(rep(1:134));names(discovery.data) = "No.Samples"
discovery.data$all.haps<-c(2,2,2,2,2,2,2,3,3,4,4,4,5,5,5,5,5,5,5,5,5,5,5,5,5,5,6,6,6,6,6,6,6,6,6,6,6,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,8,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9)
discovery.data$bp.haps<- c(2,2,2,2,2,2,2,3,3,4,4,4,5,5,5,5,5,5,5,5,5,5,5,5,5,5,6,6,6,6,6,6,6,6,6,6,6,6,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
discovery.data$tim.haps<-c(2,2,2,2,2,2,3,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
discovery.data$dun.haps<-c(0,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)

  

log.reg<-nls(all.haps~SSlogis(No.Samples,a,b,c),data=discovery.data)
log.reg1<-nls(bp.haps~SSlogis(No.Samples,a,b,c),data=discovery.data)
log.reg2<-nls(tim.haps~SSlogis(No.Samples,a,b,c),data=discovery.data)
log.reg3<-nls(dun.haps~SSlogis(No.Samples,a,b,c),data=discovery.data)

tiff("C:/Users/steph/Desktop/PhD/Writing/Chapter 4 - Analysis of eDNA - population connectivity + sex ratios/Figures/Appendices X. Discovery curve with logistic curves.tiff", width= 120, height=80, res=300, units="mm")
par(mai = c(0.7,0.7,0.2,0.2), mgp = c(1.3,0.2,0))
plot(all.haps~No.Samples, data = discovery.data,axes=FALSE, xlab = "Number of samples", ylab = "Total number of \nunique haplotypes", bty="n", ylim=c(0,10),xaxs = "i",yaxs="i",pch=20,cex.lab=0.6,cex=0.4,col="gray")
axis(1, at=seq(0,140, by=10),cex.axis = 0.5,tck =-0.01)
axis(2, at=seq(0,10, by=1),cex.axis = 0.5,tck=-0.01)
lines(discovery.data[,1], predict(log.reg, discovery.data[,1]), lwd = 1.5,col="gray")
points(discovery.data$bp.haps-0.05~discovery.data$No.Samples,col = "#440154FF",pch=20,cex=0.4)
lines(discovery.data[1:80,1],predict(log.reg1,discovery.data[1:80,1]),col="#440154FF",lwd=1.5)
points(discovery.data$tim.haps-0.10~discovery.data$No.Samples,col = "#21908CFF",pch=20,cex=0.4)
lines(discovery.data[1:38,1],predict(log.reg2,discovery.data[1:38,1]),col="#21908CFF",lwd=1.5)
points(discovery.data$dun.haps-0.15~discovery.data$No.Samples,col = "#FDE725FF",pch=20,cex=0.4)
lines(discovery.data[1:16,1],predict(log.reg3,discovery.data[1:16,1]),col = "#FDE725FF",lwd=1.5)
legend(x=5,y=9.9,legend=c("All","Banks Peninsula","Timaru","Dunedin"), fill = c("gray","#440154FF","#21908CFF","#FDE725FF"), border = c("gray","#440154FF","#21908CFF","#FDE725FF"),cex=0.5,box.lwd = 0.7)
dev.off()


rm(log.reg,log.reg1,log.reg2,log.reg3,SampleTypeN,SampleTypeN0,SampleTypeN1,SampleTypeN2,SampleTypeN3,SampleTypeSE,discovery,discovery.data)

##Appendices X----

fieldnotes<-read.csv("Fieldnotes.csv")

edna.meta$Sample.ranking = as.factor(edna.meta$Sample.ranking)
edna.meta$SampleType = as.factor(edna.meta$SampleType)
edna.meta$Site = as.factor(edna.meta$Site)
edna.meta$Prox2dolph = as.factor(edna.meta$Prox2dolph)

edna.meta$Depth = fieldnotes[match(edna.meta$EncounterID,fieldnotes$EncounterID),9]
edna.meta$Turbidity = fieldnotes[match(edna.meta$EncounterID,fieldnotes$EncounterID),10]
edna.meta$SST = fieldnotes[match(edna.meta$EncounterID,fieldnotes$EncounterID),11]
edna.meta$BF = fieldnotes[match(edna.meta$EncounterID,fieldnotes$EncounterID),7]
edna.meta$Swell = fieldnotes[match(edna.meta$EncounterID,fieldnotes$EncounterID),8]
edna.meta$CollectDateTime = as.POSIXct(as.character(paste(edna.meta$Date.collected, edna.meta$Time.collected)), format="%d/%m/%Y %I:%M:%S %p")

mapdata45$Date<-edna.meta[match(mapdata45$SampleID,edna.meta$EppendorfID),3]
mapdata92$Date<-edna.meta[match(mapdata92$SampleID,edna.meta$EppendorfID),3]

edna.meta$FilterDateTime = NA
for(i in 1:nrow(edna.meta)){
  if(i < 65){
    edna.meta[i,"FilterDateTime"] = edna.meta[i,"Time.Date.filtered"]
  }else{
    edna.meta[i,"FilterDateTime"] = paste(edna.meta[i,"Date.collected"],edna.meta[i,"Time.Date.filtered"],sep = " ")
  }
  
}
edna.meta$FilterDateTime = as.POSIXct(edna.meta$FilterDateTime,format = "%d/%m/%Y %H:%M")
edna.meta$Time2Filter = as.numeric(edna.meta$FilterDateTime-edna.meta$CollectDateTime)


HaplotypeSummaryReads45$Ceph.detect<-ifelse(HaplotypeSummaryReads45$TotalReads>0,1,0)
HaplotypeSummaryReads92$Ceph.detect<-ifelse(HaplotypeSummaryReads92$TotalReads>0,1,0)

HaplotypeSummaryReads = merge(HaplotypeSummaryReads45,HaplotypeSummaryReads92,all=TRUE)

edna.meta$TotalCephReads <- HaplotypeSummaryReads[match(edna.meta$EppendorfID,HaplotypeSummaryReads$SampleID),"TotalReads"]
edna.meta$TotalCephHaps <- HaplotypeSummaryReads[match(edna.meta$EppendorfID,HaplotypeSummaryReads$SampleID),"TotalHaps"]
edna.meta$CephDetected <-  HaplotypeSummaryReads[match(edna.meta$EppendorfID,HaplotypeSummaryReads$SampleID),"Ceph.detect"]

rm(HaplotypeSummaryReads,HaplotypeSummaryReads45,HaplotypeSummaryReads92,fieldnotes)


##MODELS: Cephalorhynchus detected----
m1.1 <- glm(CephDetected~Site,data=edna.meta,family = binomial(link = logit))
m1.2 <- glm(CephDetected~Turbidity,data=edna.meta,family = binomial(link = logit))
m1.3 <- glm(CephDetected~Depth,data=edna.meta,family = binomial(link = logit)) 
m1.4 <- glm(CephDetected~SST,data=edna.meta,family = binomial(link = logit))
m2.1<-glm(CephDetected~Sample.ranking,data=edna.meta,family=binomial(link = logit))
m2.2<-glm(CephDetected~tot.group.size,data=edna.meta,family=binomial(link = logit))
m2.3<-glm(CephDetected~boat.group.size,data=edna.meta,family=binomial(link = logit))
m2.4<-glm(CephDetected~Prox2dolph,data=edna.meta,family=binomial(link = logit))
m3.1<-glm(CephDetected~Amount.collected,data=edna.meta,family = binomial(link = logit))
m3.2<-glm(CephDetected~Amount.filtered,data=edna.meta,family = binomial(link = logit))
m3.3<-glm(CephDetected~Time2Filter,data=edna.meta,family = binomial(link = logit))

summary(m1.1)
summary(m1.2)
summary(m1.3)
summary(m1.4)
summary(m2.1)
summary(m2.2)
summary(m2.3)
summary(m2.4)
summary(m3.1)
summary(m3.2)
summary(m3.3)



##Plotting----
###Plot data----
#Standardised site to total number of samples collected
site.mat<-matrix(c(length(which(edna.meta$CephDetected == 0 & edna.meta$Site =="Dunedin"))/length(which(edna.meta$Site =="Dunedin")),length(which(edna.meta$CephDetected == 1 & edna.meta$Site =="Dunedin"))/length(which(edna.meta$Site =="Dunedin")),
                   length(which(edna.meta$CephDetected == 0 & edna.meta$Site =="Timaru"))/length(which(edna.meta$Site =="Timaru")),length(which(edna.meta$CephDetected == 1 & edna.meta$Site =="Timaru"))/length(which(edna.meta$Site =="Timaru")),
                   length(which(edna.meta$CephDetected == 0 & edna.meta$Site =="Banks Peninsula"))/length(which(edna.meta$Site =="Banks Peninsula")),length(which(edna.meta$CephDetected == 1 & edna.meta$Site =="Banks Peninsula"))/length(which(edna.meta$Site =="Banks Peninsula"))),nrow=2,ncol=3)

rank.mat<-matrix(c(length(which(edna.meta$CephDetected == 0 & edna.meta$Sample.ranking =="Poor"))/length(which(edna.meta$Sample.ranking =="Poor")),length(which(edna.meta$CephDetected == 1 & edna.meta$Sample.ranking =="Poor"))/length(which(edna.meta$Sample.ranking =="Poor")),
                   length(which(edna.meta$CephDetected == 0 & edna.meta$Sample.ranking =="Ok"))/length(which(edna.meta$Sample.ranking =="Ok")),length(which(edna.meta$CephDetected == 1 & edna.meta$Sample.ranking =="Ok"))/length(which(edna.meta$Sample.ranking =="Ok")),
                   length(which(edna.meta$CephDetected == 0 & edna.meta$Sample.ranking =="Good"))/length(which(edna.meta$Sample.ranking =="Good")),length(which(edna.meta$CephDetected == 1 & edna.meta$Sample.ranking =="Good"))/length(which(edna.meta$Sample.ranking =="Good")),
                   length(which(edna.meta$CephDetected == 0 & edna.meta$Sample.ranking =="Excellent"))/length(which(edna.meta$Sample.ranking =="Excellent")),length(which(edna.meta$CephDetected == 1 & edna.meta$Sample.ranking =="Excellent"))/length(which(edna.meta$Sample.ranking =="Excellent"))),nrow=2,ncol=4)

prox.mat<-matrix(c(length(which(edna.meta$CephDetected == 0 & edna.meta$Prox2dolph =="<1m"))/length(which(edna.meta$Prox2dolph =="<1m")),length(which(edna.meta$CephDetected == 1 & edna.meta$Prox2dolph =="<1m"))/length(which(edna.meta$Prox2dolph =="<1m")),
                   length(which(edna.meta$CephDetected == 0 & edna.meta$Prox2dolph =="1m"))/length(which(edna.meta$Prox2dolph =="1m")),length(which(edna.meta$CephDetected == 1 & edna.meta$Prox2dolph =="1m"))/length(which(edna.meta$Prox2dolph =="1m")),
                   length(which(edna.meta$CephDetected == 0 & edna.meta$Prox2dolph =="2m"))/length(which(edna.meta$Prox2dolph =="2m")),length(which(edna.meta$CephDetected == 1 & edna.meta$Prox2dolph =="2m"))/length(which(edna.meta$Prox2dolph =="2m")),
                   length(which(edna.meta$CephDetected == 0 & edna.meta$Prox2dolph =="3m"))/length(which(edna.meta$Prox2dolph =="3m")),length(which(edna.meta$CephDetected == 1 & edna.meta$Prox2dolph =="3m"))/length(which(edna.meta$Prox2dolph =="3m")),
                   length(which(edna.meta$CephDetected == 0 & edna.meta$Prox2dolph =="4m"))/length(which(edna.meta$Prox2dolph =="4m")),length(which(edna.meta$CephDetected == 1 & edna.meta$Prox2dolph =="4m"))/length(which(edna.meta$Prox2dolph =="4m")),
                   length(which(edna.meta$CephDetected == 0 & edna.meta$Prox2dolph ==">5m"))/length(which(edna.meta$Prox2dolph ==">5m")),length(which(edna.meta$CephDetected == 1 & edna.meta$Prox2dolph ==">5m"))/length(which(edna.meta$Prox2dolph ==">5m"))), nrow=2,ncol=6)


tiff("C:/Users/steph/Desktop/PhD/Writing/Chapter 4 - Analysis of eDNA - population connectivity + sex ratios/Figures/Figure3.3. Field sampling figures.tiff",height = 200, width=160, res = 300, unit="mm")
layout(matrix(c(1,2,3,4,5,6,7,8,9,10,11,0),4,3,byrow = FALSE))
par(mai = c(0.5,0.5,0.2,0.1),mgp = c(2,0.5,0))
#Environment
bp<-barplot(site.mat,beside=TRUE,ylab = "Proportion of Samples \n(#/total collected)",xlab = "Site",ylim=c(0,1), cex.axis = 0.9, cex.lab = 0.9)
axis(1,at=c(2,5,8), labels = c("DUN","TIM","BP"),cex.axis = 0.8)
abline(h=0)
boxplot(Turbidity~CephDetected,data=edna.meta, cex.axis = 0.9, cex.lab = 0.9,ylab = "Secchi disk depth (m)", xlab = "Cephalorhynchus detected")
boxplot(Depth~CephDetected,data=edna.meta, cex.axis = 0.9, cex.lab = 0.9,ylab = "Water depth (m)", xlab = "Cephalorhynchus detected")
boxplot(SST~CephDetected,data=edna.meta, cex.axis = 0.9, cex.lab = 0.9,ylab = "Sea surface temperature (ºC)", xlab = "Cephalorhynchus detected")
#Collection
rbp<-barplot(rank.mat,beside=TRUE,ylab = "Proportion of Samples \n(#/total collected)",xlab = "Sample ranking",ylim=c(0,1), cex.axis = 0.9, cex.lab = 0.9)
axis(1,at=c(2,5,8,11), labels = c("Poor","Ok","Good","Excellent"),cex.axis = 0.8)
abline(h=0)
rbp<-barplot(prox.mat,beside=TRUE,ylab = "Proportion of Samples \n(#/total collected)",xlab = "Proximity to dolphins",ylim=c(0,1), cex.axis = 0.9, cex.lab = 0.9)
axis(1,at=c(2,5,8,11,14,17), labels = c("<1m","1m","2m","3m","4m",">5m"),cex.axis = 0.8)
abline(h=0)
boxplot(tot.group.size~CephDetected,data=edna.meta, cex.axis = 0.9, cex.lab = 0.9,ylab = "Total group size", xlab = "Cephalorhynchus detected")
boxplot(boat.group.size~CephDetected,data=edna.meta, cex.axis = 0.9, cex.lab = 0.9,ylab = "Group size \nat vessel", xlab = "Cephalorhynchus detected")
#Filtration
boxplot(Time2Filter~CephDetected,data=edna.meta, cex.axis = 0.9, cex.lab = 0.9,ylim = c(0,10),ylab = "Time from collection \nto filtration (hours)", xlab = "Cephalorhynchus detected")
boxplot(Amount.filtered~CephDetected,data=edna.meta, cex.axis = 0.9, cex.lab = 0.9,ylab = "Amount filtered (L)", xlab = "Cephalorhynchus detected")
boxplot(Amount.collected~CephDetected,data=edna.meta, cex.axis = 0.9, cex.lab = 0.9,ylab = "Amount Collected (L)", xlab = "Cephalorhynchus detected")

text(x = -4.5, y = 18.9, "A)", xpd=NA,cex = 1.2)
text(x = -1.5, y = 18.9, "B)",xpd=NA,cex = 1.2)
text(x = 1.5, y = 18.9, "C)",xpd=NA,cex = 1.2)
dev.off()





##Appendices X. Off target amplification----
seq.scores<-read.csv("AllSeq_100plus.csv")
seq.scores<-subset(seq.scores, seq.scores$Cephalorhynchus == 0)
seq.scores$seqname<-gsub(">","",seq.scores$seqname)
offtarget.sites<-merge(seq.scores[,c(5,9)],all.asv, by.x = "seqname", by.y = "sequence_names",all.x=TRUE, all.y=FALSE)
offtarget.sites[is.na(offtarget.sites)]<-0

offtarget.sites<-aggregate(. ~ Species.1,offtarget.sites[,2:140],FUN =sum)

offtarget.sites$Total.Reads<-apply(offtarget.sites[2:139],1,FUN=sum)
offtarget.sites$NoSamples<-NA
for(i in 1:nrow(offtarget.sites)){
  offtarget.sites[i,141]<- sum(offtarget.sites[i,2:139]>0)
}

offtarget.sites$BP<-NA
offtarget.sites$TIM<-NA
offtarget.sites$OTA<-NA

for(i in 1:20){
  offtarget.sites[i,142]<-ifelse(sum(offtarget.sites[i,c(3:39,49:91)])>0,1,0) 
  offtarget.sites[i,143]<-ifelse(sum(offtarget.sites[i,c(42:47,107:139)])>0,1,0)   
  offtarget.sites[i,144]<-ifelse(sum(offtarget.sites[i,c(40,92:106)])>0,1,0) 
}

offtarget.sites$Average.Reads<- apply(offtarget.sites[2:139],1,FUN=mean)
offtarget.sites$sd.Reads<- apply(offtarget.sites[2:139],1,FUN=sd)

offtarget.sites<-offtarget.sites[-1,c(1,140:146)]

seq.scores<-seq.scores[,c(9,12,14)]
seq.scores$Query.Cover.1 <-as.numeric(gsub("%","",seq.scores$Query.Cover.1))
seq.scores$Per..Ident.1 <-as.numeric(gsub("%","",seq.scores$Per..Ident.1))
seq.scores<-aggregate(. ~ Species.1, seq.scores, mean)
offtarget.sites<-merge(offtarget.sites,seq.scores, by = "Species.1")

#write.csv(offtarget.sites,"Results/OffTargetAmplification.csv",row.names=FALSE)