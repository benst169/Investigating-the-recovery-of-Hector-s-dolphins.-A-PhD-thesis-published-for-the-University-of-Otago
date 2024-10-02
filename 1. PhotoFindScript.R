#### Catalogue Query Script

## Introduction: This script includes two parts: 1) setup, and 2) Catalogue Query. Prior to running the PhotoFindScript users need to set up the mugshot folder using the MugshotQuerySetup script this can be run at season start and again after new individuals are added to catalogue.
# Section 1: Setup needs to be run initially. If the catalogue of individuals .csv file and R script are contained within the same folder no changes are needed for the script to work. 
# Section 2: Catalogue query is the primary focus of this script. If mugshot photos have been put into the Query Catalogue folder then script can be run
## NOTE: R script, "Catalogue_of_Individuals_BP.csv", and query-able mugshots must be contained within the same folder.


# 1) Setup----
##run section prior to starting either "Query Catalogue" or "Query-able Mugshot Setup"
rm(list = ls(all=TRUE))
library(magick)
library(stringr)
library(dplyr)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #set working directory is set to the Query-able Catalogue photos. This code will only work if both the catalogue csv and R script are contained within the Query-able Catalogue folder

##Read in Catalogue data

cat<-read.csv("2. Catalogue_of_Individuals_BP.csv") #Read in catalogue data
cat$DateFirstSighted<-as.Date(cat$DateFirstSighted, "%d/%m/%Y")
cat$DateLastSighted<-as.Date(cat$DateLastSighted, "%d/%m/%Y")




# 2) QUERY CATALOGUE----
##Setup query-able mugshot dataframe
directory<-dirname(rstudioapi::getActiveDocumentContext()$path)
mugshots<-list.files(directory,full.names = TRUE,pattern=".JPG") 
mugshots<-as.data.frame(cbind(mugshots,do.call(rbind,strsplit(mugshots,"/")))) #Split filepath into elements so that individual ID can be isolated
mugshots[,c("IndividualID","side")]<-str_split_fixed(mugshots[,ncol(mugshots)],"_",2) #Split the Picture name into Individual ID and side.
mugshots<-mugshots[,c(1,(ncol(mugshots)-1):ncol(mugshots))]
colnames(mugshots)<-c("filepath","IndividualID","side")

mugshots<-left_join(mugshots,cat,by=c("IndividualID")) #Join query-able catalogue data to mugshot locations
attach(mugshots) #attach mugshots so can query easily by supplying just column names (no need for $)


#LIST OF CATEGORIES AND WAYS TO QUERY:
###Code: grepl("CODESTART",IndividualID) ##Mostly works only problem is doesn't seem to register the . so FS will include FSV, FSVW,FSQ etc.
###Additional data: DateFirstSighted (>/==/< "yyyy-mm-dd"), DateLastSighted (>/==/< "yyyy-mm-dd")
###Biological data: MinimumAge (</==/> #. Supply integer value),  Sex (== "F", "M"),  SightedWithCalf (== "y")
###Fin descriptors: MarkCategory (</==/> #), TNY (== 1/0), SMLL (== 1/0), LRG (== 1/0), SN (== 1/0), DN (== 1/0), FSV (== 1/0), FIN_SHAPE (== 1/0), MISSING (== 1/0), TAB (== 1/0), HOLE (== 1/0), BBW (== 1/0)
###Fin marks: FIN_# (== "D", "V", "W", "C", "MV", "MC", "SQ", "SLA", "FSL", "H", "MOS", "RIP", "SER", "M"). If unsure of the mark type (e.g. Could be SQ or C or SL) then use FIN_# (!= "0"). NOTE  Fin_# go from 1:7
###Query for the absence of a fin mark: FIN_# (== "0").
###Body marks: BODY_# (!="0"). NOTE: prior code asks to retrieve all mugshots where the dolphin has any kind of body mark on either side in certain position (BODY_# = 1:6). 
###Body marks: BODY_# (=="BBW"), (=="BBT"), (=="BS"), (=="V"), (=="MV"), (=="C"), (=="BD"), (=="PC"), (=="FS"), (=="DG"). NOTE: If dolphin has multiple types of marks in one position the coding gets confusing, use grepl("character string of what you see", BODY_#) as your search criteria. e.g. a dolphin with deep tissue scaring (BS) and associate V marks in body position 5 could be searched for like: subset(mugshots, grepl("V",BODY_5), grepl("BS", BODY_5)) 

#EXAMPLE QUERY: queried.images<-subset(mugshots, MarkCategory < 3 & Sex == "F" & SN == 1 & FIN_7 != "0") ... This example asks for all cat 1/2 individuals that are female, with a single nick only in position 7 on the dorsal fin (!= means does not = 0)
#Example Query: queried.images<-subset(mugshots, MarkCategory < 3 & FIN_5 | MarkCategory < 3 & FIN_6) ... This example asks for cat 1/2 individuals with a mark in position 5 or 6 if you were unsure of exactly where the mark falls

##Query start point----
#USER TO VARY. Query the catalogue with the above categories.
queried.images<-subset(mugshots, MarkCategory == 3 & FIN_1 == "0" & FIN_2 == "0" & FIN_3 == "0" & FIN_4 == "0" & FIN_5 == "0" & FIN_6 == "0" & FIN_7 == "SQ" & grepl("BS",BODY_4)) 



                       

img<-image_read(queried.images$filepath,strip=TRUE)
image_write(img,format = "pdf", "3.Temp_Catalogue.pdf") #Export temporary catalogue based on queries
rm(img); gc()#IMPORTANT Free up memory within R between each query



##NOTE: To query new set of data reset line 53 (queried.images) and rerun from there.
###Query end point----

#Run this section prior to closing R
detach(mugshots)

tmp_dir <- tempdir()
files <- list.files(tmp_dir, full.names = T, pattern = "^file")
file.remove(files)



#Session end----
