rm(list = ls(all=TRUE))
library(magick)
library(stringr)
library(dplyr)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #set working directory is set to the Query-able Catalogue photos. This code will only work if both the catalogue csv and R script are contained within the Query-able Catalogue folder


##Run at start of season, or after new individuals have been added to normal catalogue folders.

## START----
##USER SPECIFIED CODE
##filepaths need to be specific for the computer where original catalogue mugshots are contained. One line for each subfolder
mugshot.setup<-list.files(c("D:/NZ dolphin long term photos and catalogue_FROM_STEPH_HD/1. CataloguePhotos_mugshots Dec 2023/Body Blotch - Tattoo (BBT)",
                            "D:/NZ dolphin long term photos and catalogue_FROM_STEPH_HD/1. CataloguePhotos_mugshots Dec 2023/Body Blotch - White (BBW)",
                            "D:/NZ dolphin long term photos and catalogue_FROM_STEPH_HD/1. CataloguePhotos_mugshots Dec 2023/Body Deformity (BD)",
                            "D:/NZ dolphin long term photos and catalogue_FROM_STEPH_HD/1. CataloguePhotos_mugshots Dec 2023/Body Scar (BS)",
                            "D:/NZ dolphin long term photos and catalogue_FROM_STEPH_HD/1. CataloguePhotos_mugshots Dec 2023/Fin Hole (FH)",
                            "D:/NZ dolphin long term photos and catalogue_FROM_STEPH_HD/1. CataloguePhotos_mugshots Dec 2023/Fin nick Large (FL)",
                            "D:/NZ dolphin long term photos and catalogue_FROM_STEPH_HD/1. CataloguePhotos_mugshots Dec 2023/Fin nick SeVeral - W nicks (FSVW)",
                            "D:/NZ dolphin long term photos and catalogue_FROM_STEPH_HD/1. CataloguePhotos_mugshots Dec 2023/Fin nick SeVeral (FSV)",
                            "D:/NZ dolphin long term photos and catalogue_FROM_STEPH_HD/1. CataloguePhotos_mugshots Dec 2023/Fin nick Small (FS)",
                            "D:/NZ dolphin long term photos and catalogue_FROM_STEPH_HD/1. CataloguePhotos_mugshots Dec 2023/Fin nick Square (FSQ)",
                            "D:/NZ dolphin long term photos and catalogue_FROM_STEPH_HD/1. CataloguePhotos_mugshots Dec 2023/Fin shape (F)",
                            "D:/NZ dolphin long term photos and catalogue_FROM_STEPH_HD/1. CataloguePhotos_mugshots Dec 2023/Fin SLice (FSL)"),
                          full.names = TRUE,pattern=".JPG") 

mugshot.setup<-as.data.frame(cbind(mugshot.setup,do.call(rbind,strsplit(mugshot.setup,"/")))) #Split filepath list into a dataframe of each element so that individual ID can be isolated
photonames<-mugshot.setup[,ncol(mugshot.setup)]#The last column will contain the photo name. Important to isolate this.
mugshot.setup[,c("IndividualID","side")]<-str_split_fixed(mugshot.setup[,ncol(mugshot.setup)],"_",2) #Create Individual ID and side of photo columns.
mugshot.setup<-mugshot.setup[,c("mugshot.setup","IndividualID")] #remove excess data
mugshot.setup$photonames<-photonames #Add photonames vector as a column in the dataframe


##READ ORIGINAL CATALOGUE IMAGES INTO QUERY FOLDER
N<-length(mugshot.setup[,1])
for(i in 1:N){ 
  tmp.img<-image_read(mugshot.setup[i,1])
  info<-image_info(tmp.img)
  image_annotate(tmp.img,text=mugshot.setup[i,2], size=info$height/15,color = "white")%>%
    image_write(path = mugshot.setup[i,3], format= "jpg")
}


rm(N,tmp.img,info,i,mugshot.setup,photonames) #Remove all unnecessary objects

#END MUGSHOT SETUP----

