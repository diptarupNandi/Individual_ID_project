## This is the main program which imports every text file, extracts the desired 
## temporal features from them and saves them into a longitudinal data form

rm(list=ls())

setwd("~/Documents/Indiv_ID_project/scriptsR") ## For desktop
# setwd("~/Documents/Kelsa/Individual_identification_project/scriptsR") ## For macBook

library(dplyr)
library(plyr)
library(gtools)

source("extractTemporalFeats.R")
# Calling Files

## For desktop
txtNam = mixedsort(list.files(path="~/Documents/Indiv_ID_project/Data/chirps_On_Off_set_txt_files/" ,pattern="*.txt"))
# for (i in 1:length(temp)) assign(temp[i], read.table(file.path ( path= "~/Documents/Indiv_ID_project/Data/chirps_On_Off_set_txt_files/",temp[i]),stringsAsFactors=FALSE))

## For macBook
# txtNam = list.files(path="~/Documents/Kelsa/Individual_identification_project/Data/chirps_On_Off_set_txt_files/" ,pattern="*.txt")
# for (i in 1:length(temp)) assign(temp[i], read.table(file.path ( path= "~/Documents/Kelsa/Individual_identification_project/Data/chirps_On_Off_set_txt_files/",temp[i]),stringsAsFactors=FALSE))


temporalFeatures<-data.frame(indivID=character(),nIght=character(),chirpN=numeric()
                         ,syl1Dur=numeric(),syl2Dur=numeric(),syl3Dur=numeric(),
                         syl4Dur=numeric(),syl5Dur=numeric(),syl6Dur=numeric(),
                         syl7Dur=numeric(),syl12Gap=numeric(),syl23Gap=numeric()
                         ,syl34Gap=numeric(),syl45Gap=numeric(),
                         syl56Gap=numeric(),syl67Gap=numeric(),
                         syLpChirp=numeric(),chirpDur=numeric(),
                         chirpGap=numeric(),
                         stringsAsFactors=FALSE)



couNT=1
for (i in 1:length(txtNam) ){
  # fileNam=tolower(file_path_sans_ext(txtnam[i]))
  data = read.table(paste("~/Documents/Indiv_ID_project/Data/chirps_On_Off_set_txt_files/", txtNam[i],sep=""))
  # data=get(temp[i])
  tempSyll=extractTF(data)
  temporalFeatures[couNT:(couNT+nrow(tempSyll)-1),1]=substr(txtNam[i],1,3)
  temporalFeatures[couNT:(couNT+nrow(tempSyll)-1),2]=substr(txtNam[i],nchar(txtNam[i])-4,nchar(txtNam[i])-4)
  temporalFeatures[couNT:(couNT+nrow(tempSyll)-1),3]=1:nrow(tempSyll)
  temporalFeatures[couNT:(couNT+nrow(tempSyll)-1),4:ncol(temporalFeatures)]=tempSyll
  couNT=couNT+nrow(tempSyll)
  rm(data,tempSyll)  
}

temporalFeatures=arrange(temporalFeatures,indivID,nIght,chirpN)
# temporalFeaturesN<-temporalFeatures

# colnames(temporalFeaturesN)[11:16]<-c("syl12Gap","syl23Gap","syl34Gap","syl45Gap","syl56Gap","syl67Gap")
# temporalFeaturesN[,11:16]<-temporalFeaturesN[,11:16]-temporalFeaturesN[,4:9]

write.csv(temporalFeatures,file="perchirp_temporalFeats.csv",row.names = FALSE)

