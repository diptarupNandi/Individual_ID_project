## This is the main program which imports every text file, extracts the desired 
## temporal features from them and saves them into a longitudinal data form

rm(list=ls())

setwd("~/Documents/Indiv_ID_project/scriptsR") ## For desktop
# setwd("~/Documents/Kelsa/Individual_identification_project/scriptsR") ## For macBook

# Calling Files

## For desktop
temp =mixedsort(list.files(path="~/Documents/Indiv_ID_project/Data/chirps_On_Off_set_txt_files/" ,pattern="*.txt"))
for (i in 1:length(temp)) assign(temp[i], read.table(file.path ( path= "~/Documents/Indiv_ID_project/Data/chirps_On_Off_set_txt_files/",temp[i]),stringsAsFactors=FALSE))

## For macBook
# temp = list.files(path="~/Documents/Kelsa/Individual_identification_project/Data/chirps_On_Off_set_txt_files/" ,pattern="*.txt")
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
for (i in 1:length(temp) ){
  data=get(temp[i])
  source("extractTemporalFeats.R")
  tempSyll=extractTF(data)
  temporalFeatures[couNT:(couNT+nrow(tempSyll)-1),1]=substr(temp[i],1,3)
  temporalFeatures[couNT:(couNT+nrow(tempSyll)-1),2]=substr(temp[i],nchar(temp[i])-4,nchar(temp[i])-4)
  temporalFeatures[couNT:(couNT+nrow(tempSyll)-1),3]=1:nrow(tempSyll)
  temporalFeatures[couNT:(couNT+nrow(tempSyll)-1),4:ncol(temporalFeatures)]=tempSyll
  couNT=couNT+nrow(tempSyll)
  rm(data,tempSyll)  
}

# temporalFeaturesN<-temporalFeatures

# colnames(temporalFeaturesN)[11:16]<-c("syl12Gap","syl23Gap","syl34Gap","syl45Gap","syl56Gap","syl67Gap")
# temporalFeaturesN[,11:16]<-temporalFeaturesN[,11:16]-temporalFeaturesN[,4:9]

# write.csv(temporalFeaturesN,file="temporalFeatures1_forModel1.csv")

