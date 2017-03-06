## This is the main program which imports every text file, extracts the desired 
## temporal features from them and saves them into a longitudinal data form

rm(list=ls())
setwd("~/Documents/Kelsa/Individual_identification_project/scriptsR")

# Calling Files
temp = list.files(path="~/Documents/Kelsa/Individual_identification_project/Data/chirps_On_Off_set_txt_files/" ,pattern="*.txt")
for (i in 1:length(temp)) assign(temp[i], read.table(file.path ( path= "~/Documents/Kelsa/Individual_identification_project/Data/chirps_On_Off_set_txt_files/",temp[i]),stringsAsFactors=FALSE))


syLLFeatures<-data.frame(indivID=character(),nIght=character(),chirpN=numeric()
                         ,syl1Dur=numeric(),syl2Dur=numeric(),syl3Dur=numeric(),
                         syl4Dur=numeric(),syl5Dur=numeric(),syl6Dur=numeric(),
                         syl7Dur=numeric(),syl12Per=numeric(),syl23Per=numeric()
                         ,syl34Per=numeric(),syl45Per=numeric(),
                         syl56Per=numeric(),syl67Per=numeric(),
                         stringsAsFactors=FALSE)

chirpFeatures<-data.frame(indivID=character(),nIght=character(),chirpN=numeric()
                         ,syll1ChirpDur=numeric(),syl2ChirpDur=numeric(),
                         syl3ChirpDur=numeric(),syl4ChirpDur=numeric(),
                         syl5ChirpDur=numeric(),syl6ChirpDur=numeric(),
                         syl7ChirpDur=numeric(),syl1ChirpPer=numeric(),
                         syl2ChirpPer=numeric(),syl3ChirpPer=numeric(),
                         syl4ChirpPer=numeric(),syl5ChirpPer=numeric(),
                         syl6ChirpPer=numeric(),syl7ChirpPer=numeric(),
                         stringsAsFactors=FALSE)


couNT=1
for (i in 1:length(temp) ){
  data=get(temp[i])
  source("extractTemporalFeats.R")
  tempSyll=extractTF(data)
  syLLFeatures[couNT:(couNT+nrow(tempSyll)-1),1]=substr(temp[i],1,3)
  syLLFeatures[couNT:(couNT+nrow(tempSyll)-1),2]=substr(temp[i],4,4)
  syLLFeatures[couNT:(couNT+nrow(tempSyll)-1),3]=1:nrow(tempSyll)
  syLLFeatures[couNT:(couNT+nrow(tempSyll)-1),4:ncol(syLLFeatures)]=tempSyll
  couNT=couNT+nrow(tempSyll)
  rm(data,tempSyll)  
}

syLLFeaturesN<-syLLFeatures

colnames(syLLFeaturesN)[11:16]<-c("syl12Gap","syl23Gap","syl34Gap","syl45Gap","syl56Gap","syl67Gap")
syLLFeaturesN[,11:16]<-syLLFeaturesN[,11:16]-syLLFeaturesN[,4:9]

write.csv(syLLFeaturesN,file="syllFeatures1_forModel1.csv")

