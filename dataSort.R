### This combines and sorts data to generate the df for the models: 
### combines temporal and spectral features, removes NAs and other incosistencies

setwd("~/Documents/Indiv_ID_project/scriptsR") ## For desktop
source("samplDat.R")
## Loading the temporal features
temporalFeatures=read.csv("perchirp_temporalFeats.csv")
temporalFeats=temporalFeatures[, -c(2,3,9,10,15,16,17)] # Getting rid of the 6th and 7th syllables
temporalFeatsComp=na.omit(temporalFeats) # Removing the rows with NAs

# Creating train and test sets specifically for temporal features
trainT<-sampLDat(temporalFeatsComp$indivID)
testT=temporalFeatsComp[-trainT,]

## Loads the spectral features data
spectralFeats_whole<-read.csv("perchirp-mfccs.csv")
spectralFeats<-spectralFeats_whole[,c(1,4:15,18)] # Removing unwanted columns
spectralFeatsComp<-na.omit(spectralFeats) # Removing the rows with NAs
colnames(spectralFeatsComp)[1]<-c("indivID") # Renaming the response
trainS<-sampLDat(spectralFeatsComp$indivID) # training data for spectral features only
testS=spectralFeatsComp[-trainS,]

##  Combinign temporal and spectral features and sorting data

# Reassemsbling temp and spectral data with the nIghts column
callFeats_whole=cbind(temporalFeatures[,-c(3,9,10,15,16,17)],spectralFeats_whole[,c(4:15,18)])
callFeatsNitwise=na.omit(callFeats_whole)

# Removing individuals with less than 20 chirps
ind_c_fewChirps=which(table(callFeatsNitwise$indivID)<20) # Individuals with less than 20 chirps in combined data
remInd=which(callFeatsNitwise$indivID %in% names(ind_c_fewChirps)) # Indices of those chirps in the combined data

# Final dataframe for models
callFeatsComp=droplevels(callFeatsNitwise[-remInd,-2]) # removing those chirps and nIght column from the Data


# ## Creating training sets with chirps from only 1 night
# trainNitwise1=which(callFeatsNitwise$nIght==1) # indices for 1st night
# trainNitwise2=which(callFeatsNitwise$nIght==2) # indices for 2nd night
# 
# # removing individuals with less that 10 chirps from the trainNitwise subset
# ind_fewChirpsT1=which(table(callFeatsNitwise$indivID[trainNitwise1])<20) # Individuals with less than 20 chirps 
# ind_fewChirpsT2=which(table(callFeatsNitwise$indivID[trainNitwise2])<20) # Individuals with less than 20 chirps 
# remIndoN1=which(callFeatsNitwise$indivID %in% names(ind_fewChirpsT1)) # Indices of those chirps in the combined data
# remIndoN2=which(callFeatsNitwise$indivID %in% names(ind_fewChirpsT2))
# callFeatsTest1=droplevels(callFeatsNitwise[-remIndoN1,]) # removing those chirps from the Data
# callFeatsTest2=droplevels(callFeatsNitwise[-remIndoN2,])
# # Final dataframe for models
# callFeatsCompN=callFeatsCompcN[,-2] # removing 'nIght' column

for (i in 1:2) {
  tempIndex=which(callFeatsNitwise$nIght==i) # indices for 1st night
  indivR=which(table(callFeatsNitwise$indivID[tempIndex])<20) # Individuals with less than 20 chirps
  remIndN=which(callFeatsNitwise$indivID %in% names(indivR)) # Indices of all chirps of those those individuals
  dfNam <- paste("callFeatsCompoN", i, sep = "") # creating dataframe name
  # filling the df by dropping the chirps from selected individuals
  assign(dfNam, droplevels(callFeatsNitwise[-remIndN,]))   
  tempDF=get(dfNam)
  
  ## Train and test across nights
  # Training data with chirps from ith night
  trainNam <- paste("trainoN", i, sep = "") # creating training variable name
  # filling the df by subsetting for chirps from every night
  # assign(trainNam, which(get(paste0("callFeatsCompoN", 1))$nIght==i)) 
  assign(trainNam, which(tempDF$nIght==i))
  tempTrain=get(trainNam)
  
  # Test data with chirps from the remaining nights
  testNam <- paste("tesTforN", i, sep = "") # creating test data name
  # filling the df by removing the chirps in the train data
  # assign(testNam, get(paste0("callFeatsCompoN", i))[-get(paste0("trainoN", i)),]) 
  assign(testNam, tempDF[-tempTrain,-2])
  
  # Test data with chirps from only one of the remaining nights
  testNamN <- paste("tesToN", i, sep = "") # creating test data name
  # filling the df by selecting chirps from night that was not included in train set
  # assign(testNam, subset(get(paste0("callFeatsCompoN",i)), nIght!=i && nIght!=3))
  assign(testNamN, subset(tempDF, !(nIght %in% c(i,3)), select=-nIght))
  
  ## Train and test within nights
  dfNNam <- paste("callFeatsCompcN", i, sep = "") # creating dataframe name
  # filling the df by chirps from only one night
  assign(dfNNam, tempDF[tempTrain,-2])
  tempDFN=get(dfNNam)
  
  # Creating training set for within-night data
  trainNNam=paste("trainN",i,sep="") # df name
  assign(trainNNam,sampLDat(tempDFN$indivID)) # samples 70 % of chirps per individual
  tempTrainN=get(trainNNam)
  # Creating test set for within-night data
  testNNam=paste("testN",i,sep="") # df names
  assign(testNNam, tempDFN[-tempTrainN,]) 
  
  assign(dfNam, tempDF[,-2])

  }

# # Recreating trainsets from only 1st night chirps
# trainN=which(callFeatsCompN$nIght==1) # indices for 1st night after removing individuals with few chirps
# testN=callFeatsCompN[-trainN,] # Rest of the nights as the test set
# testN=testN[,-2] # removing the nIght column
# callFeatsCompN=callFeatsCompN[,-2] # removing the nIght column
# 
# testoN1=subset(callFeatsCompcN,nIght==2)
# testoN2=subset(callFeatsCompcN,nIght==2)
# testoN3=subset(callFeatsCompcN,nIght==3)
# testoN1=testoN1[,-2]
# testoN2=testoN2[,-2]
# testoN3=testoN3[,-2]
# 
# 
# # Trainset with chirps from differnt nights
# trainF<-sampLDat(callFeatsComp$indivID) # training data for spectral features only
# testF=callFeatsComp[-trainF,]
# 
# 
# ## Creating dataframes with chirps from within-night recordings separately
# callFeatscompN1=subset(callFeatsCompcN,nIght==1) # Chirps from only the 1st night
# callFeatscompN2=subset(callFeatsCompcN,nIght==2) # Chirps from only the 2nd night
# 
# ind_fewChirpsN1=which(table(callFeatscompN1$indivID)<20) # Individuals with less than 20 chirps in combined data
# remIndN1=which(callFeatscompN1$indivID %in% names(ind_fewChirpsN1)) # Indices of those chirps in the combined data
# callFeatsCompcN1=droplevels(callFeatscompN1[-remIndN1,-2]) # removing those chirps from the Data
# 
# ind_fewChirpsN2=which(table(callFeatscompN2$indivID)<20) # Individuals with less than 20 chirps in combined data
# remIndN2=which(callFeatscompN2$indivID %in% names(ind_fewChirpsN2)) # Indices of those chirps in the combined data
# callFeatsCompcN2=droplevels(callFeatscompN2[-remIndN2,-2]) # removing those chirps from the Data
# 
# 
# trainN1<-sampLDat(callFeatsCompcN1$indivID) # training data for spectral features only
# testN1=callFeatsCompcN1[-trainN1,]
# 
# trainN2<-sampLDat(callFeatsCompcN2$indivID) # training data for spectral features only
# testN2=callFeatsCompcN2[-trainN2,]


## Creating dataframe with equal replicates per individual
# for equal sample sizes
countIndiv<-table(callFeatsComp$indivID) # Number of chirps/indiv
noIndiv<-length(countIndiv) # Number of individuals
samSize<-min(countIndiv) # Minimum number of chirps/indiv
equalInd<-vector(mode = "numeric") # Initialize to store the selected chirp indices
indR=1 # index counter for the input data (combined)
indS=1 # index counter for the output vector
for (i in 1:noIndiv){
noRep<-unname(countIndiv[i])
equalInd[indS:(indS+samSize-1)]=sample(indR:(indR+noRep-1),samSize) # Randomly selects samSize number of chirps for every individual
indR=indR+noRep
indS=indS+samSize
}
indF<-sort(equalInd) # sorts the selected indices in ascending order for every indiviual
callFeatsCompEq<-callFeatsComp[indF,] # creates the new df with equal number of chirps/Indiv
trainEq<-sampLDat(callFeatsCompEq$indivID) # training data for spectral features only
testEq=callFeatsCompEq[-trainF,]

