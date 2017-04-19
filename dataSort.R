### This combines and sorts data to generate the df for the models: 
### combines temporal and spectral features, removes NAs and other incosistencies
# setwd("~/Documents/Indiv_ID_project/scriptsR") ## For desktop


source("samplDat.R")

## Loading the temporal features

temporalFeatures=read.csv("perchirp_temporalFeats.csv")
temporalFeats=temporalFeatures[, -c(2,3,9,10,15,16,17)] # Getting rid of the 6th and 7th syllables
temporalFeatsComp=na.omit(temporalFeats) # Removing the rows with NAs

## Creating train and test sets specifically for temporal features
trainT<-sampLDat(temporalFeatsComp$indivID)
testT=temporalFeatsComp[-trainT,]


## Loads the spectral features data
spectralFeats_whole<-read.csv("perchirp-mfccs.csv")

spectralFeats<-spectralFeats_whole[,c(1,4:15,18)] # Removing unwanted columns

spectralFeatsComp<-na.omit(spectralFeats) # Removing the rows with NAs
colnames(spectralFeatsComp)[1]<-c("indivID") # Renaming the response

trainS<-sampLDat(spectralFeatsComp$indivID) # training data for spectral features only
testS=spectralFeatsComp[-trainS,]


##  Combinign temporal and spectral features 

callFeats_whole=cbind(temporalFeats,spectralFeats[,-1]) # Combining temporal and spectral datasets
callFeatsComp=na.omit(callFeats_whole) # Removing rows with NAs

# removing individuals with less than 20 chirps
ind_c_fewChirps=which(table(callFeatsComp$indivID)<20) # Individuals with less than 20 chirps in combined data
remInd=which(callFeatsComp$indivID %in% names(ind_c_fewChirps)) # Indices of those chirps in the combined data
callFeatsComp=droplevels(callFeatsComp[-remInd,]) # removing those chirps from the testData


trainF<-sampLDat(callFeatsComp$indivID) # training data for spectral features only
testF=callFeatsComp[-trainF,]


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

callFeats_Sub1<-subset(callFeatsComp,subset=)

aDD<-temporalFeats$nights[as.factor(rownames(callFeatsComp))]

callFeats_Sub0<-mutate(callFeatsComp,nIghts=aDD)----------
