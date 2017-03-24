## Random Forest on the temporal features
# Requires the variable "temporalFeatures"

library(randomForest)

source("samplDat.R")

tempFeatures=temporalFeatures[, -c(2,3,9,10,15,16,17)] # Getting rid of the 6th and 7th syllables
tempFeaturesComp=na.omit(tempFeatures)
trainSet<-sampLDat(tempFeaturesComp$indivID)
testData=tempFeaturesComp[-trainSet,]


# bagTF0=randomForest(as.factor(indivID)~syl1Dur+syl2Dur+syl3Dur+syl4Dur+syl5Dur+syl12Gap+syl23Gap+syl34Gap+syl45Gap+chirpDur+chirpGap, data=tempFeatures, subset=trainSet, mtry=11, importance=TRUE, na.action = na.omit) 
# testResult0=predict(bagTF0,testData,type="class")
# accBag0=table(testResult0,testData$indivID)
# sum(diag(prop.table(accBag0)))



## Bagging
bagTF=randomForest(as.factor(indivID)~., data=tempFeaturesComp, subset=trainSet, mtry=11, importance=TRUE) 
testResult=predict(bagTF,testData,type="class")
accBag=table(testResult,testData$indivID)
sum(diag(prop.table(accBag)))

## Random Forest (m=p/3)

randFor1.TF=randomForest(as.factor(indivID)~., data=tempFeaturesComp, subset=trainSet, importance=TRUE) 
testResult.RF1=predict(randFor1.TF,testData,type="class")
accRF1=table(testResult.RF1,testData$indivID)
sum(diag(prop.table(accRF1)))

importance(randFor1.TF)
varImpPlot(randFor1.TF)

## Random Forest (using tuneRF)


tunedP <- tuneRF(x=tempFeaturesComp[trainSet,-1],y=as.factor(tempFeaturesComp[trainSet,1]),ntreeTry=2000,stepFactor=2)
mtry_tune <- tunedP[which(tunedP[,2]==min(tunedP[,2]))[1],1]
randForT1.TF=randomForest(as.factor(indivID)~., data=tempFeaturesComp, subset=trainSet, mtry=mtry_tune, ntree=2000,importance=T) 
testResult.RFT1=predict(randForT1.TF,testData,type="class")
accRFT1=table(testResult.RFT1,testData$indivID)
sum(diag(prop.table(accRFT1)))


## Random Forest without the 5th syllalble
tempFeaturesW5=temporalFeatures[, -c(2,3,8,9,10,14,15,16,17)] # Getting rid of the 5th and 6th, 7th syllables
tempFeaturesCompW5=na.omit(tempFeaturesW5)
trainSetW5<-sampLDat(tempFeaturesCompW5$indivID)
testDataW5=tempFeaturesCompW5[-trainSetW5,]

randFor1W5.TF=randomForest(as.factor(indivID)~., data=tempFeaturesCompW5, subset=trainSetW5, importance=TRUE) 
testResult.RF1W5=predict(randFor1W5.TF,testDataW5,type="class")
accRF1W5=table(testResult.RF1W5,testDataW5$indivID)
sum(diag(prop.table(accRF1W5)))




## Vote collection

