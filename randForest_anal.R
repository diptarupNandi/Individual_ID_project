## Random Forest on the temporal features
# Requires the variable "temporalFeatures"

library(randomForest)
source("getVotes.R")



# bagTF0=randomForest(as.factor(indivID)~syl1Dur+syl2Dur+syl3Dur+syl4Dur+syl5Dur+syl12Gap+syl23Gap+syl34Gap+syl45Gap+chirpDur+chirpGap, data=temporalFeats, subset=trainT, mtry=11, importance=TRUE, na.action = na.omit) 
# testResult0=predict(bagTF0,testData,type="class")
# accBag0=table(testResult0,testData$indivID)
# sum(diag(prop.table(accBag0)))


## Bagging

# For just temporal features with maximum data
bagTF=randomForest(as.factor(indivID)~., data=temporalFeatsComp, subset=trainT, mtry=11, importance=TRUE) 
predBagTF=predict(bagTF,testT,type="class")
accBagTF=table(testT$indivID,predBagTF)
sum(diag(prop.table(accBagTF)))

# For just spectral features with maximum data
bagSF=randomForest(as.factor(indivID)~., data=spectralFeatsComp, subset=trainS, mtry=13, importance=TRUE) 
predBagSF=predict(bagSF,testS,type="class")
accBagSF=table(testS$indivID,predBagSF)
sum(diag(prop.table(accBagSF)))

# For just temporal features from combined data
bagTFc=randomForest(as.factor(indivID)~., data=callFeatsComp[,1:12], subset=trainF, mtry=11, importance=TRUE) 
predBagTFc=predict(bagTFc,testF[,1:12],type="class")
accBagTFc=table(testF$indivID,predBagTFc)
sum(diag(prop.table(accBagTFc)))

# For just spectral features from combined data
bagSFc=randomForest(as.factor(indivID)~., data=callFeatsComp[,c(1,13:25)], subset=trainF, mtry=13, importance=TRUE) 
predBagSFc=predict(bagSFc,testF[,c(1,13:25)],type="class")
accBagSFc=table(testF$indivID,predBagSFc)
sum(diag(prop.table(accBagSFc)))

# For combined data
bagComb=randomForest(as.factor(indivID)~., data=callFeatsComp, subset=trainF, mtry=24, importance=TRUE) 
predBagComb=predict(bagComb,testF,type="class")
accBagComb=table(testF$indivID,predBagComb)
sum(diag(prop.table(accBagComb)))


## Random Forest (using tuneRF)

# For just temporal features with maximum data
tunedP.TF <- tuneRF(x=temporalFeatsComp[trainT,-1],y=as.factor(temporalFeatsComp[trainT,1]),ntreeTry=2000,stepFactor=2)
mtry_tune.TF <- tunedP.TF[which(tunedP.TF[,2]==min(tunedP.TF[,2]))[1],1]
randForT1.TF=randomForest(as.factor(indivID)~., data=temporalFeatsComp, subset=trainT, mtry=mtry_tune.TF, ntree=2000, importance=T) 
predRFT1.TF=predict(randForT1.TF,testT,type="class")
accRFT1.TF=table(testT$indivID,predRFT1.TF)
sum(diag(prop.table(accRFT1.TF)))

# For just spectral features with maximum data
tunedP.SF <- tuneRF(x=spectralFeatsComp[trainS,-1],y=as.factor(spectralFeatsComp[trainS,1]),ntreeTry=2000,stepFactor=2)
mtry_tune.SF <- tunedP.SF[which(tunedP.SF[,2]==min(tunedP.SF[,2]))[1],1]
randForT1.SF=randomForest(as.factor(indivID)~., data=spectralFeatsComp, subset=trainF, mtry=mtry_tune.SF, ntree=2000, importance=T) 
predRFT1.SF=predict(randForT1.SF,testS,type="class")
accRFT1.SF=table(testS$indivID,predRFT1.SF)
sum(diag(prop.table(accRFT1.SF)))


# For just temporal features with combined data
tunedP.TFc <- tuneRF(x=callFeatsComp[trainF,2:12],y=as.factor(callFeatsComp[trainF,1]),ntreeTry=2000,stepFactor=2)
mtry_tune.TFc <- tunedP.TFc[which(tunedP.TFc[,2]==min(tunedP.TFc[,2]))[1],1]
randForT1.TFc=randomForest(as.factor(indivID)~., data=callFeatsComp[,1:12], subset=trainF, mtry=mtry_tune.TFc, ntree=2000, importance=T) 
predRFT1.TFc=predict(randForT1.TFc,testF[,1:12],type="class")
accRFT1.TFc=table(testF$indivID,predRFT1.TFc)
sum(diag(prop.table(accRFT1.TFc)))


# For just spectral features with combined data
tunedP.SFc <- tuneRF(x=callFeatsComp[trainF,13:25],y=as.factor(callFeatsComp[trainF,1]),ntreeTry=2000,stepFactor=2)
mtry_tune.SFc <- tunedP.SFc[which(tunedP.SFc[,2]==min(tunedP.SFc[,2]))[1],1]
randForT1.SFc=randomForest(as.factor(indivID)~., data=callFeatsComp[,c(1,13:25)], subset=trainF, mtry=mtry_tune.SFc, ntree=2000, importance=T) 
predRFT1.SFc=predict(randForT1.SFc,testF[,c(1,13:25)],type="class")
accRFT1.SFc=table(testF$indivID,predRFT1.SFc)
sum(diag(prop.table(accRFT1.SFc)))

# For spectral+temporal combined data
tunedP.Comb <- tuneRF(x=callFeatsComp[trainF,-1],y=as.factor(callFeatsComp[trainF,1]),ntreeTry=2000,stepFactor=2)
mtry_tune.Comb <- tunedP.Comb[which(tunedP.Comb[,2]==min(tunedP.Comb[,2]))[1],1]
randForT1.Comb=randomForest(as.factor(indivID)~., data=callFeatsComp, subset=trainF, mtry=mtry_tune.Comb, ntree=2000, importance=T) 
predRFT1.Comb=predict(randForT1.Comb,testF,type="class")
accRFT1.Comb=table(testF$indivID,predRFT1.Comb)
sum(diag(prop.table(accRFT1.Comb)))

# Comined with equal number of chirps/individual
tunedP.CombEq <- tuneRF(x=callFeatsCompEq[trainEq,-1],y=as.factor(callFeatsCompEq[trainEq,1]),ntreeTry=2000,stepFactor=2)
mtry_tune.CombEq <- tunedP.CombEq[which(tunedP.CombEq[,2]==min(tunedP.CombEq[,2]))[1],1]
randForT1.CombEq=randomForest(as.factor(indivID)~., data=callFeatsCompEq, subset=trainEq, mtry=mtry_tune.CombEq, ntree=2000, importance=T) 
predRFT1.CombEq=predict(randForT1.CombEq,testEq,type="class")
accRFT1.CombEq=table(testEq$indivID,predRFT1.CombEq)
sum(diag(prop.table(accRFT1.CombEq)))


## Random Forest (m=p/3)

randFor1.TF=randomForest(as.factor(indivID)~., data=temporalFeatsComp, subset=trainT, importance=TRUE) 
testResult.RF1=predict(randFor1.TF,testData,type="class")
accRF1=table(testData$indivID,testResult.RF1)
sum(diag(prop.table(accRF1)))

importance(randFor1.TF)
varImpPlot(randFor1.TF)

## Random Forest without the 5th syllalble
temporalFeatsW5=temporalFeatures[, -c(2,3,8,9,10,14,15,16,17)] # Getting rid of the 5th and 6th, 7th syllables
temporalFeatsCompW5=na.omit(temporalFeatsW5)
trainTW5<-sampLDat(temporalFeatsCompW5$indivID)
testDataW5=temporalFeatsCompW5[-trainTW5,]

randFor1W5.TF=randomForest(as.factor(indivID)~., data=temporalFeatsCompW5, subset=trainTW5, importance=TRUE) 
testResult.RF1W5=predict(randFor1W5.TF,testDataW5,type="class")
accRF1W5=table(testDataW5$indivID,testResult.RF1W5)
sum(diag(prop.table(accRF1W5)))

callFeatsCompW5=callFeatsComp[,-c(6,10)] # Removing 5th syllable features
tunedP.CombW5 <- tuneRF(x=callFeatsCompW5[trainF,-1],y=as.factor(callFeatsCompW5[trainF,1]),ntreeTry=2000,stepFactor=2)
mtry_tune.CombW5 <- tunedP.CombW5[which(tunedP.CombW5[,2]==min(tunedP.CombW5[,2]))[1],1]
randForT1.CombW5=randomForest(as.factor(indivID)~., data=callFeatsCompW5, subset=trainF, mtry=mtry_tune.CombW5, ntree=2000, importance=T) 
predRFT1.CombW5=predict(randForT1.CombW5,testF,type="class")
accRFT1.CombW5=table(testF$indivID,predRFT1.CombW5)
sum(diag(prop.table(accRFT1.CombW5)))

callFeatsCompW45=callFeatsComp[,-c(5,6,9,10)] # Removing 4th and 5th syllable features
tunedP.CombW45 <- tuneRF(x=callFeatsCompW45[trainF,-1],y=as.factor(callFeatsCompW45[trainF,1]),ntreeTry=2000,stepFactor=2)
mtry_tune.CombW45 <- tunedP.CombW45[which(tunedP.CombW45[,2]==min(tunedP.CombW45[,2]))[1],1]
randForT1.CombW45=randomForest(as.factor(indivID)~., data=callFeatsCompW45, subset=trainF, mtry=mtry_tune.CombW45, ntree=2000, importance=T) 
predRFT1.CombW45=predict(randForT1.CombW45,testF,type="class")
accRFT1.CombW45=table(testF$indivID,predRFT1.CombW45)
sum(diag(prop.table(accRFT1.CombW45)))

callFeatsCompW345=callFeatsComp[,-c(4,5,6,8,9,10)] # Removing 3rd, 4th and 5th syllable features
tunedP.CombW345 <- tuneRF(x=callFeatsCompW345[trainF,-1],y=as.factor(callFeatsCompW345[trainF,1]),ntreeTry=2000,stepFactor=2)
mtry_tune.CombW345 <- tunedP.CombW345[which(tunedP.CombW345[,2]==min(tunedP.CombW345[,2]))[1],1]
randForT1.CombW345=randomForest(as.factor(indivID)~., data=callFeatsCompW345, subset=trainF, mtry=mtry_tune.CombW345, ntree=2000, importance=T) 
predRFT1.CombW345=predict(randForT1.CombW345,testF,type="class")
accRFT1.CombW345=table(testF$indivID,predRFT1.CombW345)
sum(diag(prop.table(accRFT1.CombW345)))

## Vote collection

rfAcc=getVotes(testData$indivID, testResult.RFT1)
