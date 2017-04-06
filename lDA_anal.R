## DFA on the temporal features

# rm(list=ls())
# setwd("~/Documents/Kelsa/Individual_identification_project/scriptsR")
# 
# # loads the file from the same directory
# syLLDat<-read.csv("syllFeatures1_forModel1.csv")
# 
# # removes all unwanted columns
# temporalFeatures<-temporalFeatures[,-c(9,10,15,16)]

source("samplDat.R")
trainSet=sampLDat(temporalFeatures$indivID)

library(MASS)

## Linear Discriminant Analysis

# LDA on the whole dataset (without training and test data)
ldFit1 <- lda(indivID ~ syl1Dur+syl2Dur+syl3Dur+syl4Dur+syl5Dur+syl12Gap+syl23Gap+syl34Gap+syl45Gap+chirpDur+chirpGap, data=temporalFeatures, CV=TRUE)

ldFitT1 <- lda(indivID ~ syl1Dur+syl2Dur+syl3Dur+syl4Dur+syl5Dur+syl12Gap+syl23Gap+syl34Gap+syl45Gap+chirpDur+chirpGap, data=temporalFeatures)


# With all the variables without any interaction term
ldFit2 <- lda(indivID ~ syl1Dur+syl2Dur+syl3Dur+syl4Dur+syl5Dur+syl12Gap+syl23Gap+syl34Gap+syl45Gap+chirpDur+chirpGap, data=tempFeaturesComp[trainSet,])
testResult2<-predict(ldFit2,tempFeaturesComp[-trainSet,])

# With all the variables for only 5syLChirps
ldFit5sylCh <- lda(indivID ~ syl1Dur+syl2Dur+syl3Dur+syl4Dur+syl5Dur+syl12Gap+syl23Gap+syl34Gap+syl45Gap+chirpDur+chirpGap, data=subset(temporalFeatures[trainSet,],syLpChirp==5))
testResult5sylCh<-predict(ldFit5sylCh,temporalFeatures[-trainSet,])

# With predominantly available variables 
ldFitPV <- lda(indivID ~ syl1Dur+syl2Dur+syl12Gap+syl23Gap+chirpDur+chirpGap, data=temporalFeatures[trainSet,])
testResultPV<-predict(ldFitPV,temporalFeatures[-trainSet,])

# for equal sample sizes
## Creating dataframe with equal replicates per individual

countIndiv<-table(temporalFeatures$indivID)
noIndiv<-length(countIndiv)
samSize<-min(countIndiv)

equalInd<-vector(mode = "numeric")
indR=1
indS=1
for (i in 1:noIndiv){
  noRep<-unname(countIndiv[i])
  equalInd[indS:(indS+samSize-1)]=sample(indR:(indR+noRep-1),samSize)
  indR=indR+noRep
  indS=indS+samSize
}

indF<-sort(equalInd)
temporalFeaturesEq<-temporalFeatures[indF,]

trainSetEq<-sampLDat(temporalFeaturesEq$indivID)

# Finally LDA
ldFitEq <- lda(indivID ~ syl1Dur+syl2Dur+syl3Dur+syl4Dur+syl5Dur+syl12Gap+syl23Gap+syl34Gap+syl45Gap+chirpDur+chirpGap, data=temporalFeaturesEq[trainSetEq,])
testResultEq<-predict(ldFitEq,temporalFeaturesEq[-trainSetEq,])


## Assess the accuracy of the prediction
acTable1 <- table(temporalFeatures$indivID[-ldFit1$na.action], ldFit1$class)
acTable2 <- table(tempFeaturesComp$indivID[-trainSet], testResult2$class)
acTable5sylCh <- table(temporalFeatures$indivID[-trainSet], testResult5sylCh$class)
acTablePV <- table(temporalFeatures$indivID[-trainSet], testResultPV$class)
acTableEq <- table(temporalFeaturesEq$indivID[-trainSetEq], testResultEq$class)

# percent correct for each individual
diag(prop.table(acTable1, 1))
# total percent correct
sum(diag(prop.table(acTable1)))
sum(diag(prop.table(acTable2)))
sum(diag(prop.table(acTable5sylCh)))
sum(diag(prop.table(acTablePV)))
sum(diag(prop.table(acTableEq)))

## LDA on Reiterated training and test set

acCoClass<-vector(mode="numeric")
iteR=100
source("samplDat.R")

for (i in 1:iteR){
  trainSet<-sampLDat(temporalFeatures$indivID)
  ldFit <- lda(indivID ~ syl1Dur+syl2Dur+syl3Dur+syl4Dur+syl5Dur+syl12Gap+syl23Gap+syl34Gap+syl45Gap+chirpDur+chirpGap, data=temporalFeatures[trainSet,])
  testResult<-predict(ldFit,temporalFeatures[-trainSet,])
  acTable <- table(temporalFeatures$indivID[-trainSet], testResult$class)
  acCoClass[i]<-sum(diag(prop.table(acTable)))
}







## Sequential addition of syllable and chirp features
accSeq<-as.data.frame(matrix(nrow=6,ncol=1))

ldSeqS1 <- lda(indivID ~ syl1Dur+syl12Gap, data=temporalFeatures[trainSet,])
predictTestSeqS1<-predict(ldSeqS1,temporalFeatures[-trainSet,])
acTableSeqS1 <- table(temporalFeatures$indivID[-trainSet], predictTestSeqS1$class)
accSeq[1,1]<-sum(diag(prop.table(acTableSeqS1)))

ldSeqS2 <- lda(indivID ~ syl1Dur+syl2Dur+syl12Gap+syl23Gap, data=temporalFeatures[trainSet,])
predictTestSeqS2<-predict(ldSeqS2,temporalFeatures[-trainSet,])
acTableSeqS2 <- table(temporalFeatures$indivID[-trainSet], predictTestSeqS2$class)
accSeq[2,1]<-sum(diag(prop.table(acTableSeqS2)))

ldSeqS3 <- lda(indivID ~ syl1Dur+syl2Dur+syl3Dur+syl12Gap+syl23Gap+syl34Gap, data=temporalFeatures[trainSet,])
predictTestSeqS3<-predict(ldSeqS3,temporalFeatures[-trainSet,])
acTableSeqS3 <- table(temporalFeatures$indivID[-trainSet], predictTestSeqS3$class)
accSeq[3,1]<-sum(diag(prop.table(acTableSeqS3)))

ldSeqS4 <- lda(indivID ~ syl1Dur+syl2Dur+syl3Dur+syl4Dur+syl12Gap+syl23Gap+syl34Gap+syl45Gap, data=temporalFeatures[trainSet,])
predictTestSeqS4<-predict(ldSeqS4,temporalFeatures[-trainSet,])
acTableSeqS4 <- table(temporalFeatures$indivID[-trainSet], predictTestSeqS4$class)
accSeq[4,1]<-sum(diag(prop.table(acTableSeqS4)))

ldSeqS5 <- lda(indivID ~ syl1Dur+syl2Dur+syl3Dur+syl4Dur+syl5Dur+syl12Gap+syl23Gap+syl34Gap+syl45Gap, data=temporalFeatures[trainSet,])
predictTestSeqS5<-predict(ldSeqS5,temporalFeatures[-trainSet,])
acTableSeqS5 <- table(temporalFeatures$indivID[-trainSet], predictTestSeqS5$class)
accSeq[5,1]<-sum(diag(prop.table(acTableSeqS5)))

accSeq[6,1]<-sum(diag(prop.table(acTable2)))

plot(seq(1,6,1),accSeq$V1)

source("getVotes.R")
ldaAcc=getVotes(tempFeaturesComp$indivID[-trainSet], testResult2$class)
