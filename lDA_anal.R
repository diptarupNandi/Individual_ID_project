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
trainSet<-sampLDat(temporalFeatures$indivID)

library(MASS)
ldFit1 <- lda(indivID ~ syl1Dur+syl2Dur+syl3Dur+syl4Dur+syl5Dur+syl12Gap+syl23Gap+syl34Gap+syl45Gap+chirpDur+chirpGap, data=temporalFeatures,na.action="na.omit", CV=TRUE)
ldFit2 <- lda(indivID ~ syl1Dur+syl2Dur+syl3Dur+syl4Dur+syl5Dur+syl12Gap+syl23Gap+syl34Gap+syl45Gap+chirpDur+chirpGap, data=temporalFeatures)


## Assess the accuracy of the prediction
acTable <- table(temporalFeatures$indivID[-ldFit1$na.action], ldFit1$class)
# percent correct for each individual
diag(prop.table(acTable, 1))
# total percent correct
sum(diag(prop.table(acTable)))

