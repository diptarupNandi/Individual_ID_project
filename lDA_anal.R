## DFA on the temporal features

rm(list=ls())
setwd("~/Documents/Kelsa/Individual_identification_project/scriptsR")

# loads the file from the same directory
syLLDat<-read.csv("syllFeatures1_forModel1.csv")

# removes all unwanted columns
syLLDat<-syLLDat[,-c(1,10,11,16,17)]


ldFit1 <- lda(indivID ~ syl1Dur+syl2Dur+syl3Dur+syl4Dur+syl5Dur+syl12Gap+syl23Gap+syl34Gap+syl45Gap, data=syLLDat,na.action="na.omit", CV=TRUE)
ldFit2 <- lda(indivID ~ syl1Dur+syl2Dur+syl3Dur+syl4Dur+syl5Dur+syl12Gap+syl23Gap+syl34Gap+syl45Gap, data=syLLDat)


## Assess the accuracy of the prediction
acTable <- table(syLLDat$indivID[-ldFit1$na.action], ldFit1$class)
# percent correct for each individual
diag(prop.table(acTable, 1))
# total percent correct
sum(diag(prop.table(acTable)))

