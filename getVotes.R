## Collects votes of by pooling the individual points of classification trees (LDA,bagging,randomForest)  

getVotes<-function(testIndiV,predcTn){
  
  
  testIndiV=testData$indivID ## Only for debugging
  predcTn=testResult.RFT1 ## Only for debugging
  
  testIndiV<-as.factor(testIndiV) # character to factor
  ChpInd<-table(testIndiV) # Number of chirps per individual
  
  fewChInd=which(ChpInd<4) # Individuals with less than 4 chirps in testData
  remInd=which(testIndiV %in% names(fewChInd)) # Indices of those chirps in testData
  indiV=droplevels(testIndiV[-remInd]) # removing those chirps from the testData
  predcTn=droplevels(predcTn[-remInd]) # removing those chirps from the predicted result
  accFullTable=table(predcTn,indiV) # Accuracy table based on the chirps
  
  levIndiv=levels(indiV) # The final set of individuals after dropping some chirps
  noChpInd=table(indiV) # Number of chirps per individual after removing some indiv
  noInd<-length(levIndiv) # Total number of individuals after removing some indiv
  
  
  voteRes=0.25 # minimum proportion of votes to be considered
  voteN=matrix(nrow=noInd,ncol=1/voteRes) # Actual number of votes for indiv across all props
  acCpInd=matrix(nrow=noInd,ncol=1/voteRes) # Stores the number of accurate votes
  
  countCh=rep(1,1/voteRes)
  for (ii in 1:noInd){
    for (jj in 1:(1/voteRes)){
      voteN[ii,jj]=round(jj*voteRes*unname(noChpInd[ii]))
      tempAcc=0
      if (jj!=1/voteRes){
        for (kk in seq(1,noChpInd[ii]-voteN[ii,jj],voteN[ii,jj])){
          
          if (kk==noChpInd[ii]-voteN[ii,jj]){
            voteN[ii,jj]=noChpInd[ii]-kk # to account for the rounding effect due to irregular number of chirps
          }
          # tempAcc=tempAcc+length(which(predcTn[countCh[jj]:(countCh[jj]+voteN[ii,jj]-1)] %in% levIndiv[ii]))
          tempTable=table(predcTn[countCh[jj]:(countCh[jj]+voteN[ii,jj]-1)])
          tempInd=names(which(tempTable==max(tempTable)))
          if (tempInd==levIndiv[ii]){
            tempAcc=1
          }
          countCh[jj]=countCh[jj]+voteN[ii,jj]
        }
        acCpInd[ii,jj]=tempAcc
      } else {
        maxInd=which(accFullTable[ii,]==max(accFullTable[ii,]))
        if (unname(maxInd)==ii){
          acCpInd[ii,jj]=1
        }
        else {
          acCpInd[ii,jj]=0
        }
      } 
      }
      
  }
  
  
}

