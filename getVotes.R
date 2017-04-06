## Collects votes of by pooling the individual points of classification trees (LDA,bagging,randomForest)  

getVotes<-function(oRiginal,predcTn){
  
  
  # oRiginal=testData$indivID ## Only for debugging
  # predcTn=testResult.RFT1 ## Only for debugging
  
  # oRiginal=tempFeaturesComp$indivID[-trainSet]
  # predcTn=testResult2$class

  oRiginal<-as.factor(oRiginal) # characters to factor
  ChpInd<-table(oRiginal) # Tabulates number of chirps per individual
  
  fewChInd=which(ChpInd<4) # Individuals with less than 4 chirps in testData
  remInd=which(oRiginal %in% names(fewChInd)) # Indices of those chirps in testData
  originaLF=droplevels(oRiginal[-remInd]) # removing those chirps from the testData
  predcTn=droplevels(predcTn[-remInd]) # removing those chirps from the predicted result
  accFullTable=table(originaLF,predcTn) # Accuracy table based on chirps after removing individuals
  
  levoriginaLF=levels(originaLF) # The final set of individuals (no. of levels)
  noChpInd=table(originaLF) # Number of chirps per individual after removing some indiv
  noInd<-length(levoriginaLF) # Total number of individuals after removing some indiv
  
  
  voteRes=0.25 # minimum proportion of votes to be considered
  voteN=matrix(nrow=noInd,ncol=(1/voteRes)-1) # Actual number of votes for indiv across all props
  acCpInd=matrix(0,nrow=noInd,ncol=(1/voteRes)-1) # Stores the number of accurate votes

  countCh=rep(1,(1/voteRes)-1) # Counter for keeping track of the index number in originaLF
  
  # each individual
  for (ii in 1:noInd){
    # each prop. of chirps considered for voting 
    for (jj in 1:((1/voteRes)-1)){
      voteN[ii,jj]=round(((2)^(jj-1))*voteRes*unname(noChpInd[ii])) # number of chirps to be pooled
      
      tempAcc=0 # Accuracy over pooled chirps 
      countIrr=1 # keeps track of number of chirps covered during the pooling events
      
      # only for less than 100% of the chirps/individual
      if (jj!=(1/voteRes)-1){
        # every pooling event (dependent on jj)
        for (kk in 1:(1/(((2)^(jj-1))*voteRes))){
          
          # to consider the remianders in the last pooling event when voteN != integer factor of noChpInd
          if (kk==1/(((2)^(jj-1))*voteRes) && noChpInd[ii]-countIrr>=1/(((2)^(jj-1))*voteRes)){
            voteN[ii,jj]=noChpInd[ii]-countIrr+1 # all the reamining chirps for that individual
          }
          
          tempTable=table(predcTn[countCh[jj]:(countCh[jj]+voteN[ii,jj]-1)]) # counts of individuals for the pooled chirps (votes)
          tempInd=names(which(tempTable==max(tempTable))) # name/s of the individual assigned to most chirps in predcTn
          if (length(tempInd)==1 && tempInd==levoriginaLF[ii]){
            tempAcc=1
          } else {
            tempAcc=0
          }
          countIrr=countIrr+voteN[ii,jj]
          countCh[jj]=countCh[jj]+voteN[ii,jj]
          acCpInd[ii,jj]=acCpInd[ii,jj]+tempAcc
          
        }
        
      } else {
        maxInd=which(accFullTable[ii,]==max(accFullTable[ii,]))
        if (length(maxInd)==1 && names(maxInd)==levoriginaLF[ii]){
          acCpInd[ii,jj]=1
        } else {
          acCpInd[ii,jj]=0
        }
      } 
      }
      
  }
  accFinal=colSums(acCpInd)/c(4*noInd,2*noInd,1*noInd)
  return(accFinal)
}

