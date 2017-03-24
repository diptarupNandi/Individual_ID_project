## Collects votes of by pooling the individual points of classification trees (LDA,bagging,randomForest)  

getVotes<-function(indiV,predcTn){
  indiV<-as.factor(indiV) # character to factor
  noChpInd<-table(indiV) # Number of chirps per individual
  noInd<-length(levels(indiV)) # Total number of individuals
  
  voteRes=0.25
  voteN=matrix(nrow=noInd,ncol=1/voteRes)
  acCpInd=matrix(nrow=noInd,ncol=1/voteRes)
  
  for (ii in 1:noInd){
    for (jj in 1:length(voteRes)){
      voteN[ii,jj]=jj*voteRes*noChpInd[ii]
      for (kk in seq(1,noChpInd[ii],voteN)){
        acCpInd[ii,jj]=
          
          
          
          }
      
    }
  }
  
  
}

