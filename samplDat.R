sampLDat<-function(grouPs){
  grouPs<-as.factor(grouPs)
  countG<-table(grouPs)
  noG<-length(levels(grouPs))
  
  trainInd<-vector(mode = "numeric")
  indStart=1
  for (i in 1:noG){
    noR<-unname(countG[i])
    sampSize<-round(0.75*noR)
    trainInd[indStart:indStart+sampSize-1]=sample(indStart:indStart+noR-1,sampSize)
    indStart=indStart+noR
    
  }
  
  return(trainInd)
  
}