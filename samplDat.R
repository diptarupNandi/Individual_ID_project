sampLDat<-function(grouPs){
  grouPs<-as.factor(grouPs)
  countG<-table(grouPs)
  noG<-length(levels(grouPs))
  
  trainInd<-vector(mode = "numeric")
  indStartR=1
  indStartS=1
  for (i in 1:noG){
    noR<-unname(countG[i])
    sampSize<-round(0.75*noR)
    trainInd[indStartS:(indStartS+sampSize-1)]=sample(indStartR:(indStartR+noR-1),sampSize)
    indStartR=indStartR+noR
    indStartS=indStartS+sampSize
  }
  
  trainIndF<-sort(trainInd)
  return(trainIndF)
  
}