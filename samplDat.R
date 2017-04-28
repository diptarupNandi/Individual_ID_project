## Function to randomly subsample per group with variable or equal replicates
## Mainly used in creating training datasets from the main dataframe
sampLDat<-function(grouPs){
  grouPs<-as.factor(grouPs) # the grouping variable or ID
  countG<-table(grouPs) # Number of replicates per group
  noG<-length(levels(grouPs)) # Totals number of groups
  
  sampProp<-0.70 # the proportion of data to be considered in the training set
  trainInd<-vector(mode = "numeric") # Initializing the sampled indices per group
  indStartR=1 # counter for reading in the input data
  indStartS=1 # counter for reading/storing the output indices
  for (i in 1:noG){
    noR<-unname(countG[i]) # extracting onlt the number 
    sampSize<-round(sampProp*noR) # Total number of replicates to be sampled for ith group 
    trainInd[indStartS:(indStartS+sampSize-1)]=sample(indStartR:(indStartR+noR-1),sampSize) 
    indStartR=indStartR+noR
    indStartS=indStartS+sampSize
  }
  
  trainIndF<-sort(trainInd) # Arranging the randomly selected indices in ascending order per group
  return(trainIndF)
  
}