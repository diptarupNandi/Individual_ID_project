extractTF<-function(daT){
  syllgap=.07
  chirpgap=1
  f=44100 # sampling frequency in Hz
  syllOnset=daT[,1]/f
  syllOffset=daT[,3]/f
  onset_ind=which(as.logical(daT[,2]))
  offset_ind=which(as.logical(daT[,4]))
  chirponset=daT[onset_ind,1]/f
  chirpoffset=daT[offset_ind,3]/f
  
  syLLDur=matrix(nrow=length(onset_ind),ncol=7)
  syLLPer=matrix(nrow=length(onset_ind),ncol=7)
  
  for (ii in 1:length(chirponset)) {
    syllpChirp=offset_ind[ii]-onset_ind[ii]+1
    for (jj in 1:syllpChirp) {
      syLLDur[ii,jj]=syllOffset[onset_ind[ii]-1+jj]-syllOnset[onset_ind[ii]-1+jj]
      if (onset_ind[ii]+jj <= nrow(daT)) {
        tempSP=syllOnset[onset_ind[ii]+jj]-syllOnset[onset_ind[ii]-1+jj]
        if (tempSP<syllgap) {
          syLLPer[ii,jj]=tempSP
        }
        
      }
      
    }
  }
  syLLFeats =cbind(syLLDur,syLLPer)
  return(syLLFeats)
}