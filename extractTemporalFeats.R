extractTF<-function(daT){
  syllgap=.07 # sets the maximum interval between syllables (species spcific)
  chirpgap=1  # sets the maximum interval between chirps (species spcific)
  f=44100 # sampling frequency in Hz
  syllOnset=daT[,1]/f # syllable onsets in seconds
  syllOffset=daT[,3]/f # syllable offsets in seconds
  onset_ind=which(as.logical(daT[,2])) # indices of onsets
  offset_ind=which(as.logical(daT[,4])) # indices of offsets
  chirpOnset=daT[onset_ind,1]/f # chirp onsets in seconds
  chirpOffset=daT[offset_ind,3]/f # chirp offsets in seconds
  
  syLLDur=matrix(nrow=length(onset_ind),ncol=7) # to store all syllable durations
  syLLGap=matrix(nrow=length(onset_ind),ncol=6) # to store all the interval between syllables
  chirpProps=matrix(nrow=length(onset_ind),ncol=3)
  for (ii in 1:length(chirpOnset)) {
    syllpChirp=offset_ind[ii]-onset_ind[ii]+1
    chirpProps[ii,1]=syllpChirp
    chirpProps[ii,2]=chirpOffset[ii]-chirpOnset[ii]
    if (offset_ind[ii] < nrow(daT)) {
      tempCP=chirpOnset[ii+1]-chirpOnset[ii]
      if (tempCP<chirpgap){
        chirpProps[ii,3]=tempCP-chirpProps[ii,2] 
      }
    }
      
    for (jj in 1:syllpChirp) {
      syLLDur[ii,jj]=syllOffset[onset_ind[ii]-1+jj]-syllOnset[onset_ind[ii]-1+jj]
      if (onset_ind[ii]+jj <= nrow(daT)) {
        tempSP=syllOnset[onset_ind[ii]+jj]-syllOnset[onset_ind[ii]-1+jj]
        if (tempSP<syllgap) {
          syLLGap[ii,jj]=tempSP-syLLDur[ii,jj]
        }
        
      }
      
    }
  }
  temporalFeats =cbind(syLLDur,syLLGap,chirpProps)
  return(temporalFeats)
}