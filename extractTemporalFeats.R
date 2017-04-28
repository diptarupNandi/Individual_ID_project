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
  
  syLLDur=matrix(nrow=length(onset_ind),ncol=7) # Initialization to store syllable durations
  syLLGap=matrix(nrow=length(onset_ind),ncol=6) # Initialization to store the interval between syllables
  chirpProps=matrix(nrow=length(onset_ind),ncol=3) # Intialization to store Chirp features
  
  
  for (ii in 1:length(chirpOnset)) {
    syllpChirp=offset_ind[ii]-onset_ind[ii]+1 # Syllable per chirp
    chirpProps[ii,1]=syllpChirp
    chirpProps[ii,2]=chirpOffset[ii]-chirpOnset[ii] # Chirp duartion
    
    # Chirp properties: runs till the n-1th syllable
    if (offset_ind[ii] < nrow(daT)) { 
      tempCP=chirpOnset[ii+1]-chirpOnset[ii] # Chirp period
      # Chirp gaps only for subsequent chirps within a bout of calling
      if (tempCP<chirpgap){
        chirpProps[ii,3]=tempCP-chirpProps[ii,2] # chirp gap
      }
    }
    
    # For each of the syllables in the chirp   
    for (jj in 1:syllpChirp) {
      syLLDur[ii,jj]=syllOffset[onset_ind[ii]-1+jj]-syllOnset[onset_ind[ii]-1+jj] # Syll Duration
      
      # Except the last syllable of the recording
      if (onset_ind[ii]+jj <= nrow(daT)) {
        tempSP=syllOnset[onset_ind[ii]+jj]-syllOnset[onset_ind[ii]-1+jj] # Syll period
        # Only for successive syllables within a chirp
        if (tempSP<syllgap) {
          syLLGap[ii,jj]=tempSP-syLLDur[ii,jj] # Syll Gap
        }
        
      }
      
    }
  }
  # Combines all the syllable and chirp features into a single matrix
  temporalFeats =cbind(syLLDur,syLLGap,chirpProps) 
  return(temporalFeats)
}