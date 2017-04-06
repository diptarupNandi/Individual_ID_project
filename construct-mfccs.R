setwd("~/Documents/Indiv_ID_project/scriptsR") ## For desktop
# setwd("/Users/iandurbach/Documents/Research/160523_Crickets/")

library(seewave)
library(tuneR)
library(gtools)
library(dplyr)
library(plyr)
library(tools)

options(dplyr.print_max = 1e9)

# read in filenames of audio files
nam = mixedsort(list.files(path=paste("~/Documents/Indiv_ID_project/Data/Song_Data/", sep="")))
# nam = mixedsort(list.files(path=paste("/Users/iandurbach/Documents/Research/160523_Crickets/data/", sep="")))

# read in filenames of text files with onset and offset times
txtnam = mixedsort(list.files(path=paste("~/Documents/Indiv_ID_project/Data/chirps_On_Off_set_txt_files/", sep="")))
# txtnam = mixedsort(list.files(path=paste("/Users/iandurbach/Documents/Research/160523_Crickets/metadata/chirps_onoffset/", sep="")))

# extract files in nam that are also in txtnam
nam2 = tolower(file_path_sans_ext(nam))
txtnam2 = tolower(file_path_sans_ext(txtnam))
nam2 = subset(nam2,nam2 %in% txtnam2)

# important! specifies which audio files will be included in the analysis
nam = paste(toupper(substr(nam2,1,3)),substr(nam2,4,10),".WAV",sep="")
ncric = length(nam)

# initialize empty vectors, dfs, lists
all_mfcc = c()
mymfccs = list()
perchirp_ffres = c()

# for each audio file
for(iter in 1:ncric){
  
  ## progress bar of sorts
  print(paste(nam[iter],"start"))
  
  ## read in audio file
  # sound = readWave(paste("data/", nam[iter], sep=""),from=0,units="seconds")
  sound = readWave(paste("~/Documents/Indiv_ID_project/Data/Song_Data/", nam[iter], sep=""),from=0,units="seconds")
  
  
  ## read in text file with on and offsets
  clickfile = tolower(file_path_sans_ext(nam[iter]))
  # clicktimes = read.table(paste("metadata/chirps_onoffset/", clickfile,".txt",sep=""))
  clicktimes = read.table(paste("~/Documents/Indiv_ID_project/Data/chirps_On_Off_set_txt_files/", clickfile,".txt",sep=""))
  ## section to calculate fundamental frequencies for each click
  
  # onset time
  clickstart = clicktimes[clicktimes$V2==1,1]
  # offset time
  clickend = clicktimes[clicktimes$V4==1,3]
  # extracts a matrix with nclicks rows and 2 columns (start and end)
  wc = cbind(clickstart,clickend) 
  # extract cricket id
  cricket = tolower(substring(nam[iter],1,3))
  # extract record id
  record = ifelse(nchar(nam[iter])==9, tolower(substring(nam[iter],5,5)), tolower(substring(nam[iter],5,6)))
  # for each click
  for(i in 1:nrow(wc)){
    # extract chirp lenght
    chirpL = wc[i,2] - wc[i,1]
    # extract chirp audio
    mychirp = sound[wc[i,1]:wc[i,2]]
    # results vector contains some ids, plus fundamental frequency
    # note ff is calculated by dividing chirp into 10 overlapping segments, calc ff in each, and averaging
    res = c(cricket,record,i,wc[i,1],wc[i,2],chirpL,mean(fund(mychirp,wl=chirpL/10,ovlp=50,plot=F)[,2]))
    # store results (one row per chirp)
    perchirp_ffres = rbind(perchirp_ffres,res)
  }

  ## section generating mfccs and other results for each syllable
  
  # onset of syllable
  syllstart = clicktimes[,1]
  # offset of syllable
  syllend = clicktimes[,3]
  # put in matrix
  w = cbind(syllstart,syllend)
  
  # put audio of each syllable into a list
  mysound = list()
  for(i in 1:nrow(w)){mysound[[i]] = sound[w[i,1]:w[i,2]]}
  
  # for each syllable in the list
  mfccs = c()
  for(i in 1:length(mysound)){
    # syllable duration
    syllL = w[i,2] - w[i,1]
    # fundamental frequency in each syllable (calculated as above for chirps)
    ffs = try(mean(fund(mysound[[i]],wl=syllL/10,ovlp=50,plot=F)[,2]),TRUE)
    ffs = ifelse(class(ffs)=="try-error",NA,ffs)
    # MFCCs in each syllable: win/hoptime and nbands selected so as don't get many NaNs
    # rbind combines with other results (ff etc)
    if(syllL>300){
      thismfccs <- apply(melfcc(mysound[[i]],
           wintime = 100/44100,
           hoptime = 100/44100,
           #wintime=syllL/(3*44100),
           #hoptime=syllL/(3*44100),
           sr=44100,
           nbands=20),2,mean)}else{thismfccs <- rep(NA,12)} 
    
    mfccs = rbind(mfccs,c(thismfccs,syllL,ffs))
    
  }
  # turn into dataframe so can use dplyr later
  mfccs = data.frame(mfccs)
  # rename some columns of datafrae
  names(mfccs)[13] = "sylldur"
  names(mfccs)[14] = "syllFF"
  
  # add id variables to results just generated
  mfccs$cricket = tolower(substring(nam[iter],1,3))
  mfccs$record = ifelse(nchar(nam[iter])==9, tolower(substring(nam[iter],5,5)), tolower(substring(nam[iter],5,6)))
  mfccs$chirpid = cumsum(clicktimes$V2)
  
  # add syllable id
  mfccs = mfccs %>% group_by(chirpid) %>% dplyr::mutate(syllid = 1:n())
  
  # put all results into a list (each list item is a recording)
  mymfccs[[iter]] = mfccs
  
}

# convert to dataframe for use with dplyr
perchirp_ffres = data.frame(perchirp_ffres)
names(perchirp_ffres) = c("cricket","record","chirpid","chonset","choffset","chirpdur","chff")

# unstack list into one dataframe to get final per-syllable results 
syllres <- do.call(rbind.data.frame, mymfccs)

# aggregate results over syllables to get per-chirp results (note NOT weighted by syllable duration!)
chirpres <- syllres %>% group_by(cricket,record,chirpid) %>% dplyr::summarize(V1 = mean(V1,na.rm=T),
                                                                              V2 = mean(V2,na.rm=T),
                                                                              V3 = mean(V3,na.rm=T),
                                                                              V4 = mean(V4,na.rm=T),
                                                                              V5 = mean(V5,na.rm=T),
                                                                              V6 = mean(V6,na.rm=T),
                                                                              V7 = mean(V7,na.rm=T),
                                                                              V8 = mean(V8,na.rm=T),
                                                                              V9 = mean(V9,na.rm=T),
                                                                              V10 = mean(V10,na.rm=T),
                                                                              V11 = mean(V11,na.rm=T),
                                                                              V12 = mean(V12,na.rm=T),
                                                                              sumsylldur = sum(sylldur),
                                                                              nsyll = n(),
                                                                              meansyllFF = sum(syllFF * sylldur)/sum(sylldur))

# for some reason have to convert these into a "normal" dataframe (for next join)
chirpres = data.frame(chirpres)

# merge with earlier per-chirp results to get final per-chirp results
chirpres <- plyr::join(chirpres, perchirp_ffres, by=c("cricket","record","chirpid"))

# write final results to csv
write.csv(syllres,"persyll-mfccs.csv")
write.csv(chirpres,"perchirp-mfccs.csv")
