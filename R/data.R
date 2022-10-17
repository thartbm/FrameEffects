

getAllData <- function(FUN=median) {
  
  participants <- getParticipants()
  
  allData <- list(  'A1_Anaglyph'               = getAnaglyphData(participants, FUN=FUN),
                    'A1_DepthControl'           = getDepthControlData(participants, FUN=FUN),
                    'A2_ProbeDistance'          = getProbeDistanceData(participants, FUN=FUN),
                    'B1_ApparentLag'            = getApparentLagData(participants, FUN=FUN),
                    'B2_PreDiction'             = getPreDictionData(participants, FUN=FUN),
                    'T1_ExperimentTime'         = getExperimentTimeData(participants, FUN=FUN),
                    'C1_SelfMoved'              = getSelfMotionData(participants, FUN=FUN),
                    'C2_TextureMotion'          = getTextureMotionData(participants, FUN=FUN),
                    'C2_PerceivedTextureMotion' = getPerceivedMotionData(participants, FUN=FUN)    )
  
  return(allData)
  
}

getParticipants <- function() {
  
  return(c(1:3))
  
}

# space data -----

getAnaglyphData <- function(participants, timedata=FALSE, FUN=median) {
  
  AnaDF <- NA
  
  for (ppno in participants) {
    
    filename <- sprintf('A1_Anaglyph/data/exp_1/p%03d/responses.csv',ppno)
    df <- read.csv(filename, stringsAsFactors = F)
    
    if (timedata) {
    
      df <- df[which(df$condition == 'same plane'),] 
      df <- df[,c('percept', 'period', 'amplitude', 'trial_start')]
      
    } else {
      
      df$percept <- df$percept_abs * 2 * df$xfactor
      df <- aggregate(percept ~ period + amplitude + framesize + condition, data=df, FUN=FUN, na.rm=TRUE)

    }
    
    df$participant <- ppno
    
    if (is.data.frame(AnaDF)) {
      AnaDF <- rbind(AnaDF, df)
    } else {
      AnaDF <- df
    }
      
  }
  
  return(AnaDF)

}

getDepthControlData <- function(participants, timedata=FALSE, FUN=median) {

  
  DCdf <- NA
  
  for (ppno in participants) {
    
    filename <- sprintf('A1_Anaglyph/data/exp_1/p%03d/depth_perception_check.csv',ppno)
    df <- read.csv(filename, stringsAsFactors = F)
    
    if (timedata) {
      
      cat('WARNING: no standard frame depth perception control task!\n')
      
      return(NULL)
      
    } 
      
    df$correct <- sign(df$top - df$bottom) == df$response
    df <- aggregate(correct ~ top + bottom, data=df, FUN=FUN, na.rm=TRUE)
    df$participant <- ppno
    
    if (is.data.frame(DCdf)) {
      DCdf <- rbind(DCdf, df)
    } else {
      DCdf <- df
    }
    
  }
  
  return(DCdf)
  
}

getProbeDistanceData <- function(participants, timedata=FALSE, FUN=median) {
  
  PDdf <- NA
  
  for (ppno in participants) {
    
    filename <- sprintf('A2_ProbeDistance/data/exp_1/p%03d/responses.csv',ppno)
    df <- read.csv(filename, stringsAsFactors = F)
    
    df$percept <- df$percept_abs * 2 * df$xfactor
    
    if (timedata) {
      
      df <- df[which(df$frameoffset == '[0.0, 0]' & df$framesize == '[7, 6]'),]
      df <- df[,c('percept', 'period', 'amplitude', 'trial_start')]

    } else {
      
      df <- df[which(df$amplitude == 4),]
      df <- aggregate(percept ~ period + amplitude + framesize + frameoffset, data=df, FUN=FUN, na.rm=FALSE)
      
    }
    
    df$participant <- ppno
    
    if (is.data.frame(PDdf)) {
      PDdf <- rbind(PDdf, df)
    } else {
      PDdf <- df
    }
    
  }
  
  return(PDdf)
  
}

# time data -----

getApparentLagData <- function(participants, timedata=FALSE, FUN=median) {
  
  ALdf <- NA
  
  for (ppno in participants) {
    
    filename <- sprintf('B1_ApparentLag/data/exp_1/p%03d/responses.csv',ppno)
    df <- read.csv(filename, stringsAsFactors = F)
    
    df$percept <- df$percept_abs * 2 * df$xfactor
    
    if (timedata) {
      
      df <- df[which(df$framelag == 0 & df$stimtype == 'classicframe'),]
      df <- df[,c('percept', 'period', 'amplitude', 'trial_start')]
      
    } else {
      
      df <- df[which(df$amplitude == 4),]
      df <- aggregate(percept ~ period + amplitude + stimtype + framelag, data=df, FUN=FUN, na.rm=FALSE)
      
    }
    
    df$participant <- ppno
    
    if (is.data.frame(ALdf)) {
      ALdf <- rbind(ALdf, df)
    } else {
      ALdf <- df
    }
    
  }
  
  return(ALdf)
  
}

getPreDictionData <- function(participants, timedata=FALSE, FUN=median) {
  
  PDdf <- NA
  
  for (ppno in participants) {
    
    filename <- sprintf('B2_PreDiction/data/exp_1/p%03d/responses.csv',ppno)
    df <- read.csv(filename, stringsAsFactors = F)
    
    df$percept <- df$percept_abs * 2 * df$xfactor
    
    if (timedata) {
      
      df <- df[which(df$flashoffset == 0 & df$framepasses == 1),]
      df <- df[,c('percept', 'period', 'amplitude', 'trial_start')]
      
    } else {
      
      df <- df[which(df$amplitude == 4),]
      df <- aggregate(percept ~ period + amplitude + flashoffset + framepasses, data=df, FUN=FUN, na.rm=FALSE)
      
    }
    
    df$participant <- ppno
    
    if (is.data.frame(PDdf)) {
      PDdf <- rbind(PDdf, df)
    } else {
      PDdf <- df
    }
    
  }
  
  return(PDdf)
  
}


getExperimentTimeData <- function(participants, FUN=median) {
  
  return(NULL)
  
}

# motion data -----

getSelfMotionData <- function(participants, timedata=FALSE, FUN=median) {
  
  SMdf <- NA
  
  stimData <- read.csv('C1_SelfMotion/data/stimuli.csv', stringsAsFactors = F)
  
  for (ppno in participants) {
    
    # filename <- sprintf('C1_SelfMotion/data/exp_1/p%03d/responses.csv',ppno)
    # df <- read.csv(filename, stringsAsFactors = F)
    
    df <- stimData[which(stimData$participant == ppno),]
    df <- df[,c('period','amplitude','stimtype','mapping','xfactor')]
    
    df$percept <- getParticipantSelfMotionPercepts(ppno=ppno) * 2 * df$xfactor * df$mapping
    
    if (timedata) {
      
      df <- df[which(df$stimtype == 'classicframe'),]
      df <- df[,c('percept', 'period', 'amplitude', 'trial_start')]
      #print(df)
    } else {
      
      df <- aggregate(percept ~ period + amplitude + stimtype + mapping, data=df, FUN=FUN, na.rm=FALSE)
      
    }
    
    df$participant <- ppno
    
    if (is.data.frame(SMdf)) {
      SMdf <- rbind(SMdf, df)
    } else {
      SMdf <- df
    }
    
  }
  
  return(SMdf)
  
}

getParticipantSelfMotionPercepts <- function(ppno) {
  
  percepts <- c()
  
  for (block in c(0:2)) {
    
    for (trial in c(0:14)) {
  
      filename <- sprintf('C1_SelfMotion/data/exp_1/p%03d/timing/b%d_t%d.csv',ppno,block,trial)
      df <- read.csv(filename, stringsAsFactors = F)
      
      percepts <- c(percepts, df$percept[dim(df)[1]])
      
    }
      
  }
  
  return(percepts)
  
}

getTextureMotionData <- function(participants, timedata=FALSE, FUN=median) {
  
  
}

getPerceivedMotionData <- function(participants, timedata=FALSE, FUN=median) {
  
  # participant 2 needs to have the first block removed (they responded left extreme to right extreme there)
  
  
}

