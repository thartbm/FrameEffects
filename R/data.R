
library('RJSONIO')

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
  
  return(c(1:14))
  
}

# space data -----

getAnaglyphData <- function(participants, timedata=FALSE, FUN=median) {
  
  AnaDF <- NA
  
  for (ppno in participants) {
    
    if (ppno %in% c(6,14)) {
      next()
    }
    
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
    
    if (ppno %in% c(14)) {
      next()
    }
    
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
  
  
  hor_offset_map <- c('[0.0, 0.0]'=0,
                      '[0.0, 0]'=0,
                      '[0.0, 12.0]'=0,
                      '[0.0, 3.0]'=0,
                      '[0.0, 6.0]'=0,
                      '[0.0, 9.0]'=0,
                      '[12.0, 0]'=12,
                      '[3.0, 0]'=3,
                      '[6.0, 0]'=6,
                      '[9.0, 0]'=9)
  ver_offset_map <- c('[0.0, 0.0]'=0,
                      '[0.0, 0]'=0,
                      '[0.0, 12.0]'=12,
                      '[0.0, 3.0]'=3,
                      '[0.0, 6.0]'=6,
                      '[0.0, 9.0]'=9,
                      '[12.0, 0]'=0,
                      '[3.0, 0]'=0,
                      '[6.0, 0]'=0,
                      '[9.0, 0]'=0)
  
  
  for (ppno in participants) {
    
    filename <- sprintf('A2_ProbeDistance/data/exp_1/p%03d/responses.csv',ppno)
    df <- read.csv(filename, stringsAsFactors = F)
    
    df$percept <- df$percept_abs * 2 * df$xfactor
    
    if (timedata) {
      
      df <- df[which(df$frameoffset == '[0.0, 0]' & df$framesize == '[7, 6]'),]
      df <- df[,c('percept', 'period', 'amplitude', 'trial_start')]

    } else {
      df$inner_framesize <- c('[4, 3]'=3, '[7, 6]'=6, '[10, 9]'=9)[df$framesize]
      df$hor_offset      <- hor_offset_map[df$frameoffset]
      df$ver_offset      <- ver_offset_map[df$frameoffset]

      df <- df[which(df$amplitude == 4),]
      df <- aggregate(percept ~ period + amplitude + inner_framesize + hor_offset + ver_offset, data=df, FUN=FUN, na.rm=FALSE)
      
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
      # df <- df[which(abs(df$flashoffset) < 3),]
      df <- df[which(df$framepasses < 4),]
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
  
  timeData <- list(  'A2_ProbeDistance'          = getProbeDistanceData(participants, timedata=TRUE, FUN=FUN),
                     'B1_ApparentLag'            = getApparentLagData(participants, timedata=TRUE, FUN=FUN),
                     'B2_PreDiction'             = getPreDictionData(participants, timedata=TRUE, FUN=FUN),
                     # 'C1_SelfMoved'              = getSelfMotionData(participants, timedata=TRUE, FUN=FUN),
                     'C2_TextureMotion'          = getTextureMotionData(participants, timedata=TRUE, FUN=FUN)     )
  
  
  intervalData <- NA
  
  intervalduration <- 30 * 60
  intervalnumber <- 4
  
  
  participant <- c()
  interval    <- c()
  from_s      <- c()
  to_s        <- c()
  amplitude   <- c()
  percept     <- c()
  
  
  for (ppno in participants) {
    
    ppdf <- NA
    
    for (exp in timeData) {
      
      ppexpdf <- exp[which(exp$participant == ppno),]
      if (is.data.frame(ppdf)) {
        ppdf <- rbind(ppdf, ppexpdf)
      } else {
        ppdf <- ppexpdf
      }
      
    }
    
    starttime <- getParticipantStartTime(ppno=ppno)
    ppdf$trial_start <- ppdf$trial_start - starttime
    
    #print(range(ppdf$trial_start))
    
    for (inno in c(1:intervalnumber)) {
      
      intervalstart <- (inno-1) * intervalduration
      intervalend   <- inno * intervalduration
      
      #print(c('start'=intervalstart, 'end'=intervalend))
      
      inppdf <- ppdf[which(ppdf$trial_start > intervalstart & ppdf$trial_start <= intervalend),]
      
      if (dim(inppdf)[1]>0) {
        intervaldf <- aggregate(percept ~ amplitude, data=inppdf, FUN=FUN)
        for (rowno in c(1:dim(intervaldf)[1])) {
          
          participant <- c(participant, ppno)
          interval    <- c(interval,    inno)
          from_s      <- c(from_s,      intervalstart)
          to_s        <- c(to_s,        intervalend)
          amplitude   <- c(amplitude,   intervaldf$amplitude[rowno])
          percept     <- c(percept,     intervaldf$percept[rowno])
          
        }
        
      }
      
    }
    
  }
  
  df <- data.frame(participant, interval, from_s, to_s, amplitude, percept)
  
  return(df)
  
}

getParticipantStartTime <- function(ppno) {
  
  
  tasks <-  c( 'A1_Anaglyph',
               'A2_ProbeDistance',
               'B1_ApparentLag',
               'B2_PreDiction',
               'C1_SelfMotion',
               'C2_TextureMotion',
               'C3_PerceivedMotion'  )
  
  
  starttime <- NA
  
  for (task in tasks) {
    filename <- sprintf('%s/data/exp_1/p%03d/cfg.json', task, ppno)
    
    if (task == 'A1_Anaglyph' & ppno == 14) {
      next
    }
    
    cfg <- RJSONIO::fromJSON(content=filename)
    expstart <- cfg$expstart
    
    if (is.na(starttime)) {
      starttime <- expstart
    } else {
      starttime <- min(starttime, expstart)
    }
  }
  
  return(starttime)
  
}

# motion data -----

getSelfMotionData <- function(participants, timedata=FALSE, FUN=median) {
  
  SMdf <- NA
  
  # stimData <- read.csv('C1_SelfMotion/data/stimuli.csv', stringsAsFactors = F)
  
  for (ppno in participants) {
    
    filename <- sprintf('C1_SelfMotion/data/exp_1/p%03d/responses.csv',ppno)
    df <- read.csv(filename, stringsAsFactors = F)
    
    # df <- stimData[which(stimData$participant == ppno),]
    # df <- df[,c('period','amplitude','stimtype','mapping','xfactor')]
    
    df$percept <- abs( getParticipantSelfMotionPercepts(ppno=ppno) * 2 ) # * df$xfactor #* df$mapping
    
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
  
  TMdf <- NA
  
  for (ppno in participants) {
    
    filename <- sprintf('C2_TextureMotion/data/exp_1/p%03d/responses.csv',ppno)
    df <- read.csv(filename, stringsAsFactors = F)
    
    df$percept <- df$percept_abs * 2 * df$xfactor
    
    if (timedata) {
      
      df <- df[which(df$stimtype == 'classicframe' & round(df$period,digits=6) == 0.333333),]
      df <- df[,c('percept', 'period', 'amplitude', 'trial_start')]
      
    } else {
      
      df <- df[which(df$amplitude == 4),]
      df <- aggregate(percept ~ period + amplitude + stimtype + fixdot, data=df, FUN=FUN, na.rm=FALSE)
      df$fixdot <- c('False'=FALSE, 'True'=TRUE)[df$fixdot]
      
    }
    
    df$participant <- ppno
    
    if (is.data.frame(TMdf)) {
      TMdf <- rbind(TMdf, df)
    } else {
      TMdf <- df
    }
    
  }
  
  return(TMdf)
  
}

getPerceivedMotionData <- function(participants, timedata=FALSE, FUN=median) {
  
  # participant 2 needs to have the first block removed (they responded left extreme to right extreme there)
  PMdf <- NA
  
  for (ppno in participants) {
    
    filename <- sprintf('C3_PerceivedMotion/data/exp_1/p%03d/responses.csv',ppno)
    df <- read.csv(filename, stringsAsFactors = F)
    
    if (ppno == 2) {
      # remove the first block!
      df <- df[c(53:156),]
    }
    
    df$percept <- abs(df$percept) * 2
    
    if (timedata) {
      
      cat('WARNING: perceived texture motion data not suitable for time analysis\n')
      return(NULL)

    } else {
      
      df <- aggregate(percept ~ period + amplitude + stimtype, data=df, FUN=FUN, na.rm=FALSE)
      
    }
    
    df$participant <- ppno
    
    if (is.data.frame(PMdf)) {
      PMdf <- rbind(PMdf, df)
    } else {
      PMdf <- df
    }
    
  }
  
  return(PMdf)
  
}

