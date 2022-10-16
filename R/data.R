

getAllData <- function() {
  
  participants <- getParticipants()
  
  A1 <- getAnaglyphData(participants)
  A1ctrl <- getDepthControlData(participants)
  A2 <- getProbeDistanceData(participants)
  
  B1 <- getApparentLagData(participants)
  B2 <- getPreDictionData(participants)
  
  T1 <- getExperimentTimeData(participants)
  
  C1 <- getSelfMotionData(participants)
  C2 <- getTextureMotionData(participants)
  C2ctrl <- getPerceivedMotionData(participants)
  
  allData <- list('Anagyph'                = A1,
                  'DepthControl'           = A1ctrl,
                  'ProbeDistance'          = A2,
                  'ApparentLag'            = B1,
                  'PreDiction'             = B2,
                  'ExperimentTime'         = T1,
                  'SelfMoved'              = C1,
                  'TextureMotion'          = C2,
                  'PerceivedTextureMotion' = C2ctrl)
  
  return(allData)
  
}

getParticipants <- function() {
  
  return(c(1:3))
  
}

