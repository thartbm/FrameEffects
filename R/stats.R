
doStatistics <- function() {
  
  allData <- getAllData()
  
  
  # ANAGLYPH task:
  
  cat('\n* * *  ANAGLYPH TASK  * * *\n\n')
  
  df <- allData[['A1_Anaglyph']]
  #[1] "A1_Anaglyph"               
  # "A1_DepthControl"           
  
  
  df$condition <- as.factor(df$condition)
  
  anova <- afex::aov_ez(id='participant',
                        dv='percept',
                        data=df,
                        within=c('condition'))
  
  print(anova)
  
  
  
  cat('\n* * *  PROBE DISTANCE  * * *\n\n')
  
  df <- allData[['A2_ProbeDistance']]
  # "A2_ProbeDistance"          
  
  hdf <- df[which(df$ver_offset == 0),]
  anova <- afex::aov_ez(id='participant',
                        dv='percept',
                        data=hdf,
                        within=c('hor_offset','inner_framesize'))
  
  print(anova)
  vdf <- df[which(df$hor_offset == 0),]
  anova <- afex::aov_ez(id='participant',
                        dv='percept',
                        data=vdf,
                        within=c('ver_offset','inner_framesize'))
  print(anova)
  
  
  participant <- c()
  offset      <- c()
  ifs         <- c()
  half_effect <- c()
  
  for (offset in c('hor','ver')) {
    
    zero_dir <- list('hor'='ver_offset',
                     'ver'='hor_offset')[[offset]]
    test_dir <- sprintf('%s_offset',offset)
    
    subdf <- df[which(df[,zero_dir] == 0),]
    subdf$test_dir <- subdf[,test_dir]
    
    # scale:
    # subdf$percept <- 1 - ( subdf$percept / subdf$percept[which(subdf[,test_dir] == 0)] )
    

    avg <- aggregate(percept ~ test_dir, data=subdf, FUN=mean, na.rm=TRUE)
    #scale:
    # avg$percept <- avg$percept - min(avg$percept)
    avg$percept <- avg$percept / max(avg$percept)
    avg$percept <- 1 - avg$percept
    
    plot(x=avg$test_dir,
         y=avg$percept, col='blue',
         main=list('hor'='horizontal offsets', 'ver'='vertical offsets')[[offset]],
         xlab='offset [dva]', ylab='proportion max percept',
         ylim=c(0,1))
         
    
    #lines(avg$test_dir, avg$percept, col='blue')
    
    fitpar <- fitLogisticFunction(x = avg$test_dir,
                                  y = avg$percept)
    
    print(fitpar)
    
    X <- seq(0,12,length.out=121)
    Y <- logisticFunction( par = fitpar,
                           x   = seq(0,12,length.out=121) )
    
    lines(X,
          Y,
          col='red')
    
    lines(x = c(0,   fitpar['x0'], fitpar['x0']),
          y = c(0.5, 0.5,          0),
          col='dark green', lty=2)
    
    # for (ppno in unique(df$participant)) {
    # 
    #   for (inner_framesize in c(3,6,9)) {
    #     
    #     subdf <- df[which(df$participant == ppno &
    #                       df[,zero_dir] == 0 &
    #                         df$inner_framesize == inner_framesize),]
    #     subdf$test_dir <- subdf[,test_dir]
    #     
    #     # print(subdf$test_dir)
    #     
    #     #subdf$percept <- 1 - ( subdf$percept / subdf$percept[which(subdf[,test_dir] == 0)] )
    #     # subdf$percept[which(subdf$percept < 1e-10)] <- 1e-10
    #     # subdf$percept[which(subdf$percept > 1-1e-10)] <- 1-1e-10
    #     #print(subdf$percept)
    #   
    #     #logistic_fit <- glm(percept ~ test_dir, data=subdf, family='binomial')
    #     
    #     #predict(logistic_fit, test_dir=c(0:12))
    #     print(fitLogisticFunction(x = subdf$test_dir,
    #                               y = subdf$percept)   )
    #     
    #   }
    # }
  }
  
  
  # combine vdf & hdf
  vdf$offset <- vdf$ver_offset
  vdf$direction <- 'vertical'
  hdf$offset <- hdf$hor_offset
  hdf$direction <- 'horizontal'
  
  bdf <- rbind(vdf,hdf)
  
  bdf <- bdf[,c('participant','offset','direction','percept')]
  # bdf$offset <- as.factor(bdf$offset)
  # bdf$direction <- as.factor(bdf$direction)
  
  # bdf <- aggregate(percept ~ direction + offset + participant, data=bdf, FUN=mean)
  
  anova <- afex::aov_ez(dv     = 'percept',
                        id     = 'participant',
                        data   = bdf,
                        within = c('offset','direction'))
  
  print(anova)
  
  df <- allData['B1_ApparentLag']
  
  # "B1_ApparentLag"
  
  
  
  df <- allData[['B2_PreDiction']]
  
  # [5] "B2_PreDiction"
  
  # df1 <- df[which(df$flashoffset %in% c(1,-1)),]
  a1dfPre  <- aggregate(percept ~ participant + flashoffset, data = df[which(df$flashoffset == 1),], FUN=mean)
  a1dfPost <- aggregate(percept ~ participant + flashoffset, data = df[which(df$flashoffset < 0 & df$flashoffset == -df$framepasses),], FUN=mean)
  
  a1df <- rbind(a1dfPre, a1dfPost)
  a1df$diction <- 'pre'
  a1df$diction[a1df$flashoffset < 0] <- 'post'
  
  post1 <- a1df$percept[which(a1df$diction == 'post')]
  pre1 <- a1df$percept[which(a1df$diction == 'pre')]
  t.test(post1, pre1, paired=TRUE) # this is significant, but could just be because of the 2 pass data being an outlier?
  
  for (fps in c(1,2,3)) {
    a1df <- aggregate(percept ~ participant + flashoffset, data = df1[which(df1$framepasses == fps),], FUN=mean)
    post1 <- a1df$percept[c(1:14)]
    pre1 <- a1df$percept[c(15:28)]
    print(post1 - pre1)
    cat(sprintf('frame passes: %d\n', fps))
    print(t.test(post1, pre1, paired=TRUE))
  }  
  
  
  
  df <- allData[['T1_ExperimentTime']]
  
  # "T1_ExperimentTime"
  
  
  df <- allData[['C1_SelfMoved']]
  
  # "C1_SelfMoved"
  
  
  
  df <- allData[['C2_TextureMotion']]
  
  #"C2_TextureMotion"         
  #[9] "C2_PerceivedTextureMotion"
  
}


fitLogisticFunction <- function(x,y) {
  
  # par <- c('x0' = mean(range(x)),
  #          'k'  = -10,
  #          'L'  = max(y)
  #          )
  
  # create a gird of possible starting positions:
  x0 <- seq(min(x),max(x),length.out=7)
  k  <- seq(-50,50,length.out=7)
  #L  <- seq(max(y)/2,max(y)*1.5,length.out=7)
  
  searchgrid <- expand.grid( x0 = x0,
                             k  = k
                             # ,
                             # L  = L
                             )
  
  # assess how good each of these already is:
  MSE <- apply(searchgrid, FUN=logisticFunctionError, MARGIN=c(1), x=x, y=y)
  
  #print(MSE)
  
  # run error minimization on the best starting positions:
  allfits <- do.call("rbind",
                     apply( data.frame(searchgrid[order(MSE)[1:10],]),
                            MARGIN=c(1),
                            FUN=optimx::optimx,
                            fn=logisticFunctionError,
                            method='Nelder-Mead',
                            x=x,
                            y=y  ) )
  
  # pick the best fit:
  win <- allfits[order(allfits$value)[1],]
  
  # return the best parameters:
  return(unlist(win[1:2]))
  
  
  # 
  # print(MSE)
  # 
  # fit <- optim(par=par,
  #              fn=logisticFunctionError,
  #              x=x,
  #              y=y)
  # 
  # #print(fit$par)
  # 
  # return(fit$par)
  
}

logisticFunction <- function(par,x) {
  
  x0 = par['x0']
  k  = par['k']
  #L  = par['L']
  L  = 1
  
  return( L / ( 1 + exp( -k * (x - x0) ) ) )
  
}

logisticFunctionError <- function(par,x,y) {
  
  # print(x)
  # print(y)
  
  errors <- logisticFunction(par,x) - y
  return( mean( errors^2 ) )
  
}

# horizontal / vertical distance -----

probeDistanceANOVA <- function() {
  
  participants <- getParticipants()
  df <- getProbeDistanceData(participants, FUN=median)
  
  df$participant <- as.factor(df$participant)
  
  df <- df[which(df$hor_offset > 0 | df$ver_offset > 0),]
  df$offset_dir <- NA
  df$offset_dir[which(df$ver_offset == 0)] <- 'h'
  df$offset_dir[which(df$hor_offset == 0)] <- 'v'
  df$offset_size <- pmax(df$hor_offset, df$ver_offset)
  
  
  my_aov <- afex::aov_ez(  id='participant',
                           dv='percept',
                           data=df,
                           within=c('inner_framesize','offset_size','offset_dir')
                           )
  print(my_aov)
  
}

# depth ----

depthANOVA <- function() {
  
  participants <- getParticipants()
  
  control <- getDepthControlData(participants, FUN=median)
  
  gooddepth <- aggregate(correct ~ participant, data=control, FUN=mean)
  participants <- gooddepth$participant[which(gooddepth$correct > 0.75)]
  
  df <- getAnaglyphData(participants, FUN=median)
  
  my_aov <- afex::aov_ez(  id='participant',
                           dv='percept',
                           data=df,
                           within=c('condition')
  )
  print(my_aov)
  
}

# pre- and post-diction -----

postdictionANOVA <- function() {
  
  participants <- getParticipants()
  
  df <- getPreDictionData(participants=participants, FUN=median)
  
  df$diction <- 'pre'
  df$diction[which(df$flashoffset < 0)] <- 'post'
  
  post0 <- df[which(df$framepasses == 1 & df$flashoffset == 0),]
  post0$diction <- 'post'
  
  df <- rbind(df,post0)
  
  nidx <- which(df$flashoffset < 0)
  df$flashoffset[nidx] <- df$flashoffset[nidx] + df$framepasses[nidx] - 1
  
  df$participant <- as.factor(df$participant)
  
  # my_aov <- afex::aov_ez(  id='participant',
  #                          dv='percept',
  #                          data=df,
  #                          within=c('framepasses', 'flashoffset')
  # )
  # print(my_aov)
  
  df$flashoffset <- abs(df$flashoffset)
  
  my_aov <- afex::aov_ez(  id='participant',
                           dv='percept',
                           data=df,
                           within=c('framepasses', 'flashoffset', 'diction')
  )
  print(my_aov)
  
  
}


postdictionTtests <- function() {
  
  participants <- getParticipants()
  
  df <- getPreDictionData(participants=participants, FUN=median)
  
  df$diction <- 'pre'
  df$diction[which(df$flashoffset < 0)] <- 'post'
  
  post0 <- df[which(df$framepasses == 1 & df$flashoffset == 0),]
  post0$diction <- 'post'
  
  df <- rbind(df,post0)
  
  nidx <- which(df$flashoffset < 0)
  df$flashoffset[nidx] <- df$flashoffset[nidx] + df$framepasses[nidx] - 1
  
  df$participant <- as.factor(df$participant)
  
  df$flashoffset <- abs(df$flashoffset)
  
  for (flashoffset in c(0,1,2)) {
    pre.idx <- which(df$flashoffset == flashoffset & df$diction == 'pre')
    post.idx <- which(df$flashoffset == flashoffset & df$diction == 'post')
    cat(sprintf('--------------\nflashoffset: %d\n',flashoffset))
    print( t.test( x=df$percept[pre.idx],
                   y=df$percept[post.idx],
                   paired=TRUE)
          )
  }
  
}

# lagged probes -----

probeLagANOVA <- function() {
  
  # participants <- getParticipants()
  participants <- c(1:8)
  
  df <- getApparentLagData(participants, FUN=median)
  
  df$participant <- as.factor(df$participant)
  
  my_aov <- afex::aov_ez(  id='participant',
                           dv='percept',
                           data=df,
                           within=c('stimtype', 'framelag')
  )
  print(my_aov)
  
}

# random dot texture motion perception ----

motionperceptionANOVA <- function() {
  
  participants <- getParticipants()
  
  df <- getPerceivedMotionData(participants, FUN=median)
  
  df <- df[which(round(df$period, digits=6) == 0.333333),]
  df <- df[which(df$stimtype %in% c('classicframe','dotbackground')),]
  
  df <- aggregate(percept ~ stimtype + amplitude + participant, data=df, FUN=median)
  
  df$participant <- as.factor(df$participant)
  
  my_aov <- afex::aov_ez(  id='participant',
                           dv='percept',
                           data=df,
                           within=c('stimtype', 'amplitude')
  )
  print(my_aov)
  
}

motionperceptionTtest <- function() {
  
  participants <- getParticipants()
  
  df <- getPerceivedMotionData(participants, FUN=median)
  
  df <- df[which(round(df$period, digits=6) == 0.333333),]
  df <- df[which(df$stimtype %in% c('classicframe','dotbackground')),]
  df <- df[which(df$amplitude == 4),]
  
  classic <-df$percept[which(df$stimtype == 'classicframe')]
  background <-df$percept[which(df$stimtype == 'dotbackground')]

  my_ttest <- t.test(classic, background, paired=TRUE)
  print(my_ttest)
  
  cat('proportion motion motion perceived in dot backgrounds compared to frames:\n')
  print(mean(background) / mean(classic))
  
}

# random dot texture frames -----

textureBackgroundTtests <- function() {
  
  participants <- getParticipants()
  
  df <- getTextureMotionData(participants, FUN=median)
  
  df <- df[which(round(df$period, digits=6) == 0.333333),]
  
  stimtypes <- c( 'classicframe',
                  'classicframe',
                  'dotbackground' )
  fixate   <- c( TRUE, FALSE, FALSE)
  
  cat('classic frame: fixation / free viewing\n')
  my_ttest <- t.test( x=df$percept[which(df$stimtype == 'classicframe' & df$fixdot==TRUE)],
                      y=df$percept[which(df$stimtype == 'classicframe' & df$fixdot==FALSE)],
                      paired=TRUE)
  print(my_ttest)
  
  mbtt <- BayesFactor::ttestBF( x =      df$percept[which(df$stimtype == 'classicframe' & df$fixdot==TRUE)],
                                y =      df$percept[which(df$stimtype == 'classicframe' & df$fixdot==FALSE)],
                                paired = TRUE )
  
  print(mbtt)
  
  cat('classic frame vs. dot background\n')
  my_ttest <- t.test( x=df$percept[which(df$stimtype == 'classicframe' & df$fixdot==FALSE)],
                      y=df$percept[which(df$stimtype == 'dotbackground' & df$fixdot==FALSE)],
                      paired=TRUE)
  print(my_ttest)
  
  cat('dot background vs. ZERO\n')
  my_ttest <- t.test( x=df$percept[which(df$stimtype == 'dotbackground' & df$fixdot==FALSE)])
  print(my_ttest)
  
  cat('average effect in classic frame:\n')
  print(mean(df$percept[which(df$stimtype == 'classicframe' & df$fixdot==FALSE)]))
  
  cat('proportion illusory effect in dots background compared to frames:\n')
  print(mean(df$percept[which(df$stimtype == 'dotbackground' & df$fixdot==FALSE)]) / mean((df$percept[which(df$stimtype == 'classicframe' & df$fixdot==FALSE)])))
}

textureBackgroundANOVA <- function() {
  
  participants <- getParticipants()
  
  df <- getTextureMotionData(participants, FUN=median)
  
  df <- df[which(df$stimtype %in% c('classicframe','dotbackground')),]
  
  df$condition <- 2
  df$condition[which(df$stimtype == 'dotbackground')] <- 3
  df$condition[which(df$fixdot == TRUE)] <- 1
  
  df$condition <- as.factor(df$condition)
  df$participant <- as.factor(df$participant)
  
  
  my_aov <- afex::aov_ez(  id='participant',
                           dv='percept',
                           data=df,
                           within=c('condition','period')
  )
  print(my_aov)
  
  
}

# internal motion -----


internalmotionPerceptionANOVA <- function() {
  
  participants <- getParticipants()
  
  df <- getPerceivedMotionData(participants, FUN=median)
  df <- df[which(df$stimtype %in% c('dotcounterframe','dotwindowframe','dotmovingframe','dotdoublerframe')),]
  
  df$participant <- as.factor(df$participant)
  
  my_aov <- afex::aov_ez(  id='participant',
                           dv='percept',
                           data=df,
                           within=c('stimtype')
  )
  print(my_aov)
  
}

internalmotionIllusionANOVA <- function() {
  
  participants <- getParticipants()
  
  df <- getTextureMotionData(participants, FUN=median)
  df <- df[which(df$stimtype %in% c('dotcounterframe','dotwindowframe','dotmovingframe','dotdoublerframe')),]
  
  df$participant <- as.factor(df$participant)
  
  my_aov <- afex::aov_ez(  id='participant',
                           dv='percept',
                           data=df,
                           within=c('stimtype')
  )
  print(my_aov)
  
  
  int.mot.contrasts <- list( counter.static  = c(-1, 0, 0, 1),
                             static.moving   = c(0, 0, -1, 1),
                             moving.doubler  = c(0, -1, 1, 0),
                             doubler.static  = c(0, -1, 0, 1),
                             doubler.counter = c(1, -1, 0, 0),
                             moving.counter  = c(1, 0, -1, 0))
  
  cellmeans <- emmeans::emmeans(my_aov, specs=c('stimtype'))
  cat('\n')
  print(cellmeans)
  
  concon <- emmeans::contrast(cellmeans, int.mot.contrasts, adjust='sidak')
  
  print(concon)
  
  df <- getTextureMotionData(participants, FUN=median)
  df <- df[which(df$stimtype %in% c('dotmovingframe','classicframe') & df$fixdot==FALSE & df$period==1/3),]
  
  print(t.test(x = df$percept[which(df$stimtype=='classicframe')],
               y = df$percept[which(df$stimtype=='dotmovingframe')],
               paired = TRUE))
  
  print( BayesFactor::ttestBF(
                x = df$percept[which(df$stimtype=='classicframe')],
                y = df$percept[which(df$stimtype=='dotmovingframe')],
                paired = TRUE))
  
  
}

# self-moved frames -----

selfMovedFrameANOVA <- function() {
  
  participants <- getParticipants()
  
  df <- getSelfMotionData(participants, FUN=median)
  
  df$condition <- 2
  df$condition[which(df$stimtype == 'moveframe')] <- 3
  df$condition[which(df$mapping == -1)] <- 1
  
  df$participant <- as.factor(df$participant)
  df$condition <- as.factor(df$condition)
  
  my_aov <- afex::aov_ez(  id='participant',
                           dv='percept',
                           data=df,
                           within=c('condition')
  )
  print(my_aov)
  
  
}


# time-on-task

library('lme4')
library('lmerTest')
library('optimx')

timeontaskLME <- function() {
  
  default.contrasts <- options('contrasts')
  options(contrasts=c('contr.sum','contr.poly'))
  
  participants <- getParticipants()
  
  my_data <- getExperimentTimeData(participants, FUN=median)
  
  my_data <- my_data[which(my_data$amplitude != 1.8),]
  
  # my_data$amplitude   <- as.factor(my_data$amplitude)
  # my_data$interval    <- as.factor(my_data$interval)
  my_data$participant <- as.factor(my_data$participant)
  
  # my_lmer <- lmerTest::lmer(percept ~ amplitude + interval - (1|participant),
  #                                  na.action = na.exclude,
  #                                  data = my_data,
  #                                  REML = TRUE,
  #                                  control = lmerControl(optimizer ="Nelder_Mead")
  # )
  

  my_lmer <- lmerTest::lmer(percept ~ amplitude * interval - (1|participant),
                            na.action = na.exclude,
                            data = my_data,
                            REML = TRUE,
                            control = lme4::lmerControl(optimizer ='optimx', optCtrl=list(method='L-BFGS-B'))
  )
  
  # print(my_lmer)
  
  print(anova(my_lmer,ddf='Satterthwaite',type=3))
  
}