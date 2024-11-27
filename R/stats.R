
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