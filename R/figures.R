
getColors <- function() {
  
  cols.op <- c(rgb(255, 147, 41,  255, max = 255), # orange:  21, 255, 148
               rgb(229, 22,  54,  255, max = 255), # red:    248, 210, 126
               rgb(207, 0,   216, 255, max = 255), # pink:   211, 255, 108
               rgb(127, 0,   216, 255, max = 255), # violet: 195, 255, 108
               rgb(0,   19,  136, 255, max = 255)) # blue:   164, 255, 68
  
  cols.tr <- c(rgb(255, 147, 41,  32,  max = 255), # orange:  21, 255, 148
               rgb(229, 22,  54,  32,  max = 255), # red:    248, 210, 126
               rgb(207, 0,   216, 32,  max = 255), # pink:   211, 255, 108
               rgb(127, 0,   216, 32,  max = 255), # violet: 195, 255, 108
               rgb(0,   19,  136, 32,  max = 255)) # blue:   164, 255, 68
  
  cols <- list()
  cols$op <- cols.op
  cols$tr <- cols.tr
  
  return(cols)
  
}

basicPlot <- function(target='none') {

  width  <- 9.291338583
  height <- 7.755905512

  if (target=='svg') {
    svglite::svglite(file='doc/all_data.svg', width=width, height=height, fix_text_size = FALSE)
  }
  if (target=='pdf') {
    cairo_pdf(filename='doc/all_data.pdf', width=width, height=height)
  }

  allData <- getAllData()

  layout(mat = matrix(data = c(1:9),
                      ncol = 3,
                      byrow = TRUE)  )

  
  cols <- getColors()
  cols.op <- cols$op
  cols.tr <- cols$tr

  # plot 1: anaglyph

  plot(-1000,-1000,
       xlim=c(0.5,4.5), ylim=c(0,6),
       main='Anaglyph',xlab='',ylab='illusion strength [dva]',
       bty='n', ax=F)

  df <- allData[['A1_Anaglyph']]

  lines( x = c(0.5,4.5), y = c(4, 4), col='#999999', lty=2)

  conditions <- c('same plane', 'back frame', 'front frame', 'stradled')

  for (condno in c(1:length(conditions))) {

    condition <- conditions[condno]

    cdf <- df[which(df$condition == condition),]
    avg <- mean(cdf$percept)
    ci  <- Reach::getConfidenceInterval(data=cdf$percept)
    # print(avg)
    # print(ci)

    points( x = rep(condno+0.2, dim(cdf)[1]),
            y = cdf$percept,
            pch = 16,
            col = cols.tr[condno])

    polygon( x      = condno+c(-0.35,0.,0.,-0.35),
             y      = rep(ci, each=2),
             border = NA,
             col    = cols.tr[condno] )

    lines( x   = condno+c(-0.35,0.),
           y   = rep(avg,2),
           col = cols.op[condno])

  }

  axis(side=2, at=c(0,2,4,6))
  axis(side=1, at=c(1,2,3,4), labels=rep('',length(conditions)), srt=45)

  text( seq(1, 4, by=1) + 0.2,
        par("usr")[3] - 0.75,
        labels = list('same plane'='same plane', 'back frame'='back frame', 'front frame'='front frame', 'stradled'='straddled')[conditions],
        srt = 33,
        pos = 2,
        xpd = TRUE )


  # plot 2: horizontal frame offset

  plot(-1000,-1000,
       xlim=c(-1,13), ylim=c(0,6),
       main='Horizontal Offset',xlab='horizontal offset [dva]',ylab='illusion strength [dva]',
       bty='n', ax=F)

  lines( x = c(-0.5,12.5), y = c(4, 4), col='#999999', lty=2)

  df <- allData[['A2_ProbeDistance']]

  ddf <- df[which(df$ver_offset == 0),]

  for (fsi in c(1:3)) {

    fdf <- ddf[which(ddf$inner_framesize == fsi*3),]

    avg <- c()
    hci <- c()
    lci <- c()

    X <- sort ( unique( fdf$hor_offset) )
    for (hos in X) {

      avg <- c(avg, mean(fdf$percept[which(fdf$hor_offset == hos)]))
      ci  <- Reach::getConfidenceInterval(fdf$percept[which(fdf$hor_offset == hos)])
      lci <- c(lci,ci[1])
      hci <- c(hci,ci[2])

    }

    col.idx <- ((fsi-1)*2)+1
    # col.idx <- fsi

    polygon( x = c(X, rev(X)),
             y = c(lci, rev(hci)),
             border = NA,
             col=cols.tr[col.idx])
    lines(X,avg,col=cols.op[col.idx])

  }

  axis(side=2, at=c(0,2,4,6))
  axis(side=1, at=c(0,3,6,9,12))

  legend(6,6,legend=sprintf('%d dva',c(1:3)*3),title='inner frame size', bty='n',lty=1,col=cols.op[c(1,3,5)])

  # plot 3: vertical frame offset

  plot(-1000,-1000,
       xlim=c(-1,13), ylim=c(0,6),
       main='Vertical Offset',xlab='vertical offset [dva]',ylab='illusion strength [dva]',
       bty='n', ax=F)

  lines( x = c(-0.5,12.5), y = c(4, 4), col='#999999', lty=2)

  df <- allData[['A2_ProbeDistance']]

  ddf <- df[which(df$hor_offset == 0),]

  for (fsi in c(1:3)) {

    fdf <- ddf[which(ddf$inner_framesize == fsi*3),]

    avg <- c()
    hci <- c()
    lci <- c()

    X <- sort ( unique( fdf$ver_offset) )
    for (vos in X) {

      avg <- c(avg, mean(fdf$percept[which(fdf$ver_offset == vos)]))
      ci  <- Reach::getConfidenceInterval(fdf$percept[which(fdf$ver_offset == vos)])
      lci <- c(lci,ci[1])
      hci <- c(hci,ci[2])

    }

    col.idx <- ((fsi-1)*2)+1
    # col.idx <- fsi

    polygon( x = c(X, rev(X)),
             y = c(lci, rev(hci)),
             border = NA,
             col=cols.tr[col.idx])
    lines(X,avg,col=cols.op[col.idx])

  }

  axis(side=2, at=c(0,2,4,6))
  axis(side=1, at=c(0,3,6,9,12))

  legend(6,6,legend=sprintf('%d dva',c(1:3)*3),title='inner frame size', bty='n',lty=1,col=cols.op[c(1,3,5)])

  # plot 4: apparent frame lag



  df <- allData[['B1_ApparentLag']]

  plot(-1000,-1000,
       xlim=c(-5,6), ylim=c(0,6),
       main='Apparent Motion Temporal Offset',xlab='probe lag [30 Hz frames]',ylab='illusion strength [dva]',
       bty='n', ax=F)

  lines( x=c(-4.5, 5.5), y=c(4,4), lty=2, col='#999999')

  for (stimtype in c('classicframe','apparentframe')) {

    col.idx <- c('classicframe'=1,'apparentframe'=5)[stimtype]

    cdf <- df[which(df$stimtype == stimtype),]

    avg <- c()
    hci <- c()
    lci <- c()

    X <- sort ( unique( cdf$framelag) )
    for (fl in X) {

      idx <- which(cdf$framelag == fl)

      avg <- c(avg, mean(cdf$percept[idx]))
      ci  <- Reach::getConfidenceInterval(cdf$percept[idx])
      lci <- c(lci,ci[1])
      hci <- c(hci,ci[2])

    }

    polygon( x = c(X, rev(X)),
             y = c(lci, rev(hci)),
             border = NA,
             col=cols.tr[col.idx])
    lines(X,avg,col=cols.op[col.idx])

  }


  axis(side=2, at=c(0,2,4,6))
  axis(side=1, at=c(-4,-3,-2,-1,0,1,2,3,4,5))

  legend(-1,6.5,legend=c('classic frame','apparent motion'), bty='n',lty=1,col=cols.op[c(1,5)])

  # plot 5: pre/post diction

  df <- allData[['B2_PreDiction']]

  plot(-1000,-1000,
       xlim=c(-3,3), ylim=c(0,6),
       main='Pre/Post-diction',xlab='probed pass',ylab='illusion strength [dva]',
       bty='n', ax=F)

  lines( x=c(-4.5, 4.5), y=c(4,4), lty=2, col='#999999')

  for (passes in c(1,2,3)) {

    pdf <- df[which(df$framepasses == passes),]

    for (side in c('pre','post')) {
      if (side == 'pre') {
        sdf <- pdf[which(pdf$flashoffset >= 0),]
        sdf$flashoffset <- sdf$flashoffset + 1
        xad <- -0.5
      }
      if (side == 'post') {
        sdf <- pdf[which(pdf$flashoffset <= (-1*(pdf$framepasses - 1))),]
        sdf$flashoffset <- sdf$flashoffset + (sdf$framepasses - 2)
        xad <-  0.5
      }

      X <- sort( unique( sdf$flashoffset ) )

      avg <- c()
      lci <- c()
      hci <- c()

      for (fos in X) {

        idx <- which(sdf$flashoffset == fos)
        avg <- c(avg, mean(sdf$percept[idx]))
        ci <- Reach::getConfidenceInterval(sdf$percept[idx])
        lci <- c(lci, ci[1])
        hci <- c(hci, ci[2])

      }

      polygon( x = c(X+xad, rev(X+xad)),
               y = c(lci, rev(hci)),
               border=NA,
               col=cols.tr[passes])
      lines( x = X+xad,
             y = avg,
             col = cols.op[passes])

    }

  }

  axis(side=2, at=c(0,2,4,6))
  axis(side=1, at=c(-2.5,-1.5,-0.5), labels=c('-2','-1','-0'))
  axis(side=1, at=c(0.5,1.5,2.5), labels=c('0','1','2'))

  text(-2.5,3,'post')
  text( 2.5,3,'pre')
  legend(-3,6.5,legend=c('1 pass', '2 passes', '3 passes'), bty='n',lty=1,col=cols.op[c(1,2,3)])


  # plot 6: experiment time

  df <- allData[['T1_ExperimentTime']]

  plot(-1000,-1000,
       xlim=c(0,6), ylim=c(0,6),
       main='Experiment Time',xlab='frame movement [dva]',ylab='illusion strength [dva]',
       bty='n', ax=F, asp=1)

  lines( x=c(0, 5), y=c(0, 5), lty=2, col='#999999')

  for (interval in c(1,2,3,4)) {

    idf <- df[which(df$interval == interval),]

    avg <- c()
    lci <- c()
    hci <- c()

    X <- sort( unique(idf$amplitude) )
    
    X <- c(0.8, 1.6, 2.4, 3.2, 4.0)

    for ( amplitude in X ) {

      idx <- which(idf$amplitude == amplitude)

      ip <- idf$percept[idx]
      ip <- ip[which(!is.na(ip))]

      if (length(idx) > 0) {
        #avg <- c(avg, mean(idf$percept[idx]))
        avg <- c(avg, mean(ip))
      } else {
        avg <- c(avg, NA)
      }
      if (length(idx) > 1) {
        #ci  <- SMCL::getConfidenceInterval(idf$percept[idx])
        ci  <- Reach::getConfidenceInterval(ip)
        lci <- c(lci, ci[1])
        hci <- c(hci, ci[2])
      } else {
        lci <- c(lci, NA)
        hci <- c(hci, NA)
      }

    }

    polygon( x = c(X, rev(X)),
             y = c(lci, rev(hci)),
             col = cols.tr[interval],
             border = NA)
    lines(X,avg,col=cols.op[interval])

  }

  axis(side=1,at=c(0,2,4,6))
  axis(side=2,at=c(0,2,4,6))

  legend(-2,
         6.6,
         legend=c('0-30 min.',
                  '30-60 min.',
                  '60-90 min.',
                  '90-120 min.'),
         lty=1,
         col=cols.op,
         bty='n')

  # plot 7: self-moved frames

  df <- allData[['C1_SelfMoved']]

  plot(-1000,-1000,
       xlim=c(0,3.5), ylim=c(0,6),
       main='Self-Moved Frames',xlab='',ylab='illusion strength [dva]',
       bty='n', ax=F)

  lines( x=c(0.5, 3.5), y=c(4, 4), lty=2, col='#999999')

  condf <- data.frame( 'stimtype'=c('moveframe','classicframe','moveframe'),
                       'mapping'=c(-1,1,1),
                       'label'=c('incongruent','control','congruent')         )

  for (condno in c(1,2,3)) {

    stimtype <- condf$stimtype[condno]
    mapping  <- condf$mapping[condno]
    idx      <- which(df$stimtype == stimtype & df$mapping == mapping)

    #print(idx)

    percepts <- df$percept[idx]
    avg <- mean(percepts)
    ci  <- Reach::getConfidenceInterval(percepts)

    polygon( x = condno+c(-0.35,0.0,0.0,-0.35),
             y = rep(ci,each=2),
             border = NA,
             col = cols.tr[condno])
    lines(x = condno+c(-0.35,0.0),
          y = rep(avg,2),
          col = cols.op[condno])
    points(x = rep(condno+0.2, length(percepts)),
           y = percepts,
           pch=16,
           col=cols.tr[condno])

  }

  #axis(side=1,at=c(1,2,3),labels = condf$label)
  axis(side=1,at=c(1,2,3),labels = rep('',3))
  axis(side=2,at=c(0,2,4,6))

  text( seq(1, 3, by=1) + 0.1,
        par("usr")[3] - 0.75,
        labels = condf$label,
        srt = 33,
        pos = 2,
        xpd = TRUE )


  # plot 8: perceived motion

  df <- allData[['C2_PerceivedTextureMotion']]

  plot(-1000,-1000,
       xlim=c(0,12), ylim=c(0,8),
       main='LDL Motion Perception',xlab='',ylab='perceived motion [dva]',
       bty='n', ax=F)

  lines( x=c(7.3, 11.7), y=c(4, 4), lty=2, col='#999999')
  lines( x=c(0.5, 6.5), y=c(0.5, 6.5), lty=2, col='#999999')

  df <- df[which(round(df$period, digits=6) == 0.333333),]

  # amplitude section

  adf <- df[which(df$stimtype %in% c('classicframe','dotbackground')),]

  X <- c(1,2,3,4,5,6)

  for (stimtype in c('classicframe','dotbackground')) {

    sdf <- adf[which(adf$stimtype == stimtype),]

    avg <- c()
    lci <- c()
    hci <- c()

    for (amplitude in X) {

      idx <- which(sdf$amplitude == amplitude)
      avg <- c(avg, mean(sdf$percept[idx]))
      ci  <- Reach::getConfidenceInterval(sdf$percept[idx])
      lci <- c(lci, ci[1])
      hci <- c(hci, ci[2])

    }

    if (stimtype == 'classicframe') {
      col.op <- '#999999FF'
      col.tr <- '#99999920'
    } else {
      col.op <- cols.op[5]
      col.tr <- cols.tr[5]
    }

    polygon( x = c(X, rev(X)),
             y = c(lci, rev(hci)),
             border = NA,
             col = col.tr)
    lines( x = X,
           y = avg,
           col = col.op)

  }

  legend(0,
         8,
         legend = c('dot background',
                    'control'),
         lty=1,
         col=c(cols.op[5],'#999999'),
         bty='n',
         seg.len = 1)

  stimtypes <- c('dotmovingframe',
                 'dotwindowframe',
                 'dotcounterframe',
                 'dotdoublerframe')

  for (stimno in c(1:length(stimtypes))) {

    stimtype <- stimtypes[stimno]
    sdf <- df[which(df$stimtype == stimtype),]

    percepts <- sdf$percept

    avg <- mean(percepts)
    ci  <- Reach::getConfidenceInterval(percepts)

    polygon( x = stimno+c(6.65,7,7,6.65),
             y = rep(ci, each=2),
             border = NA,
             col = cols.tr[stimno])
    points( x = rep(stimno+7.2, length(percepts)),
            y = percepts,
            pch = 16,
            col = cols.tr[stimno])

    lines( x = stimno+c(6.65,7),
           y = rep(avg,2),
           col=cols.op[stimno])

  }

  text( seq(1, 4, by=1) + 7.25,
        par("usr")[3] + 1.5,
        labels = c('dots match',
                   'dots static',
                   'dots counter',
                   'dots double'),
        srt = 90,
        pos = 2,
        xpd = TRUE )


  axis(side=1, at=c(1,2,3,4,5,6))
  axis(side=2, at=c(0,4,8))

  mtext(text='motion [dva]',
        side=1,
        line=2.5,
        at=3.5,
        cex=0.75)


  # plot 9: texture motion

  df <- allData[['C2_TextureMotion']]

  plot(-1000,-1000,
       xlim=c(0,7), ylim=c(0,6),
       main='LDL Texture Frames',xlab='',ylab='illusion strength [dva]',
       bty='n', ax=F)

  lines(c(0.2,6.8), c(4,4),
       col='#999999',lty=2)

  df <- df[which(round(df$period, digits=6) == 0.333333 & df$fixdot == FALSE),]

  stimtypes <- c( 'classicframe',
                  'dotbackground',
                  'dotmovingframe',
                  'dotwindowframe',
                  'dotcounterframe',
                  'dotdoublerframe'  )

  for (stimno in c(1:length(stimtypes))) {

    xad=0
    if (stimno == 1) {
      col.op <- '#999999'
      col.tr <- '#99999920'
    }
    if (stimno == 2) {
      col.op <- cols.op[5]
      col.tr <- cols.tr[5]
    }
    if (stimno > 2) {
      xad=1
      col.op <- cols.op[stimno-2]
      col.tr <- cols.tr[stimno-2]
    }



    percepts <- df$percept[which(df$stimtype == stimtypes[stimno])]

    avg <- mean(percepts)
    ci  <- Reach::getConfidenceInterval(percepts)

    polygon( x = stimno+c(-0.35,0,0,-0.35)+xad,
             y = rep(ci, each=2),
             border = NA,
             col = col.tr)
    points( x = rep(stimno+0.2, length(percepts))+xad,
            y = percepts,
            pch = 16,
            col = col.tr)
    lines( x = stimno+c(-0.35,0)+xad,
           y = rep(avg,2),
           col=col.op)

  }

  axis(side=2,at=c(0,2,4,6))
  axis(side=1,at=c(1,2,4,5,6,7),labels=rep('',6))

  text( c(1,2,4,5,6,7)+0.4,
        par("usr")[3] - 0.7,
        labels = c('control',
                   'dot background',
                   'dots match',
                   'dots static',
                   'dots counter',
                   'dots double'),
        srt = 33,
        pos = 2,
        xpd = TRUE )

  if (target %in% c('svg','pdf','png','tiff')) {
    dev.off()
  }

}

QRcodes <- function() {
  
  youtube <-  "https://youtu.be/aI8rsW-Ev34"
  pdf     <-  "http://mariusthart.net/tHart_CVR_2023.pdf"
  
  # layout(mat=matrix(c(1,2),ncol=1))
  
  plot(qrcode::qr_code(youtube), main='video')
  # plot.new()
  plot(qrcode::qr_code(pdf), main='poster')
  
}

fig3_offsets <- function(target='inline') {
  
  width  <- 8
  height <- 4
  dpi    <- 300
  
  if (target=='svg') {
    svglite::svglite(file='doc/fig/svg/fig3_offset.svg', width=width, height=height, fix_text_size = FALSE)
  }
  if (target=='pdf') {
    cairo_pdf(filename='doc/fig/pdf/fig3_offset.pdf', width=width, height=height)
  }
  if (target=='png') {
    png(filename='doc/fig/png/fig3_offset.png', width=width*dpi, height=height*dpi, res=dpi)
  }
  
  layout(mat = matrix(data = c(1:2),
                      ncol = 2,
                      byrow = TRUE)  )
  
  cols <- getColors()
  cols.op <- cols$op
  cols.tr <- cols$tr
  
  participants <- getParticipants()
  
  df <- getProbeDistanceData(participants, FUN=median)
  
  # df <- addShortestDistanceToFrame(df)
  
  ############ classic/raw data plots:
  
  plot(-1000,-1000,
       xlim=c(-1,13), ylim=c(0,6),
       main='',xlab='horizontal offset [dva]',ylab='perceived seperation [dva]',
       bty='n', ax=F)
  
  title(main='A', adj=0)
  
  lines( x = c(-0.5,12.5), y = c(4, 4), col='#999999', lty=2)
  
  
  ddf <- df[which(df$ver_offset == 0),]
  
  for (fsi in c(1:3)) {
    
    fdf <- ddf[which(ddf$inner_framesize == fsi*3),]
    
    avg <- c()
    hci <- c()
    lci <- c()
    
    X <- sort ( unique( fdf$hor_offset) )
    for (hos in X) {
      
      avg <- c(avg, mean(fdf$percept[which(fdf$hor_offset == hos)]))
      ci  <- Reach::getConfidenceInterval(fdf$percept[which(fdf$hor_offset == hos)])
      lci <- c(lci,ci[1])
      hci <- c(hci,ci[2])
      
    }
    
    col.idx <- ((fsi-1)*2)+1
    # col.idx <- fsi
    
    ofs <- 1 + (fsi*3)
    
    # frame motion = 4 / 2 = 2
    # probe size   = 1 / 2 = 0.5
    # add the half the outer frame size
    
    # lines(x = rep(-2+0.5+(0.5*ofs),2),
    #       y = c(0,4),
    #       col = cols.op[col.idx],
    #       lty=2)
    # lines(x = rep(2+0.5+(0.5*ofs),2),
    #       y = c(0,4),
    #       col = cols.op[col.idx],
    #       lty=3)
    
    polygon( x = c(X, rev(X)),
             y = c(lci, rev(hci)),
             border = NA,
             col=cols.tr[col.idx])
    lines(X,avg,col=cols.op[col.idx])
    
  }
  
  axis(side=2, at=c(0,2,4,6))
  axis(side=1, at=c(0,3,6,9,12))
  
  legend(6,7,legend=sprintf('%d dva',1+c(1:3)*3),title='frame size', bty='n',lty=1,col=cols.op[c(1,3,5)],xpd=TRUE)
  
  # plot 3: vertical frame offset
  
  plot(-1000,-1000,
       xlim=c(-1,13), ylim=c(0,6),
       main='',xlab='vertical offset [dva]',ylab='perceived separation [dva]',
       bty='n', ax=F)
  
  title(main='B', adj=0)
  
  
  lines( x = c(-0.5,12.5), y = c(4, 4), col='#999999', lty=2)
  
  # df <- allData[['A2_ProbeDistance']]
  
  ddf <- df[which(df$hor_offset == 0),]
  
  for (fsi in c(1:3)) {
    
    fdf <- ddf[which(ddf$inner_framesize == fsi*3),]
    
    avg <- c()
    hci <- c()
    lci <- c()
    
    X <- sort ( unique( fdf$ver_offset) )
    for (vos in X) {
      
      avg <- c(avg, mean(fdf$percept[which(fdf$ver_offset == vos)]))
      ci  <- Reach::getConfidenceInterval(fdf$percept[which(fdf$ver_offset == vos)])
      lci <- c(lci,ci[1])
      hci <- c(hci,ci[2])
      
    }
    
    col.idx <- ((fsi-1)*2)+1
    # col.idx <- fsi
    
    ofs <- 1 + (fsi*3)
    
    # lines(x = rep(-1.5+(0.5*ofs),2),
    #       y = c(0,4),
    #       col = cols.op[col.idx],
    #       lty=2)
    # lines(x = rep(1.5+(0.5*ofs),2),
    #       y = c(0,4),
    #       col = cols.op[col.idx],
    #       lty=3)
    
    polygon( x = c(X, rev(X)),
             y = c(lci, rev(hci)),
             border = NA,
             col=cols.tr[col.idx])
    lines(X,avg,col=cols.op[col.idx])
    
  }
  
  axis(side=2, at=c(0,2,4,6))
  axis(side=1, at=c(0,3,6,9,12))
  
  legend(6,7,legend=sprintf('%d dva',1+c(1:3)*3),title='frame size', bty='n',lty=1,col=cols.op[c(1,3,5)],xpd=TRUE)
  

  if (target %in% c('pdf', 'svg', 'png', 'tiff')) {
    dev.off()
  }
  
}


fig4_depth <- function(target='inline') {
  
  width  <- 4
  height <- 4
  dpi    <- 300
  
  if (target=='svg') {
    svglite::svglite(file='doc/fig/svg/fig4_depth.svg', width=width, height=height, fix_text_size = FALSE)
  }
  if (target=='pdf') {
    cairo_pdf(filename='doc/fig/pdf/fig4_depth.pdf', width=width, height=height)
  }
  if (target=='png') {
    png(filename='doc/fig/png/fig4_depth.png', width=width*dpi, height=height*dpi, res=dpi)
  }
  
  
  layout(mat = matrix(data = c(1),
                      ncol = 1,
                      byrow = TRUE)  )
  
  cols <- getColors()
  cols.op <- cols$op
  cols.tr <- cols$tr
  
  participants <- getParticipants()
  
  control <- getDepthControlData(participants, FUN=median)
  
  gooddepth <- aggregate(correct ~ participant, data=control, FUN=mean)
  participants <- gooddepth$participant[which(gooddepth$correct > 0.75)]
  
  df <- getAnaglyphData(participants, FUN=median)
  
  plot(-1000,-1000,
       xlim=c(0.5,4.5), ylim=c(0,6),
       main='',xlab='',ylab='perceived separation [dva]',
       bty='n', ax=F,
       asp=0.5)
  
  lines( x = c(0.5,4.5), y = c(4, 4), col='#999999', lty=2)
  
  conditions <- c('same plane', 'back frame', 'front frame', 'stradled')
  
  for (condno in c(1:length(conditions))) {
    
    illu <- png::readPNG(sprintf('doc/fig/src/%d_depth.png', condno), native = FALSE, info=FALSE)
    h <- dim(illu)[1]/500
    w <- dim(illu)[2]/(500*2)
    
    # print(dim(illu))
    
    rasterImage(illu, condno-(w/2), 5, condno+(w/2), 5+(h))
    
    condition <- conditions[condno]
    
    cdf <- df[which(df$condition == condition),]
    avg <- mean(cdf$percept)
    ci  <- Reach::getConfidenceInterval(data=cdf$percept)
    # print(avg)
    # print(ci)
    
    points( x = rep(condno+0.2, dim(cdf)[1]),
            y = cdf$percept,
            pch = 16,
            col = cols.tr[condno])
    
    polygon( x      = condno+c(-0.35,0.,0.,-0.35),
             y      = rep(ci, each=2),
             border = NA,
             col    = cols.tr[condno] )
    
    lines( x   = condno+c(-0.35,0.),
           y   = rep(avg,2),
           col = cols.op[condno])
    
  }
  
  axis(side=2, at=c(0,2,4,6))
  axis(side=1, at=c(1,2,3,4), labels=rep('',length(conditions)), srt=45)
  
  text( seq(1, 4, by=1) + 0.2,
        par("usr")[3] - 0.75,
        labels = list('same plane'='same plane', 'back frame'='back frame', 'front frame'='front frame', 'stradled'='straddled')[conditions],
        srt = 33,
        pos = 2,
        xpd = TRUE )
  
  
  if (target %in% c('pdf', 'svg', 'png', 'tiff')) {
    dev.off()
  }
  
}


fig5_prepost <- function(target='inline') {
  
  width  <- 5 # was 8
  height <- 4
  dpi    <- 300
  
  if (target=='svg') {
    svglite::svglite(file='doc/fig/svg/fig5_prepost.svg', width=width, height=height, fix_text_size = FALSE)
  }
  if (target=='pdf') {
    cairo_pdf(filename='doc/fig/pdf/fig5_prepost.pdf', width=width, height=height)
  }
  if (target=='png') {
    png(filename='doc/fig/png/fig5_prepost.png', width=width*dpi, height=height*dpi, res=dpi)
  }
  
  layout(mat = matrix(data = c(1), # was c(1,2)
                      nrow = 1,
                      byrow = TRUE)  )
  par(mar=c(4,4,0.5,0.5))
  
  cols <- getColors()
  cols.op <- cols$op
  cols.tr <- cols$tr
  
  participants <- getParticipants()
  
  df <- getPreDictionData(participants, FUN=median)
  
  plot(-1000,-1000,
       xlim=c(-3,3), ylim=c(-2.5,4.5),
       main='',xlab='overlap',ylab='perceived separation [dva]',
       bty='n', ax=F)
  # title(main='A', adj=0)
  
  lines( x=c(-4.5, 4.5), y=c(4,4), lty=2, col='#999999')
  lines( x=c(-4.5, 4.5), y=c(0,0), lty=2, col='#999999')
  

  for (passes in c(1,2,3)) {
    
    pdf <- df[which(df$framepasses == passes),]
    
    for (side in c('pre','post')) {
      if (side == 'pre') {
        sdf <- pdf[which(pdf$flashoffset >= 0),]
        sdf$flashoffset <- sdf$flashoffset + 1
        xad <- -0.5
      }
      if (side == 'post') {
        sdf <- pdf[which(pdf$flashoffset <= (-1*(pdf$framepasses - 1))),]
        sdf$flashoffset <- sdf$flashoffset + (sdf$framepasses - 2)
        xad <-  0.5
      }
      
      X <- sort( unique( sdf$flashoffset ) )
      
      avg <- c()
      lci <- c()
      hci <- c()
      
      for (fos in X) {
        
        idx <- which(sdf$flashoffset == fos)
        avg <- c(avg, mean(sdf$percept[idx]))
        ci <- Reach::getConfidenceInterval(sdf$percept[idx])
        lci <- c(lci, ci[1])
        hci <- c(hci, ci[2])
        
      }
      
      polygon( x = c(X+xad, rev(X+xad)),
               y = c(lci, rev(hci)),
               border=NA,
               col=cols.tr[passes])
      lines( x = X+xad,
             y = avg,
             col = cols.op[passes])
      
    }
    
  }
  
  axis(side=2, at=c(0,2,4))
  axis(side=1, at=c(-2.5,-1.5,-0.5), labels=c('0','1','2'))
  axis(side=1, at=c(0.5,1.5,2.5), labels=c('2','1','0'))
  
  text(-2.75,1.5,'pre')
  text( 2.75,1.5,'post')
  legend(1,4,legend=c('1 pass', '2 passes', '3 passes'), bty='n',lty=1,col=cols.op[c(1,2,3)])
  
  # # # # # 3 #  # # # # # 3
  #
  # SHOW CONDITIONS
  #
  # # # # # 3 #  # # # # # 3
  
  
  for (condno in c(1:6)) {
    
    # for (frame in c(1:4)) {
    #   
    #   frameloc_x <- (frame - 2.5) *  .1
    #   frameloc_y <- (frame        * -.5) -.3
    #   
    #   polygon(x = c(-0.3, 0.3, 0.3, -0.3) + frameloc_x + condno,
    #           y = c(-0.275, -0.275, 0.275, 0.275) + frameloc_y,
    #           border = '#FFFFFF',
    #           col = '#CCCCCC')
    #   
    #  
    # 
    # }
    
    x_loc <- condno - 3.5
    
    illu <- png::readPNG(sprintf('doc/fig/src/ppd-%d.png', condno), native = FALSE, info=FALSE)
    figh <- dim(illu)[1]
    figw <- dim(illu)[2]
    
    aspect_ratio <- (6 / 7) / (width/height)
    
    img_scale <- (figh/figw)
    w <- 1.2 * aspect_ratio
    h <- 1.2 * img_scale

    rasterImage(illu, x_loc-(w/2), -2.4, x_loc+(w/2), -2.4+(h))
    
  }
  
  
  # # post-hoc illustration?
  # plot(-1000,-1000,
  #      xlim=c(0.5,3.5), ylim=c(-0.6,6),
  #      main='',xlab='overlap',ylab='perceived separation [dva]',
  #      bty='n', ax=F)
  # title(main='B', adj=0)
  # 
  # lines( x=c(0.5, 3.5), y=c(4,4), lty=2, col='#999999')
  # lines( x=c(0.5, 3.5), y=c(0,0), lty=2, col='#999999')
  # 
  # # normalize data set...
  # df$diction <- 'pre'
  # df$diction[which(df$flashoffset < 0)] <- 'post'
  # 
  # post0 <- df[which(df$framepasses == 1 & df$flashoffset == 0),]
  # post0$diction <- 'post'
  # 
  # df <- rbind(df,post0)
  # 
  # nidx <- which(df$flashoffset < 0)
  # df$flashoffset[nidx] <- df$flashoffset[nidx] + df$framepasses[nidx] - 1
  # 
  # df$participant <- as.factor(df$participant)
  # 
  # df$flashoffset <- abs(df$flashoffset)
  # 
  # 
  # fodf <- aggregate (percept ~ participant + flashoffset + diction, data=df, FUN=mean)  
  # # now plot:
  # 
  # for (flashoffset in c(0,1,2)) {
  #   
  #   post <- fodf$percept[(which(fodf$flashoffset == flashoffset & fodf$diction == 'post'))]
  #   pre  <- fodf$percept[(which(fodf$flashoffset == flashoffset & fodf$diction == 'pre'))]
  #   
  #   po_avg <- mean(post)
  #   pr_avg <- mean(pre)
  #   po_CI <- Reach::getConfidenceInterval(post)
  #   pr_CI <- Reach::getConfidenceInterval(pre)
  #   
  #   polygon(x = c(0.6,0.9,0.9,0.6)+flashoffset,
  #           y = rep(po_CI,each=2),
  #           border = NA,
  #           col=cols.tr[2])
  #   lines(x=c(0.6,0.9)+flashoffset,
  #         y=rep(po_avg,2),
  #         col=cols.op[2])
  #   
  #   polygon(x = c(1.1,1.4,1.4,1.1)+flashoffset,
  #           y = rep(pr_CI,each=2),
  #           border = NA,
  #           col=cols.tr[5])
  #   lines(x=c(1.1,1.4)+flashoffset,
  #         y=rep(pr_avg,2),
  #         col=cols.op[5])
  #   
  # }
  # 
  # axis(side=2, at=c(0,2,4))
  # axis(side=1, at=c(1,2,3), labels=c('2','1','0'))
  # 
  # legend(1.2,6.7,legend=c('pre', 'post'), bty='n',lty=1,col=cols.op[c(2,5)])
  # 
  # text(x=c(1,2,3),y=2,
  #      labels=c('n.s.', '***', '***'))
  
  if (target %in% c('pdf', 'svg', 'png', 'tiff')) {
    dev.off()
  }
  
}


# fig6_org_probelag <- function(target='inline') {
#   
#   binEffects <- FALSE
#   
#   width  <- 5
#   height <- 4
#   dpi    <- 300
#   
#   if (binEffects) { width = 8 }
#   
#   if (target=='svg') {
#     svglite::svglite(file='doc/fig/svg/fig6_probelag.svg', width=width, height=height, fix_text_size = FALSE)
#   }
#   if (target=='pdf') {
#     cairo_pdf(filename='doc/fig/pdf/fig6_probelag.pdf', width=width, height=height)
#   }
#   if (target=='png') {
#     png(filename='doc/fig/png/fig6_probelag.png', width=width*dpi, height=height*dpi, res=dpi)
#   }
#   
#   
#   if (binEffects) {
#     layout(mat = matrix(data = c(1,2),
#                         nrow = 1,
#                         byrow = TRUE)  )
#   } else {
#     layout(mat = matrix(data = c(1),
#                         nrow = 1,
#                         byrow = TRUE)  )
#   }
#   
#   cols <- getColors()
#   cols.op <- cols$op
#   cols.tr <- cols$tr
#   
#   # participants <- getParticipants()
#   participants <- c(1:8)
#   
#   df <- getApparentLagData(participants, FUN=median)
#   
#   plot(-1000,-1000,
#        xlim=c(-5,6), ylim=c(-1,5),
#        main='',xlab='probe lag [% frame pass]',ylab='perceived separation [dva]',
#        bty='n', ax=F)
#   if (binEffects) {
#     title(main='A', adj=0)
#   }
#   
#   lines( x=c(-4.5, 5.5), y=c(4,4), lty=2, col='#999999')
#   lines( x=c(-4.5, 5.5), y=c(0,0), lty=2, col='#999999')
#   
#   lines( x=c(0,0),y=c(0,4), lty=2, col='#999999')
#   
#   for (stimtype in c('classicframe','apparentframe')) {
#     
#     col.idx <- c('classicframe'=1,'apparentframe'=5)[stimtype]
#     
#     cdf <- df[which(df$stimtype == stimtype),]
#     
#     avg <- c()
#     hci <- c()
#     lci <- c()
#     
#     X <- sort ( unique( cdf$framelag) )
#     for (fl in X) {
#       
#       idx <- which(cdf$framelag == fl)
#       
#       avg <- c(avg, mean(cdf$percept[idx]))
#       ci  <- Reach::getConfidenceInterval(cdf$percept[idx])
#       lci <- c(lci,ci[1])
#       hci <- c(hci,ci[2])
#       
#     }
#     
#     if (stimtype == 'classicframe') {
#       # X = X - 1
#     } else {
#       # X = X * -1
#     }
#     
#     polygon( x = c(X, rev(X)),
#              y = c(lci, rev(hci)),
#              border = NA,
#              col=cols.tr[col.idx])
#     lines(X,avg,col=cols.op[col.idx])
#     
#   }
#   
#   
#   axis(side=2, at=c(0,2,4))
#   # axis(side=1, at=c(-4,-3,-2,-1,0,1,2,3,4,5),labels=sprintf('%d%%',10*c(-4:5)),las=2)
#   axis(side=1, at=c(-4,-3,-2,-1,0,1,2,3,4,5),labels=sprintf('%d%%',9*c(-4:5)),las=2)
#   
#   legend(-4,5.6,legend=c('classic frame','apparent motion'), bty='n',lty=1,col=cols.op[c(1,5)])
#   
#   
#   # # # # #    DIFFERENCE
#   
#   if (binEffects) {
#     
#     plot(-1000,-1000,
#          xlim=c(-1.5,1.5), ylim=c(-0.25,1.25),
#          main='',xlab='probe lag',ylab='apparent / classic',
#          bty='n', ax=F)
#     
#     title(main='B', adj=0)
#     
#     lines( x=c(-1.5, 1.5), y=c(1,1), lty=2, col='#999999')
#     lines( x=c(-1.5, 1.5), y=c(0,0), lty=2, col='#999999')
#     
#     avg <- c()
#     hci <- c()
#     lci <- c()
#     
#     col.idx <- 3
#     
#     for (flgr in c('before','during','after')) {
#       # print(fl)
#       
#       lg_df <- df[which(list('before'=df$framelag < -1, 'during'=df$framelag%in%c(-1,0,1), 'after'=df$framelag>1)[[flgr]]),]
#       
#       classic  <- aggregate(percept ~ participant, data=lg_df[which(lg_df$stimtype=='classicframe'),], FUN=mean)
#       apparent <- aggregate(percept ~ participant, data=lg_df[which(lg_df$stimtype=='apparentframe'),], FUN=mean)
#       proportion <- apparent$percept / classic$percept
#       
#       
#       
#       # avg <- c(avg, mean(proportion))
#       avg <- mean(proportion)
#       ci  <- Reach::getConfidenceInterval(proportion)
#       # lci <- c(lci,ci[1])
#       # hci <- c(hci,ci[2])
#       lci <- c(lci,ci[1])
#       hci <- c(hci,ci[2])
#       
#       x <- list('before'=-1, 'during'=0, 'after'=1)[[flgr]]
#       
#       polygon( x = c(-0.3,0.3,0.3,-0.3)+x,
#                y = rep(ci, each=2),
#                border = NA,
#                col=cols.tr[col.idx], xpd=TRUE)
#       lines( x = c(-0.3,0.3)+x,
#              y = rep(avg,2),
#              col=cols.op[col.idx])
#       
#     }
#     
#     # print(avg)
#     
#     # X = c(-1, 0, 1)
#     # 
#     # polygon( x = c(X, rev(X)),
#     #          y = c(lci, rev(hci)),
#     #          border = NA,
#     #          col=cols.tr[col.idx])
#     # lines(X,avg,col=cols.op[col.idx])
#     
#     axis(side=2, at=c(0,.5,1), labels=c('0%','50%','100%'))
#     # axis(side=1, at=c(-4,-3,-2,-1,0,1,2,3,4,5),labels=sprintf('%d%%',10*c(-4:5)),las=2)
#     # axis(side=1, at=c(-4,-3,-2,-1,0,1,2,3,4,5),labels=sprintf('%d%%',9*c(-4:5)),las=2)
#     axis(side=1, at=c(-1,0,1), labels=c('-','simultaneous','+'))
#     
#   }
#   
#   if (target %in% c('pdf', 'svg', 'png', 'tiff')) {
#     dev.off()
#   }
#   
# }


# fig6_probelag <- function(target='inline') {
#   
#   binEffects <- FALSE
#   
#   width  <- 4
#   height <- 4
#   dpi    <- 300
#   
#   if (target=='svg') {
#     svglite::svglite(file='doc/fig/svg/fig6_probelag.svg', width=width, height=height, fix_text_size = FALSE)
#   }
#   if (target=='pdf') {
#     cairo_pdf(filename='doc/fig/pdf/fig6_probelag.pdf', width=width, height=height)
#   }
#   if (target=='png') {
#     png(filename='doc/fig/png/fig6_probelag.png', width=width*dpi, height=height*dpi, res=dpi)
#   }
#   
#   layout(mat = matrix(data = c(1),
#                       nrow = 1,
#                       byrow = TRUE)  )
#   
#   cols <- getColors()
#   cols.op <- cols$op
#   cols.tr <- cols$tr
#   
#   # participants <- getParticipants()
#   participants <- c(1:8)
#   
#   df <- getApparentLagData(participants, FUN=median)
#   
#   df$framelag <- abs(df$framelag)
#   
#   plot(-1000,-1000,
#        xlim=c(-0.5,5.5), ylim=c(-1,4),
#        main='',xlab='',ylab='perceived separation [dva]',
#        bty='n', ax=F)
#   
#   title(xlab='probe lag [% frame pass]', line=4)
#   
#   
#   lines( x=c(-0.25, 5.25), y=c(4,4), lty=2, col='#999999')
#   lines( x=c(-0.25, 5.25), y=c(0,0), lty=2, col='#999999')
#   
#   # lines( x=c(0,0),y=c(0,4), lty=2, col='#999999')
#   
#   models <- probeLagLinQuad(verbosity=0,returnmodels=TRUE)
#   
#   for (stimtype in c('classicframe','apparentframe')) {
#     
#     col.idx <- c('classicframe'=1,'apparentframe'=5)[stimtype]
#     
#     cdf <- df[which(df$stimtype == stimtype),]
#     
#     avg <- c()
#     hci <- c()
#     lci <- c()
#     
#     X <- sort ( unique( cdf$framelag) )
#     for (fl in X) {
#       
#       idx <- which(cdf$framelag == fl)
#       
#       avg <- c(avg, mean(cdf$percept[idx]))
#       ci  <- Reach::getConfidenceInterval(cdf$percept[idx])
#       lci <- c(lci,ci[1])
#       hci <- c(hci,ci[2])
#       
#     }
#     
#     if (stimtype == 'classicframe') {
#       # X = X - 1
#     } else {
#       # X = X * -1
#     }
#     
#     # polygon( x = c(X, rev(X)),
#     #          y = c(lci, rev(hci)),
#     #          border = NA,
#     #          col=cols.tr[col.idx])
#     # lines(X,avg,col=cols.op[col.idx])
#     
#     polygon( x = c(-0.1,0.1,0.1,-0.1)*2,
#              y = c(rep(lci[1],2), rep(hci[1],2)),
#              border=NA,
#              col=cols.tr[col.idx])
#     lines( x = c(-0.1,0.1)*2,
#            y = rep(avg[1],2),
#            col = cols.op[col.idx])
#     
#     points( x = X[2:length(X)],
#             y = avg[2:length(avg)],
#             pch = 1,
#             col = cols.op[col.idx])
#     
#     thismodel <- models[[stimtype]]
#     
#     # print(thismodel$coefficients)
#     
#     newX     <- seq(0,4,length.out=100)
#     lagp  <- newX/11
#     lagp2 <- lagp^2 
#     
#     intercept <- thismodel$coefficients['(Intercept)']
#     
#     if (names(thismodel$coefficients)[2] == 'lagp') {
#       pred <- thismodel$coefficients['lagp']*lagp + thismodel$coefficients['(Intercept)']
#     }
#     if (names(thismodel$coefficients)[2] == 'lagp2') {
#       pred <- thismodel$coefficients['lagp2']*lagp2 + thismodel$coefficients['(Intercept)']
#     }
#     
#     # Y <- predict(thismodel, newdata=data.frame(lagp=lagp, lagp2=lagp2) )
#     
#     lines( x = newX+1,
#            y = pred,
#            col = cols.op[col.idx]  )
#     
#     axis(side=2, at=c(0,2,4))
#     # axis(side=1, at=c(-4,-3,-2,-1,0,1,2,3,4,5),labels=sprintf('%d%%',10*c(-4:5)),las=2)
#     # axis(side=1, at=c(-4,-3,-2,-1,0,1,2,3,4,5),labels=sprintf('%d%%',9*c(-4:5)),las=2)
#     axis(side=1, at=c(0,1,2,3,4,5),labels=c('mid\npause',sprintf('%d%%',9*c(0:4))),las=2)
#     
#     legend(1, 
#            5.6,
#            legend=c('classic frame','apparent motion'), 
#            bty='n',lty=1,col=cols.op[c(1,5)],
#            xpd=TRUE)
#     
#   }
#   
#   if (target %in% c('pdf', 'svg', 'png', 'tiff')) {
#     dev.off()
#   }
#   
# }


fig6_probelag <- function(target='inline') {
  
  binEffects <- FALSE
  
  width  <- 4.5
  height <- 4
  dpi    <- 300
  
  if (target=='svg') {
    svglite::svglite(file='doc/fig/svg/fig6_probelag.svg', width=width, height=height, fix_text_size = FALSE)
  }
  if (target=='pdf') {
    cairo_pdf(filename='doc/fig/pdf/fig6_probelag.pdf', width=width, height=height)
  }
  if (target=='png') {
    png(filename='doc/fig/png/fig6_probelag.png', width=width*dpi, height=height*dpi, res=dpi)
  }
  
  layout(mat = matrix(data = c(1),
                      nrow = 1,
                      byrow = TRUE)  )
  
  cols <- getColors()
  cols.op <- cols$op
  cols.tr <- cols$tr
  
  # participants <- getParticipants()
  participants <- c(1:8)
  
  df <- getApparentLagData(participants, FUN=median)
  
  df$framelag <- abs(df$framelag)
  df <- df[which(df$framelag <= 4),]
  
  plot(-1000,-1000,
       xlim=c(-.5,7), ylim=c(-.5,4.5),
       main='',xlab='',ylab='perceived separation [dva]',
       bty='n', ax=F)
  
  title(xlab='probe lag [% frame pass]', line=4)
  
  
  lines( x=c(-0.25, 7), y=c(4,4), lty=2, col='#999999')
  lines( x=c(-0.25, 7), y=c(0,0), lty=2, col='#999999')
  
  # lines( x=c(0,0),y=c(0,4), lty=2, col='#999999')
  
  # models <- probeLagLinQuad(verbosity=0,returnmodels=TRUE)
  
  for (stimtype in c('classicframe','apparentframe')) {
    
    col.idx <- c('classicframe'=1,'apparentframe'=5)[stimtype]
    
    cdf <- df[which(df$stimtype == stimtype),]
    
    avg <- c()
    hci <- c()
    lci <- c()
    
    X <- sort ( unique( cdf$framelag) )
    
    for (fl in X) {
      
      idx <- which(cdf$framelag == fl)
      
      avg <- c(avg, mean(cdf$percept[idx]))
      ci  <- Reach::getConfidenceInterval(cdf$percept[idx])
      lci <- c(lci,ci[1])
      hci <- c(hci,ci[2])
      
    }
    
    if (stimtype == 'classicframe') {
      # X = X - 1
    } else {
      # X = X * -1
    }
    
    # polygon( x = c(X, rev(X)),
    #          y = c(lci, rev(hci)),
    #          border = NA,
    #          col=cols.tr[col.idx])
    # lines(X,avg,col=cols.op[col.idx])
    
    # pause_percepts <- cdf$percept[which(cdf$framelag == 0)]
    # offset <- list('classicframe'=-.15,'apparentframe'=.25)[[stimtype]]
    # points( x = seq(-.55,-.65, length.out=length(pause_percepts)) + offset,
    #         # x = rep(-.5+offset, length(pause_percepts)),
    #         y = pause_percepts,
    #         pch = 16,
    #         cex=1.5,
    #         col = cols.tr[col.idx])
    
    polygon( x = c(0,0.2,0.2,0)*2,
             y = c(rep(lci[1],2), rep(hci[1],2)),
             border=NA,
             col=cols.tr[col.idx])
    lines( x = c(0,0.2)*2,
           y = rep(avg[1],2),
           col = cols.op[col.idx])
    
    points( x = X[2:length(X)],
            y = avg[2:length(avg)],
            pch = 1,
            col = cols.op[col.idx])
    # 
    # ilindecay <- lm( avg[2:length(avg)] ~ I(X[2:length(X)] - (50/9) - 2) + 0 )
    # # print(unname(lindecay$coefficients)[1])
    # 
    # lines( x = c(1,(50/9)+1),
    #        y = c(-1-(50/9), 0) * unname(ilindecay$coefficients)[1],
    #        col = cols.op[col.idx],
    #        lty=2
    #        )
    # 
    # print(str(cdf))
    
    lm.cdf <- cdf[which(cdf$framelag >= 1 & cdf$framelag <= 4),]
    
    lf <- lm.cdf$framelag
    lp <- lm.cdf$percept
    
    lindecay <- lm( lp ~ lf )
    # print(lindecay)
    
    
    
    at <- c(min(lf), (50/9) + 1)
    
    coef <- lindecay$coefficients
    lines(at, coef[1]+(at*coef[2]), col=cols.op[col.idx])
    # lines(c(max(lf), (50/9)+1),
    #       c( coef[1]+(max(lf)*coef[2]),
    #          coef[1]+(((50/9)+1)*coef[2]) ),
    #       col=cols.op[col.idx],
    #       lty=3)
    # 
    at <- range(lf)
    ci <- predict( lindecay,
                   newdata=data.frame(lf=seq(at[1],at[2],length.out=101)),
                   interval = "confidence")
    # 
    X <- c(seq(at[1],at[2],length.out=101),rev(seq(at[1],at[2],length.out=101)))
    Y <- c(ci[,'lwr'],rev(ci[,'upr']))
    polygon(x=X,y=Y,col=cols.tr[col.idx],border=NA)
    
    
    
    
    
    
    
    # thismodel <- models[[stimtype]]
    # 
    # # print(thismodel$coefficients)
    # 
    # newX     <- seq(0,4,length.out=100)
    # lagp  <- newX/11
    # lagp2 <- lagp^2 
    # 
    # intercept <- thismodel$coefficients['(Intercept)']
    # 
    # if (names(thismodel$coefficients)[2] == 'lagp') {
    #   pred <- thismodel$coefficients['lagp']*lagp + thismodel$coefficients['(Intercept)']
    # }
    # if (names(thismodel$coefficients)[2] == 'lagp2') {
    #   pred <- thismodel$coefficients['lagp2']*lagp2 + thismodel$coefficients['(Intercept)']
    # }
    # 
    # # Y <- predict(thismodel, newdata=data.frame(lagp=lagp, lagp2=lagp2) )
    # 
    # lines( x = newX+1,
    #        y = pred,
    #        col = cols.op[col.idx]  )
    
    axis(side=2, at=c(0,2,4))
    # axis(side=1, at=c(-4,-3,-2,-1,0,1,2,3,4,5),labels=sprintf('%d%%',10*c(-4:5)),las=2)
    # axis(side=1, at=c(-4,-3,-2,-1,0,1,2,3,4,5),labels=sprintf('%d%%',9*c(-4:5)),las=2)
    # axis(side=1, at=c(0,1,2,3,4,5),labels=c('mid\npause',sprintf('%d%%',9*c(0:4))),las=2)
    axis(side=1, at=c(0),labels=c('mid\npause'),las=2)
    
    axis(side=1, at=(c(0,1,2,3,4,5)/.9)+1,labels=sprintf('%d%%',c(0,10,20,30,40,50)),las=2)
    
    legend(1, 
           5.6,
           legend=c('classic frame','apparent motion'), 
           bty='n',lty=1,col=cols.op[c(1,5)],
           xpd=TRUE)
    
  }  
  
  if (target %in% c('pdf', 'svg', 'png', 'tiff')) {
    dev.off()
  }
  
}

fig7_background <- function(target='inline') {
  
  width  <- 6.5
  height <- 7.7
  dpi    <- 300
  
  if (target=='svg') {
    svglite::svglite(file='doc/fig/svg/fig7_background.svg', width=width, height=height, fix_text_size = FALSE)
  }
  if (target=='pdf') {
    cairo_pdf(filename='doc/fig/pdf/fig7_background.pdf', width=width, height=height)
  }
  if (target=='png') {
    png(filename='doc/fig/png/fig7_background.png', width=width*dpi, height=height*dpi, res=dpi)
  }
  
  layout(mat = matrix(data = c(1:4),
                      ncol = 2, nrow = 2,
                      byrow = TRUE),
         widths = c(1.5,1),
         heights = c(1,1)
         )
  
  cols <- getColors()
  cols.op <- cols$op
  cols.tr <- cols$tr
  
  participants <- getParticipants()
  
  df <- getPerceivedMotionData(participants, FUN=median)
  
  plot(NULL,NULL,
       xlim=c(0,7), ylim=c(0,8),
       main='',xlab='motion amplitude [dva]',ylab='perceived motion amplitude [dva]',
       bty='n', ax=F)  # , asp=1
  title(main='A', adj=0)
  
  lines( x=c(0, 7), y=c(0, 7), lty=1, col='#999999')
  lines( x=c(0.5, 6.5), y=c(4, 4), lty=2, col='#999999')
  lines( x=c(4, 4), y=c(0, 8), lty=2, col='#999999')
  
  df <- df[which(round(df$period, digits=6) == 0.333333),]
  
  # amplitude section
  
  adf <- df[which(df$stimtype %in% c('classicframe','dotbackground')),]
  
  X <- c(1,2,3,4,5,6)
  
  for (stimtype in c('classicframe','dotbackground')) {
    
    sdf <- adf[which(adf$stimtype == stimtype),]
    
    avg <- c()
    lci <- c()
    hci <- c()
    
    for (amplitude in X) {
      
      idx <- which(sdf$amplitude == amplitude)
      avg <- c(avg, mean(sdf$percept[idx]))
      ci  <- Reach::getConfidenceInterval(sdf$percept[idx])
      lci <- c(lci, ci[1])
      hci <- c(hci, ci[2])
      
    }
    
    if (stimtype == 'classicframe') {
      col.op <- '#999999FF'
      col.op <- cols.op[2]
      col.tr <- '#99999920'
      col.tr <- cols.tr[2]
    } else {
      col.op <- cols.op[5]
      col.tr <- cols.tr[5]
    }
    
    polygon( x = c(X, rev(X)),
             y = c(lci, rev(hci)),
             border = NA,
             col = col.tr,
             xpd=TRUE)
    lines( x = X,
           y = avg,
           col = col.op)
    
  }
  
  legend(0,
         8.5,
         legend = c('dot background',
                    'frame'),
         lty=1,
         # col=c(cols.op[5],'#999999'),
         col=c(cols.op[5],cols.op[2]),
         bty='n',
         seg.len = 1)
  
  axis(side=1, at=c(1,2,3,4,5,6))
  axis(side=2, at=c(0,2,4,6,8))
  
  # cat('finished A\n')
  
  # # # # # # # # # # 
  # perceived probe separation over speed for dot background
  
  plot(NULL,NULL,
       xlim=c(0.2,1), ylim=c(0,8),
       main='',xlab='motion duration [ms]',ylab='perceived motion amplitude [dva]',
       bty='n', ax=F, log='x')
  title(main='B', adj=0)
  
  # lines( x=c(0, 7), y=c(0, 7), lty=1, col='#999999')
  lines( x=c(0.2, 1), y=c(4, 4), lty=2, col='#999999')
  # lines( x=c(4, 4), y=c(0, 8), lty=2, col='#999999')
  
  df <- getPerceivedMotionData(participants, FUN=median)
  df_dur <- df[which(df$amplitude == 4),]
  
  # amplitude section
  
  df_dur <- df_dur[which(df_dur$stimtype %in% c('classicframe','dotbackground')),]
  
  X <- 1/c(5,4,3,2,1)
  
  for (stimtype in c('classicframe','dotbackground')) {
    
    sdf <- df_dur[which(df_dur$stimtype == stimtype),]
    
    avg <- c()
    lci <- c()
    hci <- c()
    
    for (duration in X) {
      
      idx <- which(round(sdf$period,3) == round(duration,3))
      avg <- c(avg, mean(sdf$percept[idx]))
      ci  <- Reach::getConfidenceInterval(sdf$percept[idx])
      lci <- c(lci, ci[1])
      hci <- c(hci, ci[2])
      
    }
    
    if (stimtype == 'classicframe') {
      col.op <- '#999999FF'
      col.op <- cols.op[2]
      col.tr <- '#99999920'
      col.tr <- cols.tr[2]
    } else {
      col.op <- cols.op[5]
      col.tr <- cols.tr[5]
    }
    
    polygon( x = c(X, rev(X)),
             y = c(lci, rev(hci)),
             border = NA,
             col = col.tr,
             xpd=TRUE)
    lines( x = X,
           y = avg,
           col = col.op)
    
  }
  
  # legend(-1,
  #        8.5,
  #        legend = c('dot background',
  #                   'frame'),
  #        lty=1,
  #        # col=c(cols.op[5],'#999999'),
  #        col=c(cols.op[5],cols.op[2]),
  #        bty='n',
  #        seg.len = 1)
  
  # axis(side=1, at=round(X,2), labels=c('⅕', '¼', '⅓', '½', '1'), cex.axis=0.85)
  # axis(side=1, at=round(X,2), cex.axis=0.85)
  # axis(side=1, at=round(X,2), cex.axis=0.85, las=2)
  axis(side=1, at=c(0.2, 0.5, 1.0), labels=c('200', '500', '1000'), cex.axis=0.85, las=2)
  axis(side=2, at=c(0,2,4,6,8))
  
  # cat('finished B\n')
  
  plot(NULL,NULL,
       xlim=c(0,4), ylim=c(0,6),
       main='',xlab='',ylab='perceived separation [dva]',
       bty='n', ax=F)
  title(main='C', adj=0)
  
  lines(c(0.2,6.8), c(4,4),
        col='#999999',lty=2)
  
  df <- getTextureMotionData(participants, FUN=median)
  
  df <- df[which(round(df$period, digits=6) == 0.333333),]
  
  stimtypes <- c( 'classicframe',
                  'classicframe',
                  'dotbackground' )
  fixate   <- c( TRUE, FALSE, FALSE)
  
  for (stimno in c(1:length(stimtypes))) {
    
    xad=0
    if (stimno == 1) {
      col.op <- '#333333'
      col.tr <- '#99999920'
    }
    if (stimno == 2) {
      col.op <- '#999999'
      col.op <- cols.op[2]
      col.tr <- '#99999920'
      col.tr <- cols.tr[2]
    }
    if (stimno == 3) {
      col.op <- cols.op[5]
      col.tr <- cols.tr[5]
    }
    # if (stimno > 2) {
    #   xad=1
    #   col.op <- cols.op[stimno-2]
    #   col.tr <- cols.tr[stimno-2]
    # }
    
    
    percepts <- df$percept[which(df$stimtype == stimtypes[stimno]  & df$fixdot == fixate[stimno])]
    
    avg <- mean(percepts)
    ci  <- Reach::getConfidenceInterval(percepts)
    
    polygon( x = stimno+c(-0.35,0,0,-0.35)+xad,
             y = rep(ci, each=2),
             border = NA,
             col = col.tr)
    points( x = rep(stimno+0.2, length(percepts))+xad,
            y = percepts,
            pch = 16,
            col = col.tr)
    lines( x = stimno+c(-0.35,0)+xad,
           y = rep(avg,2),
           col=col.op,
           lty=c(3,1,1)[stimno])
    
  }
  
  axis(side=2,at=c(0,2,4,6))
  axis(side=1,at=c(1,2,3),labels=rep('',3))
  
  text( c(1,2,3)+0.4,
        par("usr")[3] - 0.7,
        labels = c('frame fixated',
                   'frame free',
                   'dots free'),
        srt = 33,
        pos = 2,
        xpd = TRUE )
  

  # cat('finished C\n')
  
  ### second version of the above plot with frame motion duration there as well...
  
  plot(NULL,NULL,
       xlim=c(0.2,1), ylim=c(0,6),
       main='',xlab='motion duration [ms]',ylab='perceived separation [dva]',
       bty='n', ax=F, log='x')
  title(main='D', adj=0)

  lines(c(0.2,1), c(4,4),
        col='#999999',lty=2)

  df <- getTextureMotionData(participants, FUN=median)

  df$Hz <- 1/df$period

  stimtypes <- c( 'classicframe',
                  'classicframe',
                  'dotbackground' )
  fixate   <- c( TRUE, FALSE, FALSE)

  for (stimno in c(1:length(stimtypes))) {

    xad=0
    if (stimno == 1) {
      col.op <- '#333333'
      col.tr <- '#99999920'
    }
    if (stimno == 2) {
      col.op <- '#999999'
      col.tr <- '#99999920'
    }
    if (stimno == 2) {
      col.op <- cols.op[2]
      col.tr <- cols.tr[2]
    }
    if (stimno == 3) {
      col.op <- cols.op[5]
      col.tr <- cols.tr[5]
    }

    # percepts <- df$percept[which(df$stimtype == stimtypes[stimno]  & df$fixdot == fixate[stimno])]
    stimdf <- df[which(df$stimtype == stimtypes[stimno]  & df$fixdot == fixate[stimno]),]

    avg <- aggregate(percept ~ Hz, data=stimdf, FUN=mean)
    ci  <- aggregate(percept ~ Hz, data=stimdf, FUN=Reach::getConfidenceInterval)


    Y <- c( ci$percept[,1], rev(ci$percept[,2]) )
    X <- c( rev(1/c(1:5)), 1/c(1:5))

    polygon( x=X,
             y=Y,
             border=NA,
             col=col.tr
             )
    lines( x = rev(1/c(1:5)),
           y = avg$percept,
           col=col.op,
           lty=c(3,1,1)[stimno])

  #   polygon( x = stimno+c(-0.35,0,0,-0.35)+xad,
  #            y = rep(ci, each=2),
  #            border = NA,
  #            col = col.tr)
  #   points( x = rep(stimno+0.2, length(percepts))+xad,
  #           y = percepts,
  #           pch = 16,
  #           col = col.tr)
  #   lines( x = stimno+c(-0.35,0)+xad,
  #          y = rep(avg,2),
  #          col=col.op,
  #          lty=c(3,1,1)[stimno])
  #
  }

  #axis(side=1,at=1/c(5,4,3,2,1),labels=c('&#8533;', '&#188;', '&#8531;', '&#189;', '1'))
  # axis(side=1,at=1/c(5,4,3,2,1),labels=c('⅕', '¼', '⅓', '½', '1'), cex.axis=0.85)
  # axis(side=1,at=round(1/c(5,4,3,2,1),2), cex.axis=0.85)
  # axis(side=1,at=round(1/c(5,4,3,2,1),2), cex.axis=0.85, las=2)
  axis(side=1, at=c(0.2, 0.5, 1.0), labels=c('200', '500', '1000'), cex.axis=0.85, las=2)
  axis(side=2,at=c(0,2,4,6))
  
  # cat('finished D\n')
  
  # ⅕
  # ¼
  # ⅓
  # ½
  
  # fraction deximal hex
  # 1/2   &#189;	&#x00BD;
  # 1/3   &#8531;	&#x2153;
  # 1/4   &#188;  &#x00BC;
  # 1/5   &#8533;	&#x2155;
  
  

  
  
  if (target %in% c('pdf', 'svg', 'png', 'tiff')) {
    dev.off()
  }
  
}


fig8_internalmotion <- function(target='inline') {
  
  width  <- 8
  height <- 4
  dpi    <- 300
  
  if (target=='svg') {
    svglite::svglite(file='doc/fig/svg/fig8_internal.svg', width=width, height=height, fix_text_size = FALSE)
  }
  if (target=='pdf') {
    cairo_pdf(filename='doc/fig/pdf/fig8_internal.pdf', width=width, height=height)
  }
  if (target=='png') {
    png(filename='doc/fig/png/fig8_internal.png', width=width*dpi, height=height*dpi, res=dpi)
  }
  
  layout(mat = matrix(data = c(1:2),
                      ncol = 2,
                      byrow = TRUE)  )
  
  cols <- getColors()
  cols.op <- cols$op
  cols.tr <- cols$tr
  
  participants <- getParticipants()
  
  df <- getPerceivedMotionData(participants, FUN=median)
  
  plot(-1000,-1000,
       xlim=c(1,6), ylim=c(0,8),
       main='',xlab='',ylab='perceived path length [dva]',
       bty='n', ax=F)
  title(main='A', adj=0)
  
  lines( x=c(1.3, 5.7), y=c(4, 4), lty=2, col='#999999')
  
  df <- df[which(round(df$period, digits=6) == 0.333333),]
  
  # amplitude section
  

  stimtypes <- c( 'dotcounterframe',
                 'dotwindowframe',
                 'dotmovingframe',
                 'dotdoublerframe')
  
  for (stimno in c(1:length(stimtypes))) {
    
    stimtype <- stimtypes[stimno]
    sdf <- df[which(df$stimtype == stimtype),]
    
    percepts <- sdf$percept
    
    avg <- mean(percepts)
    ci  <- Reach::getConfidenceInterval(percepts)
    
    polygon( x = stimno+c(0.65,1,1,0.65),
             y = rep(ci, each=2),
             border = NA,
             col = cols.tr[stimno])
    points( x = rep(stimno+1.2, length(percepts)),
            y = percepts,
            pch = 16,
            col = cols.tr[stimno])
    
    lines( x = stimno+c(0.65,1),
           y = rep(avg,2),
           col=cols.op[stimno])
    
  }
  
  axis(side=1,at=c(2,3,4,5),labels=rep('',4))
  
  text( c(1,2,3,4)+1.4,
        par("usr")[3] - 0.7,
        labels = c('dots counter',
                   'dots static',
                   'dots match',
                   'dots double'),
        srt = 33,
        pos = 2,
        xpd = TRUE )
  
  
  axis(side=2, at=c(0,2,4,6,8))
  
  # mtext(text='motion [dva]',
  #       side=1,
  #       line=2.5,
  #       at=3.5,
  #       cex=0.75)
  
  df <- getTextureMotionData(participants, FUN=median)
  
  plot(-1000,-1000,
       xlim=c(0,5), ylim=c(0,8),
       main='',xlab='',ylab='perceived separation [dva]',
       bty='n', ax=F)
  title(main='B', adj=0)
  
  lines(c(0.2,6.8), c(4,4),
        col='#999999',lty=2)
  
  df <- df[which(round(df$period, digits=6) == 0.333333 & df$fixdot == FALSE),]
  
  stimtypes <- c( 'dotcounterframe',
                  'dotwindowframe',
                  'dotmovingframe',
                  'dotdoublerframe'  )
  
  for (stimno in c(1:length(stimtypes))) {
    
    xad=0
    col.op <- cols.op[stimno]
    col.tr <- cols.tr[stimno]
  
    percepts <- df$percept[which(df$stimtype == stimtypes[stimno])]
    
    avg <- mean(percepts)
    ci  <- Reach::getConfidenceInterval(percepts)
    
    polygon( x = stimno+c(-0.35,0,0,-0.35)+xad,
             y = rep(ci, each=2),
             border = NA,
             col = col.tr)
    points( x = rep(stimno+0.2, length(percepts))+xad,
            y = percepts,
            pch = 16,
            col = col.tr)
    lines( x = stimno+c(-0.35,0)+xad,
           y = rep(avg,2),
           col=col.op)
    
  }
  
  axis(side=2,at=c(0,2,4,6,8))
  axis(side=1,at=c(1,2,3,4),labels=rep('',4))
  
  text( c(1,2,3,4)+0.4,
        par("usr")[3] - 0.7,
        labels = c('dots counter',
                   'dots static',
                   'dots match',
                   'dots double'),
        srt = 33,
        pos = 2,
        xpd = TRUE )
  
  
  
  if (target %in% c('pdf', 'svg', 'png', 'tiff')) {
    dev.off()
  }
  
}


fig9_selfmotion <- function(target='inline') {
  
  width  <- 4
  height <- 4
  dpi    <- 300
  
  if (target=='svg') {
    svglite::svglite(file='doc/fig/svg/fig9_selfmotion.svg', width=width, height=height, fix_text_size = FALSE)
  }
  if (target=='pdf') {
    cairo_pdf(filename='doc/fig/pdf/fig9_selfmotion.pdf', width=width, height=height)
  }
  if (target=='png') {
    png(filename='doc/fig/png/fig9_selfmotion.png', width=width*dpi, height=height*dpi, res=dpi)
  }
  
  layout(mat = matrix(data = c(1),
                      ncol = 1,
                      byrow = TRUE)  )
  
  par(mar=c(5,4,2,0.1))
  
  cols <- getColors()
  cols.op <- cols$op
  cols.tr <- cols$tr
  
  participants <- getParticipants()
  
  df <- getSelfMotionData(participants, FUN=median)
  
  
  plot(-1000,-1000,
       xlim=c(0,3.5), ylim=c(0,6),
       main='', xlab='',ylab='perceived separation [dva]',
       bty='n', ax=F)
  
  lines( x=c(0.5, 3.5), y=c(4, 4), lty=2, col='#999999')
  
  condf <- data.frame( 'stimtype'=c('moveframe','classicframe','moveframe'),
                       'mapping'=c(-1,1,1),
                       'label'=c('incongruent','control','congruent')         )
  
  for (condno in c(1,2,3)) {
    
    stimtype <- condf$stimtype[condno]
    mapping  <- condf$mapping[condno]
    idx      <- which(df$stimtype == stimtype & df$mapping == mapping)
    
    #print(idx)
    
    percepts <- df$percept[idx]
    avg <- mean(percepts)
    ci  <- Reach::getConfidenceInterval(percepts)
    
    polygon( x = condno+c(-0.35,0.0,0.0,-0.35),
             y = rep(ci,each=2),
             border = NA,
             col = cols.tr[condno])
    lines(x = condno+c(-0.35,0.0),
          y = rep(avg,2),
          col = cols.op[condno])
    points(x = rep(condno+0.2, length(percepts)),
           y = percepts,
           pch=16,
           col=cols.tr[condno])
    
  }
  
  #axis(side=1,at=c(1,2,3),labels = condf$label)
  axis(side=1,at=c(1,2,3),labels = rep('',3))
  axis(side=2,at=c(0,2,4,6))
  
  text( seq(1, 3, by=1) + 0.1,
        par("usr")[3] - 0.75,
        labels = condf$label,
        srt = 33,
        pos = 2,
        xpd = TRUE )
  
  
  if (target %in% c('pdf', 'svg', 'png', 'tiff')) {
    dev.off()
  }
  
}

fig10_tasktime <- function(target='inline') {
  
  width  <- 5
  height <- 4
  dpi    <- 300
  
  if (target=='svg') {
    svglite::svglite(file='doc/fig/svg/fig10_tasktime.svg', width=width, height=height, fix_text_size = FALSE)
  }
  if (target=='pdf') {
    cairo_pdf(filename='doc/fig/pdf/fig10_tasktime.pdf', width=width, height=height)
  }
  if (target=='png') {
    png(filename='doc/fig/png/fig10_tasktime.png', width=width*dpi, height=height*dpi, res=dpi)
  }
  
  layout(mat = matrix(data = c(1),
                      ncol = 1,
                      byrow = TRUE)  )
  
  cols <- getColors()
  cols.op <- cols$op
  cols.tr <- cols$tr
  
  participants <- getParticipants()
  
  
  df <- getExperimentTimeData(participants, FUN=median)
  
  plot(-1000,-1000,
       xlim=c(0,8), ylim=c(0,4),
       main='',xlab='',ylab='perceived separation [dva]',
       bty='n', ax=F, asp=1)
  
  title(xlab='frame movement [dva]', adj=0.1)
  
  lines( x=c(0, 5), y=c(0, 5), lty=2, col='#999999', xpd=TRUE)
  
  for (interval in c(1,2,3,4)) {
    
    idf <- df[which(df$interval == interval),]
    
    avg <- c()
    lci <- c()
    hci <- c()
    
    X <- sort( unique(idf$amplitude) )
    
    X <- c(0.8, 1.6, 2.4, 3.2, 4.0)
    
    for ( amplitude in X ) {
      
      idx <- which(idf$amplitude == amplitude)
      
      ip <- idf$percept[idx]
      ip <- ip[which(!is.na(ip))]
      
      if (length(idx) > 0) {
        #avg <- c(avg, mean(idf$percept[idx]))
        avg <- c(avg, mean(ip))
      } else {
        avg <- c(avg, NA)
      }
      if (length(idx) > 1) {
        #ci  <- SMCL::getConfidenceInterval(idf$percept[idx])
        ci  <- Reach::getConfidenceInterval(ip)
        lci <- c(lci, ci[1])
        hci <- c(hci, ci[2])
      } else {
        lci <- c(lci, NA)
        hci <- c(hci, NA)
      }
      
    }
    
    polygon( x = c(X, rev(X)),
             y = c(lci, rev(hci)),
             col = cols.tr[interval],
             border = NA, 
             xpd=TRUE)
    lines(X,avg,col=cols.op[interval], xpd=TRUE)
    
  }
  
  axis(side=1,at=c(0,2,4))
  axis(side=2,at=c(0,2,4))
  
  legend(4.5,
         5,
         legend=c('0-30 min.',
                  '30-60 min.',
                  '60-90 min.',
                  '90-120 min.'),
         lty=1,
         col=cols.op,
         bty='n', 
         xpd=TRUE)
  
  
  
  if (target %in% c('pdf', 'svg', 'png', 'tiff')) {
    dev.off()
  }
  
}