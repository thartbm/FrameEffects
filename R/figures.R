

basicPlot <- function(target='none') {

  width <- 9.291338583
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
    ci  <- SMCL::getConfidenceInterval(data=cdf$percept)
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
        labels = conditions,
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
      ci  <- SMCL::getConfidenceInterval(fdf$percept[which(fdf$hor_offset == hos)])
      lci <- c(lci,ci[1])
      hci <- c(hci,ci[2])

    }

    col.idx <- ((fsi-1)*2)+1

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
      ci  <- SMCL::getConfidenceInterval(fdf$percept[which(fdf$ver_offset == vos)])
      lci <- c(lci,ci[1])
      hci <- c(hci,ci[2])

    }

    col.idx <- ((fsi-1)*2)+1

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
      ci  <- SMCL::getConfidenceInterval(cdf$percept[idx])
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
        ci <- SMCL::getConfidenceInterval(sdf$percept[idx])
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

  for (interval in c(1,2,3,4,5)) {

    idf <- df[which(df$interval == interval),]

    avg <- c()
    lci <- c()
    hci <- c()

    X <- sort( unique(idf$amplitude) )

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
        ci  <- SMCL::getConfidenceInterval(ip)
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
         legend=c('~10 min.',
                  '~30 min.',
                  '~50 min.',
                  '~70 min.',
                  '~90 min.'),
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
    ci  <- SMCL::getConfidenceInterval(percepts)

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
      ci  <- SMCL::getConfidenceInterval(sdf$percept[idx])
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
    ci  <- SMCL::getConfidenceInterval(percepts)

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
    ci  <- SMCL::getConfidenceInterval(percepts)

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
