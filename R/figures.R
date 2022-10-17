

basicPlot <- function(target='none') {
  
  
  if (target=='svg') {
    svglite::svglite(file='doc/all_data.svg', width=9, height=9, fix_text_size = FALSE)
  }
  if (target=='pdf') {
    cairo_pdf(filename='doc/all_data.pdf', width=30, height=30)
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
        srt = 45, 
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
       xlim=c(-5,13), ylim=c(0,6),
       main='Vertical Offset',xlab='vertical offset [dva]',ylab='illusion strength [dva]',
       bty='n', ax=F)
  
  for (stimtype in c('classicframe','apparentframe')) {
    
    col.idx <- c('classicframe'=1,'apparentframe'=5)[stimtype]
    
    
    
  }
  
  # plot 5: pre/post diction
  
  # plot 6: experiment time
  
  # plot 7: self-moved frames
  
  # plot 8: perceived motion
  
  # plot 9: texture motion
  
  
  if (target %in% c('svg','pdf','png','tiff')) {
    dev.off()
  }
  
}