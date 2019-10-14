fPlotSAL <- function(plotData, myModel, myParam, bdate, edate){
  if (is.null(plotData)) stop("No data found.")

#  stop(paste(myModel, myParam, bdate, edate))
  medianS <- sprintf("S median = %.04f ", median(plotData$S, na.rm=TRUE))
  medianA <- sprintf("A median = %.04f ", median(plotData$A, na.rm=TRUE))
  medianL <- sprintf("L median = %.04f ", median(plotData$L, na.rm=TRUE))

  meanS <- sprintf("S mean = %.04f ", mean(plotData$S, na.rm=TRUE))
  meanA <- sprintf("A mean = %.04f ", mean(plotData$A, na.rm=TRUE))
  meanL <- sprintf("L mean = %.04f ", mean(plotData$L, na.rm=TRUE))

  tfsize <- 3
  tfam <- "mono" 
  plot.title <- sprintf("SAL %s\n%s - %s\n%s",
                        myModel, format(bdate,"%Y%m%d"),format(edate,"%Y%m%d"),myParam)
  plotSAL <- ggplot(plotData,aes(x = S, y = A, colour = plotData$L))
  plotSAL <- plotSAL + geom_point(size=5) + 
                     xlim(-2,2) + ylim(-2,2) + 
                     labs(y = "A", x = "S", colour="L", title=plot.title) +
                     scale_colour_gradient2(low = "darkblue", mid = "yellow", high = "red",
                                           limits=c(0, 2), midpoint = 1) +
                     geom_text(aes(-1.2,2,label = medianS, family=tfam), size=tfsize*1.5, colour="black") + 
                     geom_text(aes(-1.2,1.8,label = medianA, family=tfam), size=tfsize*1.5, colour="black") + 
                     geom_text(aes(-1.2,1.6,label = medianL, family=tfam), size=tfsize*1.5, colour="black") + 
                     geom_text(aes(1.2,-1.6,label = meanS, family=tfam), size=tfsize*1.5, colour="black") +  
                     geom_text(aes(1.2,-1.8,label = meanA, family=tfam), size=tfsize*1.5, colour="black") +  
                     geom_text(aes(1.2,-2,label = meanL, family=tfam), size=tfsize*1.5, colour="black")

  plotSAL
}


fPlotFuzzy <- function(plotData, myModel, myParam, myScore, myStyle, bdate, edate){
  if (is.null(plotData) || dim(plotData)[1] == 0) stop("No data found.")
  # Function to plot fuzzy verification scores
  # plotData <- collect(datasub)
  plotData$fss[which(plotData$fss < 0)] <- 0
  # fcdate <- unique(plotData$fcdate)
  # to show what data is to be plot
  # str(plotData)
  
  ### Code to calculate mean of every threshold/scale pair 
  thresh <- unique(plotData$threshold)
  scale <- unique(plotData$scale)
#  stop(names(plotData))
#  stop("oops", thresh, " and ", scale)
  data <- array(plotData[[myScore]], 
                dim=c(length(thresh),length(scale), 
                      length(plotData[[myScore]])/((length(thresh)*length(scale)))))
  data.mean <- data.frame(as.vector(apply(data, c(1,2), mean, na.rm=T) ))
  colnames(data.mean) <- myScore
  data.mean$scale <- as.vector(t(matrix(rep(scale,length(thresh)),
                                        c(length(scale),length(thresh)))))
  data.mean$threshold <- as.vector(matrix(rep(thresh,length(scale)),
                                          c(length(scale),length(thresh))))
  colnames(data.mean) <- c(myScore,"scale","threshold")

  plotData <- data.mean
  #############################################################
  
  plot.title <- sprintf(paste0(myScore, " %s\n%s - %s\n%s"),
                        myModel, format(bdate,"%Y%m%d"),format(edate,"%Y%m%d"),myParam)
  
  p <- ggplot(plotData, aes(x=as.factor(threshold),y=as.factor(scale), 
                             fill = get(myScore), 
                             label = sprintf("%1.2f", get(myScore)))) + 
    # scale_y_discrete(expand=c(0,0)) +
    geom_tile() + geom_text(colour = "black", size = 6) +
    scale_fill_gradient2(low = "red", mid="yellow", high = "darkgreen", 
                         limits=c(0, 1), midpoint=0.5, name = myScore) +
    # theme(axis.title.x = element_blank()) +   # Remove x-axis label
    ggtitle(plot.title) +
    theme(axis.title.x = element_text(size=14), axis.text.x = element_text(size=14), 
          axis.title.y = element_text(size=14), axis.text.y = element_text(size=14)) +
    ylab("spatial scale")  + xlab("threshold") +
    theme_bw(base_size=14)
    
  # guides(fill = guide_legend(title.theme = element_text(size=15, angle=0),
  #                            label.theme = element_text(size=15, angle=0))) # ,
  #   keywidth = 2, keyheight = 3))
  # scale_x_discrete(expand=c(0,0))
  p
} 

