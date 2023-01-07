
plot.treated = function (vars,  
                         x1.lab='x-axis', 
                         x2.lab='(description of x-axis)', 
                         y1.lab='y-axis', 
                         y2.lab='(description of y-axis)', 
                         main1='Graph header', 
                         main2=('description of graph header'))
             
  {
    

  #1 Colors
    col.expected='red4'
    col.band=adjustcolor('red4',0.85)
    col.observed='black'
    col.CI = 'gray88'
    cols=c(col.expected, col.band)
  
  
  #2 Short name for data variables
    s           = treated.expected_results
    obs.table   = s$observed$stimuli.effects
    obs         = s$observed$stimuli.effects$diff
    n.obs       = length(obs)
    stimuli.var = s$observed$stimuli.var
    
  #3 Set margins
    #Dynamically set bottom padding based on length of stimuli names

      max.stimuli.label = max(nchar(as.character(obs.table[,1]))) #Longest value of stimuli var
      bottom.padding = 4 + max.stimuli.label/6                    #about 6 letters per line
      par(mar=c(bottom.padding , 5.5 , 3.25 , 1)) #Set it

  #4 ylim=c()  - set based on observed values leaving some space in the bottom for information
    all.values=c(s$cbL , s$cbH, obs)  #all values that can be plotted
    yL=min(all.values)
    yH=max(all.values)
    yR=yH-yL
    ylim=c(yL-.1*yR,yH)
    
    
  #5 Plot expected line
    plot(s$expected,type='l', ylim=ylim, col=col.expected, lwd=2,las=1,xlab='',ylab='',xaxt='n')
    
  #5 Plot band around it
    points(s$cbH,type='l',lty=2,col=col.band)
    points(s$cbL,type='l',lty=2,col=col.band)          
              
  #6 Plot observed values
      order.k=order(obs)
      n.obs=length(obs)
      points(obs[order.k],pch=16,cex=1.5,col=col.observed)
      
      
  #7 Horizontal line
      abline(h=0,lty=4)
      
  #8 Legend
      legend('topleft',inset=.02, col=c(col.observed,col.expected,col.band), lty=c(NA,1,2),
         pch=c(16,NA,NA), cex=.8, pt.cex=1.25,
         legend=c("Observed effects", "All stimuli same true effect: expected", "All stimuli same true effect: 95% CI"))
 
  #9 x-axis
      #9.1 X-axis label
        mtext(side=1,line=bottom.padding - 2.25,font=2,cex=1.2,x1.lab)  #line 1 bold
        mtext(side=1,line=bottom.padding - 1,font=3,cex=1,  x2.lab)  #line 2 italics
   
      #9.2 tick Labels
        #Tilt if name is longer than 3
            srt=ifelse (max.stimuli.label>2, 35, 0)  #srt: parameter for tilted letters
        
      #9.3 Find position 
            plotH = par("usr")[4]  #highest y-value in plot area
            plotL = par("usr")[3]  #lowest  y-value in plot area
            #par('usr') gives the coordinates of the plot area, left, right, bottom, top
    
      #9.4 Print it
          text(x = 1:n.obs +.25, y = plotL - .035*(plotH-plotL),
               labels = obs.table[,1][order.k], xpd = NA, cex = .8,srt=srt, adj=1)
            
   
  #10 y-axis
          
    #10.1 If using two labels
      if (y2.lab!='') {
        mtext(side=2,line=4,font=2,cex=1.2 , y1.lab)  #line 1 bold
        mtext(side=2,line=2.9,font=3,cex=1.1,y2.lab)  #line 2 italics
      } 
        
    #10.2 If using only 1
      if (y2.lab=='') mtext(side=2,line=2.5,font=2,cex=1.2 , y1.lab)  #line 1 bold
      
    
  #11 header
    if (missing(main2)) main2=''
    mtext(side=3,line=1,font=2,cex=1.4, main1)
  