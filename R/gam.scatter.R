#' GAM version of smooth.scatter()
#'
#' Scatter plot between x and y, with best fitting GAM curve.
#'
#' @param x,y numeric vectors to be included in the scatterplot
#' @param scatter, logical, if TRUE plots the scatterplot, if FALSE only the GAM line
#' @param scatter, logical, if TRUE plots the scatterplot, if FALSE only the GAM line
#' @return invisible output of GAM model and plotted lines 
#' @examples
#' \dontrun{
#' x = rnorm(500)
#' y = x^2+rnorm(500)
#' gam.scatter(x,y) 
#' }

gam.scatter<-function(x,y,scatter=FALSE,quant.lines=NULL)
    {
   
   
    df=data.frame(x,y)
    df=df[complete.cases(df),]
    
    #Keep observations within the range
      #df=df[quantile(df$x)>range[0] & quantile(df$x)<range[1],]
      x=df$x
      y=df$y
      g1<- mgcv::gam(y~s(x),method='REML')
      xs=quantile(x,1:99/100)
      yh=predict(g1, newdata=data.frame(x=xs))
      if (is.null(names(x))) names(x)='x'
      if (is.null(names(y))) names(y)='y'
      
      ymax=max(yh)
      ymin=min(yh)
      yr = ymax-ymin
      
      
      if (scatter==FALSE) 
        {
        plot(xs,yh,xlab=names(x),ylab=names(y),type='b',main='GAM Scatter',
            ylim=c(ymin-.04*yr, ymax+.04*yr),las=1)
        }
      
      if (scatter==TRUE) 
        {
        plot(x,y,xlab=names(x),ylab=names(y),pch=16,col=adjustcolor('gray',.5),main='GAM plot',las=1)
        points(xs,yh,type='l',lwd=2)
      }
      
      if (!is.null(quant.lines))
      {
        abline(v=quant.lines[1],col='red4',lty=2)
        abline(v=quant.lines[2],col='red4',lty=2)
        
      }
      
     
      invisible(list(gam.line = data.frame(xs,yh), gam.model=g1))
 }
  
