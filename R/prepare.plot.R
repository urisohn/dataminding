#' Start a pre-formatted Base-R plot
#'
#' The x and y axes, and the figure main headers, are  described with 1 line in
#' bold, and 1 in italics below it (optional). If labels for the x-axis tick marks
#' are provided, the x-axis automatically gets padding to fit the text.
#' The ylim argument is automatically set to fit all variables and can be expanded
#' with the `ylim.padding` argument.
#'
#'@param vars list of numeric vectors with all variables that will be plotted.
#'The length of the first variable determines the x-axis size. e.g., if it has 20
#'numbers, the x coordinates will be 1-20
#'@param x1.lab character string, 1st line label for x-axis, shown in bold.
#'@param x2.lab character string, 2nd line label for x-axis, shown in italics.
#'@param y1.lab character string, 1st line label for y-axis, shown in bold.
#'@param y2.lab character string, 2nd line label for y-axis, shown in italics.
#'@param main1.lab character string, 1st line label for figure header, shown in bold.
#'@param main2.lab character string, 2nd line label for figure header, shown in italics.
#'@param ylim.padding numeric vector of length two, providing the percentage
#'padding to be added to the bottom and top of the automatically set ylim.
#'@param longest.label either numeric scalar, or character vector If numeric, it
#'is the length of the longest label to use for a x-axis tickmark. If character
#'vector it is the set of labels to use, and the longest length is computed based
#'on it.
#' @return NULL, for it produces a figure, and does not conduct calculations.
#' @examples
#' \dontrun{
#' x1=rnorm(25)
#' x2=x1+1
#' x3=x1-1
#' prepare.plot(list(x1,x2,x3))
#' points(x1)
#' points(x2)
#'
#' prepare.plot(list(x1,x2,x3),x1.lab='These are the 25 objects')
#' points(x3,type='l')
#'
#' }
#'
#' @export
#'

  prepare.plot <- function (vars=list(),
                         ylim.padding=c(0,0),
                         longest.label = 2,
                         x1.lab='x1.lab',
                         x2.lab='(x2.lab)',
                         y1.lab='y1.lab',
                         y2.lab='(y2.lab)',
                         main1='main1',
                         main2='(main2)')

  {

  #0  validation
    if (!is.list(vars)) stop ('The argument `vars` must be a list')

   #0.5 Preliminaries
    vars.numeric = unlist(vars)
    var1 = vars[[1]]

  #1 Set margins
    #Dynamically set bottom padding based on length of longest label
      bottom.padding = 4.5 + longest.label/6          #about 6 letters per line

      par.left = ifelse(y2.lab!='', 5.5, 4.5)
      par.top  = ifelse(main2!='', 4.25,3.25)
      par(mar=c(bottom.padding , par.left , par.top , 1))   #Set it


  #2 ylim=c()  - set based on observed values
    yL=min(vars.numeric)
    yH=max(vars.numeric)
    yR=yH-yL
    ylim=c(yL - ylim.padding[1]*yR,yH + ylim.padding[2])

  #3 Empty plot
    plot(var1,col='white',xaxt='n',yaxt='n',xlab='',ylab='',ylim=ylim)


  #3 x-axis
    mtext(side=1,line=bottom.padding - 2.25,font=2,cex=1.35,x1.lab)
    mtext(side=1,line=bottom.padding - 1,font=3,cex=1.15,  x2.lab)

  #4 x-axis tick Labels


  #10 y-axis

    #10.1 If using two labels
      if (y2.lab!='') {
        mtext(side=2,line=4,font=2   ,cex=1.35 , y1.lab)  #line 1 bold
        mtext(side=2,line=2.75,font=3,cex=1.15,  y2.lab)  #line 2 italics
      }

    #10.2 If using only 1
      if (y2.lab=='') mtext(side=2,line=2.85,font=2,cex=1.2 , y1.lab)  #line 1 bold


  #11 header
   #11.1 One liner
      if (main2=='')
      {
       mtext(side=3,line=1,font=2,cex=1.6, main1)
      }

   #11.2 Two liner
      if (main2!='')
      {
       mtext(side=3,line=2,font=2,cex=1.6, main1)
       mtext(side=3,line=.75,font=3,cex=1.35, main2)

      }

  } #End function

