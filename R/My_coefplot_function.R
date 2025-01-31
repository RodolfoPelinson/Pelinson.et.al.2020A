#' @title Plotting coefficients
#'
#' @description This function is to plot the maximum likelihood estimates and their respective confidence intervals.
#' @param mles A vector containing the maximum likelihood estimates to be plotted.
#' @param upper A vector containing the upper limits of the confidence intervals.
#' @param lower A vector containing the lower limits of the confidence intervals.
#' @param species_labels A vector containing the labels of each species.
#' @param xlab A label for the x axis.
#' @param cex.axis The size of the x axis.
#' @param y_spa Space to be added to the minimum and maximum values. This is to improve visualization.
#' @param rect Should assign different backgrounds to predators and non-predators? Default is to FALSE.
#' @param rect_lim Limit of the background.
#' @param ... Other graphical parameters.

#' @export

#'



My_coefplot <- function (mles, upper, lower, species_labels = NULL, xlab = NULL, cex.axis = 1,y_spa = 0, rect = F,rect_lim = 5,...)
{

  col.seq <- rep("grey", length(mles))
  col.seq[which(lower < 0 & upper < 0)] <- "red"
  col.seq[which(lower > 0 & upper > 0)] <- "blue"

  col.seq[which(lower < 0 & upper < 0 & mles < -10)] <- "indianred1"
  col.seq[which(lower > 0 & upper > 0 & mles > 10)] <- "steelblue1"


  lwd.seq <- rep(1, length(mles))
  lwd.seq[which(lower < 0 & upper < 0)] <- 2
  lwd.seq[which(lower > 0 & upper > 0)] <- 2

  lwd.seq[which(mles < -10 | mles > 10)] <- 1

  cex.seq <- rep(1, length(mles))
  cex.seq[which(lower < 0 & upper < 0)] <- 1.4
  cex.seq[which(lower > 0 & upper > 0)] <- 1.4

  cex.seq[which(mles < -10 | mles > 10)] <- 1


  At.y <- rev(1:length(mles))
  ylim <- c(min(At.y-y_spa), max(At.y+y_spa))

  plot(x = NULL, y = NULL, yaxt = "n", ylim = ylim,
       ylab = "", xlab = xlab, xlim = c(min(lower), max(upper)), ...)

  if(isTRUE(rect)){
    rect(xleft = min(lower)-1, ybottom = 0, xright =  max(upper)+1, ytop = max(At.y)-rect_lim+0.5, density = NULL, border = "transparent", col = rgb(col2rgb("forestgreen", alpha = FALSE)[1],
                                                                                                                                                     col2rgb("forestgreen", alpha = FALSE)[2],
                                                                                                                                                     col2rgb("forestgreen", alpha = FALSE)[3],
                                                                                                                                                     alpha = 40, maxColorValue = 255))
    rect(xleft = min(lower)-1, ybottom = max(At.y)-rect_lim+0.5, xright =  max(upper)+1, ytop = max(At.y)+1, density = NULL, border = "transparent", col = rgb(col2rgb("gold3", alpha = FALSE)[1],
                                                                                                                                                               col2rgb("gold3", alpha = FALSE)[2],
                                                                                                                                                               col2rgb("gold3", alpha = FALSE)[3],
                                                                                                                                                               alpha = 40, maxColorValue = 255))
  }

  points(x = mles, y = At.y, col = col.seq, pch = 16, cex = cex.seq)
  #segments(x0 = lower,
  #         x1 = upper,
  #         y1 = At.y, y0 = At.y, col = col.seq, lwd =lwd.seq)


  arrows(y1 = At.y, y0 = At.y, x1 = upper, x0 = lower,
         code = 3, angle = 90, length = 0.025,col = col.seq, lwd =lwd.seq)


  abline(v = 0, lty = 3)
  axis(2, at = At.y, labels = species_labels, las = 1, cex.axis = cex.axis)
}
