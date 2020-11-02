#' @title Plotting ordinations
#'
#' @description This function is to plot the ordinations from the GLLVMs.
#' @param model_gllvm The resultant object from function gllvm.
#' @param LVs The latent variables to be plotted. If provided, the model_gllvm argument will be disregarded.
#' @param species_loadings A vector containing the species ladings.
#' @param xlim A vector of length = 2 with upper and lower limits of the x axis
#' @param ylimy A vector of length = 2 with upper and lower limits of the x axis
#' @param x1 First grouping variable
#' @param x2 Second grouping variable
#' @param main Main Title
#' @param species Should species be plotted?
#' @param species_col Species colors. Either a color or a vector of colors with length equal to the number of species.
#' @param species_size Species sizes Either a color or a vector of colors with length equal to the number of sites.
#' @param sites_size Sites sizes. Either a number or a vector of numbers with length equal to the number of sites.
#' @param species_names Species names to be plotted. A vector of the names of the species.
#' @param sites Should sites be plotted?
#' @param elipse Should ellipses around treatments be plotted?
#' @param site_colors Sites colors. Either a color or a vector of colors with length equal to the number of sites.
#' @param pch Tipe of dots to plotted for sites.
#' @param legend_labels Labels for the legend.
#' @param name_species_size A cex value for species names. Either a number or a vector of numbers.
#' @param legend_x x coordinates to plot the legend
#' @param legend_y x coordinates to plot the legend
#' @param legend Should legend be plotted? Default to F.
#' @param legend_horiz Should legend be horizontal?
#' @param legend_ncol Number of columns in the legend.
#' @param ylab Label of y axis.
#' @param xlab Label of x axis.
#' @param alpha.elipse Transparency for the ellipses.
#' @param sep Should plots be separated by isolation distance to improve visualization?
#' @param plot_centroid_dist Should distances between centroids within distances be plotted?
#'
#'
#' @export

#'


plot_ordination <- function (model_gllvm = NULL,LVs = NULL, species_loadings = NULL,xlim = NULL, ylim= NULL, x1, x2=NULL, main = NULL, species = F,
                             species_col = "black", name_species_col = "white", species_size = 4, sites_size = 1, species_names = NULL,
                             sites = T, elipse = F, site_colors = NULL, pch = NULL, legend_labels = NULL, name_species_size = 0.8,
                             legend_x = NULL, legend_y = NULL, legend = F, legend_horiz = F,  legend_ncol = 1,
                             ylab="Latent Variable 2", xlab = "Latent Variable 1", alpha.elipse = 75, sep = F, plot_centroid_dist = FALSE){

  if(is.null(model_gllvm)){
    LVs <- LVs
    species_LV <- species_loadings
  }else{
    LVs <- model_gllvm$lvs
    species_LV <- model_gllvm$params$theta
  }


  if(is.null(xlim)){xlim = c(min(c(LVs[,1],species_LV[,1])-0.35), max(c(LVs[,1],species_LV[,1])+0.35))}
  if(is.null(ylim)){
    ylim = c(min(c(LVs[,2],species_LV[,2]))-0.35, max(c(LVs[,2], species_LV[,2]))+0.35)
    if(isTRUE(legend)){ylim[2] <- ylim[2]+1}
  }


  if(is.null(legend_x)){legend_x = xlim[1]-0.1}
  if(is.null(legend_y)){legend_y = ylim[2]+0.2}


  levels_x1 <- levels(x1)
  levels_x2 <- levels(x2)

  #atribute values to colors according to the number of levels in x1 and x2 (REVIEW THIS)
  if(is.null(site_colors)){
    if(is.null(x2)){
      site_colors <- seq(from = 1, to =length(x1), by = 1)
    } else {site_colors <- seq(from = 1, to = (length(levels_x1)*length(levels_x2)), by = 1)
    site_colorsI <- matrix(site_colors, nrow = length(levels_x2), ncol = length(levels_x1), byrow = T)
    }}
  else {
    if(is.null(x2) == FALSE){
      site_colorsI <- matrix(site_colors, nrow = length(levels_x2), ncol = length(levels_x1), byrow = T)
    }}



  #atribute values to pch according to the number of levels in x2
  if(is.null(x2)==FALSE){
    if(is.null(pch)){pch <- seq(from = 15, to = (15+length(levels_x2)-1), by = 1)}
  }else{pch <- seq(from = 15, to = (15+length(levels_x1)-1), by = 1)}


  ##Separating plots by x2
  if (isTRUE(sep) & !is.null(x2)){
    for(p in 1:length(levels_x2)){
      plot(c(-500), xlim = xlim, ylim = ylim, frame=F, axes=F,cex.lab=1,ylab=ylab, xlab = xlab, main=main[p])
      box(lty=1, lwd=3)
      axis(1,lwd=3,cex.axis=1)
      axis(2,lwd=3,cex.axis=1)


      #Should elipse be ploted?
      if(isTRUE(elipse)){
        ordiellipse(LVs[which(x2==levels_x2[p]),], groups=x1[which(x2==levels_x2[p])], display="sites",
                    kind = c("ehull"),
                    draw = c("polygon"),col=site_colorsI[i,], alpha = alpha.elipse,
                    label = F, border = "transparent", lty = NULL, lwd=NULL)
      }

      #Should sites be ploted?
      if(isTRUE(sites)){
        for(i in 1:length(levels_x1)){
          points(LVs[which(x1==levels_x1[i]&x2==levels_x2[p]),] ,pch= pch[p], col = site_colorsI[p,i], cex=sites_size)
        }
      }

      #ploting legend
      if (isTRUE(legend)){
        if(p == 1){
          legend(legend_x, legend_y, legend=legend_labels,
                 col=c(site_colorsI),
                 pch=pch, cex=sites_size, bty = "n", horiz = legend_horiz, ncol =  legend_ncol)
        }
      }


    }
  }
  if (isTRUE(sep) & is.null(x2)){
    message("x2 is NULL therefore sep was automatically changed to FALSE")
    sep <- FALSE
  }

  if (isFALSE(sep)){


    #Ploting the basis
    plot(c(-500), xlim = xlim, ylim = ylim, frame=F, axes=F,cex.lab=1,ylab=ylab, xlab = xlab, main=main)
    box(lty=1, lwd=3)
    axis(1,lwd=3,cex.axis=1)
    axis(2,lwd=3,cex.axis=1)


    ### IF WE HAVE ONLY X1

    if(is.null(x2)){
      #Should elipse be ploted?
      if(isTRUE(elipse)){ordiellipse(LVs, groups=x1, display="sites",
                                     kind = c("ehull"),
                                     draw = c("polygon"),col=site_colors, alpha = alpha.elipse,
                                     label = F, border = "transparent", lty = NULL, lwd=NULL)}

      #Should sites be ploted?
      if(isTRUE(sites)){

        for(i in 1:length(levels_x1)){
          points(LVs[which(x1==levels_x1[i]),] ,pch= pch[i], col = site_colors[i], cex=sites_size)
        }
      }

      #ploting legend
      if (isTRUE(legend)){
        legend(legend_x, legend_y, legend=legend_labels,
               col=site_colors,
               pch=pch, cex=sites_size, bty = "n", horiz = legend_horiz, ncol = legend_ncol)
      }
    }

    ### IF WE HAVE X1 AND X2

    else{
      #Should elipse be ploted?
      if(isTRUE(elipse)){
        for(i in 1:length(levels_x2)){
          ordiellipse(LVs[which(x2==levels_x2[i]),], groups=x1[which(x2==levels_x2[i])], display="sites",
                      kind = c("ehull"),
                      draw = c("polygon"),col=site_colorsI[i,], alpha = alpha.elipse,
                      label = F, border = "transparent", lty = NULL, lwd=NULL)
        }
      }

      #Should sites be ploted?
      if(isTRUE(sites)){
        for(i in 1:length(levels_x1)){
          for(j in 1:length(levels_x2)){
            points(LVs[which(x1==levels_x1[i]&x2==levels_x2[j]),] ,pch= pch[j], col = site_colorsI[j,i], cex=sites_size)
          }
        }
      }

      #ploting legend
      if (isTRUE(legend)){
        legend(legend_x, legend_y, legend=legend_labels,
               col=c(site_colorsI),
               pch=pch, cex=sites_size, bty = "n", horiz = legend_horiz, ncol =  legend_ncol)
      }

    }
    #Should species be ploted?
    if (isTRUE(species)){
      if(length(species_size)==1){
        species_size <- rep(species_size, dim(species_LV)[1])
        name_species_size <- rep(name_species_size, dim(species_LV)[1])
      }
      for(i in 1:dim(species_LV)[1]){
        points(species_LV[i,1],species_LV[i,2], bg = species_col[i], col = "black",cex = species_size[i], pch = 21)
        text(species_LV[i,1],species_LV[i,2], labels = species_names[i], col = name_species_col, cex = name_species_size[i])
      }
    }

    #Should distances between centroids be ploted?
    if(isTRUE(plot_centroid_dist)){
      centroidLV1 <- tapply(LVs[,1], list(x1,x2), mean)
      centroidLV2 <- tapply(LVs[,2], list(x1,x2), mean)
      arrows(x0 = centroidLV1[1,], x1 = centroidLV1[2,],
             y0 = centroidLV2[1,], y1 = centroidLV2[2,],
             code = 3, angle = 90, length = 0.05, c("black"), lwd = 2)

    }
  }
}
