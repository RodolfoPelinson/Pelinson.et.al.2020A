#' @title Difference in distance between centroids
#'
#' @description This function computes distances between centroids of ponds with and without fish for each isolation distance treatment. Then it computes the differences among those distances among isolation treatments and tests it for significance. IMPORTANT: This function will only work for the variables provided in this package, you may need to make changes to it if you want to apply it to a different data set.
#' @param comm The community data. A species by site matrix containing abundance data.
#' @param x1 A vector containing the identity of fish treatments.
#' @param x2 A vector containing the identity of isolation treatments.
#' @param LVs The latent variables or ordination axis to be used to compute centroids. If NULL, GLLVM will be computed. Defaut is NULL.
#' @param test Should the significance of the difference between distances be tested? Defaut is set to TRUE. If FALSE, all arguments related to test will be ignored.
#' @param nperm The number of permutations.
#' @param family Family distribution to fit the GLLVMs.
#' @param num.lv Number of latent variables used to compute GLLVMs.
#' @param strata Should communities be permuted freely or be restricted by fish treatment? Defaut to TRUE.
#' @param show.perm Show permutation steps?
#' @param orig_n.init Number of iterations used to compute the original GLLVM? Defaut to 30.
#' @param perm_n.init Number of iterations used to compute the permuted GLLVMs? Defaut to 1.
#' @param type Either "centroid" or "median". Should centroids or spatial medians be computed?
#' @param method Either "VA" or "LA". The method used to estimate latent variables. See function gllvm from package gllvm
#' @param refit_perm If TRUE, the rows of the original community matrix will be permuted and for every permutation a gllvm will be computed. It can take too much time. If false, only the rows of a matrix with the original latent variables are permuted.


#' @export

#'

Dif_dist <- function(com,x1,x2,LVs = NULL, test = TRUE, nperm, family, num.lv = 2, strata = T, show.perm = T, orig_n.init = 30,
                     perm_n.init = 1, type = "centroid", method = "VA", refit_perm = F){

  if(!is.null(LVs)){
    message("method and arguments related to it were ignored")
    if(isTRUE(refit_perm)){
      refit_perm <- F
      com <- NULL
      method <- NULL
      message("refit_perm was set to FALSE because LVs was suplied")
    }
  }
  else{
      message(paste ("Starting",  orig_n.init, "initial runs of the original GLLVM fit", sep = " "))
      fit_gllvm_original<- gllvm(com, family = family, method = method, row.eff = F,n.init = orig_n.init, seed = 1, num.lv = num.lv)
      message("Original GLLVM fit finished")
      LVs <- fit_gllvm_original$lvs
  }

  centroid_fish_30 <- c(rep(NA, dim(LVs)[2]))
  centroid_fishless_30 <- c(rep(NA, dim(LVs)[2]))
  centroid_fish_120 <- c(rep(NA, dim(LVs)[2]))
  centroid_fishless_120 <- c(rep(NA, dim(LVs)[2]))
  centroid_fish_480 <- c(rep(NA, dim(LVs)[2]))
  centroid_fishless_480 <- c(rep(NA, dim(LVs)[2]))

  for (i in 1:dim(LVs)[2]){
    if(type == "centroid"){
      centroid_fish_30[i] <- mean(LVs[which(x1=="present"&x2=="30"),i])
      centroid_fishless_30[i] <- mean(LVs[which(x1=="absent"&x2=="30"),i])

      centroid_fish_120[i] <- mean(LVs[which(x1=="present"&x2=="120"),i])
      centroid_fishless_120[i] <- mean(LVs[which(x1=="absent"&x2=="120"),i])

      centroid_fish_480[i] <- mean(LVs[which(x1=="present"&x2=="480"),i])
      centroid_fishless_480[i] <- mean(LVs[which(x1=="absent"&x2=="480"),i])
    }

    if(type == "median"){
      centroid_fish_30[i] <- median(LVs[which(x1=="present"&x2=="30"),i])
      centroid_fishless_30[i] <- median(LVs[which(x1=="absent"&x2=="30"),i])

      centroid_fish_120[i] <- median(LVs[which(x1=="present"&x2=="120"),i])
      centroid_fishless_120[i] <- median(LVs[which(x1=="absent"&x2=="120"),i])

      centroid_fish_480[i] <- median(LVs[which(x1=="present"&x2=="480"),i])
      centroid_fishless_480[i] <- median(LVs[which(x1=="absent"&x2=="480"),i])
    }
  }


  centroids_30 <- rbind(centroid_fish_30,centroid_fishless_30)
  centroids_120 <- rbind(centroid_fish_120,centroid_fishless_120)
  centroids_480 <- rbind(centroid_fish_480,centroid_fishless_480)

  dist_30 <- c(dist(centroids_30))
  dist_120 <- c(dist(centroids_120))
  dist_480 <- c(dist(centroids_480))

  distances_original <- data.frame(dist_30,dist_120,dist_480)

  dif_480_30 <- dist_480 - dist_30
  dif_480_120 <- dist_480 - dist_120
  dif_120_30 <- dist_120 - dist_30

  diferences_original <- data.frame(dif_120_30, dif_480_120, dif_480_30)

  if(test == TRUE){
    if (isTRUE(strata)){
      control <- how(within = Within(type = 'free'),plots = Plots(strata = x1, type = 'free'), nperm = nperm)
    } else {control <- how(nperm = nperm)}

    permutations <- shuffleSet(nrow(LVs), control = control)

    diferences_loop <- data.frame(dif_480_30 = rep(NA, nrow(permutations)),
                                  dif_480_120 = rep(NA, nrow(permutations)),
                                  dif_120_30 = rep(NA, nrow(permutations)))


    message("Starting Permutations")
    for(i in 1:nrow(permutations)){
      if(isFALSE(refit_perm)){LVs_new<-LVs[permutations[i,],]}
      else{
        com_new <- com[permutations[i,],]
        tryCatch({
          fit_gllvm_new<- gllvm(com_new,family = family, method = method, row.eff = F,n.init = perm_n.init, seed = 1, num.lv = num.lv)
        }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
        LVs_new <- fit_gllvm_new$lvs
      }

      centroid_fish_30 <- c(rep(NA, dim(LVs_new)[2]))
      centroid_fishless_30 <- c(rep(NA, dim(LVs_new)[2]))
      centroid_fish_120 <- c(rep(NA, dim(LVs_new)[2]))
      centroid_fishless_120 <- c(rep(NA, dim(LVs_new)[2]))
      centroid_fish_480 <- c(rep(NA, dim(LVs_new)[2]))
      centroid_fishless_480 <- c(rep(NA, dim(LVs_new)[2]))

      for (j in 1:dim(LVs_new)[2]){
        if(type == "centroid"){
          centroid_fish_30[j] <- mean(LVs_new[which(x1=="present"&x2=="30"),j])
          centroid_fishless_30[j] <- mean(LVs_new[which(x1=="absent"&x2=="30"),j])

          centroid_fish_120[j] <- mean(LVs_new[which(x1=="present"&x2=="120"),j])
          centroid_fishless_120[j] <- mean(LVs_new[which(x1=="absent"&x2=="120"),j])

          centroid_fish_480[j] <- mean(LVs_new[which(x1=="present"&x2=="480"),j])
          centroid_fishless_480[j] <- mean(LVs_new[which(x1=="absent"&x2=="480"),j])
        }

        if(type == "median"){
          centroid_fish_30[j] <- median(LVs_new[which(x1=="present"&x2=="30"),j])
          centroid_fishless_30[j] <- median(LVs_new[which(x1=="absent"&x2=="30"),j])

          centroid_fish_120[j] <- median(LVs_new[which(x1=="present"&x2=="120"),j])
          centroid_fishless_120[j] <- median(LVs_new[which(x1=="absent"&x2=="120"),j])

          centroid_fish_480[j] <- median(LVs_new[which(x1=="present"&x2=="480"),j])
          centroid_fishless_480[j] <- median(LVs_new[which(x1=="absent"&x2=="480"),j])
        }
      }



      centroids_30 <- rbind(centroid_fish_30,centroid_fishless_30)
      centroids_120 <- rbind(centroid_fish_120,centroid_fishless_120)
      centroids_480 <- rbind(centroid_fish_480,centroid_fishless_480)

      dist_30 <- c(dist(centroids_30))
      dist_120 <- c(dist(centroids_120))
      dist_480 <- c(dist(centroids_480))

      dif_480_30 <- dist_480 - dist_30
      dif_480_120 <- dist_480 - dist_120
      dif_120_30 <- dist_120 - dist_30

      diferences <- c(dif_120_30, dif_480_120, dif_480_30)

      diferences_loop[i,] <- diferences



      if(isTRUE(show.perm)){
        if (nperm > 100) {
          if (i %% 100 == 0) {message(paste("Permutation #",i, " finished",sep = ""))}
        } else { if (i %% 10 == 0) message(paste("Permutation #",i, " finished", sep = ""))}}
    }



    diferences_loop <- na.omit(diferences_loop)

    nperm <- dim(diferences_loop)[1]

    p_480_30_greater <- length(diferences_loop[,1][diferences_loop[,1] >= diferences_original$dif_480_30])/nperm
    p_480_120_greater <- length(diferences_loop[,2][diferences_loop[,2] >= diferences_original$dif_480_120])/nperm
    p_120_30_greater <- length(diferences_loop[,3][diferences_loop[,3] >= diferences_original$dif_120_30])/nperm


    p_480_30_lower <- length(diferences_loop[,1][diferences_loop[,1] <= diferences_original$dif_480_30])/nperm
    p_480_120_lower <- length(diferences_loop[,2][diferences_loop[,2] <= diferences_original$dif_480_120])/nperm
    p_120_30_lower <- length(diferences_loop[,3][diferences_loop[,3] <= diferences_original$dif_120_30])/nperm


    p_480_30_two <- length(abs(diferences_loop[,1])[abs(diferences_loop[,1]) >= abs(diferences_original$dif_480_30)])/nperm
    p_480_120_two <- length(abs(diferences_loop[,2])[abs(diferences_loop[,2]) >= abs(diferences_original$dif_480_120)])/nperm
    p_120_30_two <- length(abs(diferences_loop[,3])[abs(diferences_loop[,3]) >= abs(diferences_original$dif_120_30)])/nperm


    p_values_greater <- c(p_120_30_greater, p_480_120_greater, p_480_30_greater)
    p_ajusted_fdr_greater <- p.adjust(p_values_greater, method = "fdr")

    p_values_lower <- c(p_120_30_lower, p_480_120_lower, p_480_30_lower)
    p_ajusted_fdr_lower <- p.adjust(p_values_lower, method = "fdr")

    p_values_two <- c(p_120_30_two, p_480_120_two, p_480_30_two)
    p_ajusted_fdr_two <- p.adjust(p_values_two, method = "fdr")

    p_values <- rbind(p_values_greater, p_ajusted_fdr_greater, p_values_lower, p_ajusted_fdr_lower, p_values_two, p_ajusted_fdr_two)
    colnames(p_values) <- c("30 -> 120", "120 -> 480","30 -> 480")
    rownames(p_values) <- c("Greater", "Greater (Adjusted - FDR)", "Lower", "Lower (Adjusted - FDR)", "Two sided", "Two sided (Adjusted - FDR)")
    names(distances_original) <- c("30m", "120m","480m")
    names(diferences_original) <- c("30 -> 120", "120 -> 480","30 -> 480")

    nperm <- c("Number of Sucessful Permutations" = nperm)
    output <- list(distances = distances_original, diferences = diferences_original, p.values = p_values, nperm=nperm)
  }else{
    names(distances_original) <- c("30m", "120m","480m")
    names(diferences_original) <- c("30 -> 120", "120 -> 480","30 -> 480")
    output <- list(distances = distances_original, diferences = diferences_original)
  }

  message("FINISHED")
  return(output)
}
