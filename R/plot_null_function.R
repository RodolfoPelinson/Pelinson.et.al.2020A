#' @title Plotting null ordinations
#'
#' @description This function is just for visualization of null generated communities. It is useful to see if your permutation design is doing what it is suppose to! It relies on the plot_ordination function.

#' @param comm The community data. A species by site matrix containing abundance data.
#' @param x1 A vector containing the identity of fish treatments.
#' @param x2 A vector containing the identity of isolation treatments.
#' @param family Family distribution to fit the GLLVMs.
#' @param nperm The number of permutations.
#' @param num.lv Number of latent variables used to compute GLLVMs.
#' @param strata Should communities be permuted freely or be restricted by fish treatment? Default to TRUE.
#' @param orig_n.init Number of iterations used to compute the original GLLVM? Default to 30.
#' @param perm_n.init Number of iterations used to compute the permuted GLLVMs? Default to 1.
#' @param refit_perm If TRUE, the rows of the original community matrix will be permuted and for every permutation a gllvm will be computed. It can take too much time. If false, only the rows of a matrix with the original latent variables are permuted.

#' @export

#'


plot_null <- function (com,x1,x2,nperm = 1, family, num.lv = 2, strata = F, orig_n.init = 1, perm_n.init = 1,  refit_perm = F,...){

  show(paste ("Starting",  orig_n.init, "initial runs of the original GLLVM fit", sep = " "))
  fit_gllvm_original<- gllvm(com,family = family, method = "VA", row.eff = F, n.init = orig_n.init, seed = 1, num.lv = num.lv)
  show("Original GLLVM fit finished")

  Lvs_original <- fit_gllvm_original$lvs

  plot_ordination(LVs = Lvs_original,
                  x1 = x1, x2 = x2, main = paste("Sampling survey 3 - ORIGINAL"), ...)

  if (isTRUE(strata)){
    control <- how(within = Within(type = 'free'),plots = Plots(strata = x1, type = 'free'), nperm = nperm)
  } else {control <- how(nperm = nperm)}

  permutations <- shuffleSet(nrow(com), control = control)

  if (isFALSE(refit_perm)){
    for(i in 1:nrow(permutations)){
      new_LVs <- Lvs_original[permutations[i,],]

      plot_ordination(LVs = new_LVs,
                      x1 = x1, x2 = x2, main = paste("Sampling survey 3 - NULL #", i), ...)
    }
  }
  else{
    for(i in 1:nrow(permutations)){
      com_new <- com[permutations[i,],]
      tryCatch({

        fit_gllvm_new<- gllvm(com_new,family = family, method = "VA", row.eff = F, n.init = perm_n.init, seed = 1, num.lv = num.lv)

        plot_ordination(LVs = fit_gllvm_new$lvs,
                        x1 = x1, x2 = x2, main = paste("Sampling survey 3 - NULL #", i), ...)

      }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    }
  }



}
