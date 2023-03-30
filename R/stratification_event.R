#' stratification_event
#'
#' @param time numeric
#' @param y numeric
#' @param parms numeric
#' @param inflows list
#'
#' @export
#'
stratification_event <- function(time, y, parms, inflows){
  with(as.list(c(y, parms)), {
    if (time < t_autumn) {
      #cat("summer", time, "\n")
      vol <- vol_E + vol_H
      vol_H <- vol * (1 - frac_epi)
      vol_E <- vol * frac_epi
    } else {
      #cat("autumn", time, "\n")
      vol_E <- vol_E + vol_H
      vol_H <- 0
    }
    c(vol_E, vol_H)
  })
}
