#' volume_model
#'
#' @param time numeric
#' @param y numeric
#' @param parms numeric
#' @param inflows list
#' @param ... not used
#'
#' @export
#'
volume_model <- function(time, y, parms, inflows, ...){
  with(as.list(c(y, parms)), {

    d_vol_E <- inflows$f_in_E(time) - inflows$f_out_E(time) - inflows$f_wb_E(time)
    d_vol_H <- inflows$f_in_H(time) - inflows$f_out_H(time) - inflows$f_wb_H(time)

    ## add hypolimnetic mass balance change to epilimnion if no hypo exists
    if (vol_H < 0.0001) {
      d_vol_E <- d_vol_E + d_vol_H
      d_vol_H <- 0
    }

    list(c(d_vol_E, d_vol_H), gesamt=vol_E + vol_H)
  })

}
