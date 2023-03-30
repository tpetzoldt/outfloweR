#' approx_flows
#'
#' @param time numeric
#' @param in_E numeric
#' @param out_E numeric
#' @param in_H numeric
#' @param out_H numeric
#' @param wb_E numeric
#' @param wb_H numeric
#'
#' @export
#'
approx_flows <- function(time, in_E, out_E, in_H, out_H, wb_E, wb_H) {
  list(
    f_in_E  = approxfun(time, in_E,  rule = 2),
    f_out_E = approxfun(time, out_E, rule = 2),
    f_in_H  = approxfun(time, in_H,  rule = 2),
    f_out_H = approxfun(time, out_H, rule = 2),
    f_wb_E  = approxfun(time, wb_E,  rule = 2),
    f_wb_H  = approxfun(time, wb_H,  rule = 2)
  )
}
