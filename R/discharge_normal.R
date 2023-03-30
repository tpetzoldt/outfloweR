#' Inflow and Outflow Data
#'
#' Example data sets with inflow and outflow of a reservoir (in Mio m3/d)
#' of a normal, a wet and a dry year.
#'
#' @format Data frame with the following columns:
#' \describe{
#'   \item{time}{time in days}
#'   \item{inflow}{water inflow}
#'   \item{outflow}{managed outflow, e.g. for drinking water supply}
#'   \item{outflow_wb}{outflow to the river bed}
#' }
#'
#' @source Lukas Gunzelmann, derived from real measurements
#'
#' @name discharge_normal
#' @docType data
#' @keywords data
#'
#' @examples
#'
#' data("discharge_normal")
#'
#' plot(inflow ~ time, data = discharge_normal, type="l", ylim=c(0, 1.5))
#' lines(outflow ~ time, data = discharge_normal, col="blue")
#' lines(outflow_wb ~ time, data = discharge_normal, col="green")
#'
NULL

