#' Run scenarions of a reservoir mass balance model
#'
#' @param option character, specifying the water inflow and water abstraction regime
#' @param vol numeric value, volume of the reservoir (in Mio m3)
#' @param t_strat numeric value, day of stratification onset
#' @param t_autumn numeri value,  end of stratification
#' @param inflow numeric vector, inflow in Mio m3/d
#' @param outflow numeric vector, outflow in Mio m3/d
#' @param outflow_wb numeric vector, bottom outlet in Mio m3/d
#'
#' @export
#'
scenario <- function(option = c("standard", "cold_inflow", "wb_top"),
                     vol, t_strat, t_autumn, inflow, outflow, outflow_wb){

  zero <- numeric(length(inflow))
  times <- seq_along(inflow)

  if (option == "standard") {
    #                                in_E, out_E, in_H,   out_H, wb_E, wb_H
    inflows <- approx_flows(times, inflow,  zero, zero, outflow, zero, outflow_wb)
  } else if (option == "cold_inflow") {
    inflows <- approx_flows(times, zero,   zero, inflow, outflow, zero, outflow_wb)
  } else if (option == "wb_top") {
    inflows <- approx_flows(times, inflow,  zero, zero, outflow, outflow_wb, zero)
  } else {
    stop("unknown option")
  }

  parms <- list(in_E = 0,
                out_E = 0,
                in_H = mean(inflow),
                out_H = mean(outflow),
                out_H_wb = mean(outflow_wb),
                t_strat = t_strat,
                t_autumn = t_autumn,
                frac_epi = 0.25,
                vol_E_min = 1.0    # destratify if volume of Epi is to small
  )

  init <- c(vol_E = vol, vol_H = 0) # Mio m3




  rootfun <- function (t, y, parms, ...) {
    event_times <- unlist(parms[c("t_strat", "t_autumn")])
    vol_E_min <- unlist(parms["vol_E_min"])
    return(c(y[1] - vol_E_min, event_times - t))
  }


  #ode(init, times, volume_model, parms, method = "adams", inflows = inflows,
  #    events = list(func = stratification_event, time = event_times))

  ode(init, times, volume_model, parms, method = "adams",
      inflows = inflows, #event_times = event_times,
      rootfun = rootfun,
      events = list(func = stratification_event, root = TRUE))
}
