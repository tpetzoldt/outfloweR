## ----opts, echo = FALSE, message = FALSE--------------------------------------
library("dplyr")
library("ggpubr")
library("outfloweR")
knitr::opts_chunk$set(fig.width=6, fig.height=4)

## ---- fig.height=6------------------------------------------------------------
library("outfloweR")
data("discharge_normal")
plot_flows(discharge_normal)

## ---- fig.width=7.5-----------------------------------------------------------
out <- scenario("cold_inflow", vol=60, t_strat=90, t_autumn=300,
                discharge_normal$inflow, discharge_normal$outflow,
                discharge_normal$outflow_wb)

plot_volumes(out)

## ---- , fig.width=7.5---------------------------------------------------------
out <- scenario("standard", vol=60, t_strat=90, t_autumn=300,
                discharge_normal$inflow, discharge_normal$outflow,
                discharge_normal$outflow_wb)

plot_volumes(out)

## ---- , fig.width=7.5---------------------------------------------------------
out <- scenario("wb_top", vol=60, t_strat=90, t_autumn=300,
                discharge_normal$inflow, discharge_normal$outflow,
                discharge_normal$outflow_wb)

plot_volumes(out)

## -----------------------------------------------------------------------------
library("dplyr")
const_discharge <- data.frame(
  time = 1:365,
  inflow = 0.2,
  outflow=0.1,
  outflow_wb=0.12
)
time <- 1:365; wave_start <- 150; yscale <- 10
wave <- yscale * dlnorm(time - wave_start, meanlog=2, sdlog=1)

summer_flood <- const_discharge |>
  mutate(inflow = inflow + wave,
         outflow_wb = ifelse(wave_start + 20 < time & time <= wave_start + 40, outflow + sum(wave)/20, outflow))

plot_flows(summer_flood)

## -----------------------------------------------------------------------------
library("ggpubr")
out1 <- with(summer_flood,
            scenario("standard", vol=60, t_strat=90, t_autumn=300,
              inflow, outflow, outflow_wb))

out2 <-  with(summer_flood,
              scenario("wb_top", vol=60, t_strat=90, t_autumn=300,
                       inflow, outflow, outflow_wb))
fig1 <- plot_volumes(out1)
fig2 <- plot_volumes(out2)                        
ggpubr::ggarrange(fig1, fig2, ncol=1)

