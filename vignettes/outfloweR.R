## ----opts, echo = FALSE, include = FALSE--------------------------------------
library("dplyr", warn.conflicts = FALSE)
library("ggpubr", warn.conflicts = FALSE)
library("outfloweR", warn.conflicts = FALSE)
knitr::opts_chunk$set(fig.width=6, fig.height=4)

## ----language, echo = FALSE---------------------------------------------------
library(shiny.i18n)
i18n_json_path <- system.file("extdata", "i18n", package = "outfloweR")

# # Check if the path exists (for debugging)
# if (!dir.exists(i18n_json_path)) {
#   stop("i18n translation directory not found at: ", i18n_json_path)
# } else {
#   message("i18n translation directory found at: ", i18n_json_path)
# }

i18n <- shiny.i18n::Translator$new(translation_json_path = paste0(i18n_json_path, "/", "i18n.json"))
i18n$set_translation_language("en")

## ----fig.height=6-------------------------------------------------------------
library("outfloweR")
data("discharge_normal")
plot_flows(discharge_normal, 
           xlab = "Day of the Year",
           ylab_inflow = "Inflow (m3/d)",
           ylab_outflow = "Raw water (m3/d)",
           ylab_wildbed = "Env. Out (m3/d)")

## ----fig.width=7.5------------------------------------------------------------
out <- scenario("cold_inflow", vol=60, t_strat=90, t_autumn=300,
                discharge_normal$inflow, discharge_normal$outflow,
                discharge_normal$outflow_wb)

plot_volumes(out,
             xlab = "day of the year",
             ylab = "Volume (Mio m3)",
             legend_title = "Layer")

## ----, fig.width=7.5----------------------------------------------------------
out <- scenario("standard", vol=60, t_strat=90, t_autumn=300,
                discharge_normal$inflow, discharge_normal$outflow,
                discharge_normal$outflow_wb)

plot_volumes(out,
             xlab = "day of the year",
             ylab = "Volume (Mio m3)",
             legend_title = "Layer")

## ----, fig.width=7.5----------------------------------------------------------
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

plot_flows(summer_flood,
           xlab = "Day of the Year",
           ylab_inflow = "Inflow (m3/d)",
           ylab_outflow = "Raw water (m3/d)",
           ylab_wildbed = "Env. Out (m3/d)")

## ----fig.width = 8------------------------------------------------------------
library("ggpubr")
out1 <- with(summer_flood,
            scenario("standard", vol=60, t_strat=90, t_autumn=300,
              inflow, outflow, outflow_wb))

out2 <-  with(summer_flood,
              scenario("wb_top", vol=60, t_strat=90, t_autumn=300,
                       inflow, outflow, outflow_wb))
fig1 <- plot_volumes(out1, xlab = "day of the year", ylab = "Volume (Mio m3)", legend_title = "Layer")
fig2 <- plot_volumes(out2, xlab = "day of the year", ylab = "Volume (Mio m3)", legend_title = "Layer")
ggpubr::ggarrange(fig1, fig2, ncol=2)

