#' plot_flows
#'
#' @param discharge A data frame containing discharge data (must have `time`, `inflow`, `outflow`, `outflow_wb` columns).
#' @param xlab Label for the x-axis for all plots.
#' @param ylab_inflow Label for the y-axis of the inflow plot.
#' @param ylab_outflow Label for the y-axis of the raw water outflow plot.
#' @param ylab_wildbed Label for the y-axis of the wild bed outflow plot.
#' @return A combined ggplot object using `ggpubr::ggarrange`.
#' @import ggplot2
#' @importFrom ggpubr ggarrange
#' @export
plot_flows <- function(discharge,
                       xlab = "Tag im Jahr",
                       ylab_inflow = "Zufluss (m3/d)",
                       ylab_outflow = "Rohwasserabgabe (m3/d)",
                       ylab_wildbed = "Wildbettabgabe (m3/d)") {

  # Ensure discharge is a data frame, though ggplot usually handles it.
  discharge <- as.data.frame(discharge)

  # Plot 1: Inflow
  p1 <- ggplot(discharge, aes(x = .data$time, y = .data$inflow )) +
    geom_line() +
    ylab(ylab_inflow) +
    xlab(xlab)

  # Plot 2: Raw water outflow
  p2 <- ggplot(discharge, aes(x = .data$time, y = .data$outflow)) +
    geom_line() +
    ylab(ylab_outflow) +
    xlab(xlab)

  # Plot 3: Wild bed outflow
  p3 <- ggplot(discharge, aes(x = .data$time, y = .data$outflow_wb)) +
    geom_line() +
    ylab(ylab_wildbed) +
    xlab(xlab)

  # Arrange the plots
  ggpubr::ggarrange(p1, p2, p3, ncol = 1)
}
