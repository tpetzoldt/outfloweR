#' plot_flows
#'
#' @param discharge discharge data frame
#' @export
#'
plot_flows <- function(discharge) {
  p1 <- ggplot(discharge, aes(x = .data$time, y = .data$inflow )) + geom_line() +
    ylab("Zufluss (m3/d)") + xlab("Tag im Jahr")
  p2 <- ggplot(discharge, aes(x = .data$time, y = .data$outflow)) + geom_line() +
    ylab("Rohwasserabgabe (m3/d)") + xlab("Tag im Jahr")
  p3 <- ggplot(discharge, aes(x = .data$time, y = .data$outflow_wb)) + geom_line() +
    ylab("Wildbettabgabe (m3/d)") + xlab("Tag im Jahr")
  ggpubr::ggarrange(p1, p2, p3, ncol=1)
}
