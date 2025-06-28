#' plot_volumes
#'
#' @param out output from `ode` (deSolve Object).
#' @param xlab Label for the x-axis.
#' @param ylab Label for the y-axis.
#' @param legend_title Title for the legend.
#' @param legend_labels Labels for the legend entries (corresponding to Epilimnion and Hypolimnion).
#' @return A ggplot object.
#' @import ggplot2
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr mutate filter
#' @export
plot_volumes <- function(out,
                         xlab = "Tag im Jahr",
                         ylab = "Volumen (Mio m3)",
                         legend_title = "Teilraum",
                         legend_labels = c("Epilimnion", "Hypolimnion")
) {

  out <- as.data.frame(out)
  names(out) <- c("times", "Epilimnion", "Hypolimnion", "gesamt")

  out |>
    tidyr::pivot_longer(!times, names_to = "Teilraum", values_to = "Volumen" ) |>
    dplyr::mutate(
      Teilraum = factor(.data$Teilraum,
                        levels = c("gesamt", "Epilimnion", "Hypolimnion"),
                        labels = c("gesamt", legend_labels[1], legend_labels[2])
      )
    ) |>
    dplyr::filter(.data$Teilraum != "gesamt") |>
    ggplot(aes(x = times, y = .data$Volumen, fill = .data$Teilraum)) +
    geom_area() +
    scale_fill_manual(
      values = c("#109cd9", "#1a3553"),
      name = legend_title
    ) +
    xlab(xlab) +
    ylab(ylab) +
    theme(legend.position = "bottom", text = element_text(size = 18))
}
