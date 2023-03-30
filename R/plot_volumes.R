#' plot_volumes
#' @param out output from `ode` (deSolve Object)
#' @param ylab label at the y axis
#' @export
#'
plot_volumes <- function(out, ylab = "Volumen (Mio m3)") {

  ## note: use of native |> pipes and not dplyr %>%
  out <- as.data.frame(out)
  names(out) <- c("times", "Epilimnion", "Hypolimnion", "gesamt")

  out |>
    tidyr::pivot_longer(!times, names_to = "Teilraum", values_to = "Volumen" ) |>
    dplyr::mutate(Teilraum = factor(.data$Teilraum,
             levels = c("gesamt", "Epilimnion","Hypolimnion"))) |>
    dplyr::filter(.data$Teilraum != "gesamt") |>
    ggplot(aes(x = times, y = .data$Volumen, fill = .data$Teilraum)) +
      geom_area() + scale_fill_manual(values = c("#109cd9", "#1a3553")) +
      xlab("Tag im Jahr") + ylab(ylab) +
      theme(legend.position = "bottom", text = element_text(size = 18))
}
