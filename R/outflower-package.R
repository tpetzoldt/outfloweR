#' Simulation of Reservoir Mass Balance
#'
#' The package aims to demonstrate reservoir management options by abstracting
#' water either from the bottom outlet or the surface layer (the epilimnion).
#' It uses a simple mass-balance approach and provides a shiny-app.
#'
#' @author Thomas Petzoldt, LuGu
#'
#'
#' @references
#'
#' ATT-Schriftenreihe Bd. 7: Integrale Bewirtschaftung von Trinkwassertalsperren
#' gemäß DIN 19700. Siegburg, 2009.
#'
#'
#' @keywords package
#'
#' @importFrom dplyr filter
#' @rawNamespace import(stats, except = filter)
#' @import graphics utils deSolve ggplot2
#' @importFrom rlang .data
#' @importFrom shinyjs show removeClass
#' @import shiny.i18n
#' @importFrom purrr map
"_PACKAGE"
