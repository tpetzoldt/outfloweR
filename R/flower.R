#' Launch outfloweR shiny App
#'
#' Launch a shiny app to visualize mass balance of reservoirs.
#'
#' @param ... optional parameters passed to the app (not yet implemented)
#'
#' @details The app is still experimental.
#'
#' @return This function has no return value.
#'
#' @examples
#'
#' \dontrun{
#' flower()
#' }
#'
#' @export
#'
flower <- function(...) {
  shiny::runApp(appDir = system.file("app", package = "outfloweR"), ...)
}
