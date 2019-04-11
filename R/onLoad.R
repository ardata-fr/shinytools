#' Shiny resource
#'
#' @importFrom shiny addResourcePath
#'
#' @noRd
.onLoad <- function(...) {
  shiny::addResourcePath("shinytools", system.file("static/assets", package = "shinytools"))
}
