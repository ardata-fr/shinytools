#' @export
#' @importFrom shiny getDefaultReactiveDomain
#' @title associate a event with an input value
#' @description associate a click on an HTML element with an input value.
#' The name of input value is free.
#' @param id shiny input id on which click listener has been set
#' @param event_id reactive input to be incremented when click if fired.
#' @param event_type the type of event, can be one of: \code{"click"},
#'  \code{"dblclick"}, \code{"hover"}, \code{"mousedown"}, \code{"mouseenter"},
#'  \code{"mouseleave"}, \code{"mousemove"}, \code{"mouseout"}, \code{"mouseover"},
#'  \code{"mouseup"}.
#' @family javascript functions
#' @examples
#' library(shinytools)
#' library(shiny)
#'
#' if (interactive()) {
#'   options(device.ask.default = FALSE)
#'
#'   ui <- fluidPage(
#'     titlePanel("Hello dghiesse!"),
#'     load_jstools(),
#'     sidebarLayout(
#'       sidebarPanel(
#'         div(span(id = "aspan", "click me")),
#'         click_event("aspan", "myclick")
#'       ),
#'       mainPanel(
#'         htmlOutput(outputId = "content")
#'       )
#'     )
#'   )
#'
#'   server <- function(input, output) {
#'     observeEvent(input$myclick, {
#'       output$content <- renderUI({
#'         div(paste0("aspan has been clicked ",
#'                    input$myclick, " times"))
#'       })
#'     })
#'   }
#'
#'   print(shinyApp(ui, server))
#' }
click_event <- function(id, event_id = paste0(id, "_event"), event_type = "click"){

  events <- c("click", "dblclick", "hover",
    "mousedown", "mouseenter", "mouseleave",
    "mousemove", "mouseout", "mouseover", "mouseup")

  if( !event_type %in% events ){
    stop("invalid event_type, pick one of",
         paste0(shQuote(events), collapse = ", ") )
  }

  fun <- paste0("$(function() {",
                sprintf("var %s = 0;", event_id),
                sprintf("$('#%s').%s(function(e) {", id, event_type),
                sprintf("%s = %s+1;", event_id, event_id),
                sprintf("Shiny.onInputChange('%s', %s);", event_id, event_id),
                "});});")
  tags$head(tags$script(HTML(fun)))
}
