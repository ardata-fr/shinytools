#' @export
#' @importFrom shiny getDefaultReactiveDomain tags HTML
#' @title associate a event with an input value
#' @description associate a click on an HTML element with an input value.
#' The name of input value is free.
#' @param id shiny input id on which click listener has been set
#' @param event_id reactive input to be incremented when click if fired.
#' @family javascript functions
#' @examples
#' library(shinytools)
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
click_event <- function(id, event_id){
  session <- getDefaultReactiveDomain()
  id <- get_session_id(session, id)

  tags$head(tags$script(HTML(sprintf("registerCounter('%s', '%s', 'click');", id, event_id))))
}
