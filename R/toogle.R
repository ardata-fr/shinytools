#' @export
#' @importFrom shiny getDefaultReactiveDomain
#' @title Display or hide an element
#' @description
#' \code{html_toogle} will display an element if hidden or hide it
#' if visible.
#' \code{html_set_visible} will make an element visible.
#' \code{html_set_hidden} will hide an element.
#' @param id shiny input id
#' @param protect_id wether to process shiny input id with
#' function \code{session$ns()} or not.
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
#'         sliderInput( "obs",
#'           "Number of observations:",
#'           min = 0, max = 1000, value = 500),
#'         actionButton("toogle", "toogle"),
#'         actionButton("set_visible", "show"),
#'         actionButton("set_hidden", "hide")
#'       ),
#'       mainPanel(
#'         plotOutput("distPlot")
#'       )
#'     )
#'   )
#'
#'   server <- function(input, output) {
#'     output$distPlot <- renderPlot({
#'       hist(rnorm(input$obs))
#'     })
#'     observeEvent(input$toogle, {
#'       html_toogle("distPlot")
#'     })
#'     observeEvent(input$set_visible, {
#'       html_set_visible("distPlot")
#'     })
#'     observeEvent(input$set_hidden, {
#'       html_set_hidden("distPlot")
#'     })
#'   }
#'
#'   print(shinyApp(ui, server))
#' }
html_toogle <- function(id, protect_id = FALSE){
  session <- getDefaultReactiveDomain()
  if( !protect_id ){
    id <- get_session_id(session, id)
  }
  session$sendCustomMessage(
    type = 'html_toogle',
    message = list(id = id)
  )

}

#' @export
#' @rdname html_toogle
html_set_visible <- function(id, protect_id = FALSE){
  session <- getDefaultReactiveDomain()
  if( !protect_id ){
    id <- get_session_id(session, id)
  }
  session$sendCustomMessage(
    type = 'html_set_visible',
    message = list(id = id)
  )

}

#' @export
#' @rdname html_toogle
html_set_hidden <- function(id, protect_id = FALSE){
  session <- getDefaultReactiveDomain()
  if( !protect_id ){
    id <- get_session_id(session, id)
  }
  session$sendCustomMessage(
    type = 'html_set_hidden',
    message = list(id = id)
  )
}


