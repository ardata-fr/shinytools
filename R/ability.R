#' @export
#' @title enable or disable an element
#' @description
#' \code{ability} is to be used to enable or disable an element.
#' \code{html_enable} and \code{html_disable} are shortcut functions.
#' @param id shiny input id
#' @param state logical scalar. If state is \code{TRUE},
#' element will be enabled, if \code{FALSE}, element will be disabled.
#' @family javascript functions
#' @examples
#' library(shiny)
#' library(shinytools)
#'
#' if (interactive()) {
#'   ui <- fluidPage(
#'     load_jstools(),
#'     fluidRow(
#'       column(width = 4,
#'              actionButton(inputId = "able_slider",
#'                           label = "[slider] enabled/disabled"),
#'              br(),br(), br(),
#'              sliderInput( "slider",
#'                           "A Number:",
#'                           min = 0, max = 1000, value = 500)
#'       ),
#'       column(width = 4,
#'              actionButton(inputId = "able_select",
#'                           label = "[list] enabled/disabled"),
#'              br(),br(), br(),
#'              selectizeInput("select", "A select input:", 1:5)
#'       ),
#'       column(width = 4,
#'              actionButton(inputId = "able_btn",
#'                           label = "[btn] enabled/disabled"),
#'              br(),br(), br(),
#'              actionButton("btn", "A button", class = "btn-warning")
#'       )
#'     )
#'   )
#'
#'   server <- function(input, output) {
#'     observeEvent(input$able_slider, {
#'       ability("slider", input$able_slider%%2 < 1)
#'     })
#'     observeEvent(input$able_btn, {
#'       ability("btn", input$able_btn%%2 < 1)
#'     })
#'     observeEvent(input$able_select, {
#'       ability("select", input$able_select%%2 < 1)
#'     })
#'   }
#'
#'   print(shinyApp(ui, server))
#' }
#' @importFrom shiny getDefaultReactiveDomain
ability <- function(id, state){
  session <- getDefaultReactiveDomain()
  id <- get_session_id(session, id)

  session$sendCustomMessage(
    type = 'enable',
    message = list(id = id, state = state)
  )
}

#' @export
#' @rdname ability
html_enable <- function(id) {
  ability(id = id, state = TRUE)
}

#' @rdname ability
#' @export
html_disable <- function(id) {
  ability(id = id, state = FALSE)
}




#' @importFrom shiny tagAppendAttributes
#' @export
#' @title add disabled tag to an HTML element.
#' @description
#' The function is to be used to disable an element when created.
#' @param x html element
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
#'         default_disabled(actionButton("dosomething", "don't click"))
#'       ),
#'       mainPanel(
#'         plotOutput("distPlot")
#'       )
#'     )
#'   )
#'
#'   server <- function(input, output) {
#'     output$distPlot <- renderPlot({
#'       hist(rnorm(50))
#'     })
#'   }
#'
#'   print(shinyApp(ui, server))
#' }
#'
default_disabled <- function(x){
  tagAppendAttributes(x, disabled = TRUE)
}

