#' @export
#' @title Check if input filled
#' @description Unlike req, the filled function doesn't exit block if FALSE
#' @param x UI input or reactiveValues
#' @examples
#' library(shinytools)
#' library(shiny)
#'
#' if (interactive()) {
#'   options(device.ask.default = FALSE)
#'
#' ui <- fluidPage(
#'   fluidRow(
#'     column(2,
#'       textInput("TI", label = "", placeholder = "Write something"),
#'       textOutput("TO_ti")
#'     ),
#'     column(2,
#'       selectInput("SI", label = "Choose one or multiple letters", 
#'         choices = letters, selected = "", multiple = TRUE),
#'       textOutput("TO_si")
#'     ),
#'     column(2,
#'       actionButton("AB2", label = "Create reactiveValues"),
#'       textOutput("TO_reactiveValues")
#'     )
#'   )
#' )
#'
#' server <- function(input, output) {
#'   
#'   rv <- reactiveValues()
#'   
#'   output$TO_ti <- renderText({
#'     if (filled(input$TI)) {
#'       paste("textInput filled")
#'     } else {
#'       paste("textInput empty")
#'     }
#'   })
#'   output$TO_si <- renderText({
#'     if (filled(input$SI)) {
#'       paste("selectInput filled")
#'     } else {
#'       paste("selectInput empty")
#'     }
#'   })
#'   
#'   output$TO_reactiveValues <- renderText({
#'     if (filled(rv$ex)) {
#'       paste("reactiveValues rv$ex filled")
#'     } else {
#'       paste("reactiveValues rv$ex empty")
#'     }
#'   })
#'   
#'   observeEvent(input$AB2, {
#'     rv$ex <- input$AB2
#'   })
#' }
#'
#'   print(shinyApp(ui, server))
#' }
filled <- function(x) {
  if (length(x)>1) x <- x[1]
  if (is.null(x)) {
    return(FALSE)
  } else {
    if (is.na(x)) {
      return(FALSE)
    } else {
      if (x == "") {
        return(FALSE)
      } else {
        return(TRUE)
      }
    }
  }
}