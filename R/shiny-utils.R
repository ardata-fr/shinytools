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
      if (as.character(x) == "") {
        return(FALSE)
      } else {
        return(TRUE)
      }
    }
  }
}

#' @export
info_tooltip <- function(label = "", tooltip = "", label_style = "font-weight:bold", infoButton_style = "") {
    randomId <- paste0("tooltipID", sample(x = 1:10000, size = 1))
    infoButton <- tags$a(
        id = randomId, style = infoButton_style,
        style = "color: steelblue;", icon("info-circle", class = "fa-lg"),
        `data-toggle` = "popover", `data-trigger` = "hover", `data-animation` = "false",
        `data-container` = "body", tabindex = "0", role = "button",
        `data-content` = tooltip,
        tags$script(sprintf("$('#%s').popover();", randomId))
    )
    label_ <- tags$span(style = label_style, label)
    tags$span(label_, infoButton)
}