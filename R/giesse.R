#' @importFrom htmltools htmlDependency
#' @title add interactivity from shiny server
#' @description get dgihesse javascript dependancy suitable
#' for insertion in the \code{ui} part of a shiny application.
#' @examples
#' library(dgihesse)
#' library(shinyWidgets)
#' classes <- c("primary", "secondary",
#'   "success", "danger", "warning", "info")
#'
#' ## Only run examples in interactive R sessions
#' if (interactive()) {
#'   options(device.ask.default = FALSE)
#'
#'   # Define UI
#'   ui <- fluidPage(
#'     titlePanel("Hello dghiesse!"),
#'     load_dgihesse(),
#'     sidebarLayout(
#'       sidebarPanel(
#'         sliderInput(
#'           "obs",
#'           "Number of observations:",
#'           min = 0, max = 1000, value = 500),
#'         uiOutput(outputId = "quentin"),
#'         actionButton("titi", "desable blah blah"),
#'         actionButton("toogle", "toogle plot"),
#'         actionButton("disabled", "disabled obs"),
#'         actionButton("coco", "dqsdqsds"),
#'         radioButtons("radios", "Options", 1:5),
#'         selectInput("class", "Variable:",
#'                     classes),
#'         div()
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
#'       toogle("distPlot")
#'     })
#'     observeEvent(input$disabled, {
#'       disabled("obs", input$disabled%%2 > 0)
#'     })
#'     observeEvent(input$titi, {
#'       disabled("class", input$titi%%2 > 0)
#'     })
#'     output$quentin <- renderUI({
#'       default_disable(
#'         actionBttn(inputId = "AB_add", label = NULL,
#'                  icon = icon("plus"), size = "sm", style = "gradient", color = "default")
#'       )
#'     })
#'     observeEvent(input$class, {
#'       for(i in classes){
#'         html_unclass("coco", paste0("btn-", i))
#'       }
#'       html_addclass("coco", paste0("btn-", input$class))
#'     })
#'   }
#'
#'   print(shinyApp(ui, server))
#' }
#'
#'
#' @rdname dgihesse
#' @export
load_dgihesse <- function() {
  htmlDependency("dgihesse",
                 "1.0.0",
                 src = system.file(package="dgihesse", "static"),
                 script = "js/dgihesse.js")

}

#' @rdname dgihesse
#' @export
#' @importFrom shiny getDefaultReactiveDomain
#' @param id shiny input id
#' @section toogle:
#' Display or hide an element with function \code{toogle}.
toogle <- function(id){
  session <- getDefaultReactiveDomain()
  session$sendCustomMessage(
    type = 'toogle',
    message = list(id = id)
  )

}

#' @rdname dgihesse
#' @export
#' @param state logical scalar
#' @section disabled:
#' Desable or enable an element with function \code{disabled}.
#'
#' if state is \code{TRUE}, element will be disabled,
#' if \code{FALSE}, element will be enabled.
disabled <- function(id, state){
  session <- getDefaultReactiveDomain()
  session$sendCustomMessage(
    type = 'disabled',
    message = list(id = id, state = state)
  )
}

#' @export
default_disable <- function(x){
  shiny::tagAppendAttributes(x, disabled = TRUE)
}

#' @export
default_enable <- function(x){
  shiny::tagAppendAttributes(x, disabled = FALSE)
}

#' @export
#' @rdname dgihesse
#' @section set_active:
#' Add or remove 'active' class to an HTML element
#' with function \code{set_active}. This is
#' to be used on buttons in shiny applications.
set_active <- function(id, state){
  session <- getDefaultReactiveDomain()
  session$sendCustomMessage(
    type = 'set_active',
    message = list(id = id, state = state)
  )
}

#' @export
#' @rdname dgihesse
#' @param classname class name
#' @section html_unclass:
#' Remove a class from an HTML element
#' with function \code{html_unclass}.
html_unclass <- function(id, classname){
  session <- getDefaultReactiveDomain()
  session$sendCustomMessage(
    type = 'unClass',
    message = list(id = id, className = classname)
  )
}

#' @export
#' @rdname dgihesse
#' @section html_addclass:
#' Add a class to an HTML element
#' with function \code{html_addclass}.
html_addclass <- function(id, classname){
  session <- getDefaultReactiveDomain()
  session$sendCustomMessage(
    type = 'addClass',
    message = list(id = id, className = classname)
  )
}

