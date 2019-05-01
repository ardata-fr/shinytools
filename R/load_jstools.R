#' @importFrom htmltools htmlDependency
#' @title load shinytools JavaScript functions
#' @description load shinytools JavaScript functions in a shiny application.
#' Operation has to be made in the UI part.
#' @examples
#' \dontrun{
#' library(shinytools)
#'
#' if (interactive()) {
#'   options(device.ask.default = FALSE)
#'
#'   # Define UI
#'   ui <- fluidPage(
#'     titlePanel("Hello dghiesse!"),
#'     load_jstools(),
#'     sidebarLayout(
#'       sidebarPanel(
#'         sliderInput( "obs",
#'           "Number of observations:",
#'           min = 0, max = 1000, value = 500)
#'       ),
#'       mainPanel(plotOutput("distPlot"))
#'     )
#'   )
#'
#'   server <- function(input, output) {
#'     output$distPlot <- renderPlot({
#'       hist(rnorm(input$obs))
#'     })
#'   }
#'
#'   print(shinyApp(ui, server))
#' }
#' }
#'
#' @export
load_jstools <- function() {
  jstools_dep()
}

jstools_dep <- function(){
  htmlDependency("shinytools",
                 "1.0.0",
                 src = system.file(package="shinytools", "static"),
                 script = "js/shinytools.js")
}
