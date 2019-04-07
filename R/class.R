#' @export
#' @title Add or remove a class from an HTML element
#' @description
#' \code{html_class} will add or remove a class from an HTML element.
#' \code{html_addclass} and \code{html_unclass} are shortcut functions.
#' @param id shiny input id
#' @param classname class name
#' @param add if TRUE, class will be added, if FALSE class will be deleted
#' @family javascript functions
#' @examples
#' library(shinytools)
#' classes <- c("primary", "secondary", "success",
#'              "danger", "warning", "info")
#'
#' if (interactive()) {
#'   options(device.ask.default = FALSE)
#'
#'   ui <- fluidPage(
#'     titlePanel("Hello dghiesse!"),
#'     load_jstools(),
#'     sidebarLayout(
#'       sidebarPanel(
#'         selectInput("class", "Variable:", classes)
#'       ),
#'       mainPanel(
#'         actionButton("anybutton", "watch this button",
#'                      class = "btn-info")
#'       )
#'     )
#'   )
#'
#'   server <- function(input, output) {
#'     observeEvent(input$class, {
#'       for(i in classes){
#'         html_unclass("anybutton", paste0("btn-", i))
#'       }
#'       html_addclass("anybutton", paste0("btn-", input$class))
#'     })
#'   }
#'
#'   print(shinyApp(ui, server))
#' }
html_class <- function(id, classname, add = TRUE){
  session <- getDefaultReactiveDomain()
  id <- get_session_id(session, id)
  if( add ){
    session$sendCustomMessage(
      type = 'addClass',
      message = list(id = id, className = classname)
    )
  } else {
    session$sendCustomMessage(
      type = 'unClass',
      message = list(id = id, className = classname)
    )
  }

}

#' @export
#' @rdname html_class
html_unclass <- function(id, classname){
  session <- getDefaultReactiveDomain()
  id <- get_session_id(session, id)
  session$sendCustomMessage(
    type = 'unClass',
    message = list(id = id, className = classname)
  )
}

#' @export
#' @rdname html_class
html_addclass <- function(id, classname){
  session <- getDefaultReactiveDomain()
  id <- get_session_id(session, id)
  session$sendCustomMessage(
    type = 'addClass',
    message = list(id = id, className = classname)
  )
}
