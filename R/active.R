#' @export
#' @importFrom shiny getDefaultReactiveDomain
#' @title set an element as active or inactive
#' @description
#' \code{activate} will set an element as active or inactive.
#' \code{html_set_active} will make an element active.
#' \code{html_set_inactive} will hide an element inactive.
#' @param id shiny input id
#' @param state logical scalar. If state is \code{TRUE},
#' element will be set as active, if \code{FALSE}, element will be set as inactive.
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
#'         actionButton("activate", "activate"),
#'         actionButton("inactivate", "inactivate")
#'       ),
#'       mainPanel(
#'         actionButton("anybutton", "watch this button", class = "btn-info")
#'       )
#'     )
#'   )
#'
#'   server <- function(input, output) {
#'     observeEvent(input$activate, {
#'       html_set_active("anybutton")
#'     })
#'     observeEvent(input$inactivate, {
#'       html_set_inactive("anybutton")
#'     })
#'   }
#'
#'   print(shinyApp(ui, server))
#' }
activate <- function(id, state){
  session <- getDefaultReactiveDomain()
  id <- get_session_id(session, id)
  session$sendCustomMessage(
    type = 'activate',
    message = list(id = id, state = state)
  )

}

#' @export
#' @rdname activate
html_set_active <- function(id){
  activate(id, TRUE)
}

#' @export
#' @rdname activate
html_set_inactive <- function(id){
  activate(id, FALSE)
}


