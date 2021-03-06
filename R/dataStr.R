dataStrUI <- function(id) {
  ns <- NS(id)
  uiOutput(ns("ui_DIV_str"))
}

#' @importFrom utils str
#' @importFrom shiny verbatimTextOutput renderPrint
#' @importFrom htmltools strong p
dataStrServer <- function(input, output, session, data = reactive(NULL), warnIfNull = FALSE) {
  ns <- session$ns

  output$PR_str <- renderPrint({
    str(data())
  })

  output$ui_DIV_str <- renderUI({
    if (is.null(data())) {
      if (warnIfNull) {
        tags$div(
          class = "alert alert-warning", role = "alert",
          "No dataset loaded"
        )
      }
    } else {
      tagList(
        p(
          strong("Data structure:")
        ),
        verbatimTextOutput(ns("PR_str"))
      )
    }
  })
}
