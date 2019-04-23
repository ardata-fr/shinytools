#' @import shiny
dataDimUI <- function(id) {
  ns <- NS(id)

  uiOutput(ns("ui_DIV_dim"))
}

#' @import shiny
dataDimServer <- function(input, output, session, data = reactive(NULL), warnIfNull = FALSE) {
  ns <- session$ns

  output$ui_DIV_dim <- renderUI({
    dim_ <- dim(data())
    if (is.null(dim_)) {
      if (warnIfNull) {
        tags$div(class = "alert alert-warning", role = "alert",
          "No dataset loaded"
        )
      }
    } else {
      tags$div(class = "alert alert-info", role = "alert",
        paste("Dataset contains", dim_[1], "row(s) and", dim_[2], "column(s)")
      )
    }
  })
}