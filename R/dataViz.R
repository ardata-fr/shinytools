#' @export
#' @title Module dataViz UI part
#' @description UI part of module import
#' @param id Module call id
#' @import shiny
dataVizUI <- function(id) {
  ns <- NS(id)

  uiOutput(ns("ui_DT_viz"))
}

#' @export
#' @title Module dataViz Server part
#' @description Server part of module dataViz
#' @param input Not a real parameter, not to be set manually.
#' @param output Not a real parameter, not to be set manually.
#' @param session Not a real parameter, not to be set manually.
#' @param data Reactive data.frame
#' @param part "All" to show all table, "header" & "footer" to show head and/or tail
#' @param style Style that defines how to display DT table (only minimal & default available for the moment)
#' @import shiny
#' @importFrom utils head tail
dataVizServer <- function(input, output, session, data = reactive(NULL), part = "all", style = "minimal") {
  ns <- session$ns
  
  requireNamespace(package = "DT", quietly = TRUE)
  
  getViz <- function(data, part) {
    if (all(c("header", "footer") %in% part)) {
      cast_to_short_table(x = data, symbol = "...")
    } else if (part[1] == "header") {
      return(head(data))
    } else if (part[1] == "footer") {
      return(tail(data))
    } else if (part[1] == "all") {
      return(data)
    } else {
      return(NULL)
    }
  }

  output$DT_viz <- DT::renderDataTable({
    req(data())
    dat <- getViz(data = data(), part = part)
    if (style == "minimal") {
      DT::datatable(dat,
        rownames = FALSE,
        options =   list(dom = 'tipr',
                ordering = FALSE,
                scrollX = TRUE,
                scrollY = "300px",
                pageLength = 100,
                bInfo = FALSE
              )
      )
    } else {
      DT::datatable(dat, 
        filter = 'top', 
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          scrollY = "300px",
          autoWidth = TRUE
        )
      )
    }
  })

  output$ui_DT_viz <- renderUI({
    if (is.null(data())) {
      tags$div(class = "alert alert-warning", role = "alert",
        "No dataset loaded"
      )
    } else if (isFALSE(data())) {
      tags$div(class = "alert alert-danger", role = "alert",
        "Error when loading dataset, check your import options"
      )
    } else {
      tags$div(
        DT::dataTableOutput(ns("DT_viz"))
      )
    }
  })
}