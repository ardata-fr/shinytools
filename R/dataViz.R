#' @param id namespace identifier for the module
#' @importFrom shiny NS uiOutput req
#' @rdname dataViewer
#' @name dataViewer
#' @noRd
dataViewerUI <- function(id) {
  ns <- NS(id)
  uiOutput(ns("ui_DT_viz"))
}

#' @title shiny UI to view data
#' @description A module to view data in a DataTable output. An option is available
#' to display only first or last rows of the dataset.
#' @param input,output,session mandatory arguments for modules to be valid. These
#' should not to be defined as they will be handled by shiny.
#' @param data the input data.frame to be displayed. It must be a reactive
#' value.
#' @param part one of "all" to show all table, c("header", "footer") to show first and last rows, "header"
#' to show only first rows, "footer" to show only last rows.
#' @param style define how to display the DataTable ("minimal" or "default")
#' @importFrom utils head tail
#' @importFrom DT renderDataTable datatable dataTableOutput
#' @importFrom shiny reactive
#' @rdname dataViewer
#' @examples
#' library(shinytools)
#' library(shiny)
#'
#' # example with dataViewerUI and dataViewerServer ----
#' if (interactive()) {
#'   options(device.ask.default = FALSE)
#'
#'   ui <- fluidPage(
#'     fluidRow(column(width = 12, dataViewerUI(id = "demo") ) )
#'   )
#'
#'   server <- function(input, output, session) {
#'     callModule(module = dataViewerServer, id = "demo",
#'                data = reactive(iris), part = c("header", "footer"))
#'   }
#'   print(shinyApp(ui, server))
#' }
#'
#' @name dataViewer
#' @noRd
dataViewerServer <- function(input, output, session, data = reactive(NULL), part = "all", style = "minimal") {
  ns <- session$ns

  getViewer <- function(data, part) {
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

  output$DT_viz <- renderDataTable({
    req(data())
    dat <- getViewer(data = data(), part = part)
    if (style == "minimal") {
      datatable(dat,
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
      datatable(dat,
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
      div(class = "alert alert-warning", role = "alert",
        "No dataset loaded"
      )
    } else if (isFALSE(data())) {
      div(class = "alert alert-danger", role = "alert",
        "Error when loading dataset, check your import options"
      )
    } else {
      div(
        dataTableOutput(ns("DT_viz"))
      )
    }
  })
}
