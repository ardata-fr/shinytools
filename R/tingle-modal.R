
#' @title Tingle modal
#'
#' @description An alternative modal window. Load dependencies in
#'  UI with \code{load_tingle}, use \code{tingle_*} in server.
#'
#' @export
#'
#' @name tingle-modal
#'
#' @importFrom htmltools htmlDependency
#'
#' @examples
#' if (interactive()) {
#'   library(shiny)
#'   library(shinytools)
#'
#'   ui <- fluidPage(
#'
#'     load_tingle(),
#'
#'     tags$h1("Examples tingle modal"),
#'
#'     actionButton(inputId = "launch1", label = "Open modal"),
#'     actionButton(inputId = "launch2", label = "Another one")
#'   )
#'
#'   server <- function(input, output, session) {
#'
#'
#'     output$plot <- renderPlot({
#'       req(input$n)
#'       hist(rnorm(input$n))
#'     })
#'
#'     observeEvent(input$launch1, {
#'       tingle_show(tingle_dialog(
#'         tags$h2("Yes! A modal!"),
#'         plotOutput(outputId = "plot"),
#'         sliderInput(
#'           inputId = "n", label = "Number of observations",
#'           min = 10, max = 200, value = 20, width = "100%"
#'         )
#'       ))
#'     })
#'
#'     observeEvent(input$launch2, {
#'       tingle_show(tingle_dialog(
#'         tags$h2("Very important!"),
#'         tags$p("Do you like Shiny?"),
#'         footer = tagList(
#'           actionButton("ofcourse", "Of course!", width = "49%"),
#'           actionButton("yes", "Yes!", width = "49%")
#'         ),
#'         easy_close = FALSE
#'       ))
#'     })
#'
#'     observe({
#'       input$ofcourse
#'       input$yes
#'       tingle_remove()
#'     })
#'
#'   }
#'
#'   shinyApp(ui, server)
#' }
load_tingle <- function() {
  htmlDependency(
    name = "tingle", version = "0.14.0",
    src = list(href = "shinytools", file = "static/assets"),
    package = "shinytools",
    script = c("tingle/tingle.min.js", "tingle/tingle-bindings.js"),
    stylesheet = "tingle/tingle.min.css"
  )
}



#' @param ui Content of the modal defined with \code{tingle_dialog}.
#' @param close_id Optionnal \code{inputId} to retrieve server-side when modal is closed by user.
#' @param session The \code{session} object passed to function given to shinyServer.
#'
#' @rdname tingle-modal
#'
#' @importFrom shiny removeUI insertUI tags
#'
#' @export
#'
tingle_show <- function(ui, close_id = NULL, session = getDefaultReactiveDomain()) {
  if (!inherits(ui, "tingle_dialog"))
    stop("'ui' must be output of function tingle_dialog", call. = FALSE)
  if (!is.null(close_id))
    close_id <- session$ns(close_id)
  session$sendCustomMessage("show_tingle", dropNulls(list(
    opts = ui$opts, footer = ui$footer, id_close = close_id
  )))
  removeUI(selector = "#tingle-content", immediate = TRUE, session = session)
  insertUI(
    selector = "#tingle-modal",
    ui = tags$div(
      id = "tingle-content",
      ui$ui
    ),
    session = session
  )
  if (!is.null(ui$footer) && !isTRUE(ui$footer$footer_tingle)) {
    removeUI(selector = "#tingle-footer-content", immediate = TRUE, session = session)
    insertUI(
      selector = "#tingle-footer",
      ui = tags$div(
        id = "tingle-footer-content",
        ui$footer$footer_content
      ),
      session = session
    )
  }
}


#' @param ... UI elements for the body of the modal dialog box.
#' @param footer UI for footer. Use NULL for no footer.
#' @param easy_close If \code{TRUE}, the modal dialog can be dismissed by clicking
#'  outside the dialog box, or be pressing the Escape key, or by clicking the close button.
#' @param sticky_footer Set to \code{TRUE} for a footer always visible on screen.
#' @param css_class Custom CSS classes that will be added to tingle container.
#'
#' @export
#'
#' @rdname tingle-modal
tingle_dialog <- function(..., footer = tingle_button(), easy_close = TRUE, sticky_footer = FALSE, css_class = NULL) {
  ui <- list(...)
  opts <- dropNulls(list(
    stickyFooter = sticky_footer,
    cssClass = css_class
  ))
  if (!isTRUE(easy_close)) {
    opts$closeMethods <- ""
  }
  if (!is.null(footer)) {
    footer <- list(
      footer_tingle = inherits(footer, "tingle_button"),
      footer_content = footer
    )
  }
  structure(list(ui = ui, opts = opts, footer = footer), class = "tingle_dialog")
}


#' @param label The contents of the button or link–usually a text label, but you could also use any other HTML, like an image.
#' @param status Status for the button: \code{"default"} (default), \code{"primary"} or \code{"danger"}.
#' @param pull_right Align the button on the right.
#'
#' @export
#'
#' @rdname tingle-modal
tingle_button <- function(label = "Close", status = "default", pull_right = TRUE) {
  status <- match.arg(status, c("default", "primary", "danger"))
  if (isTRUE(pull_right)) {
    pull_right <- "tingle-btn--pull-right"
  } else {
    pull_right <- ""
  }
  class <- sprintf("tingle-btn tingle-btn--%s %s", status, pull_right)
  structure(list(label = label, class = class), class = "tingle_button")
}


#' @export
#'
#' @rdname tingle-modal
tingle_remove <- function(session = getDefaultReactiveDomain()) {
  session$sendCustomMessage("remove_tingle", list())
}

