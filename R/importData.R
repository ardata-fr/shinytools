#' @export
#' @param id namespace identifier for the module
#' @importFrom htmltools div HTML tagList
#' @importFrom shiny fluidRow column textInput reactiveValues renderUI actionButton actionLink observeEvent
#' @rdname importData
#' @name importData
importDataUI <- function(id) {
  ns <- NS(id)

  fluidRow(
    column(
      12,
      jstools_dep(),
      uiOutput(ns("ui_import"))
    )
  )
}

#' @export
#' @rdname importData
#' @name importData
#' @title shiny UI to import data
#' @description A module to enable data importation
#' in shiny applications, by clicking on a button or link action,
#' man can open a modal window to let import dataset in shiny application.
#' The module support CSV, Excel and SAS datasets.
#' @param input,output,session mandatory arguments for modules to be valid. These
#' should not to be defined as they will be handled by shiny.
#' @param forbidden_labels Optional, reactive value, forbidden labels as a character vector
#' @param default_tofact If default convert characters to factors. Default FALSE.
#' @param ui_element UI element to show, either "actionButton", or "actionLink". Default "actionLink".
#' @param ui_label Label of ui element. Default to "import".
#' @param ui_icon Icon of ui element. Default to icon("upload").
#' @param labelize if TRUE a label is required to import the data
#' @examples
#' library(shinytools)
#' library(DT)
#' library(shiny)
#'
#' if (interactive()) {
#'   options(device.ask.default = FALSE)
#'
#'   ui <- fluidPage(
#'     load_tingle(),
#'     importDataUI(id = "id1"),
#'     uiOutput("ui_SI_labels"),
#'     DT::dataTableOutput(outputId = "id2")
#'   )
#'
#'   server <- function(input, output) {
#'
#'     dataset <- callModule(
#'       module = importDataServer,
#'       id = "id1", ui_element = "actionButton",
#'       labelize = FALSE)
#'
#'     output$id2 <- DT::renderDataTable({
#'       req(dataset$trigger > 0)
#'       dataset$object
#'     })
#'   }
#'
#'   print(shinyApp(ui, server))
#' }
#'
#'
#'
#' if (interactive()) {
#'   options(device.ask.default = FALSE)
#'
#'   ui <- fluidPage(
#'     titlePanel("Import and visualize dataset"),
#'     sidebarLayout(
#'       sidebarPanel(
#'         load_tingle(),
#'         importDataUI(id = "id1"),
#'         uiOutput("ui_SI_labels")
#'       ),
#'       mainPanel(
#'         DT::dataTableOutput(outputId = "id2")
#'       )
#'     )
#'   )
#'
#'   server <- function(input, output) {
#'     all_datasets <- reactiveValues()
#'
#'     datasets <- callModule(
#'       module = importDataServer,
#'       id = "id1", ui_element = "actionButton",
#'       labelize = TRUE,
#'       forbidden_labels = reactive(names(reactiveValuesToList(all_datasets))))
#'
#'     observeEvent(datasets$trigger, {
#'       req(datasets$trigger > 0)
#'       all_datasets[[datasets$name]] <- datasets$object
#'     })
#'
#'     output$ui_SI_labels <- renderUI({
#'       x <- reactiveValuesToList(all_datasets)
#'       if (length(x) > 0) {
#'         selectInput("SI_labels", label = "Choose dataset", choices = names(x))
#'       }
#'     })
#'
#'     output$id2 <- DT::renderDataTable({
#'       req(input$SI_labels)
#'       all_datasets[[input$SI_labels]]
#'     })
#'   }
#'
#'   print(shinyApp(ui, server))
#' }
#'
#'
#' @importFrom tools file_ext
#' @importFrom utils read.csv2
#' @importFrom shiny checkboxInput fileInput updateTextInput isTruthy
#' @importFrom shinyWidgets dropdownButton tooltipOptions
#' @importFrom fpeek peek_head peek_tail
#' @importFrom openxlsx getSheetNames read.xlsx
#' @importFrom haven read_sas
importDataServer <- function(input, output, session,
                      forbidden_labels = reactive(NULL), default_tofact = FALSE,
                      ui_element = "actionLink", ui_label = "Import", ui_icon = icon("upload"),
                      labelize = FALSE) {

  ns <- session$ns

  label_ui_html <- HTML("<!---->")
  if(labelize){
    label_ui_html <- tagList(
      div(
        textInput(ns("ui_modal_label"),
                label = "Label",
                placeholder = "label for your dataset") ),
      uiOutput(ns("ui_modal_label_warning"))
      )
  }

  # Common ----
  {
    current   <- reactiveValues(x = NULL)
    toReturn  <- reactiveValues(object = NULL, name = NULL, trigger = 0)
    internal  <- reactiveValues(infile = NULL)
  }

  # UI ----
  {
    output$ui_import <- renderUI({
      if (ui_element == "actionButton") {
        actionButton(ns("event_show_modal"), label = ui_label, icon = ui_icon)
      } else {
        actionLink(ns("event_show_modal"), label = ui_label, icon = ui_icon)
      }
    })
  }

  # modal ----
  {
    # Show the modal ----
    observeEvent(input$event_show_modal, {
      # reset form
      current$x     <- NULL
      internal$infile <- NULL

      # show modal ----
      tingle_show(
        tingle_dialog(
          fluidRow(
            column(5,
              label_ui_html,
              uiOutput(ns("ui_modal_options"))
            ),
            column(7,
              fileInput(ns("FI_modal_file"), "Choose a file",
                accept = c(".xlsx", ".csv", ".sas7bdat")),
              dataDimUI(id = ns("import-dim")),
              dataViewerUI(id = ns("import-viz"))
            )
          ),
          footer = tagList(
            default_disabled(actionButton(ns("AB_modal_import"), label = "Import !")),
            actionButton(ns("AB_modal_cancel"), label = "Cancel")
          ),
          easy_close = FALSE
        )
      )
    })

    # Update internal$infile according to input$FI_modal_file ----
    observeEvent(input$FI_modal_file, {
      internal$infile <- input$FI_modal_file
      if( labelize ){
        # Update label according to file name
        default_name <- tools::file_path_sans_ext(input$FI_modal_file$name)
        updateTextInput(session, "ui_modal_label", value = default_name)
      }

    })


    # Label warning (already used) ----
    output$ui_modal_label_warning <- renderUI({
      if( labelize ){
        if (input$ui_modal_label %in% forbidden_labels()) {
          tags$div(class = "alert alert-warning", role = "alert",
                   "This label is already used"
          )
        }
      }

    })

    # Modal options ----
    output$ui_modal_options <- renderUI({
      req(internal$infile)
      file_ext <- toupper(file_ext(internal$infile$datapath))
      common <- checkboxInput(ns("CBI_char2factor"), label = "Convert character to factor ?", value = default_tofact)
      if (file_ext %in% c("XLS", "XLSX")) {
        sheets <- getSheetNames(internal$infile$datapath)
        tags$div(
          selectInput(ns("SI_modal_excelSheets"), label = "Choose Excel sheet",
            choices = sheets),
          common
        )
      } else if (file_ext == "CSV") {
        tags$div(
          dropdownButton(
            p("First 6 lines:"),
            div(tags$pre(
              paste(peek_head(path = internal$infile$datapath, intern = TRUE, n = 6 ), collapse = "\n")
            )),
            p("Last 6 lines:"),
            div(tags$pre(
              paste(peek_tail(path = internal$infile$datapath, intern = TRUE, n = 6 ), collapse = "\n")
            )),

            circle = TRUE, status = "danger", icon = icon("eye"), size = "xs", width = "100%",
            tooltip = tooltipOptions(title = "Click to view samples of the file")
          ),

          selectInput(ns("SI_modal_sep"), label = "Separator",
            choices = c("Semicolon" = ";", "Comma" = ",", "Tab" = "\t", "Whitespace" = " "),
            selected = ","),
          selectInput(ns("SI_modal_dec"), label = "Decimal",
            choices = c(".", ","), selected = "."),
          selectInput(ns("SI_modal_quote"), label = "Quote",
            choices = c("\"", "'"), selected = "\""),
          common
        )
      } else { # SAS7BDAT
        tags$div(
          common
        )
      }
    })

    # Update current$x according to options selected ----
    observe({
      req(internal$infile)
      file_ext <- toupper(file_ext(internal$infile$datapath))
      if (file_ext == "CSV") {
        req(input$SI_modal_sep)
        req(input$SI_modal_quote)
        tmp <- tryCatch({
          read.csv2(file = internal$infile$datapath,
            header = TRUE,
            sep = input$SI_modal_sep,
            quote = input$SI_modal_quote,
            dec = input$SI_modal_dec,
            stringsAsFactors = FALSE)
        }, error = function(err) {
          FALSE
        })
      } else if (file_ext %in% c("XLS", "XLSX")) {
        req(input$SI_modal_excelSheets)
        tmp <- tryCatch({
          tmp <- read.xlsx(xlsxFile = internal$infile$datapath,
            sheet = input$SI_modal_excelSheets,
            detectDates = TRUE)
          as.data.frame(tmp)
        }, error = function(err) {
          FALSE
        })
      } else if (file_ext == "SAS7BDAT") {
        tmp <- tryCatch({
          haven::read_sas(data_file = internal$infile$datapath)
        }, error = function(err) {
          FALSE
        })
      }
      # Convert characters to factors
      if (!isFALSE(tmp)) {
        if (input$CBI_char2factor) {
          tmp <- char2fact(tmp)
        } else {
          tmp <- fact2char(tmp)
        }
      }
      current$x <- tmp
    })

    # dataset dimension ----
    callModule(module = dataDimServer, id = "import-dim",
          data = reactive(current$x))

    # dataset viz -----
    callModule(module = dataViewerServer, id = "import-viz",
          data = reactive(current$x), part = c("header", "footer"))
  }

  # update toReturn -----
  {
    # Enable or disable AB_modal_import button ----
    observe({
      label_accept <- ifelse(labelize, isTruthy(input$ui_modal_label), TRUE)
      if (!is.null(current$x) && label_accept) {
        if (isFALSE(current$x)) {
          html_disable("AB_modal_import")
        } else if (labelize && input$ui_modal_label %in% forbidden_labels()) {
          html_disable("AB_modal_import")
        } else {
          html_enable("AB_modal_import")
        }
      } else {
        html_disable("AB_modal_import")
      }
    })

    # process button actions -----
    observeEvent(input$AB_modal_cancel, {
      tingle_remove()
    })
    observeEvent(input$AB_modal_import, {
      # toReturn
      toReturn$object  <- current$x
      toReturn$name  <- ifelse(labelize, input$ui_modal_label, NA_character_)
      toReturn$trigger <- toReturn$trigger + 1
      tingle_remove()
    })
  }

  return(toReturn)
}
