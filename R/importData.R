#' @export
#' @title Factor to character
#' @description UI part of module import
#' @param id Module call id
#' @param label Label of actionLink
#' @import shiny
importDataUI <- function(id, label = "Import") {
  ns <- NS(id)

  actionLink(ns("AB_import"), label = label)
}

#' @export
#' @title Factor to character
#' @description Server part of module import
#' @param input Not a real parameter, not to be set manually.
#' @param output Not a real parameter, not to be set manually.
#' @param session Not a real parameter, not to be set manually.
#' @param forbidden_labels Optional reactive, forbidden labels
#' @import shiny
#' @importFrom tools file_ext
#' @importFrom utils read.csv2
#' @examples
#' library(shinytools)
#' library(shiny)
#'
#' if (interactive()) {
#'   options(device.ask.default = FALSE)
#'
#' ui <- fluidPage(
#'   load_jstools(),
#'   importDataUI(id = "id1"),
#'   tags$hr(),
#'   uiOutput("ui_SI_labels"),
#'   dataVizUI(id = "id2")
#' )
#' 
#' server <- function(input, output) {
#' 
#'   all_datasets <- reactiveValues()
#' 
#'   datasets <- callModule(module = importDataServer, id = "id1",
#'     forbidden_labels = reactive(names(reactiveValuesToList(all_datasets))))
#' 
#'   observeEvent(datasets$trigger, {
#'     req(datasets$trigger > 0)
#'     all_datasets[[datasets$name]] <- datasets$object
#'   })
#' 
#'   output$ui_SI_labels <- renderUI({
#'     x <- reactiveValuesToList(all_datasets)
#'     if (length(x) > 0) {
#'       selectInput("SI_labels", label = "Choose dataset", choices = names(x))
#'     }
#'   })
#' 
#'   callModule(module = dataVizServer, id = "id2",
#'     data = reactive({req(input$SI_labels);all_datasets[[input$SI_labels]]}))
#' }
#'
#'   print(shinyApp(ui, server))
#' }
importDataServer <- function(input, output, session, forbidden_labels = reactive(NULL)) {

  ns <- session$ns

  ############
  ## Common ##
  ############
  {
    current   <- reactiveValues(x = NULL)
    toReturn  <- reactiveValues(object = NULL, name = NULL, trigger = 0)
    internal  <- reactiveValues(infile = NULL)
  }

  ###########
  ## modal ##
  ###########
  {
    # Show the modal
    observeEvent(input$AB_import, {
      # reset form
      current$x     <- NULL
      internal$infile <- NULL

      # show modal
      showModal(
        modalDialog(
          fluidRow(
            column(5,
              uiOutput(ns("ui_DIV_modal_path")),
              tags$br(),
              textInput(ns("TI_modal_label"), label = "Label",
                placeholder = "label for your dataset"),
              uiOutput(ns("ui_DIV_modal_label_warning")),
              tags$br(),
              uiOutput(ns("ui_DIV_modal_options"))
            ),
            column(7,
              fileInput(ns("FI_modal_file"), "Choose a file",
                accept = c(".xlsx", ".csv", ".sas7bdat")),
              dataDimUI(id = ns("import-dim")),
              dataVizUI(id = ns("import-viz"))
            )
          ),
          footer = tagList(
            default_disabled(actionButton(ns("AB_modal_import"), label = "Import !")),
            modalButton("Cancel")
          ),
          size = "l"
        )
      )
    })

    # Update internal$infile according to input$FI_modal_file
    observeEvent(input$FI_modal_file, {
      internal$infile <- input$FI_modal_file
      # Update label according to file name
      default_name <- tools::file_path_sans_ext(input$FI_modal_file$name)
      updateTextInput(session, "TI_modal_label", value = default_name)
    })

    # selected file path
    output$ui_DIV_modal_path <- renderUI({
      if (is.null(internal$infile)) {
        value <- "No file selected"
      } else {
        value <- internal$infile$datapath
      }
      tags$div(
        tags$strong("Path"),
        tags$br(),
        tags$pre(value)
      )
    })

    # Label warning (already used)
    output$ui_DIV_modal_label_warning <- renderUI({
      if (input$TI_modal_label %in% forbidden_labels()) {
        tags$div(class = "alert alert-warning", role = "alert",
          "This label is already used"
        )
      }
    })

    # Modal options
    output$ui_DIV_modal_options <- renderUI({
      req(internal$infile)
      file_ext <- toupper(file_ext(internal$infile$datapath))
      common <- checkboxInput(ns("CBI_char2factor"), label = "Convert character to factor ?", value = FALSE)
      if (file_ext %in% c("XLS", "XLSX")) {
        requireNamespace(package = "openxlsx", quietly = TRUE)
        sheets <- openxlsx::getSheetNames(internal$infile$datapath)
        tags$div(
          selectInput(ns("SI_modal_excelSheets"), label = "Choose Excel sheet",
            choices = sheets),
          common
        )
      } else if (file_ext == "CSV") {
        tags$div(
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

    # Update current$x according to options selected
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
          tmp <- openxlsx::read.xlsx(xlsxFile = internal$infile$datapath,
            sheet = input$SI_modal_excelSheets,
            detectDates = TRUE)
          as.data.frame(tmp)
        }, error = function(err) {
          FALSE
        })
      } else if (file_ext == "SAS7BDAT") {
        tmp <- tryCatch({
          requireNamespace(package = "haven", quietly = TRUE)
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

    # dataset dimension
    callModule(module = dataDimServer, id = "import-dim",
          data = reactive(current$x))

    # dataset viz
    callModule(module = dataVizServer, id = "import-viz",
          data = reactive(current$x), part = c("header", "footer"))
  }

  #####################
  ## update toReturn ##
  #####################
  {
    # Enable or disable AB_modal_import button
    observe({
      if (!is.null(current$x) & filled(input$TI_modal_label)) {
        if (isFALSE(current$x)) {
          html_disable("AB_modal_import")
        } else if (input$TI_modal_label %in% forbidden_labels()) {
          html_disable("AB_modal_import")
        } else {
          html_enable("AB_modal_import")
        }
      } else {
        html_disable("AB_modal_import")
      }
    })
    # Update to Return
    observeEvent(input$AB_modal_import, {
      # toReturn
      toReturn$object  <- current$x
      toReturn$name  <- input$TI_modal_label
      toReturn$trigger <- toReturn$trigger + 1
      removeModal()
    })
  }

  return(toReturn)
}
