#' @export
#' @rdname exportData
#' @param id namespace identifier for the module
#' @importFrom shiny uiOutput NS
exportDataUI <- function(id) {
  ns <- NS(id)
  uiOutput(ns("main_ui"))
}

#' @export
#' @rdname exportData
#' @title exportData
#' @param input,output,session mandatory arguments for modules to be valid. These
#' should not to be defined as they will be handled by shiny.
#' @param display One of c("panel", "button", "modal")
#' 
#' panel : Create a panel with export options
#'
#' button : Create an actionButton that trigger a modal
#'
#' modal : When function exportDataServer is called then display a modal
#' to be used directly into observeEvent handler
#'
#' @param ui_label Title of actionButton or panel
#' @param modal_title Title of the modal
#' @param types Potential export file type (available : excel, csv & sas)
#' @param datas Reactive data.frame
#' @importFrom openxlsx write.xlsx
#' @importFrom haven write_sas
#' @importFrom utils write.table
exportDataServer <- function(input, output, session, display = c("panel", "button", "modal"), 
                              ui_label = ifelse(display == "panel", "Export", "Download"),
                              modal_title = h1("Download dataset"),
                              types = c("excel", "csv", "sas"), datas = NULL) {

  ns <- session$ns

  ############
  ## common ##
  ############
  {
    internal <- reactiveValues(data_as_list = NULL)

    # Update internal$data_as_list ----
    observe({
      if (inherits(datas(), "reactivevalues")) {
        internal$data_as_list <- reactiveValuesToList(datas())
      } else if (inherits(datas(), "list")) {
        internal$data_as_list <- datas()
      } else {
        internal$current      <- datas()
        internal$data_as_list <- list(datas())
      }
    })

    # Update internal$current according to SI_dataset if needed ----
    observe({
      if (length(internal$data_as_list) == 0) {
        internal$current <- NULL
      } else if (length(internal$data_as_list)  == 1) {
        internal$current <- internal$data_as_list[[1]]
      } else if (isTruthy(input$SI_dataset)) {
        internal$current <- internal$data_as_list[[input$SI_dataset]]
      }
    })

    show_modal <- function() {
      showModal(
        modalDialog(
          title = modal_title,
          uiOutput(ns("ui_ALL")),
          footer = tagList(
             modalButton("Cancel")
          )
        )
      )
    }
  }

  #########################
  ## If display is modal ##
  #########################
  {
    # If display == "modal" then show Modal directly (no exportDataUI used)
    observe({
      if (display[1] == "modal") {
        show_modal()
      }
    })
  }

  ########
  ## UI ##
  ########
  {
    # Main UI (either one button or whole panel)
    output$main_ui <- renderUI({
      if (display == "panel") {
        panel(
          heading = ui_label,
          uiOutput(ns("ui_ALL"))
        )
      } else if (display == "button") {
        if (length(internal$data_as_list) > 0) {
          actionButton(ns("AB_show_export_modal"), label = ui_label, icon = icon("download"))
        } else {
          default_disabled(
            actionButton(ns("AB_show_export_modal"), label = ui_label, icon = icon("download"))
          )
        }
      }
    })

    # All UI defined here (to be used in panel or in modal)
    output$ui_ALL <- renderUI({
      tags$div(
        uiOutput(ns("ui_SI_dataset")),
        radioButtons(ns("RB_export_type"), label = "File type", choices = types),
        uiOutput(ns("ui_DIV_options")),
        splitLayout(cellWidths = c("70%", "30%"),
          textInput(ns("TI_filename"), label = NULL, placeholder = "Filename"),
          default_disabled(
            downloadButton(ns("DB_download"), label = "Download", buttonLabel = list(icon("download")))
          )
        )
      )
    })

    # Enable download button
    observe({
      if (!is.null(internal$current) && isTruthy(input$TI_filename)) {
        html_enable("DB_download")
      } else {
        html_disable("DB_download")
      }
    })

    # Dataset
    output$ui_SI_dataset <- renderUI({
      if (length(internal$data_as_list) > 1) {
        choices <- names(internal$data_as_list)
        selectInput(ns("SI_dataset"), label = "Choose dataset", choices = choices)
      } else if (length(internal$data_as_list) == 0) {
        tags$div(class = "warn",
          "Warning : No dataset to download"
        )
      }
    })

    # Modal options
    output$ui_DIV_options <- renderUI({
      req(input$RB_export_type)

      if (input$RB_export_type == "excel") {
        tags$div(
          textInput(ns("TI_opts_excel_sheetName"), label = "Sheet name", placeholder = "Sheet1")
        )
      } else if (input$RB_export_type == "csv") {
        tags$div(
          selectInput(ns("SI_opts_csv_sep"), label = "Separator",
            choices = c("Semicolon" = ";", "Comma" = ",", "Tab" = "\t", "Whitespace" = " "),
            selected = ","),
          selectInput(ns("SI_opts_csv_dec"), label = "Decimal",
            choices = c(".", ","), selected = "."),
          checkboxInput(ns("CBI_opts_csv_quote"), label = "Use double quote ?", value = FALSE)
        )
      }
    })

    # Show modal if !wholePanel
    observeEvent(input$AB_show_export_modal, {
      show_modal()
    })
  }

  ################
  ## Write file ##
  ################
  {
    # downloadHandler
    output$DB_download <- downloadHandler(
      filename = function() {
        if (input$RB_export_type == "excel") {
          ext <- ".xlsx"
        } else if (input$RB_export_type == "csv") {
          ext <- ".csv"
        } else if (input$RB_export_type == "sas") {
          ext <- ".sas7bdat"
        }
        paste0(tools::file_path_sans_ext(input$TI_filename), ext)
      },
      content = function(file){
        if (input$RB_export_type == "csv") {
          # CSV
          write.table(internal$current, file = file,
            sep = input$SI_opts_csv_sep,
            row.names = FALSE,
            col.names = TRUE,
            dec = input$SI_opts_csv_dec,
            quote = input$CBI_opts_csv_quote)
        } else if (input$RB_export_type == "excel") {
          # excel
          if (isTruthy(input$TI_opts_excel_sheetName)) {
            sheetName <- input$TI_opts_excel_sheetName
          } else {
            sheetName <- "Sheet1"
          }
          write.xlsx(x = internal$current,
            file = file,
            sheetName = sheetName,
            col.names = TRUE,
            row.names = FALSE
          )
        } else if (input$RB_export_type == "sas") {
          # SAS7BDAT
          tryCatch({
            tmp <- internal$current
            colnames(tmp) <- cleanName(colnames(tmp), "_")
            write_sas(data = tmp, path = file)
          }, error = function(err) {
            modal("Cannot create sas7bdat file", err$message)
          })
        }
        if (display %in% c("button", "modal")) removeModal()
      }
    )
  }
}
