#' @importFrom shiny callModule
#' @param id namespace identifier for the module
#' @export
#' @rdname filterData
filterDataUI <- function(id) {
  ns <- NS(id)

  fluidRow(
    column(
      12,
      jstools_dep(),
      uiOutput(ns("ui_DIV_global")),
      uiOutput(ns("ui_specific")),
      uiOutput(ns("ui_specific2"))
    )
  )
}

#' @rdname filterData
#' @export
#' @title shiny UI to filter data
#' @description A module to enable data filtering
#' in shiny applications. The ui function is populated
#' with column filters that can be manipulated by the user.
#' The server function is returning the R expression corresponding
#' to filters defined by user and eventually the filtered dataset.
#' @examples
#' library(shinytools)
#' library(shiny)
#'
#' # example with dataTableOutput and renderDataTable ----
#' if (interactive()) {
#'   options(device.ask.default = FALSE)
#'
#'   ui <- fluidPage(
#'     fluidRow(
#'       column(
#'         width = 4,
#'         filterDataUI(id = "demo")
#'       ),
#'       column(width = 8, dataTableOutput(outputId = "subsetdata"))
#'     )
#'   )
#'
#'   server <- function(input, output, session) {
#'     res <- callModule(module = filterDataServer,
#'                       id = "demo", x = reactive(iris),
#'                       return_data = TRUE)
#'
#'     observe({
#'       req(res)
#'       print(res$expr)
#'     })
#'
#'     output$subsetdata <- renderDataTable({
#'       res$filtered_data
#'     })
#'   }
#'   print(shinyApp(ui, server))
#' }
#'
#' @importFrom shiny reactiveValuesToList icon selectInput
#' @importFrom rlang parse_expr eval_tidy
#' @param input,output,session mandatory arguments for modules to be valid. These
#' should not to be defined as they will be handled by shiny.
#' @param x the input data.frame to be filtered. It must be a reactive
#' value.
#' @param show_all_filters if TRUE, all filters UI are shown. If FALSE, a compact UI
#' is displayed containing a select box to choose a variable filter and a dynamic
#' filter corresponding to the filter to display.
#' @param default_show should the filters be expanded when UI is is shown first. If
#' FALSE, the UIs showing filters are hidden.
#' @param return_data whether the filtered dataset should be also returned
#' in the reactive value returned by the module.
filterDataServer <- function(input, output, session,
                             x = reactive(NULL), default_show = TRUE,
                             show_all_filters = TRUE, return_data = FALSE) {
  ns <- session$ns

  toReturn <- reactiveValues(expr = NULL, filtered_data = NULL, filtered = FALSE)
  internal <- reactiveValues(filters_shown = default_show, trigger = 0, nb_x = 0, all_ids = c())

  # Common ----
  {
    # This reactiveValues contains as many slots as variables in x
    # Each slot is the result of filterVar (eg: `filtered_data`, `filter_expr` & `filtered`)
    all_filters <- reactiveValues()
    all_triggers <- reactiveValues()

    nb_filters <- reactive({
      if (is.null(x())) {
        return(0)
      } else {
        if (length(reactiveValuesToList(all_filters)) == 0) return(0)
        length(which(sapply(reactiveValuesToList(all_filters), function(x) x$filtered)))
      }
    })

    name_filters <- reactive({
      if (is.null(x())) {
        return("")
      } else {
        if (length(reactiveValuesToList(all_filters)) == 0) return("")
        names(which(sapply(reactiveValuesToList(all_filters), function(x) x$filtered)))
      }
    })

    resetX <- function() {
      obs_mods <<- list()
      res_mods <<- list()
      # reset if already used
      if (length(names(all_filters)) > 0) {
        for (i in names(all_filters)) {
          all_triggers[[i]] <- NULL
          all_filters[[i]] <- list(filter_expr = NULL, filtered_data = NULL, filtered = FALSE, values = NULL)
        }
      }
      internal$nb_x <- internal$nb_x + 1
      # internal$trigger <- internal$trigger + 1
      internal$all_ids <- c()
      if (!is.null(x())) {
        for (i in colnames(x())) {
          all_triggers[[i]] <- 0
          all_filters[[i]] <- list(filter_expr = NULL, filtered_data = NULL, filtered = FALSE, values = NULL)
        }
      }
    }

    observeEvent(x(), {
      resetX()
    }, ignoreNULL = FALSE)

    # Init all triggers & all_filters
    # observe({
    # if (is.null(x())) {
    # all_triggers <<- reactiveValues()
    # all_filters  <<- reactiveValues()
    # } else {
    # for (i in colnames(x())) {
    # all_triggers[[i]] <- 0
    # all_filters[[i]] <- list(filter_expr = NULL, filtered_data = NULL, filtered = FALSE, values = NULL)
    # }
    # }
    # })
  }

  # UI ----
  {
    output$ui_DIV_global <- renderUI({
      if (internal$filters_shown && !is.null(x())) {
        icon_ <- icon("minus")
      } else {
        icon_ <- icon("plus")
      }
      if (is.null(x())) {
        AB <- default_disabled(actionButton(ns("AB_show_filters"), label = NULL, icon = icon_))
      } else {
        AB <- actionButton(ns("AB_show_filters"), label = NULL, icon = icon_)
      }
      tags$ul(
        class = "list-inline",
        tags$li(paste(nb_filters(), "filter(s) applied")),
        tags$li(info_tooltip(tooltip = paste(name_filters(), collapse = ", "))),
        tags$li(AB)
      )
    })

    observeEvent(input$AB_show_filters, {
      # Triggers management
      if (internal$filters_shown) {
        if (show_all_filters) {
          # Increase global trigger if filters shown
          internal$trigger <- internal$trigger + 1
        } else {
          # Increase current trigger used
          all_triggers[[input$SI_var]] <- all_triggers[[input$SI_var]] + 1
        }
      }
      internal$filters_shown <- !internal$filters_shown
    })
  }

  # The server side is split into 2 parts : show all filters and show one
  if (show_all_filters) {

    #################### +
    ## Modules calls ----
    #################### +
    {
      # This list in global contains modules output
      res_mods <- list()
      obs_mods <- list()

      # This function set the server side and returns the ui side of module filterVar
      addModuleServer <- function(name, x, id) {
        res_mods[[name]] <<- callModule(
          module = filterVarServer, id = id,
          x = reactive(x[, name, drop = FALSE]),
          varname = reactive(name),
          label = reactive(name),
          return_data = return_data,
          default = reactive(res_mods[[name]][["values"]]),
          trigger = reactive(internal$trigger)
        )

        obs_mods[[name]] <<- observeEvent(reactiveValuesToList(res_mods[[name]]), {
          all_filters[[name]] <- reactiveValuesToList(res_mods[[name]])
        })
      }

      # Reset res_mods when x() gets NULL and call modules if x() not null
      observeEvent(internal$nb_x, {
        req(internal$nb_x > 0)

        # Call modules if needed
        if (!is.null(x())) {
          # Call modules
          all_ids <- c()
          for (i in 1:ncol(x())) {
            name_ <- colnames(x())[i]
            id <- paste("id", internal$nb_x, length(res_mods) + 1, sep = "_")
            all_ids <- c(all_ids, ns(id))
            addModuleServer(name = name_, x = x(), id = id)
          }
          internal$all_ids <- all_ids
        }
      })

      # call ui
      output$ui_MODS_filters <- renderUI({
        if (length(internal$all_ids) != 0) {
          lapply(internal$all_ids, function(i) {
            ii <- i
            filterVarUI(id = ii)
          })
        } else {
          NULL
        }
      })
    }

    # Specific UI ----
    {
      # This UI contains the ui of all modules
      output$ui_specific <- renderUI({
        if (internal$filters_shown && !is.null(x())) {
          uiOutput(ns("ui_MODS_filters"))
        }
      })
    }
  } else {
    # Specific UI ----
    {
      output$ui_specific <- renderUI({
        if (internal$filters_shown && !is.null(x())) {
          selectInput(ns("SI_var"), label = "Choose variable", choices = colnames(x()))
        }
      })

      output$ui_specific2 <- renderUI({
        if (internal$filters_shown && !is.null(x())) {
          id <- ns(paste("id", internal$nb_x, input$SI_var, sep = "_"))
          filterVarUI(id = id)
        }
      })
    }

    # Module call ----
    {

      # This list in global contains modules output
      res_mods <- list()
      obs_mods <- list()

      observeEvent(input$SI_var, {
        # User change SI_var, then must re-create UI according to new default values
        # Remember, default() is always used in a isolate not to recreate every
        # time the user change input
        all_triggers[[input$SI_var]] <- all_triggers[[input$SI_var]] + 1
      })

      observeEvent(internal$nb_x, {
        req(internal$nb_x > 0)
        # Call modules if needed
        if (!is.null(x())) {
          for (i in colnames(x())) {
            local({
              ii <- i
              res_mods[[ii]] <<- callModule(
                module = filterVarServer, id = paste("id", internal$nb_x, ii, sep = "_"),
                x = reactive(isolate(x())[, ii, drop = FALSE]),
                varname = reactive(ii),
                label = reactive(ii),
                return_data = return_data,
                default = reactive(all_filters[[ii]][["values"]]),
                trigger = reactive(all_triggers[[ii]])
              )
              obs_mods[[ii]] <<- observeEvent(reactiveValuesToList(res_mods[[ii]]), {
                all_filters[[ii]] <- reactiveValuesToList(res_mods[[ii]])
              })
            })
          }
        }
      })
    }
  }

  # return value ----
  {
    # Reset toReturn
    reset <- function() {
      toReturn$expr <- NULL
      toReturn$filtered <- FALSE
      if (return_data) {
        toReturn$filtered_data <- isolate(x())
      }
    }
    
    observe({
      tmp <- reactiveValuesToList(all_filters)
      if (length(tmp) == 0) {
        reset()
      } else {
        filters <- which(sapply(tmp, function(x) x$filtered))
        if (length(filters) == 0) {
          reset()
        } else {
          tmp_expr <- paste(sapply(filters, function(x) {
            tmp[[x]][["filter_expr"]]
          }), collapse = " & ")
          my_expr <- parse_expr(tmp_expr)
          if (return_data) {
            filtered_data <- eval_tidy(my_expr, data = isolate(x()))
            filtered_data <- isolate(x())[filtered_data, ]
          } else {
            filtered_data <- NULL
          }
          toReturn$filtered_data <- filtered_data
          toReturn$expr <- my_expr
          toReturn$filtered <- TRUE
        }
      }
    })
  }

  return(toReturn)
}
