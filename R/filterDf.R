filterDfUI <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    column(12,
      load_jstools(),
      uiOutput(ns("ui_DIV_global")),
      uiOutput(ns("ui_ui_MODS_filters"))
    )
  )
}

#' @title filterDf
#' @examples
#' library(shinytools)
#' library(shiny)
#' library(lubridate)
#' library(rlang)
#' if (interactive()) {
#'   options(device.ask.default = FALSE)
#'
#'
#'   ui <- fluidPage(
#'     filterDfUI(id = "demo")
#'   )
#'
#'   server <- function(input, output) {
#'       
#'       res <- callModule(module = filterDfServer, id = "demo", x = reactive(iris))
#'       
#'       observe({
#'           tmp <- reactiveValuesToList(res)
#'           if (tmp$filtered) {
#'             print(tmp$expr)
#'           }
#'       })
#'   }
#'   print(shinyApp(ui, server))
#' }
filterDfServer <- function(input, output, session, 
                    x = reactive(NULL), default_show = TRUE, show_all = TRUE) {
  
  if (!requireNamespace(package = "rlang"))
    message("Package 'rlang' is required to run this module")
  if (!requireNamespace(package = "lubridate"))
    message("Package 'lubridate' is required to run this module")

  ns <- session$ns
  
  toReturn <- reactiveValues(expr = NULL, filtered_data = NULL, filtered = FALSE)
  internal <- reactiveValues(filters_shown = default_show)
  
  # The server side is split into 2 parts : show all filters and show one
  if (show_all) {
    #############+
    ## Common ----
    #############+
    {
      # This reactiveValues contains as many slots as variables in x
      # Each slot is the result of filterVar (eg: `filtered_data`, `filter_expr` & `filtered`)
      all_filters <- reactiveValues()
      
      # Still the same, problem, cannot definitively remove an element from reactiveValues
      # Then it's set to NULL (but still exists)
      # reactive res_without_null makes this filter
      res_without_null <- reactive({
        tmp <- reactiveValuesToList(all_filters)
        if (length(tmp) == 0) return(NULL)
        tmp[which(!sapply(tmp, is.null))]
      })
      
      nb_filters <- reactive({
        # Pour moi : voir si le reactiveValuesToList suffit a trigger, pas sur...
        if (length(reactiveValuesToList(all_filters)) == 0) return(0)
        length(which(sapply(reactiveValuesToList(all_filters), function(x) x$filtered)))
      })
      
      name_filters <- reactive({
        # Pour moi : voir si le reactiveValuesToList suffit a trigger, pas sur...
        if (length(reactiveValuesToList(all_filters)) == 0) return(0)
        names(which(sapply(reactiveValuesToList(all_filters), function(x) x$filtered)))
      })
      
    }
    
    #########+
    ## UI ----
    #########+
    {
      output$ui_DIV_global <- renderUI({
        if (internal$filters_shown) {
          icon_ <- icon("minus")
        } else {
          icon_ <- icon("plus")
        }
        AB <- actionButton(ns("AB_show_filters"), label = NULL, icon = icon_)
        tags$ul(class = "list-inline",
          tags$li(paste(nb_filters(), "filter(s) applied")),
          tags$li(info_tooltip(tooltip = paste(name_filters(), collapse = ", "))),
          tags$li(AB)
        )
      })
      
      observeEvent(input$AB_show_filters, {
        internal$filters_shown <- !internal$filters_shown
        html_toogle("ui_ui_MODS_filters")
      })
    }
    
    ####################+
    ## Modules calls ----
    ####################+
    {
      # This list in global contains modules output
      res_mods <- list()
      obs_mods <- list()
      
      # This function set the server side and returns the ui side of module filterVar
      addModuleServer <- function(name, x, id) {
        res_mods[[name]] <<-  callModule(module = filterVarServer, id = id,
                                x = reactive(x[,name, drop = FALSE]),
                                varname = reactive(name),
                                label = reactive(name))
                                
        obs_mods[[name]] <<- observeEvent(res_mods[[name]][["filter_expr"]], {
          all_filters[[name]] <- reactiveValuesToList(res_mods[[name]])
        })
      }
      
      # Reset res_mods when x() gets NULL and call modules if x() not null
      observeEvent(x(), {
        if (is.null(x())) {
          # Reset all to NULL (in case of new x())
          # Remove observeEvents
          obs_mods <<- list()
          # Remove reactiveValues contained in res_mods global var
          res_mods <<- list()
          # Set to NULL all slots of rv all_filters
          tmp <- isolate(reactiveValuesToList(all_filters))
          if (length(tmp) > 0) {
            for (i in 1:length(tmp)) {
              name <- names(tmp)[i]
              all_filters[[name]] <- NULL
            }
          }
        } else {
          # Call modules
          all_ids <- c()
          for (i in 1:ncol(x())) {
            name_ <- colnames(x())[i]
            id <- paste0("id_", length(res_mods) + 1)
            all_ids <- c(all_ids, ns(id))
            addModuleServer(name = name_, x = x(), id = id)
            
            # call ui
            output$ui_MODS_filters <- renderUI({
              lapply(all_ids, function(i) {
                ii <- i
                filterVarUI(id = ii)
              })
            })
          }
          }
      })
      
      # This UI contains the ui of all modules
      output$ui_ui_MODS_filters <- renderUI({
        if (default_show) {
          uiOutput(ns("ui_MODS_filters"))
        } else {
          # Quand y'aura la fonction dans shinytools
          #html_hide(
            uiOutput(ns("ui_MODS_filters"))
          #)
        }
      })
    }
    
    ################+
    ## To return ----
    ################+
    {
      observe({
        tmp <- reactiveValuesToList(all_filters)
        req(length(tmp) > 0)
        filters <- which(sapply(tmp, function(x) x$filtered))
        
        if (length(filters) == 0) {
          toReturn$expr           <- NULL
          toReturn$filtered_data  <- isolate(x())
          toReturn$filtered       <- FALSE
        } else {
          tmp_expr <- paste(sapply(filters, function(x) {tmp[[x]][["filter_expr"]]}), collapse = " & ")
          my_expr <- rlang::parse_expr(tmp_expr)
          filtered_data <- rlang::eval_tidy(my_expr, data = isolate(x()))
          filtered_data <- isolate(x())[filtered_data,]
          toReturn$expr          <- my_expr
          toReturn$filtered_data <- filtered_data
          toReturn$filtered      <- TRUE
        }
      })
    }
    
    return(toReturn)
  }
}
