filterVarUI <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    column(12,
      uiOutput(ns("ui_DIV_filter"))
    )
  )
}

#' @import shiny
#' @import lubridate
#' @import rlang
filterVarServer <-  function(input, output, session, 
                      x = reactive(NULL), 
                      varname = reactive(NULL), 
                      label = reactive(NULL)) {
  
  ns <- session$ns
  
  ############+
  ## Input ----
  ############+
  {
    internal <- reactiveValues(
                  x = NULL, # To use
                  res = NULL # For results
                )
    
    # Initialize internal$x & internal$res
    observe({
      if (is.null(x())) {
        internal$x <- internal$res <- NULL
      } else {
        if (inherits(x(), "data.frame")) {
          internal$res <- x()
          if (varname() %in% colnames(x())) {
            internal$x   <- x()[,varname()]
          } else {
            internal$x <- NULL
          }
        } else {
          tmp <- data.frame(x())
          colnames(tmp) <- varname()
          internal$res <- tmp
        }
      }
    })
  }
  
  ########+
  ## UI ---
  ########+
  {
    output$ui_DIV_filter <- renderUI({
      if (is.null(internal$x)) {
        NULL
      } else {
        if (is.numeric(internal$x)) {
          min_ <- min(internal$x)
          max_ <- max(internal$x)
          sliderInput(ns("ui"), label = label(), min = min_, max = max_, value = c(min_, max_))
        } else if (is.character(internal$x) || is.factor(internal$x) || is.logical(internal$x)) {
          lvl <- as.character(unique(internal$x))
          selectizeInput(ns("ui"), label = label(), choices = lvl, selected = lvl, multiple = TRUE)
        } else if (is.Date(internal$x)) {
          min_ <- min(internal$x)
          max_ <- max(internal$x)
          dateRangeInput(ns("ui"), label = label(), start = min_, end = max_, min = min_, max = max_)
        }
      }
    })
  }
  
  #############+
  ## Output ----
  #############+
  {
    toReturn <- reactiveValues(filtered_data = NULL, filter_expr = NULL, filtered = FALSE)
    
    # If no filter applied, then return filtered = FALSE
    observe({
      if (!filled(input$ui)) {
        toReturn$filter_expr <- toReturn$filtered_data <- NULL
      } else {
        filtered <- TRUE
        if (is.numeric(internal$x)) {
          if (input$ui[1] == min(internal$x) && input$ui[2] == max(internal$x)) {
            filtered <- FALSE
          }
          expr_string <- paste(varname(), ">=", input$ui[1], "&", varname(), "<=", input$ui[2])
        } else if (is.Date(internal$x)) {
          if (input$ui[1] == min(internal$x) && input$ui[2] == max(internal$x)) {
            filtered = FALSE
          }
          expr_string <-  paste(varname(), ">=", paste0("as.Date(\"", input$ui[1], "\")"), "&", 
                            varname(), "<=", paste0("as.Date(\"", input$ui[2], "\")"))
        } else if (is.character(internal$x) || is.factor(internal$x)) {
          if (all(unique(internal$x) %in% input$ui)) {
            filtered <- FALSE
          }
          expr_string <- paste(varname(), "%in%", expr_text(expr(!!input$ui)))
        } else if (is.logical(internal$x)) {
          if (length(input$ui) == 2) {
            filtered <- FALSE
            expr_string <- paste(varname(), "%in% c(T,F)")
          } else {
            expr_string <- paste(varname(), "==", input$ui)
          }
        }
        filter_expr <- parse_expr(expr_string)
        filters <- eval_tidy(filter_expr, data = internal$res)
        filtered_data <- internal$res[filters,]
        toReturn$filter_expr <- filter_expr
        toReturn$filtered_data <- filtered_data
        toReturn$filtered <- filtered
        
      }
    })
    
    return(toReturn)
  }
}
