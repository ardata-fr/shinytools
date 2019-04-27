filterVarUI <- function(id) {
  ns <- NS(id)
  
  fluidRow(
  column(12,
    uiOutput(ns("ui_DIV_filter"))
  )
  )
}

#' @title filterVar
#' @examples
#' library(shinytools)
#' library(shiny)
#' library(lubridate)
#' library(rlang)
#' if (interactive()) {
#'   options(device.ask.default = FALSE)
#'
#'   test_date <- data.frame(col = as.Date(as.character(lakers$date), tryFormats = "%Y%m%d"))
#'   test_logical <- data.frame(col = sample(c(T,F), size = 100, replace = T))
#'   test_fact <- data.frame(Species = iris$Species)
#'   test_char <- data.frame(Species = as.character(iris$Species), stringsAsFactors = FALSE)
#'   test_num <- data.frame(Sepal.Length = iris$Sepal.Length)
#'
#'   ui <- fluidPage(
#'     filterVarUI(id = "date"),
#'     filterVarUI(id = "logical"),
#'     filterVarUI(id = "fact"),
#'     filterVarUI(id = "char"),
#'     filterVarUI(id = "num")
#'   )
#'
#'   server <- function(input, output) {
#'     # Test date
#'     res_date <- callModule(module = filterVarServer, id = "date",
#'                x = reactive(test_date),
#'                varname = reactive("col"),
#'                label = reactive("col"))
#'
#'     observe({
#'       req(res_date)
#'       tmp <- reactiveValuesToList(res_date)
#'       print(tmp)
#'     })
#'
#'     # Test logical
#'     res_logical <- callModule(module = filterVarServer, id = "logical",
#'                 x = reactive(test_logical),
#'                 varname = reactive("col"),
#'                 label = reactive("col"))
#'
#'     observe({
#'       req(res_logical)
#'       tmp <- reactiveValuesToList(res_logical)
#'       print(tmp)
#'     })
#'
#'     # Test factor
#'     res_factor <- callModule(module = filterVarServer, id = "fact",
#'                x = reactive(test_fact),
#'                varname = reactive("Species"),
#'                label = reactive("Species"))
#'
#'     observe({
#'       req(res_factor)
#'       tmp <- reactiveValuesToList(res_factor)
#'       print(tmp)
#'     })
#'
#'     # Test character
#'     res_char <- callModule(module = filterVarServer, id = "char",
#'                x = reactive(test_char),
#'                varname = reactive("Species"),
#'                label = reactive("Species"))
#'
#'     observe({
#'       req(res_char)
#'       tmp <- reactiveValuesToList(res_char)
#'       print(tmp)
#'     })
#'
#'     # Test numeric
#'     res_num <- callModule(module = filterVarServer, id = "num",
#'               x = reactive(test_num),
#'               varname = reactive("Sepal.Length"),
#'               label = reactive("Sepal.Length"))
#'
#'     observe({
#'       req(res_num)
#'       tmp <- reactiveValuesToList(res_num)
#'       print(tmp)
#'     })
#'
#'   }
#'   print(shinyApp(ui, server))
#' }
filterVarServer <-  function(input, output, session,
               x = reactive(NULL),
               varname = reactive(NULL),
               label = reactive(NULL),
               default = reactive(NULL),
               trigger = reactive(NULL)) {
  
  if (!requireNamespace(package = "rlang"))
    message("Package 'rlang' is required to run this module")
  if (!requireNamespace(package = "lubridate"))
    message("Package 'lubridate' is required to run this module")
  
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
      fool <- trigger()
      if (is.null(internal$x)) {
        NULL
      } else {
        if (is.numeric(internal$x)) {
          min_ <- min(internal$x)
          max_ <- max(internal$x)
          if (length(isolate(default())) == 2) {
            values_ <- isolate(default())
          } else {
            values_ <- c(min_, max_)
          }
          sliderInput(ns("ui"), label = label(), min = min_, max = max_, value = values_)
        } else if (is.character(internal$x) || is.factor(internal$x) || is.logical(internal$x)) {
          lvl <- as.character(unique(internal$x))
          if (is.null(isolate(default()))) {
            selected_ <- lvl
          } else {
            selected_ <- isolate(default())
          }
          selectizeInput(ns("ui"), label = label(), choices = lvl, selected = selected_, multiple = TRUE)
        } else if (lubridate::is.Date(internal$x)) {
          min_ <- min(internal$x)
          max_ <- max(internal$x)
          if (length(isolate(default())) == 2) {
            start_ <- isolate(default())[1]
            end_   <- isolate(default())[2]
          } else {
            start_ <- min_
            end_   <- max_
          }
          dateRangeInput(ns("ui"), label = label(), start = start_, end = end_, min = min_, max = max_)
        }
      }
    })
    outputOptions(output, 'ui_DIV_filter', suspendWhenHidden = FALSE)
  }
  
  #############+
  ## Output ----
  #############+
  {
    toReturn <- reactiveValues(filtered_data = NULL, filter_expr = NULL, filtered = FALSE, values = NULL)
    
    # If no filter applied, then return filtered = FALSE
    observe({
      if (!filled(input$ui)) {
        toReturn$filter_expr <- toReturn$filtered_data <- toReturn$values <- NULL
      } else {
        filtered  <- TRUE
        expr_string <- NULL
        values    <- NULL
        if (is.numeric(internal$x)) {
          if (input$ui[1] == min(internal$x) && input$ui[2] == max(internal$x)) {
            filtered <- FALSE
          } else {
            values <- c(input$ui)
            expr_string <- paste(varname(), ">=", input$ui[1], "&", varname(), "<=", input$ui[2])
          }
        } else if (lubridate::is.Date(internal$x)) {
          if (input$ui[1] == min(internal$x) && input$ui[2] == max(internal$x)) {
            filtered = FALSE
          } else {
            values <- input$ui
            expr_string <-  paste(varname(), ">=", paste0("as.Date(\"", input$ui[1], "\")"), "&",
                        varname(), "<=", paste0("as.Date(\"", input$ui[2], "\")"))
          }
        } else if (is.character(internal$x) || is.factor(internal$x)) {
          if (all(unique(internal$x) %in% input$ui)) {
            filtered <- FALSE
          } else {
            values <- input$ui
            expr_string <- paste(varname(), "%in%", rlang::expr_text(rlang::expr(!!input$ui)))
          }
        } else if (is.logical(internal$x)) {
          if (length(input$ui) == 2) {
            filtered <- FALSE
          } else {
            values <- input$ui
            expr_string <- paste(varname(), "==", input$ui)
          }
        }
        if (!is.null(expr_string)) {
          filter_expr <- rlang::parse_expr(expr_string)
          filters <- rlang::eval_tidy(filter_expr, data = internal$res)
          filtered_data <- internal$res[filters,]
        } else {
          filter_expr <- NULL
          filtered_data <- internal$res
        }
        toReturn$filter_expr <- filter_expr
        toReturn$filtered_data <- filtered_data
        toReturn$filtered <- filtered
        toReturn$values   <- values
      }
    })
    
    return(toReturn)
  }
}
