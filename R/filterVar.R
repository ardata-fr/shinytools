filterVarUI <- function(id) {
  ns <- NS(id)

  fluidRow(
    column(12,
           uiOutput(ns("ui_DIV_filter"))
    )
  )
}

#' @title filterVar
#' @noRd
#' @examples
#' library(shinytools)
#' library(shiny)
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
#'     res_date <-  callModule(module = filterVarServer, id = "date",
#'                    x = reactive(test_date),
#'                    varname = reactive("col"),
#'                    label = reactive("col"))
#'
#'     observe({
#'       req(res_date)
#'       tmp <- reactiveValuesToList(res_date)
#'       print(tmp)
#'     })
#'
#'     # Test logical
#'     res_logical <- callModule(module = filterVarServer, id = "logical",
#'                      x = reactive(test_logical),
#'                      varname = reactive("col"),
#'                      label = reactive("col"))
#'
#'     observe({
#'       req(res_logical)
#'       tmp <- reactiveValuesToList(res_logical)
#'       print(tmp)
#'     })
#'
#'     # Test factor
#'     res_factor <-  callModule(module = filterVarServer, id = "fact",
#'                      x = reactive(test_fact),
#'                      varname = reactive("Species"),
#'                      label = reactive("Species"))
#'
#'     observe({
#'       req(res_factor)
#'       tmp <- reactiveValuesToList(res_factor)
#'       print(tmp)
#'     })
#'
#'     # Test character
#'     res_char <-  callModule(module = filterVarServer, id = "char",
#'                    x = reactive(test_char),
#'                    varname = reactive("Species"),
#'                    label = reactive("Species"))
#'
#'     observe({
#'       req(res_char)
#'       tmp <- reactiveValuesToList(res_char)
#'       print(tmp)
#'     })
#'
#'     # Test numeric
#'     res_num <- callModule(module = filterVarServer, id = "num",
#'                  x = reactive(test_num),
#'                  varname = reactive("Sepal.Length"),
#'                  label = reactive("Sepal.Length"))
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
#' @importFrom shiny dateRangeInput selectizeInput sliderInput
#' @importFrom rlang expr_text expr
filterVarServer <-  function(input, output, session,
                             x = reactive(NULL),
                             varname = reactive(NULL),
                             label = reactive(NULL),
                             return_datas = FALSE,
                             default = reactive(NULL),
                             trigger = reactive(NULL)) {

  ns <- session$ns

  ############+
  ## Input ----
  ############+
  {
    internal <- reactiveValues(
      x = NULL, # To use
      res = NULL, # For results
      has_na = NULL # Add or not NA
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
        # Check if NA present
        if (length(which(is.na(internal$x))) > 0) {
          internal$has_na <- TRUE
          x <- internal$x[!is.na(internal$x)]
          label_ <- NULL
        } else {
          internal$has_na <- FALSE
          x <- internal$x
          label_ <- label()
        }
        if (is.numeric(x)) {
          min_ <- min(x)
          max_ <- max(x)
          if (length(isolate(default())) == 2) {
            values_ <- isolate(default())
          } else {
            values_ <- c(min_, max_)
          }
          button <- sliderInput(ns("ui"), label = label_, min = min_, max = max_, value = values_)
        } else if (is.character(x) || is.factor(x) || is.logical(x)) {
          lvl <- enc2native(as.character(unique(x)))
          if (is.null(isolate(default()))) {
            selected_ <- lvl
          } else {
            selected_ <- isolate(enc2native(default()))
          }
          button <- selectizeInput(ns("ui"), label = label_, choices = lvl, selected = selected_, multiple = TRUE)
        } else if (inherits(x, "Date")) {
          min_ <- min(x)
          max_ <- max(x)
          if (length(isolate(default())) == 2) {
            start_ <- isolate(default())[1]
            end_   <- isolate(default())[2]
          } else {
            start_ <- min_
            end_   <- max_
          }
          button <- dateRangeInput(ns("ui"), label = label_, start = start_, end = end_, min = min_, max = max_)
        }
        if (internal$has_na) {
          tagList(
            tags$span(
              tags$label(label()), HTML("&nbsp;&nbsp;"), naInput(ns("ui_na"))
            ),
            button
          )
        } else {
          button
        }
      }
    })
  }

  #############+
  ## Output ----
  #############+
  {
    toReturn <- reactiveValues(filtered_data = NULL, filter_expr = NULL, filtered = FALSE, values = NULL)

    # If no filter applied, then return filtered = FALSE
    observe({
      if (!isTruthy(input$ui)) {
        toReturn$filter_expr <- toReturn$filtered_data <- toReturn$values <- NULL
      } else {
        filtered    <- TRUE
        expr_string <- NULL
        values      <- NULL
        if (internal$has_na) {
          x <- internal$x[!is.na(internal$x)]
          if (input$ui_na) {
            na_wtd    <- "present_tokeep"
          } else {
            na_wtd    <- "present_toremove"
          }
        } else {
          x <- internal$x
          na_wtd <- "absent"
        }

        if (is.numeric(x)) {
          if (round(input$ui[1],2) == round(min(x),2) && round(input$ui[2],2) == round(max(x),2)) {
            if (na_wtd == "present_toremove") {
              expr_string <- paste0("!is.na(", varname(), ")")
            } else {
              filtered <- FALSE
            }
          } else {
            values <- c(input$ui)
            expr_string <- paste(varname(), ">=", input$ui[1], "&", varname(), "<=", input$ui[2])
            if (na_wtd == "present_tokeep") {
              expr_string <- paste("(", expr_string, paste0("| is.na(", varname(), ")"), ")")
            }
          }
        } else if (inherits(x, "Date")) {
          if (input$ui[1] == min(x) && input$ui[2] == max(x)) {
            if (na_wtd == "present_toremove") {
              expr_string <- paste0("!is.na(", varname(), ")")
            } else {
              filtered <- FALSE
            }
          } else {
            values <- input$ui
            expr_string <-  paste(varname(), ">=", paste0("as.Date(\"", input$ui[1], "\")"), "&",
                                  varname(), "<=", paste0("as.Date(\"", input$ui[2], "\")"))
            if (na_wtd == "present_tokeep") {
              expr_string <- paste("(", expr_string, paste0("| is.na(", varname(), ")"), ")")
            }
          }
        } else if (is.character(x) || is.factor(x)) {
          if (all(unique(x) %in% input$ui)) {
            if (na_wtd == "present_toremove") {
              expr_string <- paste0("!is.na(", varname(), ")")
            } else {
              filtered <- FALSE
            }
          } else {
            values <- input$ui
            expr_string <- paste(varname(), "%in%", expr_text(expr(!!input$ui)))
            if (na_wtd == "present_tokeep") {
              expr_string <- paste("(", expr_string, paste0("| is.na(", varname(), ")"), ")")
            }
          }
        } else if (is.logical(x)) {
          if (length(input$ui) == 2) {
            if (na_wtd == "present_toremove") {
              expr_string <- paste0("!is.na(", varname(), ")")
            } else {
              filtered <- FALSE
            }
          } else {
            values <- input$ui
            expr_string <- paste(varname(), "==", input$ui)
            if (na_wtd == "present_tokeep") {
              expr_string <- paste("(", expr_string, paste0("| is.na(", varname(), ")"), ")")
            }
          }
        }
        if (!is.null(expr_string)) {
          filter_expr <- parse_expr(expr_string)
          if (return_datas) {
            filters <- eval_tidy(filter_expr, data = internal$res)
            filtered_data <- internal$res[filters,]
          } else {
            filtered_data <- NULL
          }
        } else {
          filter_expr <- NULL
          if (return_datas) {
            filtered_data <- internal$res
          } else {
            filtered_data <- NULL
          }
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
