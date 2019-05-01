info_tooltip <- function(label = "", tooltip = "", label_style = "font-weight:bold", infoButton_style = "") {
  randomId <- paste0("tooltipID", sample(x = 1:10000, size = 1))
  infoButton <- tags$a(
    id = randomId, style = infoButton_style,
    style = "color: steelblue;", icon("info-circle", class = "fa-lg"),
    `data-toggle` = "popover", `data-trigger` = "hover", `data-animation` = "false",
    `data-container` = "body", tabindex = "0", role = "button",
    `data-content` = tooltip,
    tags$script(sprintf("$('#%s').popover();", randomId))
  )
  label_ <- tags$span(style = label_style, label)
  tags$span(label_, infoButton)
}

#' @importFrom shinyWidgets prettyToggle
naInput <- function(id) {
  prettyToggle(
    inputId = id,
    value = TRUE,
    label_on = "NA", icon_on = icon("ok", lib = "glyphicon"),
    label_off = "NA", icon_off = icon("remove", lib = "glyphicon"),
    status_on = "success", status_off = "danger",
    inline = TRUE
  )
}
