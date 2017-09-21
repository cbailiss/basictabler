#' Render a table as a HTML widget.
#'
#' The \code{basictabler} function is primarily intended for use with Shiny web
#' applications.
#'
#' @import htmltools
#' @import htmlwidgets
#' @export
#' @param bt The table to render.
#' @param width The target width.
#' @param height The target height.
#' @param styleNamePrefix A text prefix to be prepennded to the CSS declarations
#'   (to ensure uniqueness).
#' @return A HTML widget.
#' @examples
#' # See the Shiny vignette in this package for examples.
basictabler <- function(bt, width=NULL, height=NULL, styleNamePrefix=NULL) {
  settings <- list() # may need this in the future
  widgetData <- list(
    tableCss = bt$getCss(styleNamePrefix=styleNamePrefix),
    tableHtml = as.character(bt$getHtml(styleNamePrefix=styleNamePrefix)),
    settings = settings
  )
  htmlwidgets::createWidget("basictabler", widgetData, width=width, height=height)
}

#' Standard function for Shiny scaffolding.
#' @export
#' @param outputId The id of the html element that will contain the htmlwidget.
#' @param width The target width of the htmlwidget.
#' @param height The target height of the htmlwidget.
basictablerOutput <- function(outputId, width = "100%", height = "100%") {
  shinyWidgetOutput(outputId, "basictabler", width, height, package = "basictabler")
}

#' Standard function for Shiny scaffolding.
#' @export
#' @param expr The R expression to execute and render in the Shiny web application.
#' @param env Standard shiny argument for a render function.
#' @param quoted Standard shiny argument for a render function.
renderBasictabler <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  shinyRenderWidget(expr, basictablerOutput, env, quoted = TRUE)
}
