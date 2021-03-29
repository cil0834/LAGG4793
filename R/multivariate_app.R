#' Multivariate Shiny App
#'
#' @description This is the multivariate shiny app that contains 6 widgets and 3 graphs that show various results for multivariate data.
#' @import shiny
#'
#' @export
#'
#' @examples
#' 'multivariate_app()'
#'
multivariate_app<-function(){
  shiny::runApp(system.file("multivariate_shiny", package="LAGG4793"),launch.browser = TRUE)
}
