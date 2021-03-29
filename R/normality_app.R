#' Normality Shiny App
#'
#' @description This app is for checking normality for data. There are a total of 8 widgets 3 data frames and 7 plots that run various tests on
#' the normality of data
#' @import shiny
#'
#' @export
#'
#' @examples
#' 'normality_app()'
#'
normality_app<-function(){
  shiny::runApp(system.file("norm_shiny", package="LAGG4793"),launch.browser = TRUE)
}
