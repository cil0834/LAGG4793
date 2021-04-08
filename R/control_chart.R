#' control chart
#'
#'@description This function takes in data and the name of the data, and it makes an extraordinary event chart
#'
#' @param data a vector of data
#' @param y_name the name of the data
#'
#' @return plots an extraordinary event chart
#' @export
#'
#' @examples
#' data = LAGG4793::project_data
#' data = data$Voltage
#' control_chart(data, "voltage")
control_chart <- function(data, y_name){

  upper = mean(data) + 3*stats::sd(data)
  lower = mean(data) - 3*stats::sd(data)

  n = length(data)
  observation = 1:n
  df = data.frame("data" = data, "observation" = observation)


  ggplot2::ggplot(data = df, ggplot2::aes(x=observation, y = data)) +
    ggplot2::geom_point() +
    ggplot2::geom_line() +
    ggplot2::geom_hline(yintercept=lower) +
    ggplot2::geom_hline(yintercept=upper) +
    ggplot2::geom_hline(yintercept=mean(data)) +
    ggplot2::geom_point(ggplot2::aes(x = observation , y = data , colour = (data > upper | data < lower))) +
    ggplot2::scale_colour_discrete("Extraordinary Event") +
    ggplot2::labs(title = "Extraordinary Event Graph", y = paste(y_name))
}
