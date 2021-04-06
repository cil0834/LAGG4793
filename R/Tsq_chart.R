#' Tsq_chart
#'
#'@description This function takes in data and plots a T-squared plot
#'
#' @param data a dataframe
#'
#' @export
#'
#' @examples
#' data = LAGG4793::project_data
#' Tsq_chart(data)
Tsq_chart = function(data){ # a dataframe
  n = dim(data)[1]
  p = dim(data)[2]
  m = colMeans(data)
  co_i = solve(stats::cov(data))
  ucll = stats::qchisq(0.05, p, lower.tail = FALSE)
  uclu = stats::qchisq(0.01, p, lower.tail = FALSE)

  m_vec = as.matrix(data)
  tsqs = c()

  for(k in 1:n){
    tsq = t(m_vec[k,]-m)%*%co_i%*%(m_vec[k,] - m)
    tsqs = round(c(tsqs, tsq),2)
  }


  observation = 1:n
  df = data.frame("observation" = observation, "Tsq" = tsqs)



  g = ggplot2::ggplot(data = df, ggplot2::aes(x=observation, y = Tsq)) +
    ggplot2::geom_point() +
    ggplot2::geom_hline(yintercept=ucll, linetype="dashed") +
    ggplot2::geom_hline(yintercept=uclu) +
    ggplot2::geom_text(x=max(observation)*.75, y=stats::qchisq(0.04, p, lower.tail = FALSE), label=paste("95% limit")) +
    ggplot2::geom_text(x=max(observation)*.75, y=stats::qchisq(0.0075, p, lower.tail = FALSE), label=paste("99% limit")) +
    ggplot2::ylim(0, stats::qchisq(0.007, p, lower.tail = FALSE)) +
    ggplot2::labs(title = "T-squared Plot")
  print(g)
}
