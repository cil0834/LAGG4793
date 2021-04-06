#' chisq_plot
#'
#'@description this function takes in a data frame and make a chi-squared quantile plot. The function also runs a shapiro-wilk test
#'on the data that has been transformed and determines if the data is normal
#'
#' @param data A data frame
#'
#' @export
#'
#' @examples
#' data = LAGG4793::project_data
#' chisq_plot(data)
chisq_plot = function(data){
  d = c()
  n = dim(data)[1]
  co = solve(stats::cov(data))
  m = colMeans(data)
  p = dim(data)[2]
  for(i in 1:n){
    vec = as.matrix((data[i,]-m))
    vec = vec%*%co%*%t(vec)
    d = c(d, vec)
  }
  d = sort(d)
  qs = stats::qnorm((1:n- .5)/n, 0, 1)
  p_value = stats::shapiro.test(d)
  p_value = round(p_value$p.value,4)
  df = data.frame("d" = d, "qs" = qs)
  g = ggplot2::ggplot(data = df, ggplot2::aes(x = qs, y = d)) +
    ggplot2::geom_point() +
    ggplot2::geom_smooth(method='lm', formula= y~x) +
    ggplot2::labs(title="Chi-sq Plot") +
    ggplot2::geom_text(x=0, y=mean(d) + stats::sd(d), label=paste("P-value", p_value))
  print(g)
}
