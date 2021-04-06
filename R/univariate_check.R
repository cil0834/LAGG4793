#' univariate check
#'
#' This funciton uses a shapiro-wilk test to see if each variable in a dataset is normal
#'
#' @param data a dataframe
#'
#' @return qq plots of each variable with their shapiro-wilk p-value on it
#' @export
#'
#' @examples
#' data = data.frame(rnorm(10))
#' LAGG4793::univariate_check(data)
univariate_check = function(data){ # a dataframe
  p_values = c()
  for(i in 1:length(colnames(data))){
    p_value = stats::shapiro.test(data[,i])
    p_value = round(p_value$p.value,4)
    p_values = c(p_values, p_value)
    p = ggpubr::ggqqplot(data[,i]) +
      ggplot2::labs(title = paste(colnames(data)[i],"univariate qq plot"), y = paste(colnames(data)[i])) +
      ggplot2::geom_text(x=0, y=mean(data[,i]) + stats::sd(data[,i]), label=paste("P-value", p_value))
    print(p)
  }
  return(p_values)
}
