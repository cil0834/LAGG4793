library(ggplot2)
library(ggforce)
#' t-test
#'
#' @Description This function runs a t-test to see if a given set of means for a list of variables is possible given certain data.
#'
#' @param data- a data frame
#' @param means the means that are being tested
#' @param alpha - the alpha level we are testing at
#'
#' @return The function returns a list of various information about the data and about the results of the test.
#' @import ggplot2, ggforce
#'
#' @examples
#' t_test(data, means, alpha = 0.1)
t_test = function(data, means, alpha=0.05){# data: 2x2 data matrix means: length 2 vector of means alpha: confidence level
  library(ggplot2)
  library(ggforce)
  n = dim(data)[1]
  p = dim(data)[2]
  S = cov(data)
  S_i = solve(S)
  x_bar = as.matrix(colMeans(data))
  csq = n*t((x_bar - means))%*%S_i%*%(x_bar-means)

  df1 = dim(ovens.df)[2]
  df2 = n - dim(ovens.df)[2]
  f = qf(0.05, df1, df2, lower.tail = FALSE)
  crit_value = (df1*(n-1)/df2)*f

  eigens = eigen(S)
  values = eigens$values
  vectors = eigens$vectors
  val_1 = eigens$values[1]
  val_2 = eigens$values[2]
  vec_1 = eigens$vectors[,1]
  vec_2 = eigens$vectors[,2]

  major_lengths = c()
  minor_lengths = c()
  ratios = c()

  for(i in 1:length(means)){
    for(j in 1:length(means)){
      if (i < j){
        val_1 = eigens$values[i]
        val_2 = eigens$values[j]
        vec_1 = eigens$vectors[,i]
        vec_2 = eigens$vectors[,j]
        half_1 = sqrt(df1*(n-1)*f/(n*(n-df1)))*sqrt(val_1)
        half_2 = sqrt(df1*(n-1)*f/(n*(n-df1)))*sqrt(val_2)
        major_lengths = c(major_lengths, half_1)
        minor_lengths = c(minor_lengths, half_2)
        ratio = half_1/half_2
        ratios = c(ratios, ratio)
        c = eigens$vectors[1,1]
        s = eigens$vectors[2,1]
        angle = atan(s/c)

        g = ggplot() + geom_ellipse(aes(x0 = x_bar[i], y0 = x_bar[j], a = half_1, b = half_2, angle = angle))
        g = g + labs(x = "x", y = "y", title = "Confidence Ellipse")
        print(g)
      }
    }
  }


  if (csq <= crit_value){
    test = "accpted"
  }
  else{
    test = "rejected"
  }
  invisible(list(Test_Result =test, c_squared = csq, critical_value = crit_value, eigen_vectors = eigens$vectors, eigen_values = eigens$values,
                 major_lengths = major_lengths, minor_lengths = minor_lengths, axis_ratio = ratio))
}

