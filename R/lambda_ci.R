#' lambda_ci
#'
#' @description This function takes in data, the number of variances interestd in looking at, and an alpha
#' level to make a (100-alpha)% bonferroni and Anderson and Girshick confidence interval for each variance
#'
#' @param data- the data
#' @param dimensions- the number of variance that are going to be investigated
#' @param alpha- the alpha level
#'
#' @return a list containing the two confidence intervals
#' @export
#'
#' @examples
#' df = LAGG4793::t.84
#' bonferroni_lambda_ci(df, dimensions = 3, alpha = 0.1)
lambda_ci = function(data, dimensions = 1, alpha = .05)
{
  S = cov(data)
  p = dim(S)[2]
  n = dim(data)[1]

  if(dimensions >= p)
  {
    dimensions = p
  }

  eigens = eigen(S)

  values = eigens$values[1:dimensions]


  z_val = qnorm(alpha/(2*p), lower.tail = FALSE)
  left_denom = 1+(z_val*sqrt(2/n))
  right_denom = 1-(z_val*sqrt(2/n))


  b_left = values/left_denom
  b_right = values/right_denom



  z_val = qnorm(alpha/(2), lower.tail = FALSE)
  left_denom = 1+(z_val*sqrt(2/n))
  right_denom = 1-(z_val*sqrt(2/n))


  ag_left = values/left_denom
  ag_right = values/right_denom

  confidence_interval = list("bon_left" = b_left, "bon_right" = b_right, "ag_left" = ag_left, "ag_right" = ag_right)
  return(confidence_interval)
}
