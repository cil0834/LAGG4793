#' mybootpca
#'
#' @description This function takes in data and makes bootstrap estimates for eigen vectors and values and for Anderson and Girshick confidence intervals
#'
#' @param data- Data for boot strap
#' @param alpha- alpha level
#' @param iter- number of iterations
#' @param cov- covariance
#' @param p- eigen vector
#'
#' @export
#'
#' @return a list with the confidence intervals for Anderson and Girshick and the bootstrap confidence interval
#'
#' @examples
#' df = LAGG4793::project_data
#' df$X = NULL
#' LAGG4793::mybootpca(df, 0.05)
mybootpca = function(data, alpha, iter = 100, cov = TRUE, e_vec = 1)
{
  p = dim(data)[2]
  n = dim(data)[1]


  if(cov)
  {
    mat = cov(data)
    vals <-boot::boot(data,LAGG4793::lambda_estimation_cov, R = iter)
    vec <- boot::boot(data,LAGG4793::vec_estimation_cov, R = iter, p = e_vec)
    eig_vals = eigen(mat)$values
  }else{
    mat = stats::cov2cor(stats::cov(data))
    vals <-boot::boot(data,LAGG4793::lambda_estimation_cor, R = iter)
    vec <- boot::boot(data,LAGG4793::vec_estimation_cor, R = iter, p = e_vec)
    eig_vals = eigen(mat)$values
  }

  for(i in 1:p)
  {
    h <- graphics::hist(vals$t[,i], plot = FALSE)
    d <- h$density
    den <- d/max(d)
    graphics::hist(vals$t[,i], freq = TRUE,
         col = grDevices::rgb(0,den,1-den^2, 0.7),
         main = expression(paste("Bootstrap distribution of ", widehat(lambda)[i])), xlab = bquote(lambda~.(i)))
  }

  left = c()
  right = c()

  for(i in 1:p){
    ci <- stats::quantile(vals$t[,i], c(0.05/2, 1-0.05/2))
    left = c(left, ci[1])
    right = c(right, ci[2])
    print(paste("Confidence interval", "lambda", i,":", round(ci[1],4),round(ci[2],4)))
  }


  boot_ci = list("left" = left, "right" = right)

  d = matrix(vals$t, ncol = 1)
  splice = c()

  for(i in 1:p)
  {
    a = rep(i, length(d)/p)
    splice = c(splice, a)
  }
  da = matrix(c(vals$t,splice), ncol = 2, byrow = FALSE)
  da = as.data.frame(da)

  for(i in 1:p)
  {
    left = ((i-1)*length(d)/p + 1)
    right = (i*length(d)/p)
    df = dplyr::slice(da, left:right)
    g <- ggplot2::ggplot(data = df, ggplot2::aes(x=V2, y=V1)) +
      ggplot2::geom_violin() + ggplot2::labs(x = "Eigen value number", y = "Eigen value", title = paste("Eigen Value", i,  "Violin plot"))
    print(g)
  }


  d = matrix(vec$t, ncol = 1)
  splice = c()

  for(i in 1:p)
  {
    a = rep(i, length(d)/p)
    splice = c(splice, a)
  }
  da = matrix(c(vec$t, splice), ncol = 2, byrow = FALSE)
  da = as.data.frame(da)
  da$V2 <- as.factor(da$V2)
  g <- ggplot2::ggplot(data = da, ggplot2::aes(x=V2, y=V1)) +
    ggplot2::geom_violin() + ggplot2::labs(x = "Eigen vector component", y = "Eigen vector value", title = paste("eigen vector", e_vec))
  print(g)


  left = c()
  right = c()
  for( i in 1:p){
    ci <- stats::quantile(vec$t[,i], c(0.05/2, 1-0.05/2))
    left = c(left, round(ci[1],4))
    right = c(right, round(ci[2],4))
    print(paste("Confidence interval", "eigen_vector component", i,":", round(ci[1],4),round(ci[2],4)))
  }
  bootvec_ci = list("left" = left, "right" = right)

  z_val = stats::qnorm(alpha/2, lower.tail = FALSE)

  z_val = sqrt(2)/sqrt(n)*z_val

  left_ag = 1 + z_val
  right_ag = 1 - z_val

  z_val = stats::qnorm(0.05/2, lower.tail = FALSE)

  z_val = sqrt(2)/sqrt(n)*z_val

  left = round(eig_vals/(1 + z_val),4)
  right = round(eig_vals/(1 - z_val),4)

  A_G = list("left" = left, "right" = right)


  confidence_intervals = list("AG_ci" = A_G, "Boot_ci" = boot_ci, "Bootvec_ci" = bootvec_ci)
  return(confidence_intervals)
}

