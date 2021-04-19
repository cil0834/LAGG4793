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
    mat = cov2cor(cov(data))
    vals <-boot::boot(data,LAGG4793::lambda_estimation_cor, R = iter)
    vec <- boot::boot(data,LAGG4793::vec_estimation_cor, R = iter, p = e_vec)
    eig_vals = eigen(mat)$values
  }

  for(i in 1:p)
  {
    h <- hist(vals$t[,i], plot = FALSE)
    d <- h$density
    den <- d/max(d)
    hist(vals$t[,i], freq = TRUE,
         col = rgb(0,den,1-den^2, 0.7),
         main = expression(paste("Bootstrap distribution of ", widehat(lambda)[i])), xlab = bquote(lambda~.(i)))
  }

  left = c()
  right = c()

  for(i in 1:p){
    ci <- quantile(vals$t[,i], c(0.05/2, 1-0.05/2))
    left = c(left, ci[1])
    right = c(right, ci[2])
    print(paste("Confidence interval", "lambda", i,":", round(ci[1],4),round(ci[2],4)))
  }


  boot_ci = list("left" = left, "right" = right)


  val <- rep(1:p, iter*(rep(1,p)))
  val <- matrix(val,nr = iter, nc = 3, byrow = FALSE)
  boxplot(vals$t ~ val,
          xlab = expression(lambda),
          ylab = "size of lambda")


  cat <- rep(1:p, iter*(rep(1,p)))
  cat <- matrix(cat,nr = iter, nc = 3, byrow = FALSE)
  cat_df = as.data.frame(cat)

  boxplot(vec$t  ~ cat,
          xlab = "eigen vector component",
          ylab = "size of eigenvector component")


  vec <- boot::boot(df,LAGG4793::vec_estimation_cov, R = iter, p = e_vec)
  d = matrix(vec$t, ncol = 1)
  splice = c()

  for(i in 1:p)
  {
    a = rep(i, length(d)/p)
    splice = c(splice, a)
  }
  da = matrix(c(vec$t,splice), ncol = 2, byrow = FALSE)
  da = as.data.frame(da)


  left = c()
  right = c()
  for( i in 1:p){
    ci <- quantile(vec$t[,i], c(0.05/2, 1-0.05/2))
    left = c(left, round(ci[1],4))
    right = c(right, round(ci[2],4))
    print(paste("Confidence interval", "eigen_vector component", i,":", round(ci[1],4),round(ci[2],4)))
  }
  bootvec_ci = list("left" = left, "right" = right)

  z_val = qnorm(alpha/2, lower.tail = FALSE)

  z_val = sqrt(2)/sqrt(n)*z_val

  left_ag = 1 + z_val
  right_ag = 1 - z_val

  z_val = qnorm(0.05/2, lower.tail = FALSE)

  z_val = sqrt(2)/sqrt(n)*z_val

  left = round(eig_vals/(1 + z_val),4)
  right = round(eig_vals/(1 - z_val),4)

  A_G = list("left" = left, "right" = right)


  confidence_intervals = list("AG_ci" = A_G, "Boot_ci" = boot_ci, "Bootvec_ci" = bootvec_ci)
  return(confidence_intervals)
}

