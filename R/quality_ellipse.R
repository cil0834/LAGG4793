#' quality_ellipse
#'
#' Creates a quality ellipse based on two variables
#'
#' @param alpha the alpha level of the ellipse
#' @param y The vector for the y component of the ellipse
#' @param x The vector for the x component of the ellipse
#' @param y_names The name of the variable of the y component
#' @param x_names The name of the variable of the x component
#'
#' @return a (100-alpha)% ellipse
#' @export
#'
#' @examples
#' data = LAGG4793::project_data
#' x = data$Voltage
#' y = data$Current
#' quality_ellipse(0.5, y, x, "Current", "Voltage")
quality_ellipse = function(alpha, y, x, y_names, x_names){ # alpha: The alpha level for the ellipse, y: the y variable, x: the x variable
  dt = data.frame("x"=x, "y"=y)
  m = colMeans(dt)
  co_i = solve(stats::cov(dt))
  n = dim(dt)[1]
  p = dim(dt)[2]
  m_vec = as.matrix(dt)
  chis = c()

  for(k in 1:n){
    chi = t(m_vec[k,]-m)%*%co_i%*%(m_vec[k,] - m)
    chis = round(c(chis, chi),2)
  }

  chi_sq = stats::qchisq(alpha, 2, lower.tail = FALSE)
  mat = matrix(c(x, y), byrow = FALSE, ncol = 2)
  me = colMeans(mat)
  co = stats::cov(mat)
  e_val = eigen(co)$values
  e_vec = eigen(co)$vectors
  es = sqrt(e_val)
  major = es[1]*sqrt(chi_sq)
  minor = es[2]*sqrt(chi_sq)

  eigens = eigen(co)
  c = eigens$vectors[1,1]
  s = eigens$vectors[2,1]
  angle = atan(s/c)

  p = ggplot2::ggplot() +
    ggforce::geom_ellipse(ggplot2::aes(x0 = me[1], y0 = me[2], a = major, b = minor, angle = angle)) +
    ggplot2::geom_point(ggplot2::aes(x = as.numeric(x), y = as.numeric(y)), colour = "blue") +
    ggplot2::geom_point(ggplot2::aes(x = x, y = y, colour = chis < chi_sq)) +
    ggplot2::labs(title= paste(y_names, "vs.", x_names, "Ellipse"), y=y_names, x=x_names) +
    ggplot2::scale_colour_discrete("Inside Ellipse")

  print(p)
}
