#' Bivariate Check
#'
#' @description This function takes in multivariate data and an alpha level and makes confidence ellipses depending on the alpha chosen.
#' Additonally, it returns a list of proportions that lie within the list.
#'
#'
#' @param data A dataframe
#' @param alpha The alpha level you want your ellipse to be
#'
#' @return a list of the proportion of points that lies within the ellipse.
#' @export
#'
#' @examples
#' data = LAGG4793::project_data
#' bivariate_check(data, 0.5)
bivariate_check = function(data, alpha){ # data: a data frame, #alpha: an alpha level
  props = c()
  for(i in 1:length(colnames(data))){
    for(j in 1:length(colnames(data))){
      if(i < j){
        x = as.numeric(data[,i])
        y = as.numeric(data[,j])
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
          ggplot2::labs(title= paste(colnames(data)[i], "vs.", colnames(data)[j], "Ellipse"), y=colnames(data)[j], x=colnames(data)[i]) +
          ggplot2::scale_colour_discrete("Inside Ellipse")
        print(p)
        prop = length(which(chis < chi_sq))/n
        props = c(props, prop)
      }
    }
  }
  return(props)
}
