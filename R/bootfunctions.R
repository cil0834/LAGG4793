#' lambda_estimation_cov
#'
#' @param data- the data
#' @param indices- indices for bootstrap
#'
#' @return- bootstrap lambda value
#' @export
lambda_estimation_cov <- function(data, indices){
  S <- stats::cov(data[indices,])
  E <- eigen(S)

  lambda <-E$values
  return(c(lambda))
}

#' lambda_estimation_cor
#'
#' @param data- the data
#' @param indices- indices for bootstrap
#'
#' @return- bootstrap lambda value
#' @export
lambda_estimation_cor <- function(data, indices){
  S <- stats::cov2cor(stats::cov(data[indices,]))
  E <- eigen(S)

  lambda <-E$values
  return(c(lambda))
}

#' vec_estimation_cov
#'
#' @param data- the data
#' @param indices- indices for bootstrap
#'
#' @return- bootstrap eigen vector value
#' @export
vec_estimation_cov <- function(data, indices, p){
  S <- stats::cov(data[indices,])
  E <- eigen(S)

  lambda <-E$vectors[,p]
  return(c(lambda))
}

#' vec_estimation_cor
#'
#' @param data- the data
#' @param indices- indices for bootstrap
#'
#' @return- bootstrap eigen vector value
#' @export
vec_estimation_cor <- function(data, indices, p){
  S <- stats::cov2cor(stats::cov(data[indices,]))
  E <- eigen(S)

  lambda <-E$vectors[,p]
  return(c(lambda))
}
