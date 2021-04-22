#' PCA_decomposition
#'
#' @description This function takes in data and returns a list that contains the covariance matrix for the input data,
#' a matrix contianing the components of each principle component, the total sample variance, and the correlation matrix
#' between the x components and the principle components
#'
#' @param df- the data
#'
#' @return a list that contains the covariance matrix for the input data,
#' a matrix contianing the components of each principle component, the total sample variance, and the correlation matrix
#' between the x components and the principle components
#' @export
#'
#' @examples
#' df = LAGG4793::t.84
#' PCA_decomposition(df)
PCA_decomposition = function(df){
  S = cov(df)
  p = dim(S)[1]
  eigens_sig = eigen(S)
  values_sig = eigens_sig$values
  vectors_sig = eigens_sig$vectors

  pc_sig = c()
  num_p = dim(S)[2]
  for (i in 1:dim(S)[2]){
    pc_sig = c(pc_sig, vectors_sig[,i])
  }
  pc_sig_df = as.data.frame(matrix(pc_sig, nrow = num_p, byrow = FALSE))

  samp_var = sum(values_sig)

  corr_sig = c()
  for(i in 1:p){
    for(k in 1:p){
      e_val = sqrt(values_sig[i])
      e_vec = vectors_sig[i,k]
      standard_deviation = sqrt(S[k,k])
      P = e_val*e_vec/standard_deviation
      corr_sig = c(corr_sig, P)
    }
  }



  corr_sig_mat = matrix(corr_sig, nrow = num_p, byrow = TRUE)

  output = list("covariance" = S, "PCAs" = pc_sig_df, "tot_variance" =  samp_var, "correlation_matrix" = corr_sig_mat,
                "sample variances" = values_sig)
  return(output)
}
