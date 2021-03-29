proportion_norm = function(data){
  # This function takes in data and returns whether or not it passes the proportion tests of normality
  # The test outputs which tests (upper and lower) were passed
  # data: a data frame

  var_name = c()
  test_result_1 = c()
  test_result_2 = c()
  name = names(data)
  n = length(data[,1])

  for(i in 1:length(name)){
    var_name = c(var_name , name[i])
    sample_data = data[1,i]

    #if(class(sample_data) == "numeric" || class(sample_data) == "integer"){
      curr_variable = data[,1]
      standard_deviation = sd(curr_variable)
      m = mean(curr_variable)

      lower_1 = m - standard_deviation
      upper_1 = m + standard_deviation
      interval_1 = curr_variable[curr_variable >= lower_1 & curr_variable <= upper_1]
      prop_1 = length(interval_1)/length(curr_variable)

      lower_2 = m - 2*standard_deviation
      upper_2 = m + 2*standard_deviation
      interval_2 = curr_variable[curr_variable >= lower_2 & curr_variable <= upper_2]
      prop_2 = length(interval_2)/length(curr_variable)


      lower = 1.396/sqrt(n)
      upper = .628/sqrt(n)
      crit_1 = abs(prop_1 - .683)
      crit_2 = abs(prop_2 - .954)

      # test if passed or failed
      if(crit_1 > lower){
        result_1 = "failed"
      }
      else{
        result_1 = "passed"
      }

      if(crit_2 > upper){
        result_2 = "failed"
      }
      else{
        result_2 = "passed"
      }
      test_result_1 = c(test_result_1, result_1)
      test_result_2 = c(test_result_2, result_2)
    }
  #}
  results = list(variables = var_name, result_1 = test_result_1, result_2 = test_result_2)
  return(results)
}

qq_measurements = function(data){# vector of data
  # inputs in data and gives out the quantile measurements for the univariate case
  # a dataframe of data
  obs = sort(data)
  qs = qnorm((1:length(data) - .5)/length(data), 0, 1)
  list("qs" = qs, "observations" = obs)
}


r_q = function(obs, qs){

  q_m = mean(qs)
  x_m = mean(obs)

  qs = qs - q_m
  obs = obs - x_m

  num = sum(qs*obs)


  denom  = sqrt(sum(obs^2)) * sqrt(sum(qs^2))
  denom
  num/denom
}

z_mat <- function(data){
  m = colMeans(data)
  co = cov(data)
  p = dim(data)[2]
  df_mat = as.matrix(data)
  col_names = c()
  z_mat = matrix()
  zs = c()
  for(column in 1:dim(df_mat)[2]){
    for(row in 1:dim(df_mat)[1]){
      z = round((data[row, column] - m[column])/sqrt(co[column,column]),4)
      zs = c(zs, z)
    }
    col_names = c(col_names, paste("z", column, sep=""))
  }
  df = as.data.frame(matrix(zs, ncol=p, byrow = FALSE))
  names(df) = col_names
  df
}


box_cox = function(x, lambda = 1){
  if (lambda == 0){
    x = log(x)
  }
  else{
    x = ((x^(lambda))-1)/lambda
  }
  x
}

boxcox_max = function(lambda, x){
  n = length(x)
  box_x = box_cox(x, lambda)
  mean_l = sum(box_x)/n

  (-n/2)*log(sum((box_x - mean_l)^2)/n) + (lambda-1)*sum(log(x))
}


drop_data_chi = function(data, point){
  n = length(data[,1])
  mat = as.matrix(data)
  col_names = colnames(data)

  point = round(point,4)

  a = round(c(mat[,2]),4)

  row = match(point, a)
  row

  mat = mat[-row,]
  df = as.data.frame(mat)
  names(dt) = col_names
  qs = qnorm((1:n- .5)/n, 0, 1)
  d = mat[,1]


  list("d" = d, "quantiles" = qs)
}






