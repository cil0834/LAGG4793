rd <- function(X, coln = c(2,3)){ # compare cols 2 and 3

  Xn <- X[,coln] # data to summarize
  n <- dim(X)[1] # number of rows
  cr <- 1:n # initial vector to hold correlations

  for(i in 1:n){
    cr[i] <- cor(Xn[-i,])[1,2] # correlation proper
  }

  cr
}


my.tilde=function(x1,x2,theta){ # x1,2 vectors, theta scalar
  x1t= x1*cos(theta) + x2*sin(theta)
  x2t=-x1*sin(theta)+x2*cos(theta)
  list(x1t=x1t,x2t=x2t)
}

my.newt=function(s11, s22, s12){
  #f the function to be zeroed
  myfun=function(x) s12*(cos(x)^2 - sin(x)^2) - (s11 - s22)*(cos(x)*sin(x))
  root = rootSolve::uniroot.all(myfun, interval = c(0, pi/2))
  root
}
