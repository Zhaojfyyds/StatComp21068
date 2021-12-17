#' @title A function to compute Coefficients of the unary linear regression equation by Batch Gradient Descent approach with fixed step
#' @description  A function to compute Coefficients of the unary linear regression equation by Batch Gradient Descent approach with fixed step
#' @param x the covariate
#' @param y the response variable
#' @param theta the initial value of beta
#' @param maxiter the max number of iteration
#' @param epsilon the end condition
#' @param step the learning rate
#' @return the estimation
#' @examples 
#' \dontrun{
#' n <- 5000
#' x <- runif(n, 0, 2)
#' y <- x * 4. + 5. + runif(n)
#' BGD(x, y, theta, 0.1)
#' }
#' @importFrom stats runif
#' @export
BGDR <- function(x, y, theta, step, maxiter = 10000, epsilon = 1e-4){
  x1 <- as.matrix(x)
  m <- nrow(x1)
  x0 <- rep(1, m)
  X <- cbind(x0, x1)
  n <- ncol(X)
  beta <- matrix(theta, n, 1)
  iter <- 0
  
  while(iter < maxiter){
    grad <- t(X) %*% (X %*% beta - y) / m  
    beta_old <- beta
    beta <- beta - step * grad
    cost <- function(beta, X, y){
      return(sum((y - X %*% beta)^2)/(2 * m))
    }
    if(abs(cost(beta, X, y)-cost(beta_old, X, y)) < epsilon){
      break
    }
    iter <- iter + 1
  }
  beta <- as.numeric(beta)
  covariant <- c('x0', 'x1')
  result <- cbind(covariant, beta)
  return(result)
}




#' @title A function to compute Coefficients of the unary linear regression equation by Stochastic Gradient Descent approach with fixed step
#' @description  A function to compute Coefficients of the unary linear regression equation by Stochastic Gradient Descent approach with fixed step
#' @param x the covariate
#' @param y the response variable
#' @param theta the initial value of beta
#' @param maxiter the max number of iteration
#' @param epsilon the end condition
#' @param step the learning rate
#' @return the estimation
#' @examples 
#' \dontrun{
#' SGD(x, y, theta, 0.1)
#' }
#' @export
SGDR <- function(x, y, theta, step, maxiter = 10000, epsilon = 1e-4){
  x1 <- as.matrix(x)
  m <- nrow(x1)
  x0 <- rep(1, m)
  X <- cbind(x0, x1)
  n <- ncol(X)
  beta <- matrix(theta, n, 1)
  iter <- k <- 0
  
  while(iter<maxiter){
    iter<-iter+1
    rand_i = ceiling(runif(1, 0, m))
    Xi <- t(X[rand_i,])
    yi <- y[rand_i]
    grad <- t((Xi %*% beta - yi) %*% Xi)
    beta_old <- beta
    beta <- beta - step * grad
    cost <- function(beta, X, y){
      return(sum((y - X %*% beta)^2)/(2 * m))
    }
    if(abs(cost(beta, X, y)-cost(beta_old, X, y)) < epsilon){
      break
    }
  }
  beta <- as.numeric(beta)
  covariant <- c('x0', 'x1')
  result <- cbind(covariant, beta)
  return(result)
}



#' @title A function to compute Coefficients of the unary linear regression equation by Mini-batch Gradient Descent approach with fixed step
#' @description  A function to compute Coefficients of the unary linear regression equation by Mini-batch Gradient Descent approach with fixed step
#' @param x the covariate
#' @param y the response variable
#' @param theta the initial value of beta
#' @param maxiter the max number of iteration
#' @param epsilon the end condition
#' @param step the learning rate
#' @param batch the batch size
#' @return the estimation
#' @examples 
#' \dontrun{
#' MBGD(x, y, theta, 0.1)
#' }
#' @export
MBGDR <- function(x, y, theta, step ,batch = 10, maxiter = 10000, epsilon = 1e-4){
  x1 <- as.matrix(x)
  m <- nrow(x1)
  x0 <- rep(1, m)
  X <- cbind(x0, x1)
  n <- ncol(X)
  beta <- matrix(theta, n, 1)
  size <- m %/% batch
  iter <- k <- 0
  while(iter<maxiter){
    iter<-iter+1
    group <- sample(1:m, size)
    Xb <- X[group, ]
    yb <- y[group]
    grad <- t(Xb) %*% (Xb %*% beta - yb) / size 
    beta_old <- beta
    beta <- beta - step * grad
    cost <- function(beta, X, y){
      return(sum((y - X %*% beta)^2)/(2 * m))
    }
    if(abs(cost(beta, X, y)-cost(beta_old, X, y)) < epsilon){
      break
    }
  }
  beta <- as.numeric(beta)
  covariant <- c('x0', 'x1')
  result <- cbind(covariant, beta)
  return(result)
}


