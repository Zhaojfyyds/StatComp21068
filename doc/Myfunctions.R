## ---- eval=FALSE--------------------------------------------------------------
#  BGDR <- function(x, y, theta, step, maxiter = 10000, epsilon = 1e-4){
#      x1 <- as.matrix(x)
#      m <- nrow(x1)
#      x0 <- rep(1, m)
#      X <- cbind(x0, x1)
#      n <- ncol(X)
#      beta <- matrix(theta, n, 1)
#      iter <- 0
#  
#      while(iter < maxiter){
#          grad <- t(X) %*% (X %*% beta - y) / m
#          beta_old <- beta
#          beta <- beta - step * grad
#          cost <- function(beta, X, y){
#              return(sum((y - X %*% beta)^2)/(2 * m))
#          }
#          if(abs(cost(beta, X, y)-cost(beta_old, X, y)) < epsilon){
#              break
#          }
#          iter <- iter + 1
#      }
#      beta <- as.numeric(beta)
#      covariant <- c('x0', 'x1')
#      result <- cbind(covariant, beta)
#      return(result)
#  }

## ---- eval=FALSE--------------------------------------------------------------
#  SGDR <- function(x, y, theta, step, maxiter = 10000, epsilon = 1e-4){
#      x1 <- as.matrix(x)
#      m <- nrow(x1)
#      x0 <- rep(1, m)
#      X <- cbind(x0, x1)
#      n <- ncol(X)
#      beta <- matrix(theta, n, 1)
#      iter <- k <- 0
#  
#      while(iter<maxiter){
#          iter<-iter+1
#          rand_i = ceiling(runif(1, 0, m))
#          Xi <- t(X[rand_i,])
#          yi <- y[rand_i]
#          grad <- t((Xi %*% beta - yi) %*% Xi)
#          beta_old <- beta
#          beta <- beta - step * grad
#          cost <- function(beta, X, y){
#              return(sum((y - X %*% beta)^2)/(2 * m))
#          }
#          if(abs(cost(beta, X, y)-cost(beta_old, X, y)) < epsilon){
#              break
#          }
#      }
#      beta <- as.numeric(beta)
#      covariant <- c('x0', 'x1')
#      result <- cbind(covariant, beta)
#      return(result)
#  }

## ---- eval=FALSE--------------------------------------------------------------
#  MBGDR <- function(x, y, theta, step ,batch = 10, maxiter = 10000, epsilon = 1e-4){
#      x1 <- as.matrix(x)
#      m <- nrow(x1)
#      x0 <- rep(1, m)
#      X <- cbind(x0, x1)
#      n <- ncol(X)
#      beta <- matrix(theta, n, 1)
#      size <- m %/% batch
#      iter <- k <- 0
#      while(iter<maxiter){
#          iter<-iter+1
#          group <- sample(1:m, size)
#          Xb <- X[group, ]
#          yb <- y[group]
#          grad <- t(Xb) %*% (Xb %*% beta - yb) / size
#          beta_old <- beta
#          beta <- beta - step * grad
#          cost <- function(beta, X, y){
#              return(sum((y - X %*% beta)^2)/(2 * m))
#          }
#          if(abs(cost(beta, X, y)-cost(beta_old, X, y)) < epsilon){
#              break
#          }
#      }
#      beta <- as.numeric(beta)
#      covariant <- c('x0', 'x1')
#      result <- cbind(covariant, beta)
#      return(result)
#  }

## -----------------------------------------------------------------------------
library(StatComp21068)
library(microbenchmark)

n <- 5000
x <- runif(n, 0, 2)
y <- x * 4. + 5. + runif(n)
theta <- c(0,0)

BGDR(x, y, theta, 0.1)
SGDR(x, y, theta, 0.1)
MBGDR(x, y, theta, 0.1)

ts <- microbenchmark(MBGD=MBGDR(x, y, theta, 0.1),SGD=SGDR(x, y, theta, 0.1),BGD=BGDR(x, y, theta, 0.1))
summary(ts)[,c(1,3,5,6)]

## -----------------------------------------------------------------------------
fit <- lm(y~x)
summary(fit)

