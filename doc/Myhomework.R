## -----------------------------------------------------------------------------
plot(1:10)

## -----------------------------------------------------------------------------
x<-1:10;
y<-x^2;
lmr<-lm(y~x);
co<-summary(lmr)$coefficients;
print(co);

## -----------------------------------------------------------------------------
knitr::kable(co,digits=2,align = 'c')

## -----------------------------------------------------------------------------
Rayleigh<- function(n,σ){
r<- sqrt((-2)*(σ^2)*log(1-runif(n)))
return (r)
}

## -----------------------------------------------------------------------------
σ=1
x<-Rayleigh(1000,σ)
hist(x, prob = TRUE, main = "σ=1")
y=seq(0,10,.01)
lines(y,(y/σ^2)*exp(-(y^2)/(2*σ^2)) )

## -----------------------------------------------------------------------------
σ=2
x<-Rayleigh(1000,σ)
hist(x, prob = TRUE, main = "σ=2")
y=seq(0,10,.01)
lines(y,(y/σ^2)*exp(-(y^2)/(2*σ^2)) )

## -----------------------------------------------------------------------------
library(MASS)
loc.mix <- function(n, p, mu1, mu2, Sigma) {
n1 <- rbinom(1, size = n, prob = p)
n2 <- n - n1
x1 <- mvrnorm(n1, mu = mu1, Sigma)
x2 <- mvrnorm(n2, mu = mu2, Sigma)
X <- rbind(x1, x2) 
return(X[sample(1:n), ]) 
}

## -----------------------------------------------------------------------------
x <- loc.mix(1000, .75, 0, 3, Sigma =1)
r <- range(x) * 1.2
hist(x)

## -----------------------------------------------------------------------------
x1<- loc.mix(1000, .5, 0, 3, Sigma =1)
x2<- loc.mix(1000, .25, 0, 3, Sigma =1)
x3<- loc.mix(1000, .95, 0, 3, Sigma =1)
x4<- loc.mix(1000, .05, 0, 3, Sigma =1)
#par(mfrow = c(2, 2))
hist(x1);hist(x2)
hist(x3);hist(x4)
par(mfrow = c(1, 1))

## -----------------------------------------------------------------------------
PG<-function(n,r,beta,lambda,t){   
     Y<-matrix(0,n,1)
     for (i in 1:n) {
           k=rpois(1,t*lambda)
           X<-rgamma(k,shape=r,rate=beta)
           Y[i,]<-sum(X)
     }
     return(Y)
}

## -----------------------------------------------------------------------------
x=PG(1000,1,1,1,10)
mean(x);var(x)

## -----------------------------------------------------------------------------
x=PG(1000,2,2,2,10)
mean(x);var(x)

## -----------------------------------------------------------------------------
x<-seq(.1,.9,length=9)
u<-runif(10000)
cdf<-numeric(length(x))
for (i in 1:length(x)) {
      g<-30*x[i]^3*u^2*(1-x[i]*u)^2
      cdf[i]<-mean(g)
}
Phi<-pbeta(x,3,3)
print(round(rbind(x,cdf,Phi),3))

## -----------------------------------------------------------------------------
Rayleigh<- function(n,σ){
        u<- runif(n)
        X_a<- sqrt((-2)*(σ^2)*log(u))
        X<-sqrt((-2)*(σ^2)*log(1-u))
    return(list(X,X_a))
}
Y<-Rayleigh(100000,1)
Z<-matrix(unlist(Y),100000,2)
X<-Z[,1]
X_a<-Z[,2]

## -----------------------------------------------------------------------------
Rayleigh2<- function(n,σ){
t<- sqrt((-2)*(σ^2)*log(1-runif(n)))
return (t)
}
X1<-Rayleigh2(100000,1)
X2<-Rayleigh2(100000,1)

## -----------------------------------------------------------------------------
Y1<-var((X1+X2)/2)
Y2<-var((X+X_a)/2)
print((Y1-Y2)/Y1)

## -----------------------------------------------------------------------------
m<- 10000
g<-function(x){x^2/sqrt(2*pi)*exp(-x^2/2)*(x>1)}  


x<-rnorm(m,1.5,1)
g1<-g(x)/dnorm(x,1.5,1)
theta.hat1<-mean(g1)                                         


x<-rgamma(m,4.5,2.5)
g2<-g(x)/dgamma(x,4.5,2.5)
theta.hat2<-mean(g2)

print(c(theta.hat1,theta.hat2))                                
integrate(g,1,Inf)                                                   
print(c(sd(g1),sd(g2)))                                        

## -----------------------------------------------------------------------------
m<- 10000
f<-function(x){x^2/sqrt(2*pi)*exp(-x^2/2)*(x>1)}  

x<-rgamma(m,4.5,2.5)
f1<-f(x)/dgamma(x,4.5,2.5)
theta.hat1<-mean(f1)

print(c(theta.hat1,sd(f1)))                              
integrate(f,1,Inf)                                                   

## -----------------------------------------------------------------------------
set.seed(1000)
n<-20
alpha<-0.05
CI<-replicate(1000,expr={
      x<-rchisq(n,df=2)
      CI_1<-mean(x)+sqrt(var(x)/n)*qt(alpha/2,df=n-1)
      CI_2<-mean(x)-sqrt(var(x)/n)*qt(alpha/2,df=n-1)
      return(c(CI_1,CI_2))
})
mean(CI[1,]<2&CI[2,]>2)

## -----------------------------------------------------------------------------
set.seed(100)
n<-20;alpha<- .05;mu0<-1
m<-10000
p_1<-p_2<-p_3<-numeric(m)
for(j in 1:m){
x<-rchisq(n,df=1)
ttest_1<-t.test(x,alternative="two.sided",mu=mu0)
p_1[j]<-ttest_1$p.value
y<-runif(n,0,2)
ttest_2<-t.test(y,alternative="two.sided",mu=mu0)
p_2[j]<-ttest_2$p.value
z<-rexp(n,1)
ttest_3<-t.test(z,alternative="two.sided",mu=mu0)
p_3[j]<-ttest_3$p.value
}
p_1.hat<-mean(p_1<alpha)
p_2.hat<-mean(p_2<alpha)
p_3.hat<-mean(p_3<alpha)
print(c(p_1.hat,p_2.hat,p_3.hat))

## -----------------------------------------------------------------------------
library(MASS)

set.seed(21068)
d<-3
mu=c(0,0,0)
sigma=matrix(c(1,0,0,0,4,0,0,0,9),3,3)
m=1000
n=c(10, 20, 30, 50, 100, 500)
cv<-qchisq(0.95,d*(d+1)*(d+2)/6)  #卡方分布的下0.95分位点

sk<-function(x){
    n<-nrow(x)
    d<-ncol(x)
    xcen<-matrix(0,n,d)
    for(i in 1:d){
       xcen[,i]<-x[,i]-mean(x[,i])
    }
    Sigma.hat<-t(xcen)%*%xcen/n    #Sigma的估计
    y<-xcen%*%solve(Sigma.hat)%*%t(xcen)
    z<-sum(colSums(y^{3}))/(n*n)   #b1,d 的值
    test<-n*z/6                    #nb/6 
    return(test)
}                                   
                                     
p.reject=numeric(length(n))
for(i in 1:length(n)) {
    sktests<-numeric(m)
    for (j in 1:m) {
       x=mvrnorm(n[i],mu,sigma)
       sktests[j]<-as.integer(abs(sk(x))>=cv)  #检验决策 比较二者大小
    }
    p.reject[i]<-mean(sktests)  
}

rbind(n,cv,p.reject)

## -----------------------------------------------------------------------------
library(MASS)

set.seed(21068)
alpha <- .1
n <- 30;d<-2
m <- 2000;mu<-c(0,0)
epsilon <- c(seq(0, .15, .01), seq(.15, 1, .05))
N <- length(epsilon)
pwr <- numeric(N)
cv <- qchisq(.9,d*(d+1)*(d+2)/6)
x <- matrix(0,n,d)

for (j in 1:N) { 
   e <- epsilon[j]
   sktests <- numeric(m)
   for (i in 1:m) { 
      sigma <- sample(c(1, 100), replace = TRUE,size = n, prob = c(1-e, e))  
      for (k in 1:n) {
         x[k,]=mvrnorm(1, mu, sigma[k]*diag(2))
      }   
         sktests[i] <- as.integer(abs(sk(x))>= cv )
    } 
    pwr[j] <- mean(sktests)
}

plot(epsilon, pwr, type = "b",
        xlab = bquote(epsilon), ylim = c(0,1))
abline(h = .1, lty = 3)
se <- sqrt(pwr * (1-pwr) / m) 
lines(epsilon, pwr+se, lty = 3)
lines(epsilon, pwr-se, lty = 3)

## -----------------------------------------------------------------------------
library(bootstrap)
data(scor,package = "bootstrap")

n <- nrow(scor)
X <- cov(scor)
Y <- eigen(X)$value
theta.hat <- Y[1]/sum(Y)

B <- 200
scor.b <- matrix(0, 88, 5)
theta.boot <- numeric(B)
for (b in 1:B) {
  i <- sample(1:n, size = n, replace = T)
  scor.b[,1] <- scor$mec[i]
  scor.b[,2] <- scor$vec[i]
  scor.b[,3] <- scor$alg[i]
  scor.b[,4] <- scor$ana[i]
  scor.b[,5] <- scor$sta[i]
  z <- eigen(cov(scor.b))$value
  theta.boot[b] <- z[1]/sum(z)
}

bias.boot <- mean(theta.boot) - theta.hat
se.boot <- sd(theta.boot)
print(c(bias.boot,se.boot))

## -----------------------------------------------------------------------------
library(bootstrap)
data(scor,package = "bootstrap")

n <- nrow(scor)
X <- cov(scor)
Y <- eigen(X)$value
theta.hat <- Y[1]/sum(Y)

theta.jack <- numeric(n)
for (i in 1:n) {
   Z <-   eigen(cov(scor[-i,]))$value
   theta.jack[i] <- Z[1]/sum(Z)
}

bias.jack <- (n-1)*(mean(theta.jack)-theta.hat)
se.jack <- sqrt((n-1) * mean((theta.jack - mean(theta.jack))^2)) 
print(c(bias.jack,se.jack))

## -----------------------------------------------------------------------------
library(boot)
data(scor,package = "bootstrap")

theta.b <- function(data,indices) {
  a <- data[indices,]
  b <- eigen(cov(a))$value
  b[1]/sum(b)
}

boot.obj <- boot(data = scor, statistic = theta.b, R=200)
ci <- boot.ci(boot.obj, type = c("perc", "bca"))
cat("Percentile CI:", paste0("(",paste(ci$percent[4:5], collapse=", "),")"), "\n")
cat("BCa CI:", paste0("(",paste(ci$bca[4:5], collapse=", "),")"), "\n") 

## -----------------------------------------------------------------------------
library(boot)
set.seed(123)
m <- 1000
n <- 100
sk <- 0

boot.sk <- function(data, indices) {
     d <- data[indices]
     n <- length(d)
     e <- sum((d-mean(d))^3)/n
     f <- (sum((d-mean(d))^2)/n)^(3/2)
     return(e/f)
}

ci.norm <- ci.basic <- ci.perc <- matrix(NA,m,2)
for (i in 1:m) {
   U <- rnorm(n, 0, 1)
   de <- boot(data = U, statistic = boot.sk, R = 1000)
   ci <- boot.ci(de, type = c("norm", "basic", "perc"))
   ci.norm[i,] <- ci$norm[2:3]
   ci.basic[i,] <- ci$basic[4:5]
   ci.perc[i,] <- ci$percent[4:5] 
}

rbind(distribution=c('norm', 'basic', 'perc'),
         ci_est=c(mean(ci.norm[,1] <= sk & ci.norm[,2] >= sk), 
            mean(ci.basic[,1] <= sk & ci.basic[,2] >= sk),
            mean(ci.perc[,1] <= sk & ci.perc[,2] >= sk)),
         ci_leftmiss=c(mean(ci.norm[,1] > sk), mean(ci.basic[,1] > sk), mean(ci.perc[,1] > sk)),
         ci_rightmiss=c(mean(ci.norm[,2] < sk), mean(ci.basic[,2] < sk), mean(ci.perc[,2] < sk)))

## -----------------------------------------------------------------------------
library(boot)
set.seed(123)
m <- 1000
n <- 200
sk <- sqrt(1.6)

boot.sk <- function(data, indices) {
     d <- data[indices]
     n <- length(d)
     e <- sum((d-mean(d))^3)/n
     f <- (sum((d-mean(d))^2)/n)^(3/2)
     return(e/f)
}

ci.norm <- ci.basic <- ci.perc <- matrix(NA,m,2)
for (i in 1:m) {
   U <- rchisq(n, 5)
   de <- boot(data = U, statistic = boot.sk, R = 1000)
   ci <- boot.ci(de, type = c("norm", "basic", "perc"))
   ci.norm[i,] <- ci$norm[2:3]
   ci.basic[i,] <- ci$basic[4:5]
   ci.perc[i,] <- ci$percent[4:5] 
}

rbind(distribution=c('norm', 'basic', 'perc'),
         ci_est=c(mean(ci.norm[,1] <= sk & ci.norm[,2] >= sk), 
            mean(ci.basic[,1] <= sk & ci.basic[,2] >= sk),
            mean(ci.perc[,1] <= sk & ci.perc[,2] >= sk)),
         ci_leftmiss=c(mean(ci.norm[,1] > sk), mean(ci.basic[,1] > sk), mean(ci.perc[,1] > sk)),
         ci_rightmiss=c(mean(ci.norm[,2] < sk), mean(ci.basic[,2] < sk), mean(ci.perc[,2] < sk)))

## -----------------------------------------------------------------------------
Cor <- function(z, ix) {
    x <- z[ , 1] 
    y <- z[ix, 2] 
   return(cor(x,y,method = "spearman"))
}

library(boot)
z <- as.matrix(iris[1:50, c(1,3)])
x <- z[, 1];y <- z[, 2]
boot.obj <- boot(data = z, statistic = Cor, R = 999, sim = "permutation")
tb <- c(boot.obj$t0, boot.obj$t)
cat('ASL =' ,mean(tb >= boot.obj$t0), 'p.value =', cor.test(x, y)$p.value)

## -----------------------------------------------------------------------------
library(energy)
library(Ball)
library(RANN)
library(boot)

Tn <- function(z, ix, sizes, k) {
    n1 <- sizes[1]
    n2 <- sizes[2]
    n <- n1 + n2
    if(is.vector(z)) z <- data.frame(z, 0);
    z <- z[ix, ];
    NN <- nn2(data=z, k=k+1) 
    block1 <- NN$nn.idx[1:n1, -1]
    block2 <- NN$nn.idx[(n1+1):n, -1]
    i1 <- sum(block1 < n1 + .5)
    i2 <- sum(block2 > n1+.5)
    (i1 + i2) / (k * n)
}

eqdist.nn <- function(z, sizes, k){
    boot.obj <- boot(data = z, statistic = Tn, R = R, sim = "permutation", sizes = sizes,k = k)
    ts <- c(boot.obj$t0, boot.obj$t)
    p.value <- mean(ts >= ts[1])
    list(statistic = ts[1], p.value = p.value)
}

m <- 100
k <- 3
p <- 2
n1 <- n2 <- 50
R<-200
n <- n1 + n2
N = c(n1, n2)
p_NN <- p_energy <- p_ball <- numeric(m)
alpha <- 0.1

for(i in 1:m) {
   x <- matrix(rnorm(n1*p, mean = 0,sd = 0.7), ncol=p)
   y <- matrix(rnorm(n2*p, mean = 0,sd = 0.5), ncol=p)
   z <- rbind(x,y)
   p_NN[i] <- eqdist.nn(z, N, k)$p.value
   p_energy[i] <- eqdist.etest(z, sizes = N, R = R)$p.value
   p_ball[i] <- bd.test(x = x, y = y, num.permutations = 999, seed = i * 12345)$p.value
}

cat('pow_NN = ', mean(p_NN < alpha), 'pow_energy = ', mean(p_energy < alpha), 'pow_ball = ', mean(p_ball < alpha))

## -----------------------------------------------------------------------------
library(energy)
library(Ball)
library(RANN)
library(boot)

Tn <- function(z, ix, sizes, k) {
    n1 <- sizes[1]
    n2 <- sizes[2]
    n <- n1 + n2
    if(is.vector(z)) z <- data.frame(z, 0);
    z <- z[ix, ];
    NN <- nn2(data=z, k=k+1) 
    block1 <- NN$nn.idx[1:n1, -1]
    block2 <- NN$nn.idx[(n1+1):n, -1]
    i1 <- sum(block1 < n1 + .5)
    i2 <- sum(block2 > n1+.5)
    (i1 + i2) / (k * n)
}

eqdist.nn <- function(z, sizes, k){
    boot.obj <- boot(data = z, statistic = Tn, R = R, sim = "permutation", sizes = sizes,k = k)
    ts <- c(boot.obj$t0, boot.obj$t)
    p.value <- mean(ts >= ts[1])
    list(statistic = ts[1], p.value = p.value)
}

m <- 100
k <- 3
p <- 2
n1 <- n2 <- 50
R<-200
n <- n1 + n2
N = c(n1, n2)
 p_NN <- p_energy <- p_ball <- numeric(m)
alpha <- 0.1

for(i in 1:m) {
   x <- matrix(rnorm(n1*p, mean = 0.1,sd = 0.55), ncol=p)
   y <- matrix(rnorm(n2*p, mean = 0.3,sd = 0.7), ncol=p)
   z <- rbind(x,y)
   p_NN[i] <- eqdist.nn(z, N, k)$p.value
   p_energy[i] <- eqdist.etest(z, sizes = N, R = R)$p.value
   p_ball[i] <- bd.test(x = x, y = y, num.permutations = 999, seed = i * 12345)$p.value
}

cat('pow_NN = ', mean(p_NN < alpha), 'pow_energy = ', mean(p_energy < alpha), 'pow_ball = ', mean(p_ball < alpha))

## -----------------------------------------------------------------------------
library(energy)
library(Ball)
library(RANN)
library(boot)

Tn <- function(z, ix, sizes, k) {
    n1 <- sizes[1]
    n2 <- sizes[2]
    n <- n1 + n2
    if(is.vector(z)) z <- data.frame(z, 0);
    z <- z[ix, ];
    NN <- nn2(data=z, k=k+1) 
    block1 <- NN$nn.idx[1:n1, -1]
    block2 <- NN$nn.idx[(n1+1):n, -1]
    i1 <- sum(block1 < n1 + .5)
    i2 <- sum(block2 > n1+.5)
    (i1 + i2) / (k * n)
}

eqdist.nn <- function(z, sizes, k){
    boot.obj <- boot(data = z, statistic = Tn, R = R, sim = "permutation", sizes = sizes,k = k)
    ts <- c(boot.obj$t0, boot.obj$t)
    p.value <- mean(ts >= ts[1])
    list(statistic = ts[1], p.value = p.value)
}

m <- 100
k <- 3
p <- 2
n1 <- n2 <- 50
R<-200
n <- n1 + n2
N = c(n1, n2)
 p_NN <- p_energy <- p_ball <- numeric(m)
alpha <- 0.1

for(i in 1:m) {
   x <- matrix(rt(n1*p, df = 1), ncol = p)
   y <- cbind(rnorm(n2, mean = 0.3, sd = 1.5),rnorm(n2, mean = 0.5, sd = 2))
   z <- rbind(x,y)
   p_NN[i] <- eqdist.nn(z, N, k)$p.value
   p_energy[i] <- eqdist.etest(z, sizes = N, R = R)$p.value
   p_ball[i] <- bd.test(x = x, y = y, num.permutations = 999, seed = i * 12345)$p.value
}

cat('pow_NN = ', mean(p_NN < alpha), 'pow_energy = ', mean(p_energy < alpha), 'pow_ball = ', mean(p_ball < alpha))

## -----------------------------------------------------------------------------
library(energy)
library(Ball)
library(RANN)
library(boot)

Tn <- function(z, ix, sizes, k) {
    n1 <- sizes[1]
    n2 <- sizes[2]
    n <- n1 + n2
    if(is.vector(z)) z <- data.frame(z, 0);
    z <- z[ix, ];
    NN <- nn2(data=z, k=k+1) 
    block1 <- NN$nn.idx[1:n1, -1]
    block2 <- NN$nn.idx[(n1+1):n, -1]
    i1 <- sum(block1 < n1 + .5)
    i2 <- sum(block2 > n1+.5)
   (i1 + i2) / (k * n)
}

eqdist.nn <- function(z, sizes, k){
    boot.obj <- boot(data = z, statistic = Tn, R = R, sim = "permutation", sizes = sizes,k = k)
    ts <- c(boot.obj$t0, boot.obj$t)
    p.value <- mean(ts >= ts[1])
    list(statistic = ts[1], p.value = p.value)
}

m <- 100
k <- 3
p <- 2
n1 <- 10
n2 <- 100
R<-200
n <- n1 + n2
N = c(n1, n2)
 p_NN <- p_energy <- p_ball <- numeric(m)
alpha <- 0.1

for(i in 1:m) {
   x <- matrix(rnorm(n1*p, mean = 0,sd = 0.8), ncol=p)
   y <- matrix(rnorm(n2*p, mean = 0,sd = 0.5), ncol=p)
   z <- rbind(x,y)
   p_NN[i] <- eqdist.nn(z, N, k)$p.value
   p_energy[i] <- eqdist.etest(z, sizes = N, R = R)$p.value
   p_ball[i] <- bd.test(x = x, y = y, num.permutations = 999, seed = i * 12345)$p.value
}

cat('pow_NN = ', mean(p_NN < alpha), 'pow_energy = ', mean(p_energy < alpha), 'pow_ball = ', mean(p_ball < alpha))

## -----------------------------------------------------------------------------
set.seed(21068)
f <- function(x) {1/(pi*(1+x^2))}

m <- 10000
mu <- 0
sigma <- 1
x <- numeric(m)
x[1] <- rnorm(1, mu, sigma)
u <- runif(m)

for (i in 2:m) {
  xt <- x[i-1]
  y <- rnorm(1, xt, sigma)     
  r1 <- f(y) * dnorm(xt, y, sigma)
  r2 <- f(xt) * dnorm(y, xt, sigma)
  r <- r1 / r2
  if (u[i] <= r) x[i] <- y else
      x[i] <- xt
}

b <- 1001      #discard the burn-in sample
y <- x[b:m]
a <- ppoints(10)
QR <- qt(a, df=1)  #quantiles of Cauchy
Q <- quantile(x, a)

par(mfrow=c(1, 2))
qqplot(QR, Q, main="", xlab="Cauchy Quantiles", ylab="Sample Quantiles")
abline(0, 1, col='blue', lwd=2)
hist(y, breaks="scott", main="", xlab="", freq=FALSE)
lines(QR, f(QR))

## -----------------------------------------------------------------------------
set.seed(21068)
N <- 10000
burn <- 1000 
X <- matrix(0, N, 2) 
a<- b <- 1
n <- 25
X[1,1] <- rbinom(1, n, 0.5)  #initialize
X[1,2] <- rbeta(1, 50, 50)   #initialize

###### generate the chain #####

for (i in 2:N) {
  x2 <- X[i-1, 2]
  X[i, 1] <- rbinom(1, n, x2)
  x1 <- X[i, 1]
  X[i, 2] <- rbeta(1, x1+a, n-x1+b)
}

b <- burn + 1
x <- X[b:N, ]

#par(mfrow=c(1, 2))
plot(x[,1],type='l',col=1,lwd=2,xlab='Index',ylab='Random numbers')
plot(x[,2],type='l',col=2,lwd=2,xlab='Index',ylab='Random numbers')
plot(X[,1],X[,2],xlab = "x",ylab = "y")

## -----------------------------------------------------------------------------
Gelman.Rubin <- function(psi) {
    psi <- as.matrix(psi)
    n <- ncol(psi)
    k <- nrow(psi)

    psi.means <- rowMeans(psi)     #row means
    B <- n * var(psi.means)        #between variance est.
    psi.w <- apply(psi, 1, "var")  #within variances
    W <- mean(psi.w)               #within est.
    v.hat <- W*(n-1)/n + (B/n)     #upper variance est.
    r.hat <- v.hat / W             #G-R statistic
    return(r.hat)
}

f <- function(x) {1/(pi*(1+x^2))}

cauthy.chain <- function(sigma, N, X1) {
    x <- rep(0, N)
    x[1] <- rnorm(1, X1, sigma)
    u <- runif(N)

    for (i in 2:N) {
       xt <- x[i-1]
       y <- rnorm(1, xt, sigma)     #candidate point
       r1 <- f(y) * dnorm(xt, y, sigma)
       r2 <- f(xt) * dnorm(y, xt, sigma)
       r <- r1 / r2
       if (u[i] <= r) x[i] <- y else
            x[i] <- xt
       }
    return(x)
}

sigma <- 1      #parameter of proposal distribution
k <- 4          #number of chains to generate
n <- 15000      #length of chains
b <- 1000       #burn-in length

x0 <- c(-10, -5, 5, 10)

set.seed(123)
X <- matrix(0, nrow=k, ncol=n)
for (i in 1:k)
   X[i, ] <- cauthy.chain(sigma, n, x0[i])

psi <- t(apply(X, 1, cumsum))
for (i in 1:nrow(psi))
    psi[i, ] <- psi[i, ] / (1:ncol(psi))

rhat <- rep(0, n)
for (j in (b+1):n)
  rhat[j] <- Gelman.Rubin(psi[,1:j])
plot(rhat[(b+1):n], type="l", xlab="", ylab="R")
abline(h=1.2, lty=2)

## -----------------------------------------------------------------------------
Gelman.Rubin <- function(psi) {
    psi <- as.matrix(psi)
    n <- ncol(psi)
    k <- nrow(psi)

    psi.means <- rowMeans(psi)     #row means
    B <- n * var(psi.means)        #between variance est.
    psi.w <- apply(psi, 1, "var")  #within variances
    W <- mean(psi.w)               #within est.
    v.hat <- W*(n-1)/n + (B/n)     #upper variance est.
    r.hat <- v.hat / W             #G-R statistic
    return(r.hat)
}

binombeta.chain <- function(N, p, alpha, beta, n, a, b) {
    X <- matrix(0, N, 2) 
    X[1,1] <- rbinom(1, n, p)
    X[1,2] <- rbeta(1, alpha, beta)

    for (i in 2:N) {
       x2 <- X[i-1, 2]
       X[i, 1] <- rbinom(1, n, x2)
       x1 <- X[i, 1]
       X[i, 2] <- rbeta(1, x1+a, n-x1+b)
    }
    return(X)
}
  
k <- 4          #number of chains to generate
m <- 15000      #length of chains
burn <- 1000    #burn-in length
n <- 100
a <- b <- 1

X0 <- matrix(c(0.2, 0.4, 0.6, 0.8, 20, 40, 60, 80, 80, 60, 40, 20), 4, 3)

set.seed(1234)
X <- matrix(0, nrow=k, ncol=m)
Y <- matrix(0, nrow=k, ncol=m)
for (i in 1:k){
  Z <- binombeta.chain(m, X0[i,1], X0[i,2], X0[i,3], n, a, b)
  X[i, ] <- Z[ ,1]
  Y[i, ] <- Z[ ,2]
}

psi <- t(apply(X, 1, cumsum))
for (i in 1:nrow(psi))
    psi[i, ] <- psi[i, ] / (1:ncol(psi))

rhat <- rep(0, m)
for (j in (burn+1):m)
  rhat[j] <- Gelman.Rubin(psi[,1:j])
plot(rhat[(burn+1):m], type="l", xlab="", ylab="R")
abline(h=1.2, lty=2)

psj <- t(apply(Y, 1, cumsum))
for (i in 1:nrow(psj))
    psj[i, ] <- psj[i, ] / (1:ncol(psj))

rhat <- rep(0, m)
for (j in (burn+1):m)
  rhat[j] <- Gelman.Rubin(psj[,1:j])
plot(rhat[(burn+1):m], type="l", xlab="", ylab="R")
abline(h=1.2, lty=2)

## -----------------------------------------------------------------------------
vector_length <- function(x) {
   temp <- 0
   for (i in 1: length(x)) {
         temp <- temp + x[i]^2
    }
    vlength <- sqrt(temp)
    return(vlength)
}

A_k <- function(k, x) {
  d <- length(x)
  a <- (-1/2)^k/factorial(k)
  b <- exp(lgamma((d+1)/2) + lgamma(k+3/2) - lgamma(k+d/2+1))
  c <- vector_length(x)
  e <- (c^(2*k+2))/((2*k+1)*(2*k+2))
  f <- a * e * b
  return(f)
}

## -----------------------------------------------------------------------------
aksum <- function(x) {
  aksum <- A_k(0, x)
  i <- 1
  while (abs(A_k(i, x) - A_k(i-1, x)) > 1e-1000) {
      aksum <- aksum + A_k(i, x)
      i = i + 1
  } 
  return(list(m=aksum, k=i))
}

## -----------------------------------------------------------------------------
x <- c(1, 2)
aksum(x)$m

## -----------------------------------------------------------------------------
k=c(4:25,100,500,1000)
solution <- numeric(25)

for (i in 1:length(k)) {
  solution[i]=uniroot(
    function(a) {pt(sqrt(a^2*(k[i]-1)/(k[i]-a^2)),df=k[i]-1)-pt(sqrt(a^2*(k[i])/(k[i]+1-a^2)),df=k[i])},
     c(1e-4, sqrt(k[i])-1e-4))$root
}

x <- cbind(k, solution)
knitr::kable(x, col.names=c("k","solution"))

## -----------------------------------------------------------------------------
y <- c(0.54, 0.48, 0.33, 0.43, 1.00, 1.00, 0.91, 1.00, 0.21, 0.85)
n <- length(y)
tau <- 1
r <- length(which(y < tau))

mlogL <- function(lambda=1) {
  return(-log(factorial(n)/factorial(n-r)) - r*log(lambda) + lambda*(sum(y)))
}

library(stats4)
L.MLE <- mle(mlogL)
L.MLE

## -----------------------------------------------------------------------------
y <- c(0.54, 0.48, 0.33, 0.43, 1.00, 1.00, 0.91, 1.00, 0.21, 0.85)

L <- 1
N <- 10000
n <- length(y)
tau <- 1

for (i in 1:N) {
      r <- length(which(y < tau))
      L.old <- L
      L <- n/(sum(y) + (n-r)*(1/L.old))
      
      if (abs((L - L.old)/L.old) < 1e-8) break
}
cat("L.EM =", L)

## -----------------------------------------------------------------------------
trims <- c(0, 0.1, 0.2, 0.5)
x <- rcauchy(100)
lapply(trims, function(trim) mean(x, trim = trim))
lapply(trims, mean, x = x)

## -----------------------------------------------------------------------------
rsq <- function(mod) summary(mod)$r.squared

## ---- eval = TRUE-------------------------------------------------------------
formulas <- list(
  mpg ~ disp,
  mpg ~ I(1 / disp),
  mpg ~ disp + wt,
  mpg ~ I(1 / disp) + wt
)


a <- lapply(formulas, function(x) lm(formula = x, data = mtcars))

f <- vector("list", length(formulas))
for (i in seq_along(formulas)){
  f[[i]] <- lm(formulas[[i]], data = mtcars)
}

sapply(a, rsq)
sapply(f, rsq)

## ---- eval = TRUE-------------------------------------------------------------
bootstraps <- lapply(1:10, function(i) {
  rows <- sample(1:nrow(mtcars), rep = TRUE)
  mtcars[rows, ]
})


a <- lapply(seq_along(bootstraps), function(i) {lm(mpg ~ disp,data=bootstraps[[i]])})


f <- vector("list", length(bootstraps))
for (i in seq_along(bootstraps)){
  f[[i]] <- lm(mpg ~ disp, data = bootstraps[[i]])
}

sapply(a, rsq)
sapply(f, rsq)

## -----------------------------------------------------------------------------
vapply(cars, sd, numeric(1))

## -----------------------------------------------------------------------------
vapply(mtcars[vapply(mtcars, is.numeric, logical(1))], sd, numeric(1))

## -----------------------------------------------------------------------------
GibbsR <- function(N, x0, n, a, b){
  X <- matrix(0, N, 2) 
  X[1,] <- x0
  for(i in 2:N){
    x2 <- X[i-1, 2]
    X[i, 1] <- rbinom(1, n, x2)
    x1 <- X[i, 1]
    X[i, 2] <- rbeta(1, x1+a, n-x1+b)
  }
  return(X)
}

## -----------------------------------------------------------------------------
library(Rcpp)

cppFunction('NumericMatrix GibbsC(int N, NumericVector x0, int n, double a, double b) {
            NumericMatrix X(N,2);
            X(0, 0) = x0[1];
            X(0, 1) = x0[2];
            for(int i = 1; i < N; i++)
            {
              double x2 = X(i-1, 1);
              X(i, 0) = rbinom(1, n, x2)[0];
              double x1 = X(i, 0);
              X(i, 1) = rbeta(1, x1+a, n-x1+b)[0];
            }
            return X;
}')

## -----------------------------------------------------------------------------
set.seed(21068)
N <- 10000
burn <- 1000 
a<- b <- 1
n <- 25
x0 <- c(0, 0.5)

X<-GibbsR(N, x0, n, a, b)
Y<-GibbsC(N, x0, n, a, b)

#par(mfrow = c(2, 2))
plot(Y[1001:N,1],Y[1001:N,2],xlab = "y1",ylab = "y2")
plot(X[1001:N,1],X[1001:N,2],xlab = "x1",ylab = "x2")

qqplot(X[1001:N,1],Y[1001:N,1],xlab='R',ylab='cpp',main='1')
abline(0, 1)
qqplot(X[1001:N,2],Y[1001:N,2],xlab='R',ylab='cpp',main='2')
abline(0, 1)

## -----------------------------------------------------------------------------
library(microbenchmark)

ts <- microbenchmark(R=GibbsR(N, x0, n, a, b),Rcpp=GibbsC(N, x0, n, a, b))
print(summary(ts)[, c(1,3,5,6)])

