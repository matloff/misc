sim <- function(n,nreps) {
   b = 1:2
   res <- replicate(nreps,{
      x <- matrix(rexp(2*n),ncol=2)
      meany <- 1 / (x %*% b)
      y <- meany + (runif(n) - 0.5) * meany
      xy <- cbind(x,y)
      xy <- data.frame(xy)
      nlout <- nls(X3 ~ 1 / (b1*X1+b2*X2),
         data=xy,start=list(b1 = 1,b2=1))             
      b <- coef(nlout)
      vc <- vcov(nlout)
      vchc <- nlsvcovhc(nlout)
      z1 <- (b[1] - 1) / sqrt(vc[1,1])
      z2 <- (b[1] - 1) / sqrt(vchc[1,1])
      c(z1,z2)
   })
   print(mean(res[1,] < 1.28))
   print(mean(res[2,] < 1.28))
}

