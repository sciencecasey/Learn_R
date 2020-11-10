k = c(0:20)
library(mosaic)
(pck = makeFun((factorial(n)*/(factorial(k)*factorial(n-k))) * (p^k) * (1-p)^(n-k) ~ n, k, p))

pdf=c()
for(i in 1:20){
  pdf[i]=pck(n = 20, k=i, p=.04)
  }
cbind(k, pdf)
mu = 0
for(i in 1:20){
  mu= mu + k[i]*pdf[i]
}
mu


  