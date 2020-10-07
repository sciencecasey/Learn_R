binomial = makeFun(((factorial(n)/(factorial(k)*factorial(n-k))) * (p^k) * (1-p)^(n-k)) ~ n, k, p)
hypergeometric = makeFun((factorial(r)/(factorial(k)*factorial(r-k))*
                            (factorial(N-r)/(factorial((N-r)-(n-k))*factorial(n-k)))) ~ N & k & r)
(hypergeometric(N = 50, r = 15, k = 3, ))                   
