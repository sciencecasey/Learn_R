
n = 6
df = n-1
alpha = .05
#get the lower chi-squared stat
qchisq(alpha/2, df)
#get the upper chi-squared stat
qchisq((1-alpha)/2, df)
#or
qchisq(alpha/2, df, lower.tail = F)


#Estimate the variance using unbiased estimator based on MLE
#using a vector of quantities
s2 = function(y, ybar, n){
  1/(n-1)*sum((y-ybar)^2)
}
#using grandmean
s2.grandmean = function(grandsum, sqgrandsum, n){
  (n*sqgrandsum - grandsum^2)/(n*n-1)
}
#calculate the confidence interval for chistat
ci = function(n, s2estimator, lowchistat, upchistat){
  c(((n-1)*s2estimator)/upchistat, ((n-1)*s2estimator)/lowchistat)
}
#checking it works with case study 7.5.1
low = qchisq(.05/2, 18)
high = qchisq(.05/2, 18, lower.tail = F)
ci(n=19, s2estimator = 7.334, low, high)

#check 7.5.16
s2= s2.grandmean(grandsum = 758.62, sqgrandsum = 19195.7398, n =30)
alpha = .05
qchisq(1-alpha, 29) #42.55697
29*s2 ##observed chi-sq is 11.8674
