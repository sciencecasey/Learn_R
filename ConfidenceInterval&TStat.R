#Functions created for this assignment
#gives the t statistic for 2 tail critical values
t = function(alpha, n){
  (qt(1-(alpha)/2, df = n-1))
}
#gives confidence interval
ci = function(ybar, tstat, sd, n){
  c(ybar-tstat*sd/sqrt(n), ybar+tstat*sd/sqrt(n))
}
#gives tstat for observed sample to compare hypoth. testing
t.observed = function(ybar, mu, sd, n){
  (ybar-mu)/(sd/sqrt(n))
}


#Calculation for Confidence Interval question 7.4.8 
n=16
ybar=(1019+1090+1109+1235+1275+1361+1437+1446+1461+1525+1670+1685+1783+1787+1889+2484)/n #288.76
alpha = .05
sd=369.02
tstat = t(alpha, n) #2.1314
conf = c(ybar-tstat*sd/sqrt(n), ybar+tstat*sd/sqrt(n)) #calculation for the confidence interval
conf #1319.363, 1712.637



#Calculation for SD and CI 7.4.9
n=12
sample = c(40, 34, 23, 40, 31, 33, 49, 33, 34, 43, 26, 39)
ybar = sum(sample)/n #35.4167
sd = sd(sample) #7.22
#check sd by "hand"
sampSd = sqrt(sum(((sample-ybar)^2)/(n-1))) #same as above
alpha = .05
tstat = t(alpha, n)
conf = c(ybar-tstat*sd/sqrt(n), ybar+tstat*sd/sqrt(n)) #calculation for the confidence interval
#Plot the date with respect to age
date = c(1543, 1600, 1665, 1746, 1774, 1830, 1858, 1864, 1896, 1901, 1905, 1926)
plot(x = date, y = sample, xlab = "Year of discovery", ylab = "Age of Discovery", type = 'l')
#checking my view that they don't correlate
cor(sample, date, method = "pearson")  #.1273


#Calculation for 7.4.18
mu = 132.4
ybar = 143.8
s = 6
alpha = .05
n = 84
tstat = t(alpha, n) #1.9889598
conf = ci(ybar, tstat, s, n) #142 to 145, mu not within this range-- reject
tstat_observed = t.observed(ybar, mu, s, n) #17.413 much larger than  given tstat
sample = c(141, 148, 132, 138, 154, 142, 150, 146, 155, 158, 150, 140, 147, 148, 144 , 150, 149, 
           145, 149, 158, 143, 141, 144, 126, 140, 144,142, 141, 140, 145, 135, 147, 146, 141, 
           136, 140, 146, 142, 137, 148, 154, 137, 149, 143, 140, 131, 143, 141, 149, 148, 135, 
           148, 153, 143, 144, 141, 143, 147, 146, 150, 132, 142, 142, 143, 153, 149, 146, 149, 
           138, 142, 149, 142, 137, 137, 144, 146, 147, 140, 142, 140, 137, 152, 145)
sd(sample) #5.9762 -- larger than z which is similar to t
#check if symmetric by plot
plot(sample)
#simply plotting was too simple


#Calculation for 7.4.19
#Part 1: is there a statistically sig difference, course v no course
h0 = 0
alpha = .05
sample = c(35, 37, 33, 34, 38, 40, 35, 36, 38, 33, 28, 34, 47, 42, 46)
sd = sd(sample) #5.0492
n = length(sample) 
tstat = t(.05*2, n) #1.7613 #multiplied by 2 since single tail
ybar = sum(sample)/n #37.066
t.observed(ybar, h0, sd, n) #28.43149 >2.144, stat sig difference
conf = ci(ybar, tstat, sd, n) #34.8, 39.4 -- 0 not included! stat sig difference
#there is a statistically sig. difference from the test

#Part 2: is the difference equal to 40?
h0 = 40
tstat = t(alpha, n) #2.1447
t.observed(ybar, h0, sd, n) #-2.249 -- this is less than -2.1447, reject
conf = ci(ybar, tstat, sd, n) #verify same: (34.3, 39.9) -- 40 just outside this! reject the null
#Difference not statistically equal to 40

