#Single Sample Variance Calculations 3 ways
sample_var = function(sumx, sumsqaredX, n){
  num = n*(sumsqaredX)-sumx^2
  denom = n*(n-1)
  num/denom
}
#these two from previous assignment
s2.grandmean = function(grandsum, sqgrandsum, n){
  (n*sqgrandsum - grandsum^2)/(n*n-1)
}

s2 = function(y, ybar, n){
  1/(n-1)*sum((y-ybar)^2)
}

#this one from Josh helping me with an error to ensure my data entry was fine
joshVar = function(y){
  n = length(y)
  numer = n*sum(y^2) - sum(y)^2
  denom = n*(n-1)
  return(numer/denom)
}



#Pooled Variance
spool = function(varx, vary, n, m){
  num = (n-1)*varx+(m-1)*vary
  denom = n+m-2
  num/denom
}

#T Test mux=muy with same SD
t2_sameSd = function(xbar, ybar, sp, n, m){
  num = xbar - ybar
  denom = sp*(sqrt(1/n + 1/m))
  num/denom
}

#Behren's Fischer degrees of Freedom
bf_dof = function(sdx, sdy, n, m){
  num = (sdx^2/sdy^2 + n/m)^2
  denom = (sdx^2/sdy^2)^2*1/(n-1) + n^2/m^2*1/(m-1)
  num/denom
}

#BF T test for w
bf_t = function(xbar, ybar, sx, sy, n, m){
  num = xbar-ybar
  denom = sqrt(sx^2/n+sy^2/m)
  num/denom
}

#9.2.9
xbar = 2.41
sx = .96
n = 49
alpha = .05

ybar = 3.00
sy = 1.02
m = 25

#F test if SD is statistically the same
lower_tail = qf(.025, 48, 24)
upper_tail = qf(.975, 48, 24)
sx^2/sy^2
#not statistically different!


#T Test mux = muy
sp = spool(sx^2, sy^2, n, m)
lower_tail = qt(alpha/2, (n+m-2))
upper_tail = qt(alpha/2, (n+m-2), lower.tail = F)
observed = t2_sameSd(xbar, ybar, sp, n, m) #-.0959
lower_tail>= observed || observed>=upper_tail #False- fail to reject

#Problem 9.2.12
xbar = 81.6
ybar = 79.9
varx = 17.6
vary = 22.9
n = 10
m = 20

zobserverd = (xbar-ybar)/(sqrt(varx/n + vary/m)) #.9974149
pval = 2*pnorm(-abs(zobserverd))
pval = pnorm(-zobserverd)


#Problem 9.2.17
n = 11 #(ill)
m = 11 #(not ill)
sdx = 428
sdy = 183
df = data.frame(status = c(rep("ill", 11), rep("not ill", 11)),
                titer = c(640, 80,1280, 160, 640, 640, 1280, 640, 160, 320, 160, 10, 320, 320, 320, 
                          320, 80, 160, 10, 640, 160, 320))
library(dplyr)
df = df %>% mutate(titersq = titer^2) #add a squared column for observed sd
#realized this value is given

#F test if SD is statistically the same
upper_tail = qf(.95, 10, 10)
sdx^2/sdy^2>=upper_tail
#statistically different!

#Calculate the DOF for Behren's Fisher
#calculate observed mean
str(df)
xbar = df %>% filter(status == "ill") %>% select(titer)%>% colMeans() #545.4545
ybar = df %>% filter(status != "ill") %>% select(titer)%>% colMeans() #241.8182
names(xbar) = NULL
names(ybar) = NULL

#Find BF degrees of freedom
dof = round(bf_dof(sdx, sdy, n, m))
#Find the critical t with dof = 3
critical = qt(.95, dof)
wstat = bf_t(xbar, ybar, sdx, sdy, n, m)

bf_dof(sqrt(115.9929), sqrt(35.7604), 12, 12)

#9.2.18
qt(.95, 11+11-2)


#9.3.3
n = 20
m = 20
tat = c(2,1,1,3,1,7,2,1,3,1,0,2,4,2,3,3,0,1,2,2,8,4,6,3,1,4,4,6,4,2,2,1,1,4,3,3,2,6,3,4)
df = data.frame(child = c(rep("schizophrenic", 20), rep("not", 20)), tat, squared = tat^2) 
#is this data normal? 
hist(tat[1:20])
hist(tat[21:40])
## these data don't look normal (which we need for F test) but the back uses F test... sigh



#get the sums for variance calculations
sumsqrsX = df %>% filter(child == "not") %>% select(squared) %>% colSums()
sumX = df %>% filter(child == "not") %>% select(tat) %>% colSums()
sumsqrsY = df %>% filter(child != "not") %>% select(squared) %>% colSums()
sumY = df %>% filter(child != "not") %>% select(tat) %>% colSums()
#set all the names to null for calculation

#testing all the variance equations and data entry
varX1 = s2.grandmean(grandsum = sumX, sqgrandsum = sumsqrsX, n = 20)
joshVar(y = tat[21:40]) #3.523
varX1diff = s2.grandmean(sum(tat[21:40]), sum(tat[21:40]^2), 20)
varX2 = s2(y = tat[21:40], ybar = mean(tat[21:40]), n = 20)
var2diff = s2(y = (df %>% filter(child == "not") %>% select(tat)), ybar = (sumX/20), n = 20) #great! the sum is correct!!
varX3 = sample_var(sumX, sumsqrsX, 20)
varY1 = s2.grandmean(grandsum = sumY, sqgrandsum = sumsqrsY, n = 20)
varY2 = s2(y = tat[1:20], ybar = mean(tat[1:20]), n = 20)
varY3 = sample_var(sumY, sumsqrsY, 20)
#Check the standard error: (another CLT test)
varX2/sqrt(n) #.7879197
varY2/sqrt(m) #.5525442  I don't really know what do with this but... cool

#calculate the observed and critical f stats
observed = (varX2/varY2)
upper_tail = qf(.975, 19, 19) #2.5264
lower_tail = qf(.025, 19, 19) #.395812
observed <= lower_tail || observed>= upper_tail

#part b: test mux and muy
xbar = df %>% filter(child == "not") %>% select(tat) %>% colMeans() 
ybar = df %>% filter(child != "not") %>% select(tat) %>% colMeans()
pooled = spool(sqrt(varX2), sqrt(varY2), 20, 20)
criticalt = t2_sameSd(xbar, ybar, pooled, n, m) #2.0394
upper_tail = qt(.975, (40-2))
lower_tail = qt(.025, (40-2))
criticalt >= upper_tail || criticalt <= lower_tail #true!!


#9.5.7
x = c(62, 60, 78, 62, 49, 67, 80, 48)
y = c(24, 56, 42, 74, 44, 28)
ci_muX_muY= function(x, y, alpha){
  n = length(x)
  m = length(y)
  #varx
  numer = n*sum(x^2) - sum(x)^2
  denom = n*(n-1)
  varx = numer/denom
  #vary
  numer = m*sum(y^2) - sum(y)^2
  denom = m*(m-1)
  vary = numer/denom
  #spool
  num = (n-1)*varx+(m-1)*vary
  denom = n+m-2
  sp = num/denom
  left = mean(x)-mean(y)-qt(alpha/2, n+m-2)*sp*sqrt(1/n + 1/m)
  right = mean(x)-mean(y)+qt(alpha/2, n+m-2)*sp*sqrt(1/n + 1/m)
  return(c(left, right))
}

ci_varX_over_varY= function(x, y, alpha){
  n = length(x)
  m = length(y)
  #varx
  numer = n*sum(x^2) - sum(x)^2
  denom = n*(n-1)
  varx = numer/denom
  #vary
  numer = m*sum(y^2) - sum(y)^2
  denom = m*(m-1)
  vary = numer/denom
  left = varx/vary*(qf(alpha/2, m-1, n-1))
  right = varx/vary*(qf(alpha/2, m-1, n-1, lower.tail = FALSE))
  return(c(left, right))
}

ci_varX_over_varY(x, y, .05) #0.0589, 2.133517

