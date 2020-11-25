#find the b, slope value of regression line
b = function(x, y){
  if(length(x)!= length(y)){
    return("Use vectors of equal length")
  }else {
    num = length(x)*(sum(x*y))-sum(x)*sum(y)
    denom = length(x)*(sum(x^2))-(sum(x))^2
    return(num/denom)
  }
}

#find the intercept of the regression line
a = function(x, y){
  mean(y)-b(x,y)*mean(x)
}

#find the predicted values for y given x
predict_yhat = function(x,y){
  vec = a(x,y) + b(x,y)*x
  return(vec)
}

#find redisual epsilon 
resid_error = function(x, y){
  vec = predict_yhat(x, y)
  return(y - vec)
}


#Example 11.2.1 - make sure everything works
x = .001*c(745, 700, 690, 680,675,670,665,660,655,655,650,650,645,635,630,625,625,620,615,615,615,610,590,590,565) +2
y = .001*c(80, 45, 50, 5, 35, 35, 20, 5, 10,  0, 0, 5, 15)+2
y = c(y, 1.990, 1.990, 1.995, 1.985, 1.970, 1.985, 1.990, 1.995, 1.990, 1.975, 1.995, 1.955)
yhati = predict_yhat(x, y)
p = resid_error(x,y) 
#practice checking with built in functions
g = lm(y~x) #this works correctly!!
g2 = lsfit(x, y) #lsfit$residuals gives the same values as p!
plot(lm(y~x)) #this makes 4 plots!! looks like outliers were found and adjusted
library(lattice)
xyplot(0+p~x, type = c('p')) #this makes the residual plot I want
xyplot(y~x, type = c('p','r')) #traditional data plot with resid line
xyplot(yhati~x, type = 'l') #just the residual line


#11.2.1
#plot the data
x = c(20, 16, 19.8, 18.4, 17.1, 15.5, 14.7, 17.1, 15.4, 16.2, 15, 17.2, 16, 17, 14.4)
y = c(88.6, 71.6, 93.3, 84.3, 80.6, 75.2, 69.7, 82, 69.4, 83.3, 79.6, 82.6, 80.6, 83.5, 76.3)
b(x, y)
sum(x)
sum(y) #entry was correct
equa = c(a(x,y), b(x,y))
point = equa[1]+equa[2]*18 #84.47201
xyplot(84 + y~x, lty = 2, xlab = "Cricket Chirps per Minute", ylab = "Observed temperature", type = c('p', 'g'))

#11.2.2
age=c(0,.5,1,2,3,4,5,6,7,8)
proof = c(104.6, 104.1, 104.4, 105, 106, 106.8, 107.7, 108.7, 110.6, 112.1)
yhati = predict_yhat(age, proof)
resid = resid_error(age, proof)
xyplot(yhati+proof~age, 
       type = c('p', 'l','g'), 
       key = list(
         text = list(c("Predicted", "Observed")),
         lines = list(lwd = 2, col = c("blue", "magenta"))
         ), ylab = "Proof")
#As a regression plot
xyplot(resid~x, type = c('p', 'g'))
abline(h=0, type = 'l', col = 'black', lwd = 3)


#11.2.3
temp = c(0, 4, 10, 15, 21, 29, 36, 51, 68)
disolvedNaNO3 = c(66.7, 71, 76.3, 80.6, 85.7, 92.9, 99.4, 113.6, 125.1)
#check data entry
sum(temp)
sum(disolvedNaNO3)
resid = resid_error(temp, disolvedNaNO3)
predict_yhat(temp, disolvedNaNO3)
xyplot(resid~temp, type = c("p", "l","g"), col = c("magenta"),
       xlab="Temperature", ylab="Residual Error",
       panel=function(...) {
         panel.xyplot(...)
         panel.abline(h=0)})


#11.2.7
spendingPerPupil = c(10, 10.2, 10.2, 10.3, 10.3, 10.8, 11, 11, 11.2, 11.6, 12.1, 12.3, 
                     12.6, 12.7, 12.9, 13, 13.9, 14.5, 14.7, 15.5, 16.4, 17.5, 18.1, 
                     20.8, 22.4, 24)
gradRat = c(88.7, 93.2, 95.1, 94, 88.3, 89.9, 67.7, 90.2, 95.5, 75.2, 84.6, 85, 94.8, 
            56.1, 54.4, 97.9, 83, 94, 91.4, 94.2, 97.2, 94.4, 78.6, 87.6, 93.3, 92.3)
a = a(spendingPerPupil, gradRat)
b = b(spendingPerPupil, gradRat)
xyplot(gradRat~spendingPerPupil, type = c("p", "g"), xlab = "Spending Per Pupil", 
       ylab = "Graduation rate", panel = function(...){
         panel.xyplot(...)
         #this adds the regression line shown with the calculated values
         panel.abline(a = a, b = b)}) 



