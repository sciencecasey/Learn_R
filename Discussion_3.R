library("mosaic")
library("mosaicCalc")
#Question 3.4.9

#example using the antiDerivative symbolic function:
#The part of the question greater outside [-1:1]
#note: the variable inputs are listed to the right of the tilde, 
#the funciton itself to the inputs (this tells R what to derivative solve for)
gt1 = antiD(0 ~y)
print(gt1)

#example using mosaic's Derivative solver:
#same tilde logic, this is porblem 3.4.15 
#the parenthesis around the outside tells R to print output immediately
(prob_15_D= D(1/(1+exp(-y)) ~y))

#this is the equation we want the antiderivative of, which is saved but is not pretty 
#due to absolute value
lteq1 = antiD(1-abs(y) ~ y, lower.bound = -1)

#try again without abs. val for visualization purposes
(modified_lteq1 = antiD(1-y ~ y, lower.bound = -1))

#okay, let's plot with Mosaic
plotFun(antiD(1-abs(y) ~ y), y.lim = range(-2,2), xlim = range(-2,2), 
        ylab = "Probability", xlab = "y input")



plotFun(1/(1+exp(-y)) ~y, y.lim = range(-30, 30))
plotFun(1/(1+exp(-y*k)) ~y+k, y.lim = range(-30, 30), k.lim = range(-2, 2))

plotFun(1/(1+exp(-y)) ~y, y.lim = range(-10, 0))
deriv_15 = D(expression( "y")

antiD(exp(x) ~x)
plotFun(wageModel (age = age, educ = educ) ~age+educ, age.lim = range(20,50), educ.lim= range(5,14), main = "Test")