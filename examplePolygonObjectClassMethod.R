library(methods)
#name of function, type(s) it holds
#in this case x and y are vectors of coordinates
setClass("polygon", representation(x = "numeric", 
                                      y = "numeric"))

#name of generic you want to implement for the new class
setMethod("plot", "polygon",
          #the function call must exactly match the original function (plot)
          function(x, y, ...){
            #define your function here
            plot(x@x, x@y, type = "n", ...) #match xs and ys to create coordinates
            xp <- c(x@x, x@x[1]) #create an x point by grabbing x vector and then first item in it
            yp <- c(x@y, x@y[1]) #create a y point
            lines(xp, yp) #draw lines between each point (xp, yp)
          })

test = new("polygon", x = c(1, 2, 3, 4), y = c(1, 2, 3, 1))
#points (1,1) (2,2) (3,3) (4,1)
plot(test)
