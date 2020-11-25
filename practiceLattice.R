afterdim <- read.table(header = TRUE, text = "
Dim_Reduction  Dimensions  Time RMSE
PCA, 9, 15.39, 0.287
PCA, 8, 16.84, 0.290
PCA, 7, 14.13, 0.289
PCA, 6, 12.14, 0.292
PCA, 5, 12.54, 0.293
PCA, 4, 11.23, 0.295
ICA, 11, 20.23, 0.287
ICA, 10, 20.88, 0.288
ICA, 9, 16.34, 0.290
ICA, 8, 16.99, 0.294
ICA, 7, 14.34, 0.291
ICA, 6, 13.33, 0.292
ICA, 5, 12.12, 0.294")

xyplot(RMSE ~ Dimensions, data=afterdim,groups =  Dim_Reduction,
       type = c("l", "g"), auto.key =list(spline = "bottom", points = FALSE, lines = TRUE), 
       xlab="Dimensions", ylab="RMSE",scales=list(ylim=c(0,1)),
       panel=function(...) {
         panel.xyplot(...)
         panel.abline(h=.29)
       })





xyplot(mpg~wt,data=mtcars,
       groups = cyl,
       auto.key = TRUE,
       par.settings = list(superpose.symbol = list(col = c("blue","red"),
                                                   pch = 19),
                           superpose.line = list(col = c("blue","red"),
                                                 lwd = 2)),
       #xlab="grade point average",
       #ylab="speed (mph)",
       #main="Fastest Speed Ever Driven,\nby Grade Point Average",
       type=c("p","smooth"))


#load reproducible data set
attach(mtcars)

# change the rownames into a column value and remove the rownames
mtcars <- as.data.frame(cbind(car.name=rownames(mtcars), mtcars))
rownames(mtcars) <- NULL

panel = function (x, y, ...) {
  panel.xyplot(x, y, ...)
  v <- abs(x - y) > 100
  panel.text(x[v], y[v], labels=mtcars$car.name[v], pos=3)
}

xyplot(hp~disp, data=mtcars, main=NULL, ylab="hp", xlab="disp", 
       jitter=TRUE, pch=2, as.table=TRUE, panel=panel)

attach(iris)
xyplot(Sepal.Length ~ Sepal.Width | Species, data = iris,
       type = c("p","smooth"), alpha=.1, pch=19, col="black", col.line="red", lwd=2)

xyplot(Sepal.Length ~ Sepal.Width | Species, data = iris,
       type = c("p","smooth"), 
       par.settings = list(superpose.symbol = list(cex=1, pch=20, col="black", alpha = 0.1),
                           superpose.line = list(col = "red", lwd = 2)))

xyplot(Sepal.Length ~ Sepal.Width | Species, data = iris, 
       grid = TRUE, scales=list(tck=c(1,0), x=list(cex=1.1), y=list(cex=1.1)),
       par.settings = list(superpose.symbol = list(pch =20, cex = 1, col = c("#0808084d"))),
       type = c("p", "smooth"), col.line =c("red"),lwd = 2)

xyplot(Sepal.Length ~ Sepal.Width | Species, data = iris,
       par.strip.text=list(cex=0.6),
       type = c("p","smooth"), 
       par.settings = list(superpose.symbol = list(cex=1, pch=20, col="black", alpha = 0.1),
                           superpose.line = list(col = "red", lwd = 2)))

# want transparent black symbols, with solid red trend lines.
trans_black <- adjustcolor("black",alpha.f=0.2)
xyplot(Sepal.Length ~ Sepal.Width | Species, data = iris,
       type = c("p","smooth"), pch=19, col=trans_black,
       col.line="red", lwd=2)


d1 <- data.frame(x=c(NA, 13:20, NA), y = 25, z = c(rep('march', 5),
                                                   rep("april", 5)), color = c(c(rep(c("red", "green"), 2), "red"),
                                                                               c(rep(c("blue", "yellow"), 2), "blue")), stringsAsFactors = FALSE)
xyplot(y ~ x | factor(z), data = d1,
       panel = function(x, y, subscripts) {
         panel.xyplot(x = x, y = y,  
                      subscripts=subscripts, 
                      col = d1[subscripts, "color"])
       })





xyplot(speed ~ dist, data=cars,
      panel=function(x, y, col, ...) {
        panel.xyplot(x, y, col='red', ...)
        panel.abline(lm(y~x), col='blue')
      },
      type='l'
)



df <- data.frame(x=1:10,y=c(10,9,8,1,3,4,6,7,8,10))

library(lattice)
library(latticeExtra)

xyplot(y ~ x, data=df, type=c("r"),col=c("gray")) +
  as.layer( xyplot(y ~ x, data=df, type=c("l"),col=c("blue")))



#