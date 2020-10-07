setClass("pdf", representation(x = "numeric", 
                                  p = "numeric"))

setMethod("print", "pdf", 
          function(x,...){
            df = cbind(x@x, x@p) #"print function" x = "pdf" x
            print(df) # "print function" x = "pdf" p
          })

mypdf = new("pdf", x = c(1,2,3,4,5,6), p = rep(1/6, 6))

print(mypdf)
