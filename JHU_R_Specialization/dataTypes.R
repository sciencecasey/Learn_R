#Lists can be complex
myVector = c(3,4,5)
myList = list(c(2,3,4), "Casey", myVector)
vectorOfLists = list(1, "myname",  myVector, myList)

#Factors have categories
is.factor(myVector)
orderMeUp = as.factor(myVector)
is.factor(orderMeUp)
levels(orderMeUp)
class(orderMeUp)
unclass(orderMeUp)
#you can state explicit order as well
orderMeUp= factor(orderMeUp, levels = c("5", "4", "3")) #now order is reversed
levels(orderMeUp) 


#dataFrames aren't as complex, but they're flexible with dyplyr and ggplot
myList[1]
myDF = data.frame(myVector, myList[1])
#dataFrames have names columns and rows
colnames(myDF)
colnames(myDF) = c("Vector1", "Vector2") #renames the columns and rows
colnames(myDF)
#can also rename 1 at a time
colnames(myDF)[2] = "Column 2"
colnames(myDF)

