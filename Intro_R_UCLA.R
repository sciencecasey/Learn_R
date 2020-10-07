## INSTRUCTIONS:  
## 1. TO RUN A COMMAND, PLACE CURSOR INSIDE COMMAND
##    OR HIGHLIGHT COMMAND(S) AND HIT CTRL-ENTER (COMMAND-ENTER FOR MACS)
## 2. TO RUN ALL CODE FROM BEGINNING OF FILE TO CURRENT LINE,
##    HIT CTRL-ALT-B
## 3. COMMANDS THAT BEGIN WITH "" ARE COMMENTS AND WILL NOT BE EXECUTED
## 4. NOTE: A FEW COMMANDS PURPOSEFULLY RESULT IN ERRORS FOR TEACHING PURPOSES
## 5. USE CTRL-F TO FIND
## # uncomment (remove ##) to run
## install.packages("installr", dependencies=TRUE)
## install.packages("tidyverse", dependencies=TRUE)
library(tidyverse)
library(installr)
## # not run during seminar
## #updateR()
## #get current directory (not run)
## getwd()
## 
## # set new working directory (not run)
## setwd("/path/to/directory")
# Using R as a calculator
2 +
  3
# assign the number 3 to object called abc
abc <- 3

# print contents
abc

# list all objects in current session
ls()
# seq() creates a sequence of numbers 

# specifying arguments by name
seq(from=1, to=5, by=1)

# specifying arguments by position
seq(10, 0, -2)
## ??logistic
## # list all available vignettes
## vignette()
## 
## # View the "Introduction to dplyr" vignette
## vignette("introduction")
# create a vector
first_vec <- c(1, 3, 5)
first_vec

# vector type
typeof(first_vec)

# character vector
char_vec <- c("these", "are", "some", "words")
length(char_vec)

# the result of this comparison is a logical vector
first_vec > c(2, 2, 2)
# second argument is number of repetitions
rep(0, times=3)
rep("abc", 4)

# from, to, by
seq(from=1, to=5, by=2)
seq(10, 0, -5)

# colon operator
3:7

# you can nest functions
rep(seq(1,3,1), times=2)

# each vs times
rep(seq(1,3,1), each=2)
# the single value `1` is a vector of length 1
#  it is recycled to be c(1,1,1)
c(1,2,3) + 1

# second vector recycled twice to make c(1,2,1,2,1,2)
c(1,2,3,4,5,6) + c(1,2)

# The 2 becomes c(2,2,2)
c(1,2,3) < 2

# what is R complaining about here?
c(2,3,4) + c(10, 20)
# create a vector 10 to 1
# putting () around a command will cause the result to be printed
(a <- seq(10,1,-1))

# second element
a[2]

# first 5 elements
a[seq(1,5)]

# first, third, and fourth elements
a[c(1,3,4)]
scores <- c(John=25, Marge=34, Dan=24, Emily=29)
scores[c("John", "Emily")]
scores[c(FALSE, TRUE, TRUE, FALSE)]
# this returns a logical vector...
scores < 30

# ...that we can now use to subset
scores[scores<30]
# list accepts a mixture of data types
# a list of a numeric vector, an integer vector, and a 
#   character vector
mylist <- list(1.1, c(1L,3L,7L), c("abc", "def"))
mylist
# list elements can be named as well
mary_info <- list(classes=c("Biology", "Math", "Music",
                            "Physics"),
                  friends=c("John", "Dan", "Emily"),
                  SAT=1450)
mary_info
# by position
mary_info[[2]]

# by name
mary_info$SAT

# second element of friends vector
mary_info$friends[2]
# create a 2x3 matrix, filling down columns
a <- matrix(1:6, nrow=2)
a

# now fill across rows 
b <- matrix(5:14, nrow=2, byrow=TRUE)
b
# row 2 column 3
a[2,3]

# all rows column 2
b[,2]

# all columns row 1
a[1,]
# a logical vector and numeric vector of equal length
mydata <- data.frame(diabetic = c(TRUE, FALSE, TRUE, FALSE), 
                     height = c(65, 69, 71, 73))
mydata
# row 3 column 2
mydata[3,2]

# using column name
mydata[1:2, "height"]

# all rows of column "height"
mydata[,"diabetic"]

# subsetting creates a numeric vector
mydata$height[2:3]

# this is a numeric vector
mydata[["height"]]
mydata[["height"]][2]
# get column names
colnames(mydata)

# assign column names
colnames(mydata) <- c("Diabetic", "Height")
colnames(mydata)

# to change one variable name, just use indexing
colnames(mydata)[1] <- "Diabetes"
colnames(mydata)
# number of rows and columns
dim(mydata)

#d is of class "data.frame"
#all of its variables are of type "integer"
str(mydata)
# mydata is of class data.frame
class(mydata)

# Height is a numeric vector
class(mydata$Height)

# colMeans(), for means of columns, wants input of class data.frame or matrix
colMeans(mydata)

# vector input to colMeans() produces an error
colMeans(mydata$Height)
# mydata is a data.frame
class(mydata)
#summary() calls summary.data.frame() if given a data.frame input
summary(mydata)
# run a regression and save model of class "lm" in object
model1 <- lm(Height ~ Diabetes, data=mydata)
class(model1)
# summary() calls summary.lm() if given an lm object
summary(model1)
# what classes of objects does generic function summary() accept?
methods(summary)
# what functions accept data frames as arguments?
methods(class="data.frame")
a <- c(0, 1)
a
a <- c(0, 1)
a
a <- c(10, seq(5, 1, -1))
a
a <- c(10, seq(5, 1, -1))
a
a <- c(rep(0,2), seq(1,5,by=2))
a
a <- c(rep(0,2), seq(1,5,by=2))
a
b <- data.frame(letters=c("a", "b", "c"), numbers=c(1,2,3))
dim(b)
b <- data.frame(letters=c("a", "b", "c"), numbers=c(1,2,3))
dim(b)

b
b <- data.frame(letters=c("a", "b", "c"), numbers=c(1,2,3))
b[2,]
b <- data.frame(letters=c("a", "b", "c"), numbers=c(1,2,3))
b[2,]
b <- data.frame(letters=c("a", "b", "c"), numbers=c(1,2,3))
b[b$numbers<2,]
b <- data.frame(letters=c("a", "b", "c"), numbers=c(1,2,3))
b[b$numbers<2,]
b <- data.frame(letters=c("a", "b", "c"), numbers=c(1,2,3))
b <- data.frame(letters=c("a", "b", "c"), numbers=c(1,2,3))

# letters column, element 3 (recommended method)
b$letters[3]

# row 3 column 1
b[3,1]

# element 1 (column 1) of data frame, then element 3 of that
b[[1]][3]
# confirm that it is a data frame
class(b)

# NA is not what we want, what is the warning trying to tell us?
mean(b)
# columns of data frames are vectors
class(b$numbers)

mean(b$numbers)

# no mean.data.frame
methods(mean)
# run this if you missed it earlier
library(tidyverse)
# comma separated values
dat_csv <- read_csv("https://stats.idre.ucla.edu/stat/data/hsbdemo.csv")
# tab separated values
dat_tab <- read_delim("https://stats.idre.ucla.edu/stat/data/hsb2.txt",
                      delim="\t")
dat_csv
# create a tibble data set manually
my_data <- tibble(height = c(67, 69, 72), weight=c(159, 181, 215))
my_data

# my_data is of class tibble (tbl_df), class table (tbl) and class data.frame
class(my_data)

# now just a data.frame
class(as.data.frame(my_data))

# print as a data.frame (will print entire dataset)
as.data.frame(my_data)
require(haven)
# SPSS files
dat_spss <- read_spss("https://stats.idre.ucla.edu/stat/data/hsb2.sav")
# Stata files
dat_dta <- read_stata("https://stats.idre.ucla.edu/stat/data/hsbdemo.dta")
library(readxl)
# this step only needed to read excel files from the internet
download.file("https://stats.idre.ucla.edu/stat/data/hsb2.xls", "myfile.xls", mode="wb")

dat_xls <- read_excel("myfile.xls")
# first 2 rows
head(dat_csv, 2)
# last 8 rows
tail(dat_csv, 8)
## View(dat_csv)
## # write a csv file
## write_csv(dat_csv, file = "path/to/save/filename.csv")
## 
## # Stata .dta file (need to load foreign package)
## write_dta(dat_csv, file = "path/to/save/filename.dta")
## 
## # save these objects to an .Rdata file
## save(dat_csv, mydata, file="path/to/save/filename.Rdata")
d_semi <- read_delim("https://stats.idre.ucla.edu/stat/data/hsbsemi.txt",
                     delim=";")
# more on next page
d_semi
d <- read_csv("https://stats.idre.ucla.edu/stat/data/hsbraw.csv")
d
mean(d$read)
median(d$read)
var(d$read)

summary(d$read)
# a scatterplot of read vs write
ggplot(data=d, aes(x=write, y=read)) + geom_point()
# use the bins= argument to control the number of intervals
ggplot(d, aes(x=write)) + geom_histogram(bins=10)
# Requesting the rows where math is less than its mean
ggplot(d[d$math < mean(d$math),], 
       aes(x=write)) + geom_histogram(bins=10)
ggplot(d, aes(x = write)) + geom_density()
# for the overall distribution of one variable, specify x=1 (or any other value)
ggplot(d, aes(x = 1, y = math)) + geom_boxplot()
# for the overall distribution of one variable, specify x=1 (or any other value)
ggplot(d, aes(x = 1, y = science)) + geom_boxplot()
# table() produces counts
table(d$female)
table(d$ses)

# for proportions, use output of table() 
#   as input to prop.table()
prop.table(table(d$female))
prop.table(table(d$ses))
# before, ses is a character variable
str(d$ses)

# converting ses to factor
#   we need to specify levels explicitly, otherwise R will
#   sort alphabetically
d$ses <- factor(d$ses, levels=c("low", "middle", "high"))

# Now a factor, notice the integer representation
str(d$ses)

# levels() lists all factor levels in order
levels(d$ses)
head(d$ses)
head(as.numeric(d$ses))

# the first observation of ses is equal to "low"...
d$ses[1] == "low"

# ...and its underlying integer is equal to 1
as.numeric(d$ses[1]) == 1
# alphabetic ordering fine here, so no need to specify levels
d$female <- factor(d$female)
levels(d$female)

d$prog <- factor(d$prog)
levels(d$prog)
ggplot(d, aes(x=prog)) + geom_bar()
# just a single correlation
cor(d$write, d$read)

# now isolate all test score variables
scores <- d[, c("read", "write", "math", "science", "socst")]
cor(scores)
# both scatter plot and loess smooth layers
ggplot(d, aes(x=read, y=write)) + 
  geom_point() +
  geom_smooth()
# first we group our data frame, d, by gender(female)
by_female <- group_by(d, female)

# notice that by_female is a grouped_df (data frame) now
class(by_female)
summarize(by_female, mean(math), var(math))
#remember for density plots we map the continuous variable to x
ggplot(d, aes(x=math, color=female)) +
  geom_density()
# for boxplot, map continuous variable to y
ggplot(d, aes(x=female, y=math)) +
  geom_boxplot()
# this time saving the freq table to an object
my2way <- table(d$prog, d$ses)

# counts in each crossing of prog and ses
my2way
# row proportions, 
#   proportion of prog that falls into ses
prop.table(my2way, margin=1)

# columns proportions,
#   proportion of ses that falls into prog
prop.table(my2way, margin=2)

ggplot(d, aes(x=ses, fill=prog)) + 
  geom_bar()
ggplot(d, aes(x=ses, fill=prog)) + 
  geom_bar(position="dodge")
# both scatter plot and loess smooth layers
ggplot(d, aes(x=read, y=write, color=prog)) + 
  geom_point() + geom_smooth()
# all functions after ggplot know
#   to look for variables in dataset "d"
ggplot(d, aes(x=ses, fill=prog)) + 
  geom_bar(position="dodge") +
  facet_wrap(~female)
hist(d$write)
plot(d$write, d$read)
# barplot wants a table input, not a data frame
#   (ggplot always wants a data.frame)
barplot(table(d$prog))
plot(d$write, d$read, col=d$prog)
ggplot(d, aes(x=awards)) + geom_density()
ggplot(d, aes(x=awards)) + geom_density()
# these are the progs again
table(d_semi$prog)
by_prog <- group_by(d_semi, prog)
summarize(by_prog, max(math))
by_prog <- group_by(d_semi, prog)
summarize(by_prog, median(math), IQR(math))
ggplot(d, aes(x=prog, y=math)) + geom_boxplot()
ggplot(d, aes(x=prog, y=math)) + geom_boxplot()
# read data in
d <- read_csv("https://stats.idre.ucla.edu/stat/data/hsbraw.csv")

# load packages for this section (if needed)
library(tidyverse)
d <- arrange(d, science, socst)
d
# subset to science values equal to -99, and then change
#  them all to NA
d$science[d$science == -99] <- NA
head(d$science, 10)
# read in data, specifying missing data codes
d <- read_csv("https://stats.idre.ucla.edu/stat/data/hsbraw.csv",
              na=c("", -99, "-99", "NA"))
d
# a sum involving "undefined" is "undefined"
1 + 2 + NA

# NA could be larger or smaller or equal to 2
c(1, 2, 3, NA) > 2

# mean is undefined
mean(c(1,2,3,4,NA))
# NA values will be removed first
sum(c(1,2,NA), na.rm=TRUE)

mean(c(1,2,3,4,NA), na.rm=TRUE)
# one of the values is NA
x <- c(1, 2, NA)

# check for equality to NA using == is wrong
# RStudio may give you a warning about this (to use is.na() instead)
x == NA

# this is correct
is.na(x)
# extract starting at first character, stopping at third
d$prog_short <- substr(d$prog, start=1, stop=3)

head(d[,c("prog", "prog_short")], n=5)
d$schtyp_ses1 <- paste(d$schtyp, d$ses, sep=" ")
head(d[, c("schtyp", "ses", "schtyp_ses1")], n=5)
# changing the delimiter to comma
d$schtyp_ses2 <- paste(d$schtyp, d$ses, sep=",")
head(d[, c("schtyp", "ses", "schtyp_ses2")], n=5)
my_char_vec <- c("here", "are", "some", "words", "to", "explore")

# indexes of elements that contain "re"
#   NOTICE that the pattern to be matched goes first, and
#     the input vector goes second
grep(pattern="re", x=my_char_vec)

# value=TRUE returns the strings that are matched
grep("re", my_char_vec, value=TRUE)
# this will add a column variable called logwrite to d
d$logwrite <- log(d$write)

# now we see logwrite as a column in d
colnames(d)

# d has 200 rows, and the rep vector has 300
d$z <- rep(0, 300)

# create 4 transformations of math
d <- mutate(d,
            logmath = log(math),
            mathrank = min_rank(math),
            mathgrade = cut(math,
                            breaks = c(0, 35, 45, 55, 65, 80),
                            labels = c("F", "D", "C", "B", "A")),
            zmath = scale(math)
)
# subset to observations with max reading score
max_read <- d[d$read==max(d$read),]
max_read
# subset to females with math and writing over 65
d_fem_hi_math <- d[(d$female == "female" & d$math > 65 & d$write > 65),]
d_fem_hi_math
# subset to students with math < 50 in the general or academic programs
d_gen_aca_low_math <- filter(d, (prog == "general" | prog == "academic") & math < 50)
head(d_gen_aca_low_math, n=3)
# rbind works because they have the same variables
d_append <- rbind(d_fem_hi_math, d_gen_aca_low_math)

# dimensions of component datasets
dim(d_fem_hi_math)

dim(d_gen_aca_low_math)

# appended dataset has rows = sum of rows of components
dim(d_append)
# select 4 variables
d_use <- select(d, id, female, read, write)
head(d_use, n=3)
# select everything BUT female, read, write
# note the - preceding c(female...)
d_dropped <- select(d, -c(female, read, write))
head(d_dropped, n=3)
d_all <- cbind(d_use, d_dropped)
head(d_all, n=3)
# first group data by cid (there are 20 classes)
by_class <- group_by(d, cid)

# then get mean/median on math by class
class_stats <- summarize(by_class, meanmath=mean(math), medmath=median(math))
class_stats
d_merged <- inner_join(d, class_stats)

# showing just a few variable for space
head(select(d_merged, cid, math, meanmath, medmath))
b <- c(1,2,3,NA)
sum(b<3)
b <- c(1,2,3,NA)
sum(b<3)

# remove NA first
sum(b<3, na.rm=TRUE)
# tibble() is basically same as data.frame()
#  but adds class "tbl_df" to data.frame
directory <- 
  tibble(names=c("Leo Smith", "Karen Smith", 
                 "Audrey Jones", "Dylan Jones"),
         phone=c("(323)555-5432", "(323)555-5421",
                 "(213)555-2154", "(213)555-2155"))
directory <- 
  data.frame(names=c("Leo Smith", "Karen Smith", 
                     "Audrey Jones", "Dylan Jones"),
             phone=c("(323)555-5432", "(323)555-5421",
                     "(213)555-2154", "(213)555-2155"))

directory$area_code <- substr(directory$phone, 2, 4)

directory

directory <- 
  tibble(names=c("Leo Smith", "Karen Smith", 
                 "Audrey Jones", "Dylan Jones"),
         phone=c("(323)555-5432", "(323)555-5421",
                 "(213)555-2154", "(213)555-2155"))
directory <- 
  tibble(names=c("Leo Smith", "Karen Smith", 
                 "Audrey Jones", "Dylan Jones"),
         phone=c("(323)555-5432", "(323)555-5421",
                 "(213)555-2154", "(213)555-2155"))

# match "Jones" in names
my_jones <- grep("Jones", directory$names)
my_jones

directory[my_jones,]
y1 <- tibble(Names=c("Mary", "Sue"),
             scores=c(36, 78))
y2 <- tibble(names=c("John", "Jack"),
             scores=c(25, 44))

# what happened?
rbind(y1, y2)
y1 <- tibble(names=c("Mary", "Sue"),
             scores=c(36, 78))
y2 <- tibble(names=c("John", "Jack"),
             scores=c(25, 44))

# now it will work when all columns match names exactly
rbind(y1, y2)
# create a datset of just test scores
test <- select(d, read, write, math, science, socst)

nontest <- select(d, -c(read, write, math, science, socst))

# sort test scores by test
test <- arrange(test, math)

# CONT>>>
# cbind runs without error
remerged <- cbind(test, nontest)

# but what's wrong here?
head(remerged, n=3)
# the values on the test scores don't match!
rbind(d[d$id==1,], remerged[remerged$id==1,])
# This time, add id to test dataset
test <- select(d, id, read, write, math, science, socst)

nontest <- select(d, -c(read, write, math, science, socst))

# sort test scores by test
test <- arrange(test, math)

# cbind runs without error
remerged2 <- merge(test, nontest)
# these should match now
rbind(remerged2[remerged2$id==1,], d[d$id==1,])
d <- read_csv("https://stats.idre.ucla.edu/stat/data/hsbdemo.csv")
# program and ses class appear to be associated
chisq.test(d$prog, d$ses)
# formula notation for independent samples t-test
t.test(math ~ female, data=d)
t.test(d$read, d$write, paired=TRUE)
# fit a linear model (ANOVA and linear regression)
m1 <- lm(write ~ read + female, data=d)
# printing an lm object will list the coefficients only
m1
# class of model object 
class(m1)

# associated methods for lm objects
methods(class="lm")
# ANOVA sequential sums of squares
anova(m1)
# regression table
summary(m1)
# just the coefficients
coef(m1)

# 95% confidence intervals
confint(m1)

# first 5 observed values, predicted values and residuals
head(cbind(d$write, predict(m1), residuals(m1)), n=5)
# plots all 4 plots at once (otherwise one at a time)
layout(matrix(c(1,2,3,4),2,2))

# 4 diagnostic plots
plot(m1)
layout(1)
# with no covariate dataset, predict returns predictions for the sample
# get predicted write scores for sample
d$predwrite <- predict(m1)
# plot read vs observed write as scatter
#  and read vs predicted write as lines colored by gender
ggplot(d, aes(x=read, y=write)) +
  geom_point() +
  geom_line(aes(y=predwrite, color=female))
# convert honors to a TRUE/FALSE (0/1) variable
d$hon = (d$honors == "enrolled")

# family=binomail uses link=logit by default
m2 <- glm(hon ~ read + female, data=d, family=binomial)
summary(m2)
# ORs
exp(coef(m2))

# confidence intervals on ORs
exp(confint(m2))
# first create the dataset of new values
#   female="male"" and female="female"" each repeated 100 times,
#   and read 1:100, repeated twice
preddata <- data.frame(female=c(rep("male", 100), rep("female", 100)),
                       read=rep(1:100, 2))

# then get predicted values based on model
preddata$honors <- predict(m2, newdata=preddata, type="response") 
ggplot(preddata, aes(x=read, y=honors, color=female)) +
  geom_line()
