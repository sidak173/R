#ASSIGNMENT1

#generate RANDOM DATA USING LCM Method
# find mean,median,mode,standard deviation

v1=c()
a<-3
c<-10
m<-101
num<-750
x0<-13
total=c()

for(i in 1:750)
{
  v1<-append(v1,0)
  total<-append(total,0)
}

getmode<- function(v1) {
  uniqv<- unique(v1)
  uniqv[which.max(tabulate(match(v1, uniqv)))]
}

for(i in 1:6)
{
  
  cat(sprintf("Subject %d \n", i))
  for(j in 1:num)
  {
    x0=(a*x0 + c)%%m
    v1[j]=x0
  }
  
  
  print(v1)
  cat(sprintf("%f is mean \n", mean(v1,750)))
  cat(sprintf("%f is median \n", median(v1)))
  cat(sprintf("%f is mode \n", getmode(v1)))
  cat(sprintf("%f is standard deviation \n", sd(v1)))
  

  
  hist(v1)
  total=total+v1[]
  x0=runif(1,1,100)
}

cat(sprintf("total of 6 Courses: \n"))
print(total)

cat(sprintf("%f is mean \n", mean(total,750)))
cat(sprintf("%f is median \n", median(total)))
cat(sprintf("%f is mode \n", getmode(total)))
cat(sprintf("%f is standard deviation \n", sd(v1)))
hist(total,col="red")





#ASSIGNMENT2
#find descriptive stats,find outliers
#find mean,median,sd of all colls

data <- read.csv("Sacramentorealestatetransactions.csv")

print(data)


data[is.na(data)] <- 0

print("Mean: ")

#Mean of all columns

colMeans(data[sapply(data, is.numeric)]) 

print("Median: ");

#Median of all columns

apply(data[,c(1:ncol(data))], 1, median) 


#Standard deviation

print("Standard Deviation:");

apply(data[,c(1:ncol(data))], 1, sd)


#Plotting histogram for all values

hist(data$beds)
hist(data$baths)

#Outliers

print("Outliers")

outliers <- function(x) {
  
  Q1 <- quantile(x, probs=.25)
  Q3 <- quantile(x, probs=.75)
  iqr = Q3-Q1
  
  upper_limit = Q3 + (iqr*1.5)
  lower_limit = Q1 - (iqr*1.5)
  
  for(i in x)
  {
    if (i>upper_limit | i<lower_limit)
      print(i)
  }
  
}

print_outliers<- function(data, cols = names(data)) {
  for (col in cols) {
    cat(sprintf("%s: \n", col))
    outliers(data[,col]) #PRINTING OUTLIERS FOR THAT PATICULAR COLUMN
  }
}

print_outliers(data, colnames(data))

#OULIERS ALTERNATE METHOD
x=summary(data$baths)
iqr=x[5]-x[1]
upperlimit=x[5]+1.5*iqr
lowerlimit=x[1]-1.5*iqr
for(i in data$baths)
{
  if(i>upperlimit | i<lowerlimit)
    {
    print(i)
  }
}

#ASSIGNMENT 3
# FIND MEAN,MEDIAN,SD,RANGE,variance,root mean square deviation
#draw gausian plots to identify skewness and kurtosis

library(moments)
library(Metrics)

mode <- function(v)
{
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
subjects = c("Sub1", "Sub2", "Sub3", "Sub4", "Sub5", "Sub6")
m = matrix(nrow = 6, ncol = 750)
q = matrix(nrow = 1, ncol = 750)
#sum = runif(750, min = 0, max = 0)
#sum = rnorm(750)
sum = round(sample(0, size=750, replace=TRUE), 0)
for(i in 1:6)
{
  print(paste("Subject :",subjects[i]))
  # a = round(runif(1, max = 50), 0)
  #c = round(runif(1, max = 50), 0)
  #a = round(rnorm(1, mean=50), 0)
  #c = round(rnorm(1, mean=50), 0)
  # a = round(sample(1:100, size=1, replace=TRUE), 0)
  # c = round(sample(1:100, size=1, replace=TRUE), 0)
  for(j in 1:750)
  {
    m[i,j] = round(sample(1:100, size=1, replace=TRUE), 0)
    q[1,j] = round(sample(1:100, size=1, replace=TRUE), 0)
    sum[j] = sum[j] + m[i,j]
  }
  print(paste("Mean : ", round(mean(m[i,]), 2)))
  print(paste("Median : ", median(m[i,])))
  print(paste("Mode : ", mode(m[i,])))
  print(paste("Standard Deviation : ",round(sd(m[i,]), 2)))  
  print(paste("Range : ", range(m[i,])))
  print(paste("Mean Deviation : ", mad(m[i,], center=mean(m[i,]))))   # sigma(xi-mean)/N
  print(paste("Variance : ", var(m[i,])))   #sigma^2=1/n sigma(fi(xi-mean)^2))
  print(paste("RMSE : ", rmse(m[i,], q[1,])))  # sqrt(1/n sigma (fi (mi-qi)^2)) ROOT MEAN SQUARE ERROT
  print(paste("Skewness : ", skewness(m[i, ])))
  #skewness is a measure of the asymmetry of the probability distribution of a random variable about its mean
  print(paste("Kurtosis : ", kurtosis(m[i, ])))
  #kurtosis is a measure of flatness or peakedness of the probabilty distribution curve
  plot(density(m[i, ]), main = paste("Gaussian Plot : ", subjects[i]))
  writeLines("\n")
}

#ASSIGNMENT 4
#QUES1
#birthday paradox
#A. 0.747565

n=floor(runif(1, 2,100))
n
prod=1
i=0
while(i<n)
{
  prod=prod*((365-i)/365)
  i=i+1
}
#prod
prob=1-prod
cat("Probabaility of two people having same birthday for ",n," persons: ",prob)

n=1
prob=0
while(prob<0.5)
{
  prod=1
  i=0
  while(i<n)
  {
    prod=prod*((365-i)/365)
    i=i+1
  }
  prob=1-prod
  n=n+1
}
cat("Smallest Value of n for which probability is greater than 0.5: ",n-1,"\n")

#C calculate average probabilty over N days
# how prababilty varies as days/trials increas

n=floor(runif(1, 2 , 365))
prob<-vector(length=n)
s<-1
for(i in 0:n-1){
  s=(s*(365-i))/365
  pro=1-s
  prob[i]=pro
}

sameday=sum(prob)/n
print(cat("Probability on same day for ",n," persons: ",sameday))
plot(prob)

#ques2
#payoff function
#A friend has a coin with probability .6 of heads. She proposes the following
# gambling game. You will toss it 10 times and count the number of heads. The amount
# you win or lose on k heads is given by (k2-7k)

n=300 # no of trials
x=0
arr=c()
amt=0

for(i in 1:n)
{
  k=sample(c(0,1),size=10, replace = TRUE, c(0.4,0.6))
  x=sum(k) #number of heads
  amt=(x*x)-(7*x) #k^2-7k
  arr=append(arr,amt)
}

hist(arr) #payoff fn
plot(arr)

if(sum(arr)>0)
{
  cat(sprintf("good"))
} else {
  cat(sprintf("bad"))
}

#ASSIGNMENT-5
#ques1
#In a selection of a sample of size 250 one by one where both defective and nondefective items are equally likely. Now perform the simulation to calculate the
# estimated probability of getting the same type of item 16 times in a row

sum=0

for(i in 1:1000)
{
  
  s=rbinom(n=250,size=1,prob=0.5)
  #bernoulli distribution generated for 250 flips of coins
  # 0 1 0 0 ->defective non defective defective..

  v=table(s)
  b=rle(s)
  #runtime length encoding
  # lengths:0 0 1 1 1 0
  # values:2    3    1
  a=sum(rle(s)$length==16) # rle==16
  sum=sum+(a/250) #a/size
}

cat("Average probability for 1000 rounds:", sum/1000)

#QUES2?
#In sample of size eight of question 1, estimate the probability of selecting a
# different type of item in each selection, that is, that will never obtain get two
# defective items or two non-defective items in a row. 
s = rbinom(n=8,size=1, prob=0.5)
a = sum(rle(s)$length >= 2) 
cat("Probability in sample size 8:",1 - a/8) 

s2 = rbinom(n=250,size=1, prob=0.5)
#print(rle(s2)$value)
#print(rle(s2)$length)
a2 = sum(rle(s2)$length >= 2)
cat("Probability in sample size 250:",1 - a2/250)

#ASSIGNMENT-6
#ques1 Sorting
# Six animals with some names are lined up together. Calculate the probability of
# lineup in an order of alphabetic series with a assumption that none is having the same
# name.

fun<- function(b,a)
{
  x=sample(b) # makes random permutaiton of b
  if(identical(x,b)) # checks if  permutation is = b
    return(1)
  else{
    return(0)
  }
}
a<-c("dog","cat","rat","lion","fox","tiger") 
b=sort(a)
n=1000
s<-replicate(n,fun(b,a)) #run fun(b,a) n times
print(sum(s)/n)
plot(s)

#QUES2
# In Question 3, let suppose 3 animals are dogs and remaining are horses. Now
# calculate the probability all dogs come first.
# D D D H H ...

fun<- function(b,a)
{
  x=sample(b)
  if(identical(x,b))
    return(1)
  else{
    return(0)
  }
  
}
a<-c("dog","dog","dog","horse","horse","horse")
b=sort(a)
n=100
s<-replicate(n,fun(b,a))
print(sum(s)/n)
plot(s)




#ASSIGNMENT 7
#Ques1
# Simulate normal distribution values. Imagine a population in which the average height
# is 1.70 m with a standard deviation of 0.1. Use rnorm to simulate the height of 1000 people
# and save it in an object called heights.

#NORMAL DISTRIBUTION - continous distribution characterized by bell shaped curve
#Plot the density of the simulated values

heights<-rnorm(1000, 1.70, 0.1) #rnorm(n,mean,sd) generates 1000 random nos for given mean and sd
d<-(density(heights)) 
plot(d, main = 'Density')

heights1<-rnorm(10000, 1.70, 0.1)
d1<-(density(heights1))
plot(d1, main = 'Density',col='red')

#Find the 90% interval of a population with mean = 1.70 and standard deviation = .1
# between 0.05 and 0.95

c(qnorm(0.05, 1.70, 0.1), qnorm(0.95, 1.70, 0.1)) # ans
#qnorm (x,mean,sd) gives cuttoff associated with given probabilty x
#if p(x)=p0 what is x?

#Calculate the qvalue corresponding to every percentile in standard normal distribution.

quantiles<-seq(0, 1, by = .05)
qvalues<-qnorm(quantiles)

#qvalues - p(x)=p0 x->qvalue

plot(qvalues,
     type = 'l',
     xaxt = 'n',
     xlab = 'Probability Density')
#Calculte the pvalues corresponding to z values ranging from 0 to 1 at an interval of
#0.05

# p values denote area under the curve before x
# cdf

pvalues<-pnorm(quantiles)
plot(pvalues,
     xaxt = 'l',
     main = 'cdf of the standard normal',
     xlab = 'Quantiles',
     ylab = 'Probability Density')

#ques2
# Calculate simple (linear) correlation between car price and its fuel economy
# (measured in miles per gallon, or mpg)

a<-read.csv("auto.csv")
res1 <- cor.test(a[,3], a[, 4], method = "pearson") #perform pearson corelation test b/w col3 and col1
print(res1)

# Create a correlation matrix by selecting each pair of columns from dataset one by
# one and calculate correlation between selected pairs. Fill the values in matrix
# named as correlation matrix.

data1<-a[, c(3, 4, 5, 6, 7, 8, 9, 10, 11, 12)]
correlation_matrix <- cor(data1) #corelation matrix

# Create a new dataframe, auto_num, that contains only columns with numeric
# values from the auto dataframe. You can do this using the Filter function. Use the
# cor function to create a matrix of correlation coefficients for variables in the
# auto_num dataframe

df_new <- data.frame(data1)

#install.packages('corrgram')
library(corrgram)

# Use the corrgram function from the corrgram package to create a default
# correlogram to visualize correlations between variables in the auto dataframe.

corrgram(df_new, order = TRUE,
         lower.panel = panel.shade,
         upper.panel = panel.pie,
         text.panel = panel.txt,
         main = "correlegram")

# Create a new dataframe, auto_subset, by subsetting the auto dataframe to include
# only the Price, MPG, Hroom, and Rseat variables. Use the new dataframe to
# create a correlogram that (1) shows correlation coefficients on the lower panel, and
# (2) shows scatter plots (points) on the upper panel.


data2<-a[, c(3, 4, 5, 6)]
df2 <- data.frame(data2)



corrgram(df2, order = TRUE,
         lower.panel = panel.cor,
         upper.panel = panel.pts,
         text.panel = panel.txt,
         main = "correlegram")


#ASSIGNMENT8
#q1
#Implement the linear regression on a regression dataset to be downloaded from LMS
# using the concept of training and testing in order to understand the accuracy of results
# using the following metric

dataset<-read.csv('regressionDataSet.csv')
#install.packages('caTools')
# install.packages('Metrics')
library(caTools)
library(Metrics)
split = sample.split(dataset$RMSD, SplitRatio = 0.8) # RMSD IS ANY COL FROM DATASET
# 80% TRAINING 20 % TESTING
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)
regressor = lm(formula = RMSD ~ .,data = training_set) # RMSD ~ ALL COLS
y_pred = predict(regressor, newdata = test_set) # PREDICTING FROM TESTING SET
#correlation between predicted value and actual value on testing data
res1 <- cor.test(y_pred, test_set$RMSD, method = "pearson")
print(res1)

#ACCURACY METRIC

rmse(dataset$RMSD,y_pred)
print(paste("AIC =",AIC(regressor))) #Akaike's  Information Criterion
#mesaure of the goodness of fit
print(paste("BIC =",BIC(regressor))) # Bayesian Information Criterion

#) Visualization of best fit line

plot(y_pred ~ test_set$RMSD, data = test_set)
abline(lm(y_pred ~ test_set$RMSD))

#QUES2 Assignment-8

# Execution of the following 3 R commands will give us the data {(x(i), y(i), z(i), i = 1,
#                                                                 2, .,100}.
# x<-rpois(100, 50)
# y<-rpois(100, 100)
# z<-rpois(100, 150)
# Using this data

x <- rpois(100, 50)
y <- rpois(100, 100)
z <- rpois(100, 150)

#Fit the linear regression model of the form z = a +b.x + c.y using,

model<- lm(z ~ x + y)
summary(model)
plot(z, x + y)

# Fit the 3 models of the form y = a + b.x, y = a + b.x + c.x2
# , and y = a.b^x
# to this data
# using

model1 <-lm(y ~ x)
summary(model1)

model2 <-lm(y ~ poly(x, 2, raw = TRUE))
summary(model2)

model3 <-lm(log(y) ~ x)
summary(model3)

