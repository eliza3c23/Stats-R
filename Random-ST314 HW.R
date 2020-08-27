### R code
a = 42; b = 5;

r = seq(0,1,0.01) #Define range of x from 0 to 1

pdf = function(x) {a*x^b*(1-x)} #creates the pdf function


plot(r,pdf(r),type = "l", xlab = "", main = "Continuous VAR", lwd =2)


##End code

mu = 0; sigma = 1; x = -4.73; # Note you will have to change the value of x.
pnorm(x,mu,sigma)
#######
mu = 0; sigma = 1; p = 0.39; # Note you will have to change the value of p.
qnorm(p,mu,sigma)

mu = 0; sigma = 1; p = 0.76; # Note you will have to change the value of p.
qnorm(p,mu,sigma)
######################################################
##Code from HW 3
##Q7
IQ.data = c(83,	94,	99,	102,	103,	103,	106,	107,	107,	107,	108,	108,
            109,	109,	113,	113,	115,	115,	115,	116,	117,	117,	118,	118,
            122,	122,	123,	124,	127,	132,	136,	140,	140,	144)

summary(IQ.data) 
sd(IQ.data) 
hist(IQ.data) 
boxplot(IQ.data, horizontal = TRUE) 
##############################
##Q8
time=c(	382,	350,	355,	360,	378,	420,	324,	395,	404,	374,	374,	371,	362,
        366,	365,	327,	339,	395,	390,	368,	377,	356,	354,	409,	331,	397)
stem(time,1)
summary(time)
hist(time)
#Note: If your stems from R are not the same as below. Change 1 in the stem function to 2.
##########################################################################################
###Q10
gpa=c(116.40, 115.90, 114.80, 115.20, 115.60)
summary(gpa)
gpa-50
gpa
summary(gpa-50)
sd(gpa)
sd(gpa)^2
######################
DA_3_1= c(33.3,29.1,35.6)
summary(DA_3_1)
####################
##ranSam=c(2096,2010,2008,1855,2019)
ranSam=c(1994,1972,2004,1826,2018)
summary(ranSam)
sd(ranSam)
#####################
###############################################Webassign HW WK5

data = c(417,420, 422,423,426,426,432,434,437,438,446,447,448,451,456,461,464)
boxplot(data, horizontal = TRUE)
mean(data)
sd(data)
summary(data)
#####################q2
data2 =c(2752,2897,3004,2841,2881)
mean(data2)
sd(data2)
summary(data2)
##When look for the 95% t- critical value, we will find the 97.5th percentile/2.5th percentile.
qt(0.975, 4)
##Use the ans from above to plug into the t-test formula.
## Find the p-value (Only one sided), needs to find two sided because the Ha is not 3000.
##0.975 is not t-stat. The correct way is pt(t-stat,4)
pt(qt(0.975,4),4)
##use the t-test function to find t-score and p-value:
t.test(data2, mu = 3000, conf.level = 0.95)
########
#####################q3
Percent = c(5.5,3.8,3.8,2.0,5.9,2.5,2.1,3.1,3.8,0.8,3.5,4.6,3.7,2.7,4.1,4.2,3.8,0.0,3.5,4.5,7.2,8.3,4.2,8.5,6.9,5.0,3.8,2.8)		
#Get Histogram for data[Percent]
hist(Percent)
mean(Percent)
sd(Percent)
#Perform t-test. Mu is the clain aka Ho. IT ALSO CALCULATED THE CI.
t.test(Percent, mu = 5.0, conf.level = 0.90)
#####
####################q5
n1 = 9
x1 = 115.3
s1 = 5.01
n2 = 9
x2 = 129.1
s2 = 5.39
min(n1-1,n2-1)
qt(0.975,8)
#####################q6
nA =9
xA =23.73
sA =0.53
nB =4
xB =20.82
sB =0.540
min(nA-1,nB-1)
qt(0.975,3)
pt((qt(0.975,3)),3)
A= rnorm(9,mean=23.73,sd=0.53)
B= rnorm(4,mean=20.82,sd=0.54)
t.test(A,B,conf.level = 0.95,var.equal = TRUE)
####

#####################q7
##YF would be the n1 population, and OF would be the n2 population
YF = c(28,35,31,27,28,32,31,34,32,28,29,37,29,34,33,27,28,32,31,34)
OF = c(23,19,21,23,22,18,15,25,19,13,21,17)
boxplot(YF,OF)
#This t-test perform Ha: YF > OF
#Mu1-Mu2 > 0
t.test(YF,OF,alternative = "greater")
t.test(YF,OF)$conf.int
####
#######################q9
U = c(36.3,55.0,51.5,38.8,43.2,48.8,25.6,49.7)  
A = c(28.5,20.0,46.0,34.5,36.0,52.5,26.5,46.5)
t.test(U,A,alternative = "greater",paired = TRUE)
##To calculate the sd for d(difference)
sd(U-A)
#######
#########################
##Week6 HW

Wheat=c(	5.3,	4.6,	6.0,	6.2,	6.7,	5.7)
Barley=c(	6.6,	8.1,	6.2,	7.5,	5.9,	5.5)
Maize=c(	5.8,	4.8,	6.4,	4.9,	6.1,	5.1)
Oats = c(	8.4,	6.2,	7.9,	6.9,	5.4,	7.1)
##Mean for each group
mean(Wheat)
mean(Barley)
mean(Maize)
mean(Oats)

##Var for each group
var(Wheat)
var(Barley)
var(Maize)
var(Oats)
##
##############
###WebAssign Wk8 
#7 Calculate 95%CI
x=c(5	,12,	14,	16,	23,	30,40	,50,	55,	67,	72,	83,	96,	112,	127)
y=c(4,	10,	13,	15,	15,	25,	27,	46,	38,	46,	53,	72,	82,	99,	103)
mod7 = lm(y~x)
summary(mod7)
confint(mod7, conf.level=0.95)
##############################
#8
x=c(	13,	18,	30,	43,	45)
y=c(	250,	350,	460,	500,	560)
# Construct a scatterplot using R. 
plot(x,y)

# Calculate the least squares regression line 
mod = lm(y~x)
summary(mod)

# Correlation 
cor(x,y)
##################
##Wk8 Collaborative Learning Data
st314Data = read.csv(file.choose(), header = TRUE)
#attach(st314Data)
xx =st314Data$CreditHours
yy =st314Data$SchoolWorkHours
#Scatterplot for the variables above
plot(xx,yy, main= "The number of school work hours vs The number of credit hours",col = "orange",pch=16, xlab = "# of credit hours",ylab = "# of school work hours")
cor(xx,yy)
##########
#####WEB ASSIGN WK9
x1 =c (1250,1300,1350,1250,1300,1250,1300,1350 ,1350)
x2 =c(6,7,6,7,6,8,8,7,8)
y =c(80,95,101,85,92,87,96,106,108)
######create linear model
mod = lm(y ~ x2+x1)
summary(mod)
######Conf Level
confint(mod,conf.level = 0.95)
#######
1 - pf(12.49999713, 6, 30)
##########
x3 = c(16.7,17.4,18.4,16.8,18.9,17.1,17.3,18.2,21.3,21.2,20.7,18.5) 
x4 = c(30,42,47,47,43,41,48,44,43,50,56,60)
y1 = c(210,110,103,103,91,76,73,70,68,53,45,31)
mod = lm(y1~x3+x4)
summary(mod)
#confint.lm(mod,x3 =16.8,x4 = 47,conf.level=0.95)
predict(mod,data.frame(x3 = 16.8,x4 = 47),conf.level =0.95,interval = "confidence")
###### Example from webassign
##########
x1 = c(16.7,17.4,18.4,16.8,18.9,17.1,17.3,18.2,21.3,21.2,20.7,18.5) 
x2 = c(30,42,47,47,43,41,48,44,43,50,56,60)
y = c(210,110,103,103,91,76,73,70,68,53,45,31)
mod = lm(y~x1+x2)
summary(mod)
predict(mod,data.frame(x1 = 18.9, x2= 43),conf.level = 0.95,interval = "confidence")
confint(mod,conf.level=0.95)
####
1-pf(2.80,2,14)
#####
temp = c(31, 21 ,33 ,36 ,29 ,27,28 ,31 ,20 ,24 ,21 ,26 ,26 ,28 ,29)
temp.factor =c(rep("50",5),rep("60",5),rep("70",5))
modA = aov(temp~temp.factor)
summary(modA)
####
price =c(180,160,85,70,60,45)
score =c(76,71,61,57,30,35)

modL =lm(price~score)
summary(modL)   #To get the SSE?!
sum(modL$residuals*2)
#######
##WK10 Webassign HW
k.sdevs = c(0.204,	0.317,	0.098,	0.185,	0.231,	0.211,	0.324,	0.285,
            0.147,	0.208,	0.054,	0.148,	0.275,	0.354,	0.161,	0.216,
            0.388,	0.186,	0.148,	0.227,	0.274,	0.120,	0.093,	0.059)
mean(k.sdevs)

# Construct your own Control Chart.
LCL = 0.03781 ##### change this value to be the same as your calculated LCL
UCL = 0.3716 ##### change this value to be the same as your calculated UCL
plot(k.sdevs)
abline(h = mean(k.sdevs))
abline(h = LCL, lty = 2) 
abline(h = UCL, lty = 2)
#############
non.conforming = c(9, 16, 21, 19, 40, 19, 7, 28, 14, 27, 27, 13, 15, 24, 21, 19, 18, 20, 11, 20, 17, 18, 13, 21, 29, 14, 8, 18, 12, 30) 
#Construct a p chart by using the following code. You will need to enter your values for pbar, LCL and UCL. 
pbar = 0.0945
  LCL =0.034 
  UCL =0.1566
  plot(non.conforming/200, ylim = c(0,.5))
abline(h = pbar, lty = 2)
abline(h = LCL, lty = 3)
abline(h = UCL, lty = 3)
##############
defects = c(2, 7, 5, 4, 5, 3, 8, 3, 4, 2, 5, 8, 1, 3, 1, 4, 7, 3, 2, 4, 5, 1, 5, 4, 5) 
#Construct a c chart by using the following code. You will need to enter your values for xbar, LCL and UCL. 
mean(defects)
xbar = 4.04
  LCL = 0 
  UCL = 10.0699
  plot(defects, ylim = c(0,12))
abline(h = xbar, lty = 2)
abline(h = LCL, lty = 3)
abline(h = UCL, lty = 3)
###############

#Sample No.	Moisture-Content Observations
subsamp1=c(	  12.1  ,	  12.1  ,	  13.1  ,	  13.0  ,	  13.0  )
subsamp2=c(	  12.4  ,	  13.3  ,	  12.8  ,	  12.6  ,	  12.9  )
subsamp3=c(	  12.9  ,	  12.7  ,	  14.2  ,	  12.5  ,	  12.9  )
subsamp4=c(	  13.2  ,	  13.0  ,	  13.0  ,	  12.6  ,	  13.7  )
subsamp5=c(	  12.8  ,	  12.3  ,	  12.2  ,	  13.3  ,	  12.0  )
subsamp6=c(	  13.7  ,	  13.4  ,	  13.1  ,	  12.4  ,	  13.2  )
subsamp7=c(	  12.2  ,	  14.4  ,	  12.4  ,	  12.4  ,	  12.5  )
subsamp8=c(	  12.6  ,	  12.8  ,	  13.5  ,	  13.9  ,	  13.1  )
subsamp9=c(	  14.6  ,	  13.4  ,	  12.2  ,	  13.7  ,	  12.1  )
subsamp10=c(	  12.8  ,	  12.3  ,	  12.6  ,	  13.2  ,	  12.8  )
subsamp11=c(	  12.6  ,	  13.1  ,	  12.7  ,	  13.2  ,	  12.3  )
subsamp12=c(	  13.5  ,	  12.1  ,	  12.8  ,	  13.1  ,	  12.9  )
subsamp13=c(	  13.4  ,	  13.3  ,	  12.0  ,	  12.9  ,	  13.1  )
subsamp14=c(	  13.5  ,	  12.4  ,	  13.0  ,	  13.6  ,	  13.4  )
subsamp15=c(	  12.3  ,	  12.8  ,	  13.0  ,	  12.8  ,	  13.5  )
subsamp16=c(	  12.6  ,	  13.4  ,	  12.1  ,	  13.2  ,	  13.4  )
subsamp17=c(	  12.1  ,	  12.7  ,	  13.4  ,	  13.0  ,	  13.9  )
subsamp18=c(	  13.0  ,	  12.8  ,	  13.0  ,	  13.3  ,	  13.1  )
subsamp19=c(	  12.4  ,	  13.2  ,	  13.0  ,	  14.0  ,	  13.1  )
subsamp20=c(	  12.7  ,	  12.4  ,	  12.4  ,	  13.9  ,	  12.8  )
subsamp21=c(	  12.6  ,	  12.8  ,	  12.7  ,	  13.4  ,	  13.0  )
subsamp22=c(	  12.7  ,	  13.4  ,	  12.1  ,	  13.2  ,	  13.2  )

# Combine the rows of sub samples into a data frame. 
# This structures data appropriately for control chart functions. 
moisture_content = data.frame(rbind(subsamp1,subsamp2,subsamp3,subsamp4,subsamp5,subsamp6, 
                                    subsamp7,subsamp8,subsamp9,subsamp10,subsamp11,subsamp12,subsamp13,subsamp14,
                                    subsamp15,subsamp16,subsamp17,subsamp18,subsamp19,subsamp20,subsamp21,subsamp22))
# Check out the data frame 
moisture_content 
# Check subgroup means 
rowMeans(moisture_content)
# Create x-bar - s chart 
qcc(moisture_content, type = "xbar") 
#######Practice P-chart
p.data =c(20,10,13,15,20,17,16,14,29,16)
n=100
qcc(p.data,type = "p",sizes = n)
