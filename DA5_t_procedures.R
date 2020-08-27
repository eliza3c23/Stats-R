##########PART 1 START ############################################################################# 
# R code and explanation for data analysis 5 t procedures. 
# Read in the microbeers.csv dataset

microbeers= read.csv(file.choose(), header = TRUE)
# gives variable names.

names(microbeers) # gives variable names. 
hist(microbeers$abv,col="blue", main= "Alcohol by volume in sample craft beer",xlab ="The alcohol by volume in %",xlim = c(2,14),ylim = c(0,485))
mean(microbeers$abv)
sd(microbeers$abv)
summary(microbeers$abv)
t.test(microbeers$abv,mu= 5.0,conf.level = 0.95)

# Make an appropriate visual display for abv. 
# Recall hist() or boxplot()
# Add a title. 
# Add color and other aesthetics if you like. 
# See week 3 lessons or the script from Data Analysis #3. 

# Calculate the mean and standard deviation. mean() and sd()
# Again week 3 lessons or the script from Data Analysis #3. 

# Perform a t test using the t.test() command. 
# The format is t.test(data, mu = mu_0, alternative = "alt") 
# where data is a quantitative variable mu_0 is the hypothesized mean,
# and alt is either less, greater or two.sided (default).
# See lesson 27 for an R code example for t.test() or week 5 module R tutorials. 

##########PART 1 END ############################################################################# 

##########PART 2 START ########################################################################### 
# This section is only for your information. 
# You don't have to run this section. 

# Upload student data set
st314data = read.csv(file.choose(), header = TRUE)

# creates a side by side boxplot with customized axes.
boxplot(st314data$GamingHours ~st314data$International, 
        col = c("lightgreen", "lightblue"),
        axes = FALSE, horizontal = TRUE, 
        main = "Comparison of Gaming Hours between International and US ST314 Students", xlab = "Hours Spent Gaming")
# Adds customized axes
axis(2, at = c(1,2), c("International", "US"))
axis(1, at = c(seq(0,10,1),seq(20,70,10)))

# Calculate means, sd and sample size by group
aggregate(st314data$GamingHours~st314data$International, data = st314data, mean)
aggregate(st314data$GamingHours~st314data$International, data = st314data, sd)
aggregate(st314data$GamingHours~st314data$International, data = st314data, length)

# two sample t test
t.test(st314data$GamingHours~st314data$International)

##########PART 2 END ############################################################################# 


