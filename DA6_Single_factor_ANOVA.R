##########PART 1 START ############################################################################# 
# R code and explanation for data analysis 6. 
# Read in the Student Informatio Data. Call it st314data. 

st314data= read.csv(file.choose(), header = TRUE)
# gives variable names.

names(st314data) # gives variable names.
levels(st314data$Major) # gives group names for majors

# Majors has too many options with too few students in a few of them. We need to combine a few groups. 
# We can do  this with the revalue() command from the plyr package. You need to run this step prior to the rest. 

install.packages("plyr")
library("plyr")
simple.majors = revalue(st314data$Major, replace =c("Chemical Engineering" = "CE, IE or Other", 
                                                    "Other" = "CE, IE or Other",
                                                    "Industrial Engineering" = "CE, IE or Other",
                                                    "Computer Science" = "CS or ECE",
                                                    "Electrical Engineering" = "CS or ECE"))


# This analysis compares number of roommates among majors of ST314 students. 
# Follow along with my code. 
# For your own analysis choose either "Salary","GamingHours" or "SocialTime  to compare between majors. 

# Create a side-by-side boxplot to compare number of roommates among the different majors. 
# Expand  the plot if you can't see the group names.
# add a title with main = "title" and a vertical axis title using ylab = "title". 
# color can be added with the rainbow commmand or blues9, or by assigning a specific color like "yellow"

boxplot(Roommates~simple.majors, data = st314data, main = "Comparison between Major and Number of Roommates 
        for ST314 FallStudents", ylab = "Number of roommmates", col = rainbow(5))

# Find the mean, sd and group sizes using the aggregate command. 
# aggregate splits the quantitative variable by the categorical variable groups then performs the command specified. 

aggregate(Roommates~simple.majors, data =st314data, mean)
aggregate(Roommates~simple.majors, data =st314data, sd)
aggregate(Roommates~simple.majors, data =st314data, length)

# Perform a single factor ANOVA F test to compare the means using the aov() command. 
# call the output something. I like to call it "mod". Then take the summary() of this object to get the ANOVA table.  
mod = aov(Roommates~simple.majors, data =st314data)
summary(mod)

# If the single factor ANOVA test indicates a difference in means, 
# perform a multiple comparisons procedure to see which means differ significantly. 
# Use the TukeysHSD() command with your object from the aov() command. For this example this is called "mod".  
TukeyHSD(mod, conf.level = 0.90)

######### YOUR TURN!!!! ###################################################################################

#Choose "Salary", "GamingHours" or "SocialTime and compare it among majors. 
##Pick SOCIAL HOURSSSS
#You can do this by simply changing Roommates (and only roommates) in each line of code above. 
boxplot(SocialTime~simple.majors, data = st314data, main = "Comparison between Major and Social Time 
        for ST314 FallStudents", ylab = "Number of Social Time per day", col = rainbow(5))

aggregate(SocialTime~simple.majors, data =st314data, mean)
aggregate(SocialTime~simple.majors, data =st314data, sd)
aggregate(SocialTime~simple.majors, data =st314data, length)

mod = aov(SocialTime~simple.majors, data =st314data)
summary(mod)
TukeyHSD(mod, conf.level = 0.90)
#############################################################################################################

