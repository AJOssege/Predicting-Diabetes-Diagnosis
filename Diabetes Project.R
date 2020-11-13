library(faraway)
library(psych)

# read in diabetes data
# do exploratory

# load data
data(diabetes)

# view data frame
View(diabetes)

# plots of categoricals
plot(diabetes$location, main="Location Samples", ylab="Count", col="orange")
plot(diabetes$gender, main="Gender", ylab="Count", col="green")
plot(diabetes$frame,  main= "Body Frame", ylab="Count", col="blue" )


#descriptive Statistics
describe(diabetes)
