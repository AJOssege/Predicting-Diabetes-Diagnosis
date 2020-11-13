library(faraway)
library(psych)

# read in diabetes data
# do exploratory

# load data
data(diabetes)

# view data frame
View(diabetes)

# plots of categoricals
plot(diabetes$location)
plot(diabetes$gender)
plot(diabetes$frame)

#descriptive Statistics
describe(diabetes)
