library(faraway)

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

# means 
mean(diabetes$chol, na.rm = TRUE)
mean(diabetes$stab.glu, na.rm = TRUE)
mean(diabetes$hdl, na.rm = TRUE)
mean(diabetes$ratio, na.rm = TRUE)
mean(diabetes$glyhb, na.rm = TRUE)
mean(diabetes$age, na.rm = TRUE)
mean(diabetes$height, na.rm = TRUE)
mean(diabetes$weight, na.rm = TRUE)
mean(diabetes$bp.1s, na.rm = TRUE)
mean(diabetes$bp.1d, na.rm = TRUE)
mean(diabetes$bp.2s, na.rm = TRUE)
mean(diabetes$bp.2d, na.rm = TRUE)
mean(diabetes$waist, na.rm = TRUE)
mean(diabetes$hip, na.rm = TRUE)
mean(diabetes$time.ppn, na.rm = TRUE)

# std dev
sd(diabetes$chol, na.rm = TRUE)
sd(diabetes$stab.glu, na.rm = TRUE)
sd(diabetes$hdl, na.rm = TRUE)
sd(diabetes$ratio, na.rm = TRUE)
sd(diabetes$glyhb, na.rm = TRUE)
sd(diabetes$age, na.rm = TRUE)
sd(diabetes$height, na.rm = TRUE)
sd(diabetes$weight, na.rm = TRUE)
sd(diabetes$bp.1s, na.rm = TRUE)
sd(diabetes$bp.1d, na.rm = TRUE)
sd(diabetes$bp.2s, na.rm = TRUE)
sd(diabetes$bp.2d, na.rm = TRUE)
sd(diabetes$waist, na.rm = TRUE)
sd(diabetes$hip, na.rm = TRUE)
sd(diabetes$time.ppn, na.rm = TRUE)


