# Exploratory data Analysis 

# Functions of Apply Family 
# apply, sapply, lapply, tapply ... mapply, vapply, ...
head(state.x77)
help(state.x77)

apply(state.x77,  2   , median)  # Two options for second argument either 2 or 1
# 2 means - function will operate columnwise 
apply(state.x77,  1   , median) 
apply(state.x77,  2   , mean)
apply(state.x77, 2, function(x) c(median(x), max(x), min(x)))

library(lattice)
head(barley)
unique(barley$site)  # Unique values of cols 
length(unique(barley$site)) # Count of unique values
length(barley$site)
dim(barley)
lapply1 <- lapply(barley, function(x) length(unique(x)))  # List apply 
class(lapply1)

sapply(barley, function(x) length(unique(x))) # Simple Apply, table/vector 
class(sapply(barley, function(x) length(unique(x))))

# tapply function 
tapply(barley$yield, barley$site, mean)
tapply(barley$yield, list(barley$year, barley$site) , mean)
View(barley)
unique(barley$year)
x1 <- c("Anuja", "Deepika")
length(x1)
nchar(x1)

tapply(barley$yield, barley$site, function(x) c(mean(x), min(x)))

# vapply , mapply ... 

# Boxplot 
head(mtcars)
View(mtcars)
fivenum(mtcars$mpg)
IQR(mtcars$mpg)  # Interquartile Range , diff of 75th percentile - 25th percentile
22.80 - 15.35
boxplot(mtcars$mpg)

# Outlier 
mtcars1 <- edit(mtcars)
View(mtcars1)
boxplot(mtcars1$mpg)

# If any obs > 3rd quartile + 1.5*IQR
# If any obs < 1st quartile - 1.5*IQR

fivenum(mtcars1$mpg)
IQR(mtcars1$mpg)
23.60 + (1.5*8)  #35.6
15.20 - (1.5*8)  #3.2

# P1 = 10 , P2 = 15, P3 = 8 , P4 = 65, P5 = 7 
# A1 = 23,  A2 = 29, A3 = 17, A4 = 67, A5 = 15

data(iris)
head(iris)
unique(iris$Species)
table(iris$Species)
# sapply(iris, function(x) length(unique(x)))
boxplot(iris$Sepal.Length)
boxplot(iris$Sepal.Length  ~ iris$Species)
boxplot(iris$Sepal.Length  ~ iris$Species, main = "MyFirstBoxPlot")
boxplot(iris$Sepal.Length  ~ iris$Species, main = "MyFirstBoxPlot", 
                         xlab="Species", ylab="Sepal Length")
boxplot(iris$Sepal.Length  ~ iris$Species, main = "MyFirstBoxPlot", 
        xlab="Species", ylab="Sepal Length", col = "red")
colors()
boxplot(iris$Sepal.Length  ~ iris$Species, main = "MyFirstBoxPlot", 
        xlab="Species", ylab="Sepal Length", col = c("red", "blue", "green"))

# Want to save the plot in a png file 
# Open a png device 
png(file="myplot1308.png")
getwd()
boxplot(iris$Sepal.Length  ~ iris$Species, main = "MyFirstBoxPlot", 
        xlab="Species", ylab="Sepal Length", col = c("red", "yellow", "violet"))
# Close the png device
dev.off()

# To change background colors 
par(bg="white")
boxplot(iris$Sepal.Length  ~ iris$Species, main = "MyFirstBoxPlot", 
        xlab="Species", ylab="Sepal Length", col = c("red", "yellow", "violet"))


str(iris)
plot(iris$Sepal.Length)
plot(iris$Sepal.Length, type = "l") # Line Chart
plot(iris$Sepal.Length, type = "h") # Horizontal Plot
plot(iris$Sepal.Length, type = "b")
plot(iris$Sepal.Length, type = "o")

# To see more than one plot in the plotting area 
par(mfrow=c(2,2))
plot(iris$Sepal.Length, type = "l") # Line Chart
plot(iris$Sepal.Length, type = "h") # Horizontal Plot
plot(iris$Sepal.Length, type = "b")
plot(iris$Sepal.Length, type = "o")

par(mfrow=c(1,1))

plot(iris$Sepal.Length)
plot(iris$Sepal.Length, iris$Petal.Length)  # Talking about correlation 
plot(iris$Species)
plot(iris$Species, iris$Sepal.Length)

# Concept of pch 
par(mfrow=c(3,3))
plot(iris$Sepal.Length, pch = 2)
plot(iris$Sepal.Length, pch = 4)
plot(iris$Sepal.Length, pch = 5)
plot(iris$Sepal.Length, pch = 10)
plot(iris$Sepal.Length, pch = 12)
plot(iris$Sepal.Length, pch = 14)
plot(iris$Sepal.Length, pch = 16)
plot(iris$Sepal.Length, pch = 19)
plot(iris$Sepal.Length, pch = 24)

hist(iris$Sepal.Length)
hist(iris$Sepal.Length, labels = TRUE)
hist(iris$Sepal.Length, labels = TRUE, breaks = 20)

pie(table(iris$Species))

# Correlation 
# Negative 
# Positive 
# Neutral 
# Its a number between -1 to + 1
# Strong correlation is more than .75
head(mtcars)
plot(mtcars$disp, mtcars$mpg)
cor(mtcars$disp, mtcars$mpg)
cor(mtcars)  # Correlation Matrix

library(corrgram)
corrgram(mtcars)

# Plot for categorical data
library(vcd)
HairEyeColor
mosaic(HairEyeColor)

# Missing Value in R are represented as "NA"
head(airquality)
View(airquality)
dim(airquality)
is.na(airquality$Ozone)
which(is.na(airquality$Ozone))
length(which(is.na(airquality$Ozone)))
sum(which(is.na(airquality$Ozone)))
sum((is.na(airquality$Ozone)))
colSums(is.na(airquality)) # Count of missing values in every col 

colSums(is.na(airquality)) / (nrow(airquality))

# Ignore the record where you have missing value
airquality1 <- na.omit(airquality)
colSums(is.na(airquality1))
nrow(airquality1)
nrow(airquality)

# Replace the missing value 
mean(airquality$Ozone, na.rm = TRUE)

airquality$Ozone[which(is.na(airquality$Ozone))] <- mean(airquality$Ozone, na.rm = TRUE)
colSums(is.na(airquality))

airquality$Solar.R[which(is.na(airquality$Solar.R))] <- mean(airquality$Solar.R, na.rm = TRUE)
colSums(is.na(airquality))

# Salary is missing 
# Avg sal = 85K
# Salary is missing, it is a professor 
# Avg sal of professor = 105K 
# Salary is missing, it is a professor, and in NY 
# Avg sal of professor in NY = 125K 

# Impute Vs Ignore 
# After Treatment you should have a good amount of data for modelling 
# 1st case - 20 million records with 20% missing data  -- Ignore missing value 
# 2nd case - 20000 records with 20% missing data -- Impute 



















