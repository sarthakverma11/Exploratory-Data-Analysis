############# Lollipop Chart ########################################################

# Data Set used: Crime in Atlanta 2009-2017

# Source of Data : data.world

crime = read.csv("E://DV for Batch 2//Tutoial on GGPLOT//Data Sets//Crime in Atlanta 2009-2017.csv") 

#### Objective is to identified which month had highest count of crime and ploting them on lollipop chart


# Extracting Month from Date column
str(crime)

# Converting date column from factor to date 
crime$date = as.Date(crime$date, format = "%m/%d/%Y")
str(crime$date)

# Creating a month column and I need month in abbreviation
crime$Month = format(crime$date, "%b")

# Using dplyr summarise the crime month wise
library(dplyr)

crime_month_wise = crime %>% group_by(Month) %>% summarise(Count = n()) 

# Ploting lollipop chart
library(ggplot2)

chart = ggplot(crime_month_wise, aes(x= Month, y=Count)) + geom_point(size=3) + 
  geom_segment(aes(x=Month, xend=Month, y=0, yend=Count)) + 
  labs(title="Lollipop Chart", subtitle="Month Wise Chart", 
       caption="source: Data.World") + scale_x_discrete(limits = month.abb)
chart

############# Diverging Bars #######################################################################
# Data Set used: Crime in Atlanta 2009-2017

# Source of Data : data.world

# Problem Statement: Objective is to identifiy the months with below average and above average crime

# The Crime count from crime_month_wise dataset is normalised by computing the z score. 

# Those Months with Crime Count above Average are marked green and those below are marked red.

# Lets calculate the z score
crime_month_wise$Count_z = round((crime_month_wise$Count - mean(crime_month_wise$Count))/sd(crime_month_wise$Count), 2)  


# Above/Below average flag
crime_month_wise$Avg_type = ifelse(crime_month_wise$Count_z < 0, "below", "above")  

# Sorting the data based on Normalized value column
crime_month_wise = arrange(crime_month_wise, Count_z)  

# Ploting
chart2 = ggplot(crime_month_wise, aes(x= Month, y= Count_z, label= Count_z)) + 
  geom_bar(stat='identity', aes(fill=Avg_type), width=.5) + 
  scale_fill_manual(name="Crime", 
                    labels = c("Above Average", "Below Average"), 
                    values = c("above"="Red", "below"="Green")) + 
  labs(subtitle="Avg. Crime Month Wise", 
       title= "Diverging Bars", x = "Month", y = "Normalized Crime Rate") + scale_x_discrete(limits = month.abb)

chart2
chart2 + coord_flip()

######### Diverging Lollipop Chart ##############################################################

# Problem Statement: Create Diverging Lollipop chart using same data set. 

# Solution: 

ggplot(crime_month_wise, aes(x= Month,  y= Count_z, label=Count_z)) + geom_point(aes(col = Avg_type), size=6)  + 
  geom_segment(aes(x = Month, xend = Month, y = 0, yend = Count_z)) +
  labs(title="Diverging Lollipop Chart", subtitle="Normalized Crime Rate: Lollipop") + 
  scale_x_discrete(limits = month.abb) + geom_text(color="white", size=2) + coord_flip()

######## Scatter Plot ##########################################################################
# Data Set used: mpg

# Source of Data : R - Inbuild data set

# Problem statement : Objective is to find the corelation between highway mileage and City mileage of each class. 


data1 = mpg

# Lets plot a simple scatter Plot
chart3 = ggplot(data1, aes(x=cty, y=hwy)) + geom_point(aes(col=class)) + 
  labs(subtitle="City Mileage v/s Highway Mileage", y="Highway Mileage", x="City Mileage", title="Scatterplot", 
       caption = "Source: mpg data set")

chart3

############ Jitter Plot ###################################################################
# Data Set used: mpg

# Source of Data : R - Inbuild data set

# Problem statement : Find the corelation between highway mileage and City mileage of each class without having the problem of data points over lapping

chart4 = ggplot(mpg, aes(x = cty, y = hwy)) + geom_jitter(aes(col = class), width = .5, size=1) +
  labs(subtitle="City Mileage v/s Highway mileage", y="Highway Mileage", x="City Mileage", 
       title="Jittered Points")

chart4

############ Count Plot ####################################################################

# Data Set used: mpg

# Source of Data : R - Inbuild data set

# Problem statement : Find the corelation between highway mileage and City mileage of each class without having the problem of data points over lapping

chart5 = ggplot(mpg, aes(x = cty, y = hwy)) + geom_count(aes(col= class)) +
  labs(subtitle="City Mileage v/s Highway mileage", y="Highway Mileage", x="City Mileage", 
       title="Count Plot")

chart5

############ Bubble chart #################################################################
# Data Set used: mpg

# Source of Data : R - Inbuild data set

# Problem Statement: Find the corelation between displacement and mileage (Both city and highway mileage) of various class.

chart6 = ggplot(mpg, aes(x = displ, y = cty)) + geom_jitter(aes(col = class, size = hwy)) + 
  labs(subtitle="Displacement v/s Mileage", y="City Mileage", x="Displacement", 
       title="Bubble Chart")

chart6

chart6.1 = ggplot(mpg, aes(x = displ, y = cty)) + geom_jitter(aes(col = hwy, size = class)) + 
  labs(subtitle="Displacement v/s Mileage", y="City Mileage", x="Displacement", 
       title="Bubble Chart")

chart6.1

library(gridExtra)

grid.arrange(chart6, chart6.1)

############ Animated Bubble Chart #######################################################
library(ggplot2)
library(gapminder)
devtools::install_github("dgrtwo/gganimate")
library(gganimate)
library(cowplot)
library(animation)
library(installr) 

#install.ImageMagick("https://www.imagemagick.org/script/download.php/Windows%20Binary%20Release") 

# Data Set used: SSA

# Source of Data: Data.gov.in

# Problem Statement: Find the Literacy Rate of South Indian State from given SSA data.

# Solution: 
library(readxl)
library(dplyr)

SSA = read_excel("E://DV for Batch 2//Tutoial on GGPLOT//Data Sets//SSA.xlsx")
str(SSA)

SSA$State.Name = as.factor(SSA$State.Name)

SSA$Year = as.integer(SSA$Year)

South_India = SSA %>% select(State.Name, Year, Total.Population, Male.Literacy.Rate, Female.Literacy.Rate) %>%
  filter(State.Name == "Karnataka" | State.Name == "Kerala" | State.Name =="Tamil Nadu" | State.Name =="Andhra Pradesh") %>%
  arrange(desc(Total.Population))

str(South_India)

South_India$State.Name = factor(South_India$State.Name)
South_India$Year = as.integer(South_India$Year)


chart7 = ggplot(South_India, aes(Male.Literacy.Rate, Female.Literacy.Rate, size = Total.Population, frame = Year)) +
  geom_jitter(aes(col = State.Name))
  
chart7

gganimate(chart7)

## Taking example of gapminder data set.

chart8 = ggplot(gapminder, aes(gdpPercap, lifeExp, size = pop, frame = year)) +
  geom_jitter(aes(col = continent)) 

gganimate(chart8)

########### Correlogram ###########################################################################
# We will create correlogram in ggplot as well as corrplot() also.
# lets first try with corrplot()

# Data Set used: mtcars

# Source of Data : R - Inbuild data set

# Problem statement : To find the correlation between various variable and dividing them into to hierarchical clustering.

library(corrplot)
data_cor = mtcars

# First think to draw a correlogram is to create a correlation matrix between various variables.

str(data_cor)

Matrix = cor(data_cor)

# Ploting the correlation plot

chart9 = corrplot(Matrix, method = "square", type = "upper")

# Reordering the correlation matrix
# The correlation matrix can be reordered according to the correlation coefficient. 
# This is important to identify the hidden structure and pattern in the matrix. 
# "hclust" for hierarchical clustering order is used for this purpose.

chart9.1 = corrplot(Matrix, method = "square", type = "upper", order = "hclust")

chart9.1

##### Using ggcorrplot
install.packages("ggcorrplot")
library(ggcorrplot)

Matrix1 = cor(data_cor)

Chart10 = ggcorrplot(Matrix1, hc.order = TRUE, type = "upper", lab = TRUE, lab_size = 3, method="circle")

Chart10


######### Histogram on a continuous variable ##################################################
## We will try to use the option of bin() and binwidth()

# Data Set used: mpg

# Source of Data : R- Inbuild data set

# Problem statement : To create a histogram of displacement(which is a continuous variable) of various car's class.

data1 = mpg

# Iam not giving the number of bins in this case.

chart11 = ggplot(data1, aes(displ)) +
  geom_histogram(aes(fill = class), binwidth = .4, color = "black") + labs(title = "Histogram without defining Numbers of Bins") 


chart11

# Now I will define the number of bins

chart12 = ggplot(data1, aes(displ)) + 
  geom_histogram(aes(fill = class), bins = 3, color = "black") + labs(title = "Histogram with Numbers of Bins")


chart12


######## # Histogram on a Categorical variable ###################################

# Data Set used: mpg

# Source of Data : R- Inbuild data set

# Problem statement : To create a histogram of manufacturer(which is a categorical variable) of various car's class.

chart13 = ggplot(data1, aes(manufacturer)) + 
  geom_histogram(aes(fill = class)) + theme(axis.text.x = element_text(angle=90)) +
  labs(title = "Histogram on Categorical Variable")
  
chart13  #### Does it giving an Error????

########## Box Plot ################################################################

# Data Set used: mpg

# Source of Data : R- Inbuild data set

# Problem statement : To create a box plot with city mileage grouped by Class of car for each cylender

chart14= ggplot(data1, aes(class, cty)) + geom_boxplot(varwidth = TRUE)

chart14

# Now I want to plot the case for each cylender car

chart14 = ggplot(data1, aes(class, cty)) + geom_boxplot(aes(fill = factor(cyl)), varwidth = TRUE)

chart14
  
################################################ EOD ################################################