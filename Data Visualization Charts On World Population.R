####### Population Pyramid #####################################################
# Data Set used: World propulation age wise in 2016

# Source of Data : www.census.gov

# Problem statement : To find the Population of Male and Female at different age group.

library(readxl)
pop = read_excel("E://DV for Batch 2//Tutoial on GGPLOT//Data Sets//Population_Age_Wise.xlsx") 

## cut the age variable into age groups with 5-year intervals
#pop$Agecut = cut(pop$Age, breaks = seq(0, 100, 10)) 

pop$Agecut = cut(pop$Age, breaks = seq(0,100,10), right = FALSE)
## aggregate the data by gender and age group
library(dplyr)
popGH = pop %>% group_by(Gender, Agecut) %>% summarise(Total.Pop = sum(Population))

## barplots for male populations goes to the left (thus negative sign)

popGH$Total.Pop <- ifelse(popGH$Gender == "Male", -1*popGH$Total.Pop, popGH$Total.Pop)

## 
brks <- seq(-650000000, 650000000, 50000000)
lbls = paste0(as.character(c(seq(650, 0, -50), seq(50, 650, 50))), "m")

## pyramid charts are two barcharts with axes flipped
pyramid <- ggplot(popGH, aes(x = Agecut, y = Total.Pop, fill = Gender)) + 
  geom_bar(data = subset(popGH, Gender == "Female"), stat = "identity") +
  geom_bar(data = subset(popGH, Gender == "Male"), stat = "identity")

pyramid + scale_y_continuous(breaks = brks, labels = lbls) + coord_flip()

pyramid

## Changing the text on x axis
pyramid + theme(axis.text.x = element_text(angle = 90))

########### Pie chart ####################################################################

# Data Set used: Cost per event and cost per athlete in the Olympics.

# Source of Data : data.world

# Problem statement : To identify the cost per athlete in the olympics category wise.

athlete = read.csv(file.choose())

# Data prep for Pie chart
library(dplyr)
Cost = athlete %>% group_by(Type) %>% summarise(Total_Cost_Per_Athlete = sum(Cost.per.athlete..mio..USD))

# Ploting Pie chart
library(ggplot2)

pie1 = ggplot(Cost, aes(x = "", y = Total_Cost_Per_Athlete, fill = Type)) + 
  geom_bar(stat = "identity", width = 1) + theme(axis.line = element_blank(), plot.title = element_text(hjust=0.5)) + 
  labs(x=NULL, y=NULL, 
       title="Pie Chart of Cost Per Athlete in Million USD", 
       caption="Source: Data.world")

# Using coord_polar to create a pie chart
pie2 = pie1 + coord_polar(theta = "y", start=0) 

# Using theme_void() to remove the axis and axis labels.

pie3 = pie2 + theme_void() 

pie3

# Lets label the chart and recreate it

Cost1 =  Cost %>% # factor levels need to be the opposite order of the cumulative sum of the values
  mutate(Type = factor(Type, levels = c("Winter", "Summer")),
         cumulative = cumsum(Total_Cost_Per_Athlete),
         midpoint = cumulative - Total_Cost_Per_Athlete / 2,
         label = paste0(round(Total_Cost_Per_Athlete / sum(Total_Cost_Per_Athlete) * 100, 1), "%"))

## If you want to add percent age as well as number use
# " (", Total_Cost_Per_Athlete, ") " in label.

pie3 = ggplot(Cost1, aes(x = "", y = Total_Cost_Per_Athlete, fill = Type)) + 
  geom_bar(stat = "identity", width = 1) + theme(axis.line = element_blank(), plot.title = element_text(hjust=0.5)) + 
  labs(x=NULL, y=NULL, 
       title="Pie Chart of Cost Per Athlete in Million USD", 
       caption="Source: Data.world")

pie4 = pie3 + coord_polar(theta = "y", start=0) 

# Using theme_void() to remove the axis and axis labels.

pie5 = pie4 + theme_void() 

pie5 + geom_text(aes(x = 1, y = midpoint, label = label))

########### Stacked bar chart #######################################################
# Data set used: mpg

# Data Source: R-Inbuild data set

# Problem Statement: manufacturer wise, class wise count of cars

stack_bar = ggplot(mpg, aes(x = manufacturer)) + geom_bar(aes(fill = class), width = .6) 

stack_bar

stack_bar + theme(axis.text.x = element_text(angle = 90))

##  stacked bar chart with % contribution
stack_fill = ggplot(mpg, aes(x = manufacturer)) + 
  geom_bar(aes(fill = class),position = "fill", width = .6) 

stack_fill
stack_fill + theme(axis.text.x = element_text(angle = 90))

############### Time series plot ####################################################################
# Time Series Plot From a Data Frame

# Data Set used: Economics data set

# Source of data set: R-Inbuild data set

# Problem statement: Want to plot the unemployment rate over the year.

library(ggplot2)

data1 = economics

help("economics")



View(data1)

chart1 = ggplot(data1, aes(x=date, y = unemploy)) + geom_line()

chart1 

# Adding points to line

chart1 + geom_point()

# Problem statement 2: Want to change the line thickness based on unemployment % over the population

data1$rate = round((data1$unemploy/data1$pop)*100, digits = 2)

chart2 = ggplot(data1, aes(x = date, y = unemploy, size = rate )) + geom_line()

chart2


# Problem statement 3: Plotting multiple line charts

chart4 = ggplot(data1, aes(x = date)) + 
  geom_line(aes(y = unemploy), col = "Red") +
  geom_line(aes(y = pce), col = "Green")

chart4 

# Or

chart4 = ggplot(data1, aes(x = date)) + 
  geom_line(aes(y = unemploy, color = "Unemployment")) + 
  geom_line(aes(y = pce, color = "Price"))

chart4 

# Melting the data frame on date to plot all the variables

library(reshape2)
library(dplyr)

data1 = data1[,1:6]
data2 = melt(data1, id = "date")

data2.1 = filter(data2, variable == "pce" | variable == "unemploy")

chart5 = ggplot(data2, aes(x = date, y = value, col = variable)) + geom_line() 

chart5
chart5 + scale_color_manual(labels = c("pce", "unemploy"), 
                            values = c("pce"="Red", "unemploy"="Green"))

# My chart is showing data over a period of 10 years. I want to show for each year
library(lubridate)

brks <- data1$date[seq(1, length(data1$date), 12)]
lbls <- lubridate::year(brks)

chart4 + scale_x_date(labels = lbls, breaks = brks) + theme(axis.text.x = element_text(angle = 90))


############ Stacked Area chart ############################################
# Time Series Plot From a Data Frame

# Data Set used: Economics data set

# Source of data set: R-Inbuild data set

# Problem statement: To draw stacked area chart for Unemployment and Price

data1 = economics
library(ggplot2)
chart6 = ggplot(data1, aes(x=date)) + 
  geom_area(aes(y=unemploy, fill="Unemployment")) + 
  geom_area(aes(y=pce, fill="Price"))

chart6

########### Google Vis: Get the chart rolling ############################
# Data set used: SSA

# Data Source: data.gov.in

# Problem Statement: Ploting the states on map of india based on Total population 

library(googleVis)
library(readxl)
library(dplyr)

SSA = read_excel("E://DV for Batch 2//Tutoial on GGPLOT//Data Sets//SSA.xlsx")

SSA_State_wise = SSA %>% group_by(State.Name) %>% summarise(Total.Pop = sum(Total.Population)) %>% arrange(desc(Total.Pop))

Top_10 = head(SSA_State_wise, 10)

States = gvisBarChart(Top_10, xvar = "State.Name", yvar = "Total.Pop")

plot(States)

# Lets see a bubble chart example

Data1 = Fruits

Bubble = gvisBubbleChart(Fruits, idvar="Fruit", xvar="Sales", yvar="Expenses",
                          colorvar="Year", sizevar="Profit")
                      
plot(Bubble)

# Lets see a Gauge chart

Gauge = gvisGauge(CityPopularity, options=list(min=0, max=800, greenFrom=500,
                                 greenTo=800, yellowFrom=300, yellowTo=500,
                                 redFrom=0, redTo=300, width=400, height=300))
plot(Gauge)

# Lets see a geo map

GeoStates = gvisGeoChart(SSA_State_wise, "State.Name", "Total.Pop",
                          options=list(region="IN", 
                                       displayMode="regions", 
                                       resolution="provinces",
                                       width=600, height=400))
plot(GeoStates)


### Group bar chart

sample <- data.frame(
  Category <- c("Furniture","Furniture","Furniture","Furniture",
                "Office Supplies","Office Supplies", "Office Supplies", "Office Supplies",
                "Office Supplies", "Office Supplies", "Office Supplies", "Office Supplies",
                "Office Supplies", "Technology","Technology","Technology","Technology"),
  SubCategory <- c("Bookcases","Chairs","Furnishings","Tables","Appliances","Art","Binders","Envelopes", 
                   "Fasteners","Labels","Paper","Storage",  "Supplies", "Accessories","Copiers","Machines",
                   "Phones"),
  sales <- c(889222.51,920892.65,239840.16,445823.93,614737.91,225594.68,281494.68,104903.88,50156.06,44269.30,
             150113.36,692903.08,152196.19,463383.33,965899.78,458655.43,1005525.38)
)

colnames(sample)<-c("category","subcategory","Sales")

ggplot(sample, aes(category, Sales)) +   
  geom_bar(aes(fill = category, color=subcategory), position = "dodge", stat="identity") + theme(legend.position = "none")

# to fill for each subcategory
ggplot(sample,aes(x=Category,y=sales)) + 
  geom_bar(stat="identity",width=0.5, position="dodge", aes(fill=SubCategory),
           color="black")
