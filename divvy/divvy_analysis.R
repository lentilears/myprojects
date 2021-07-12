## Load Libraries
library("ggplot2")
library("dplyr")
library("RColorBrewer")
library("ggthemes")
library("ggrepel")
library("gridExtra")
library("lubridate")

## Load Data
data = read.csv("./data/data.csv")
head(data)
str(data)
summary(data)

data$starttime = ymd_hms(data$starttime)

## Frequency distribution versus Usertype 

data %>%
  group_by(usertype) %>%
  summarize(freq = n()) %>%
  ggplot(aes(reorder(usertype, freq), y = freq, fill = freq)) +   
  geom_bar(stat = "identity", position = "dodge") +
  xlab("Usertype") +
  ylab("Frequency") +
  geom_text(aes(label = freq), vjust = -0.1, size = 4.5) +
  ggtitle("Overall Usertype") +
  theme(plot.title = element_text(hjust = 0.5))

summary(data$tripduration)

#Extract only subscribers from usertype field, as they contain demographic data 
data_subscriber = subset(data, (usertype == 'Subscriber'))
data_subscriber$starttime = ymd_hms(data_subscriber$starttime)
head(data_subscriber)


data_subscriber %>%
  group_by(gender) %>%
  summarize(freq = n()) %>%
  ggplot(aes(reorder(gender, freq), y = freq, fill = freq)) +   
  geom_bar(stat = "identity", position = "dodge") +
  xlab("Gender") +
  ylab("Frequency") +
  geom_text(aes(label = freq), vjust = -0.1, size = 4.5) +
  ggtitle("Overall Demographics for Gender") +
  theme(plot.title = element_text(hjust = 0.5))

#Summary of Overall Trip Duration
summary(data$tripduration)
summary(data_subscriber$tripduration)

#Boxplots of Overall Trip Duration by Gender
data_subscriber %>%
  select(tripduration, gender) %>%
  group_by(gender) %>%
  ggplot(aes(x = factor(gender), y = tripduration, fill = gender)) + 
  geom_boxplot() +
  xlab("Gender") +
  ylab("Trip Duration (mins)") +
  theme(legend.position="None") +
  ggtitle("Trip Duration by Gender") +
  theme(plot.title = element_text(hjust = 0.5))

## Frequency distribution versus Weather conditions
## Subscribers are predominantly using bikes on cloudy days >> clear days > rain/snow > not clear > tstorms 
data_subscriber %>%
  group_by(events) %>%
  summarize(freq = n()) %>%
  ggplot(aes(reorder(events, freq), y = freq, fill = freq)) +   
  geom_bar(stat = "identity", position = "dodge") +
  xlab("Weather Conditions") +
  ylab("Frequency") +
  geom_text(aes(label = freq), vjust = -0.1, size = 4.5) +
  ggtitle("Subscriber Usage versus Weather Conditions") +
  theme(plot.title = element_text(hjust = 0.5))

## What's the usage frequency under different weather conditions versus gender 
data_subscriber %>%
  group_by(events, gender) %>%
  summarize(freq = n()) %>%
  ggplot(aes(reorder(events, freq), y = freq, fill = gender)) +   
  geom_bar(stat = "identity", position = position_dodge(1)) +
  xlab("Weather Conditions") +
  ylab("Frequency") +
  geom_text(aes(label = freq), vjust = -0.4, hjust = 0.4, size = 3.0) +
  ggtitle("Gender Usage versus Weather Conditions") +
  theme(plot.title = element_text(hjust = 0.4))


#Boxplots of Overall Trip Duration by Weather Type and Gender (not showing outliers and y-axis re-scaled)
data_subscriber %>%
  select(tripduration, gender, events) %>%
  group_by(events) %>%
  ggplot(aes(x = factor(events), y = tripduration, fill = gender)) + 
  geom_boxplot(outlier.shape = NA) +
  coord_cartesian(ylim = c(0,40)) +
  xlab("Weather Condition (events)") +
  ylab("Trip Duration (mins)") +
  theme(legend.position="right") +
  ggtitle("Trip Duration by Weather and Gender") +
  theme(plot.title = element_text(hjust = 0.5))

#no. of empty entries in from_station_name
sum(data_subscriber$from_station_name == "") # no blank emtries, so data seems to be clean


## What's the overall usage frequency for different months (2014-2017) versus gender 
## Use 3-letter month designation
data_subscriber$month = month(data_subscriber$month, label = TRUE)

data_subscriber %>%
  group_by(month, gender) %>%
  count() %>%
  ggplot(aes(month, y = n, fill = gender)) +    
  geom_bar(stat = "identity", position = position_dodge(1)) +
  xlab("Months of the Year") +
  ylab("Frequency") +
  geom_text(aes(label = n), vjust = -0.4, hjust = 0.4, size = 3.0) +
  ggtitle("Gender Usage versus Months (2014-2017)") +
  theme(plot.title = element_text(hjust = 0.4))

## What's the overall gender usage by day of the week (2014-2017)
## Use 3-letter day designation
data_subscriber$day = wday(ymd_hms(data_subscriber$starttime), label = TRUE, abbr = TRUE)

data_subscriber %>%
  group_by(day, gender) %>%
  count() %>%
  ggplot(aes(day, y = n, fill = gender)) +
  geom_bar(stat = "identity", position = position_dodge(1)) +
  xlab("Days of the Week") +
  ylab("Frequency") +
  geom_text(aes(label = n), vjust = -0.4, hjust = 0.4, size = 3.0) +
  ggtitle("Gender Usage versus Days of the Week (2014-2017)") +
  theme(plot.title = element_text(hjust = 0.4))

## What's the overall gender usage in a day (2014-2017)
data_subscriber %>%
  group_by(hour, gender) %>%
  count() %>%
  ggplot(aes(hour, y = n, fill = gender)) +
  geom_bar(stat = "identity", position = position_dodge(1)) +
  xlab("Hours in a Day (24 hr)") +
  ylab("Frequency") +
  geom_text(aes(label = n), vjust = -0.4, hjust = 0.4, size = 3.0) +
  ggtitle("Gender Usage Over a 24 Hour Period (2014-2017)") +
  theme(plot.title = element_text(hjust = 0.5))


### Usage By Week Year-over-Year 2014-2017  
data_subscriber %>%
  group_by(year,week) %>%
  summarize(freq = n()) %>%
  ggplot(aes(week,freq)) + 
  geom_line(col="blue") + 
  facet_wrap(~year,ncol=4) + 
  xlab("Weeks") +
  ylab("Frequency") +
  theme(axis.text.x=element_text(angle=45,vjust=0.5),legend.position='none',plot.title = element_text(size=12)) +
  ggtitle("Trips by Week 2014-2017") +
  theme(plot.title = element_text(hjust = 0.5))

### Usage By Gender and Week Year-over-Year 2014-2017  
data_subscriber %>%
  group_by(year,week,gender) %>%
  summarize(freq = n()) %>%
  ggplot(aes(week,freq,group = gender)) + 
  geom_line(aes(color=gender)) + 
  #facet_wrap(~year,ncol=4) + 
  xlab("Weeks") +
  ylab("Frequency") +
  theme(axis.text.x=element_text(angle=45,vjust=0.5),legend.position='none',plot.title = element_text(size=12)) +
  ggtitle("Periodic Bike Usage Growth by Gender, 2014-2017") +
  theme(plot.title = element_text(hjust = 0.5))


## What's the overall gender usage by year (2014-2017)
data_subscriber %>%
  group_by(year, gender) %>%
  count() %>%
  ggplot(aes(year, y = n, fill = gender)) +
  geom_bar(stat = "identity", position = position_dodge(1)) +
  xlab("Years") +
  ylab("Frequency") +
  geom_text(aes(label = n), vjust = -0.4, hjust = 0.4, size = 3.0) +
  ggtitle("Gender Usage by Year (2014-2017)") +
  theme(plot.title = element_text(hjust = 0.5))

#Get the maximum no. of counts of female/male riders for each year
year <- as.numeric(c(2014,2015,2016,2017))
countsFemale <- as.numeric(c(396506,560397,682372,739206))
countsMale <- as.numeric(c(1219098,1659953,2018600,2217648))
countsbyYear <- data.frame(year,female,male)

#Fit a simple OLS model to female ridership growth
femaleFit <- lm(female ~ year, countsbyYear)
femaleFit
#equation is: y(count) = -231202996 + 115008*year

#Fit a simple OLS model to male ridership growth
maleFit <- lm(male ~ year, countsbyYear)
maleFit
#equation is: y(count) = -674279736 + 335430*year

library(gridExtra)
#Plot the simple linear model for female ridership and annotate with the lm() equation
g1 <- ggplot(data = countsbyYear, aes(x = year, y = female)) + 
  geom_point(color='blue') +
  geom_smooth(method = "lm", color='black', se = FALSE) +
  xlab("Year") +
  ylab("Counts (Female)") +
  ggtitle("Linear Model of Female Usage Growth") +
  theme(plot.title = element_text(hjust = 0.5)) +
  annotate("text", x=2016, y=500000, label= "y = -231202996 + 115008*year")
# y(count) = -231202996 + 115008*year

#Plot the simple linear model for male ridership and annotate with the lm() equation
g2 <- ggplot(data = countsbyYear, aes(x = year, y = male)) + 
  geom_point(color='blue') +
  geom_smooth(method = "lm", color='black', se = FALSE) +
  xlab("Year") +
  ylab("Counts (Male)") +
  ggtitle("Linear Model of Male Usage Growth") +
  theme(plot.title = element_text(hjust = 0.5)) +
  annotate("text", x=2016, y=1500000, label= "y = -674279736 + 335430*year")
#y(count) = -674279736 + 335430*year

grid.arrange(g1,g2,ncol=1)

#subdata2015 = subset(data_subscriber, (year == 2015))
#subdata2016 = subset(data_subscriber, (year == 2016))
#subdata2017 = subset(data_subscriber, (year == 2017))




