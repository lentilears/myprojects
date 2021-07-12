library(dplyr)

#Read-in Divvy_Trips_2013 data
divvy2013 <- read.csv("Divvy_Trips_2013-new.csv")

#Extract subscribers from usertype field, as they contain demographic data 
data2013subscriber = subset(divvy2013, (usertype == 'Subscriber'))
write.csv(data2013subscriber,'Divvy_Trips_2013-1new.csv')

#Read-in Divvy_Trips_2014 Q1Q2 data
divvy2014q1q2 <- read.csv("Divvy_Trips_2014_Q1Q2.csv")

#Extract subscribers from usertype field, as they contain demographic data
data2014q1q2subscriber = subset(divvy2014q1q2, (usertype == 'Subscriber'))
write.csv(data2014q1q2subscriber,'Divvy_Trips_2014-Q1Q2-new.csv')

#Read-in Divvy_Trips_2014 Q3Q4 data
divvy2014q3q4 <- read.csv("Divvy_Trips_2014-Q3Q4-new.csv")

#Extract subscribers from usertype field, as they contain demographic data
data2014q3q4subscriber = subset(divvy2014q3q4, (usertype == 'Subscriber'))
write.csv(data2014q3q4subscriber,'Divvy_Trips_2014-Q3Q4-new.csv')

#Read-in Divvy_Trips_2016_Q3 data
divvy2016q3 <- read.csv("Divvy_Trips_2016_Q3-new.csv")

#Extract subscribers from usertype field, as they contain demographic data 
data2016Q3subscriber = subset(divvy2016q3, (usertype == 'Subscriber'))
#data = subset(divvy2016q3, (gender == 'Male' | gender == 'Female'))
write.csv(data2016Q3subscriber,'Divvy_Trips_2016_Q3-new.csv')


#create a new column 'age' (YYYY - birthyear), where YYYY is year data was collected
data2016Q3subscriber$age <- with(data2016Q3subscriber, (2016-data2016Q3subscriber$birthyear))
write.csv(data2016Q3subscriber,'Divvy_Trips_2016_Q3-new.csv')

#Read-in Divvy_Trips_2016_Q4 data
divvy2016q4 <- read.csv("Divvy_Trips_2016_Q4.csv")

#Extract subscribers from usertype field, as they contain demographic data 
data2016Q4subscriber = subset(divvy2016q4, (usertype == 'Subscriber'))

#create a new column 'age' (YYYY - birthyear), where YYYY is year data was collected
data2016Q4subscriber$age <- with(data2016Q4subscriber, (2016-data2016Q4subscriber$birthyear))
write.csv(data2016Q4subscriber,'Divvy_Trips_2016_Q4-new.csv')

#Read-in Divvy_Trips_2017_Q2 data
divvy2017q2 <- read.csv("Divvy_Trips_2017_Q2.csv")

#Extract subscriber from userype filed, as they contain demographic data
data2017Q2subscriber = subset(divvy2017q2, (usertype == 'Subscriber'))

#create a new column 'age' (YYYY - birthyear), where YYYY is year data was collected
data2017Q2subscriber$age <- with(data2017Q2subscriber, (2017-data2017Q2subscriber$birthyear))
write.csv(data2017Q2subscriber,'Divvy_Trips_2017_Q2-new.csv')

#Read-in Divvy_Trips_2017_Q3 data
divvy2017q3 <- read.csv("Divvy_Trips_2017_Q3.csv")

#Extract subscriber from userype filed, as they contain demographic data
data2017Q3subscriber = subset(divvy2017q3, (usertype == 'Subscriber'))

#create a new column 'age' (YYYY - birthyear), where YYYY is year data was collected
data2017Q3subscriber$age <- with(data2017Q3subscriber, (2017-data2017Q3subscriber$birthyear))
write.csv(data2017Q3subscriber,'Divvy_Trips_2017_Q3-new.csv')
