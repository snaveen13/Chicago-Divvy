library(data.table)
library(plyr)
library(lubridate)
library(ggplot2)
library(graphics)
library(ggmap)
library(devtools)
library(forecast)

install_version("ggplot2", version = "2.1.0", repos = "http://cran.us.r-project.org")

Q1 <- fread(file = "/Users/Naveen/Downloads/Divvy_Trips_2016_Q1Q2/Divvy_Trips_2016_Q1.csv", header = TRUE)
View(Q1)
Q2 <- fread(file = "/Users/Naveen/Downloads/Divvy_Trips_2016_Q1Q2/Divvy_Trips_2016_04.csv", header = TRUE)
View(Q2)
Q3 <- fread(file = "/Users/Naveen/Downloads/Divvy_Trips_2016_Q1Q2/Divvy_Trips_2016_05.csv", header = TRUE)
View(Q3)
Q4 <- fread(file = "/Users/Naveen/Downloads/Divvy_Trips_2016_Q1Q2/Divvy_Trips_2016_06.csv", header = TRUE)
View(Q4)
Q5 <- fread(file = "/Users/Naveen/Downloads/Divvy_Trips_2016_Q3Q4/Divvy_Trips_2016_Q3.csv", header = TRUE)
Q6 <- fread(file = "/Users/Naveen/Downloads/Divvy_Trips_2016_Q3Q4/Divvy_Trips_2016_Q4.csv", header = TRUE)

divvy_list <- list(Q1,Q2,Q3,Q4,Q5,Q6)

divvy <- ldply(divvy_list, data.table)
class(divvy)
View(divvy)

str(divvy)
summary(divvy)

numeric_list <- c('trip_id', 'bikeid', 'tripduration', 'from_station_id', 'to_station_id', 'birthyear')
for (i in numeric_list) {
  divvy[,i] = as.integer(divvy[,i])
}

divvy$gender[divvy$gender==""] <- 'unknown'
divvy$birthyear[is.na(divvy$birthyear)] <- 1980

divvy$starttime = as.POSIXlt(divvy$starttime,format="%m/%d/%Y %H:%M")
divvy$stoptime = strptime(divvy$stoptime, format = "%m/%d/%Y %H:%M")

divvy$usertype = as.factor(divvy$usertype)
divvy$gender = as.factor(divvy$gender)

divvy <- divvy[!(divvy$usertype=='Dependent'),]
divvy$usertype <- droplevels(divvy$usertype)

str(divvy)
summary(divvy)

divvy <- divvy[!(divvy$birthyear < 1940),]

plot(divvy$usertype)
plot(divvy$gender)

hist(divvy$birthyear)

boxplot(divvy$birthyear)

divvy_final <- divvy
View(divvy_final)

divvy_final$start_hour <- hour(divvy_final$starttime)
divvy_final$start_month <- month(divvy_final$starttime)
str(divvy_final)

hist(divvy_final$start_hour)
hist(divvy_final$start_month)

divvy_final$week <- weekdays(divvy_final$starttime)
divvy_final$week <- as.factor(divvy_final$week)

barplot(sort(prop.table(table(divvy_final$week)), decreasing = TRUE))

plot(divvy_final$week, divvy_final$start_month)

# 4. No of users based on subscription
# Annual users user more on weekdays - possible reason is they work or go to college using divvy.
# Per day users use more on weekends - tourists.

divvy_subscriber <- divvy_final[divvy_final$usertype=='Subscriber',]
class(divvy_subscriber)

divvy_customer <- divvy_final[divvy_final$usertype=='Customer',]  
class(divvy_customer)  

str(divvy_subscriber)
summary(divvy_subscriber)

str(divvy_customer)
summary(divvy_customer)
View(divvy_customer)

par(mfrow= c(1,2))
barplot(sort(prop.table(table(divvy_subscriber$week)), decreasing = TRUE), main = 'Subscriber', cex.names = 0.7)
barplot(sort(prop.table(table(divvy_customer$week)), decreasing = TRUE), main = 'Customer', cex.names = 0.7)

par(mfrow = c(1,1))

str(as.factor(divvy_final$from_station_id))

# 5. 5. 10 most used station and 10 least used station. 
# best 35, worst 562.

barplot(sort(table(as.factor(divvy_final$from_station_id)), decreasing = TRUE)[1:10], main = 'Frequently utilized station', xlab = "Station ID", ylab = 'No of users', col = 'lightblue')
best_station_id <- sort(table(as.factor(divvy_final$from_station_id)), decreasing = TRUE)[1:10]
names(best_station_id)

best_station <- c()
for(i in names(best_station_id))
{
  best_station[i] = divvy_final$from_station_name[divvy_final$from_station_id==i][1]
}
print(best_station)

barplot(sort(table(as.factor(divvy_final$from_station_id)))[1:10], main = 'Least utilized station', xlab = "Station ID", ylab = 'No of users', col = 'lightblue')
least_station_id <- sort(table(as.factor(divvy_final$from_station_id)))[1:10]
names(least_station_id)

least_station <- c()
for(i in names(least_station_id))
{
  least_station[i] = divvy_final$from_station_name[divvy_final$from_station_id==i][1]
}
print(least_station)

# 6. 6. get latitude and longitude of the places for geographic view.

location <- fread("/Users/Naveen/Downloads/Divvy_Trips_2016_Q3Q4/Divvy_Stations_2016_Q4.csv", header = TRUE)
View(location)
location$name <- NULL
location$online_date = NULL

divvy_final <- cbind(divvy_final, location[match(divvy_final$from_station_id, location$id),][,2:3])

point1 <- divvy_final[match(names(best_station_id), divvy_final$from_station_id),][,16:17]
point2 <- divvy_final[match(names(least_station_id), divvy_final$from_station_id),][,16:17]

points <- rbind(point1, point2)
points$class <- c(rep('Most Used',10), rep('Least Used',10))
View(points)

map <- get_map(location = 'chicago', zoom = 11)
ggmap(map) + geom_point(data = points, aes(longitude, latitude, color=factor(class)), size = 3)


table(divvy_final$start_month)

day_wise <- table(as.Date(divvy_final$starttime))
class(day_wise)
as.data.frame(day_wise)
View(day_wise)
plot(day_wise)

#7. for 20 best and worst

barplot(sort(table(as.factor(divvy_final$from_station_id)), decreasing = TRUE)[1:20], main = 'Frequently utilized station', xlab = "Station ID", ylab = 'No of users', col = 'lightblue')
best_station_id <- sort(table(as.factor(divvy_final$from_station_id)), decreasing = TRUE)[1:20]
names(best_station_id)

best_station <- c()
for(i in names(best_station_id))
{
  best_station[i] = divvy_final$from_station_name[divvy_final$from_station_id==i][1]
}
print(best_station)

barplot(sort(table(as.factor(divvy_final$from_station_id)))[1:20], main = 'Least utilized station', xlab = "Station ID", ylab = 'No of users', col = 'lightblue')
least_station_id <- sort(table(as.factor(divvy_final$from_station_id)))[1:20]
names(least_station_id)

least_station <- c()
for(i in names(least_station_id))
{
  least_station[i] = divvy_final$from_station_name[divvy_final$from_station_id==i][1]
}
print(least_station)

#Geographic view
point1 <- divvy_final[match(names(best_station_id), divvy_final$from_station_id),][,16:17]
point2 <- divvy_final[match(names(least_station_id), divvy_final$from_station_id),][,16:17]

points <- rbind(point1, point2)
points$class <- c(rep('Most Used',20), rep('Least Used',20))
View(points)

map <- get_map(location = 'chicago', zoom = 11)
ggmap(map) + geom_point(data = points, aes(longitude, latitude, color=factor(class)), size = 3)

#8. Time series

Q7 <- fread("/Users/Naveen/Downloads/Divvy_Stations_Trips_2014_Q1Q2/Divvy_Trips_2014_Q1Q2.csv", header = TRUE)
Q8 <- fread("/Users/Naveen/Downloads/Divvy_Stations_Trips_2014_Q3Q4/Divvy_Trips_2014-Q3-07.csv", header = TRUE)
Q9 <- fread("/Users/Naveen/Downloads/Divvy_Stations_Trips_2014_Q3Q4/Divvy_Trips_2014-Q3-0809.csv", header = TRUE)
Q10 <- fread("/Users/Naveen/Downloads/Divvy_Stations_Trips_2014_Q3Q4/Divvy_Trips_2014-Q4.csv", header = TRUE)
Q11 <- fread("/Users/Naveen/Downloads/Divvy_Trips_2015-Q1Q2/Divvy_Trips_2015-Q1.csv", header = TRUE)
Q12 <- fread("/Users/Naveen/Downloads/Divvy_Trips_2015-Q1Q2/Divvy_Trips_2015-Q2.csv", header = TRUE)
Q13 <- fread("/Users/Naveen/Downloads/Divvy_Trips_2015_Q3Q4/Divvy_Trips_2015_Q4.csv", header = TRUE)
Q14 <- fread("/Users/Naveen/Downloads/Divvy_Trips_2015_Q3Q4/Divvy_Trips_2015_07.csv", header = TRUE)
Q15 <- fread("/Users/Naveen/Downloads/Divvy_Trips_2015_Q3Q4/Divvy_Trips_2015_08.csv", header = TRUE)
Q16 <- fread("/Users/Naveen/Downloads/Divvy_Trips_2015_Q3Q4/Divvy_Trips_2015_09.csv", header = TRUE)


divvy_list <- list(Q1,Q2,Q3,Q4,Q5,Q6,Q7,Q8,Q9,Q10,Q11,Q12,Q13,Q14,Q15,Q16)

divvy <- ldply(divvy_list, data.table)
class(divvy)
View(divvy)

str(divvy)

numeric_list <- c('trip_id', 'bikeid', 'tripduration', 'from_station_id', 'to_station_id', 'birthyear')
for (i in numeric_list) {
  divvy[,i] = as.integer(divvy[,i])
}

divvy$gender[divvy$gender==""] <- 'unknown'
divvy$birthyear[is.na(divvy$birthyear)] <- 1980

divvy$starttime = as.POSIXlt(divvy$starttime,format="%m/%d/%Y %H:%M")
divvy$stoptime = strptime(divvy$stoptime, format = "%m/%d/%Y %H:%M")

divvy$usertype = as.factor(divvy$usertype)
divvy$gender = as.factor(divvy$gender)

summary(divvy)

divvy <- divvy[!(divvy$usertype=='Dependent'),]
divvy$usertype <- droplevels(divvy$usertype)

str(divvy)
summary(divvy)

divvy <- divvy[!(divvy$birthyear < 1940),]

divvy_time <- as.data.frame(table(as.Date(divvy$starttime)))
View(divvy_time)
plot(divvy_time)

time_series <- ts(divvy_time$Freq, frequency = 364)
time_series
plot(time_series)

time_dec <- decompose(time_series)
time_dec$seasonal
plot(time_dec)


acf(time_series)
pacf(time_series)

fit <- arima(time_series, order = c(1,2,7))
tsdisplay(residuals(fit))

fcast <- forecast(fit, h=30)
plot(fcast)
