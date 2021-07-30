weather_df <- readRDS('weather.rds')
summary(weather_df)

library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)

#practice1

#practice2
weather_df <-weather_df[,-1]

#practice3
weather_tidy <- gather(weather_df, dayOfMonth, val, -year, -month, -measure)
weather_tidy <- spread(weather_tidy, measure, val)
head(weather_tidy, 10)
str(weather_tidy)

#practice4
weather_tidy$dayOfMonth <- str_replace(weather_tidy$dayOfMonth, "X", "")
weather_tidy$dayOfMonth <- as.numeric(weather_tidy$dayOfMonth)
#practice5

weather_tidy <- unite(weather_tidy, date, year, month, dayOfMonth, sep = "-")
weather_tidy$date <- as.Date(weather_tidy$date)

#practice 6
weather_tidy$PrecipitationIn <- str_replace(weather_tidy$PrecipitationIn, "T", "0")

#practice 7

#ver1
weather_tidy$CloudCover <- as.double(weather_tidy$CloudCover)
weather_tidy$Events <- as.factor(weather_tidy$Events)
weather_tidy[, 4:23] <- sapply(weather_tidy[, 4:23], function(x) as.double(x))
glimpse(weather_tidy)

#ver2
x <- sapply(weather_tidy[,4:23], typeof)
col_int <- which(x %in% c("character"))
col_int <- col_int+3
weather_tidy[, col_int] <- sapply(weather_tidy[, col_int], function(x) as.double(x))
glimpse(weather_tidy)

#practice8
any(is.na(weather_tidy))
sum(is.na(weather_tidy))
colSums(is.na(weather_tidy))

#practice9
summary(weather_tidy$Max.Humidity)
boxplot(weather_tidy$Max.Humidity)
plot(weather_tidy$Max.Humidity)
mean(weather_tidy$Max.Humidity, na.rm = T)
weather_tidy$Max.Humidity <- str_replace(weather_tidy$Max.Humidity, "1000","100")
typeof(weather_tidy$Max.Humidity)
weather_tidy$Max.Humidity <- as.double(weather_tidy$Max.Humidity)
boxplot(weather_tidy$Max.Humidity)

#practice10
boxplot(weather_tidy$Mean.VisibilityMiles)
plot(weather_tidy$Mean.VisibilityMiles)
weather_tidy[196,]

#practice11
#levels(weather_tidy$Events) <- c("None", levels(weather_tidy$Events[2:12]))
weather_tidy$Events <- str_pad(weather_tidy$Events,1,"both")
weather_tidy$Events <- str_replace(weather_tidy$Events, "", "None")

#practice12
names(weather_tidy)<-tolower(names(weather_tidy))
head(weather_tidy,3)

#practice13
saveRDS(weather_tidy,"weather_tidy1.rds")







