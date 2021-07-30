load(url('https://github.com/hbchoi/SampleData/raw/master/country.RData'))
str(country)

#practice1
num_var <-country[, sapply(country, typeof) %in% c("integer", "double")]
var_name <- colnames(num_var)

#practice2
x = sapply(country, typeof)
col_int <- x %in% c("integer", "double")
col_int
colnames(country[,col_int])
country[, col_int] <- sapply(country[,col_int], function(x){rank(-x)})
head(country, 10)

#practice3
country[grep("South Korea", country$country_name),]

#practice4

#practice5
app <- read.table('apps_delimiter-1.csv', head =TRUE, sep="^", stringsAsFactors = FALSE)
str(app)
app<- app[,-1]

#practice6
aggregate(app$Rating ~ app$Genres, data= app, FUN = mean)

#practice7
app[order(app$Reviews, decreasing = T),]


