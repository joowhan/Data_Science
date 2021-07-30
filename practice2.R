
load(url('https://github.com/hbchoi/SampleData/raw/master/country.RData'))
#practice1
head(country, 10)
tail(country, 10)

#practice2

# str(country)
# country$continent <- as.factor(country$continent

# supply(country, is.character)
# supply(country, factor)
# cont_names <- names(country)

country[ ,sapply(country, is.character)]
idx <- sapply(country, function(x){
  nlevels(factor(x)) <= 10
})
idx
country[,idx] <- factor(country[,idx])
str(country)

#practice3
summary(country$continent)

#practice4
levels(country$continent)<- c("AF", "AS", "EU", "NA", "OC", "SA")
summary(country$continent)
table(country$continent)

#practice5
meanGDP <- mean(country$GDP)
GDP_group <- ifelse(country$GDP>meanGDP, 'HIGH', 'LOW')
country <- cbind(country, GDP_group)

#practice 6
Find_continent <- function(n){
  ifelse(n == country$continent, TRUE, FALSE)
}
Find_continent("AS")
Find_continent("EU")
country$continent
sum(Find_continent("AS"))
mean(Find_continent("AS")) # rate of true (rate of Asia)