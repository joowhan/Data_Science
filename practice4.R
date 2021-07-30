GDP <- read.csv('GDP.csv')
POP <- read.csv('population.csv')
LIFE_EXP <- read.csv('Life Expectancy.csv')

#practice1
colnames(POP) <- c('Country','POP')

#practice2
data <-merge(GDP, POP)

#practice3
total_data <- merge(data, LIFE_EXP)

#practice4-1
korea <- total_data[total_data$Country=='South Korea',]
korea$GDP
highGDP <- subset(total_data, GDP > korea$GDP)
highGDP$Country

#practice4-2
LowPOP <- subset(highGDP, korea$POP > POP)
LowPOP[,1:3]

#practice4-3
total_data$GDP
typeof(total_data$POP)
total_data$Country_GDP <- (total_data$GDP * (total_data$POP / 1000))

#practice5_1
sampleCountry <- sample(total_data$Country, 20, replace = FALSE)
sample_idx <- total_data$Country %in% sampleCountry
sample.mt <- rbind(sapply(total_data[sample_idx,2:5], mean))

#practice5_2
for(i in 1:9){
  sampleCountry <- sample(total_data$Country, 20, replace = FALSE)
  sample_idx <- total_data$Country %in% sampleCountry
  sample.mt <- rbind(sample.mt, sapply(total_data[sample_idx,2:5], mean))
}

#practice5-3
sample.mean <- colMeans(sample.mt)
original.mean <- colMeans(total_data[,2:5])

#practice5-4
set.seed(2021)
sampleCountry2 <- sample(total_data$Country, 20, replace = FALSE)
sample_idx2 <- total_data$Country %in% sampleCountry2
sample.mt2 <- rbind(sapply(total_data[sample_idx2,2:5], mean))

for(i in 1:9){
  set.seed(2021)
  sampleCountry2 <- sample(total_data$Country, 20, replace = FALSE)
  sample_idx2 <- total_data$Country %in% sampleCountry2
  sample.mt2 <- rbind(sample.mt2, sapply(total_data[sample_idx2,2:5], mean))
}

#practice6
total_data$Country_GDP <- total_data$Country_GDP/1000000
total_data$Country_GDP<-round(total_data$Country_GDP, 2)
total_data$Country_GDP <- paste0(total_data$Country_GDP, 'B')

#practice7
GDPindex <- which(original.mean[1]<total_data$GDP)
POPindex <- which(original.mean[2]<total_data$POP)
LIFEindex <- which(original.mean[3]<total_data$Life_exp)

interGDP_POP <- intersect(GDPindex, POPindex)
intersect(interGDP_POP, LIFEindex)

#practice8

# GDP의 크기에 따라 국가를 총 4개의 그룹으로 분류하려고 한다. 
#(Very Low, Low, High, Very High) 각 그룹에 속하는 나라의 수를 최대한 동등하게 나눠보려고 한다.
# quantile과 cut 함수를 이용해서 ’GDP_group’이라는 새로운 변수를 만들어보자. 
#table 함수를 이용하여 잘 나눠졌는지 확인하라.

cut_points <- quantile(total_data$GDP, probs = c(0, 0.25, 0.5, 0.75, 1))
total_data$GDP_group <- cut(total_data$GDP, breaks = cut_points, include.lowest = T)
levels(total_data$GDP_group) <- c('Very Low', 'Low', 'High', 'Very High')
table(total_data$GDP_group)

quantile(total_data$GDP)

#practice9

# ‘aggregate’ function 8번 문제에서 나눈 그룹을 기준으로 인구, 기대수명의 그룹별 평균을 구하여 비교해보아라.
# 경제수준(GDP)와 인구, 기대 수명이 상관관계가 있다고 생각되는가? 
  
result.life <- aggregate(Life_exp ~ GDP_group, data = total_data, FUN = mean)
result.pop <- aggregate(POP ~ GDP_group, data = total_data, FUN = mean)
final_result <- merge(result.life, result.pop)
final_result[c(4,2,1,3),]


