pokemon <- read.csv('pokemon_fix-1.csv')
# install.packages("dplyr")
library(dplyr)
#install.packages('ggplot2')
library(ggplot2)

#practice1
glimpse(pokemon)

#practice2
pokemon$type1 <- factor(pokemon$type1)
levels(pokemon$type1)
table(pokemon$type1)

type1.total <- aggregate(cbind(attack, defense, speed, hp, sp_attack, sp_defense) ~ type1, data=pokemon, FUN= mean)
barplot(cbind(type1.total$attack, type1.total$defense, type1.total$speed, type1.total$hp, type1.total$sp_attack, type1.total$sp_defense) ~ type1.total$type1, 
        beside=T, col=rainbow(6), xlab="type", ylab = "average", main="graph of type1",horiz=F)
#axis(side=2, at=seq(0,120,20))
legend("topright", legend=c("attack", "defense", "speed", "HP", "sp_attack", "sp_defense"), fill=rainbow(6))

#practice3
summary(pokemon$weight_kg)

pokemon$weight_height <- pokemon$weight_kg* pokemon$height_m
#colnames(weight_height) <- c("weight", "height")


cut_points <- quantile(pokemon$weight_height, na.rm = T)
pokemon$weight_group <- cut(pokemon$weight_height, breaks = cut_points, include.lowest = T)
levels(pokemon$weight_group) <- c('Very Low', 'Low', 'High', 'Very High')
table(pokemon$weight_group)
pokemon.weight.group <- data.frame(
  pokemon$weight_group, pokemon$attack, pokemon$defense, pokemon$speed, pokemon$hp, pokemon$sp_attack, pokemon$sp_defense)
table(pokemon.weight.group$pokemon.weight_group)

colnames(pokemon.weight.group)<- c("weight_group", "attack", "defense", "speed", "hp", "sp_attack", "sp_defense")

weight_result <- aggregate(cbind(attack, defense, speed, hp, sp_attack, sp_defense) ~ weight_group, data = pokemon.weight.group, FUN=mean)
barplot(
  cbind(weight_result$attack, weight_result$defense, weight_result$speed, weight_result$hp, weight_result$sp_attack, weight_result$sp_defense) ~ weight_result$weight_group, 
        beside=T, col=rainbow(6), xlab="levels of weight", ylab = "average", main="graph of type1",horiz=F)
legend("topleft", legend=c("attack", "defense", "speed", "HP", "sp_attack", "sp_defense"), fill=rainbow(6))

plot(x= pokemon$weight_kg, y=pokemon$height_m)

#practice4

pokemon$generation <- factor(pokemon$generation)
levels(pokemon$generation)
summary(pokemon$generation)

##for 6 abilities
generation.total <- aggregate(cbind(attack,defense, speed, hp, sp_attack, sp_defense) ~ generation, data = pokemon, FUN = mean)

barplot(
  cbind(generation.total$attack, generation.total$defense, generation.total$speed, generation.total$hp, generation.total$sp_attack, generation.total$sp_defense) ~ generation.total$generation, 
        beside=T, col=rainbow(6), xlab="generation", ylab = "average", main="graph of type1",horiz=F)
#axis(side=2, at=seq(0,120,20))
legend("topright", legend=c("attack", "defense", "speed", "HP", "sp_attack", "sp_defense"), fill=rainbow(6))

#another way

generation1 <- pokemon[pokemon$generation=="1",]
generation2 <- pokemon[pokemon$generation=="2",]
generation3 <- pokemon[pokemon$generation=="3",]
generation4 <- pokemon[pokemon$generation=="4",]
generation5 <- pokemon[pokemon$generation=="5",]
generation6 <- pokemon[pokemon$generation=="6",]
generation7 <- pokemon[pokemon$generation=="7",]

glimpse(generation1)

generation1$classfication <- factor(generation1$classfication)
levels(generation1$classfication)
generation2$classfication <- factor(generation2$classfication)

num.pokemon <- generation1[, sapply(generation1, is.numeric)]
num.mean <- colMeans(num.pokemon, na.rm = T)
num.pokemon <-generation2[, sapply(generation2, is.numeric)]
num.mean <- cbind(num.mean, colMeans(num.pokemon, na.rm = T))
num.pokemon <-generation3[, sapply(generation3, is.numeric)]
num.mean <- cbind(num.mean, colMeans(num.pokemon, na.rm = T))
num.pokemon <-generation4[, sapply(generation4, is.numeric)]
num.mean <- cbind(num.mean, colMeans(num.pokemon, na.rm = T))
num.pokemon <-generation5[, sapply(generation5, is.numeric)]
num.mean <- cbind(num.mean, colMeans(num.pokemon, na.rm = T))
num.pokemon <-generation6[, sapply(generation6, is.numeric)]
num.mean <- cbind(num.mean, colMeans(num.pokemon, na.rm = T))
num.pokemon <-generation7[, sapply(generation7, is.numeric)]
num.mean <- cbind(num.mean, colMeans(num.pokemon, na.rm = T))

colnames(num.mean) <- c("generation1","generation2", "generation3","generation4", "generation5", "generation6", "generation7")
num.mean <-round(num.mean, 3)

#practice5
pokemon[pokemon$name == 'Pikachu', ]


