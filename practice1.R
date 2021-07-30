#practice1
movie <- c("The Avengers", "Harry Potter", "Home Alone", "Toy Story", "Frozen", "The Notebook", "Intersteller")
movie

#practice2
my_rating <- c(4.5, 4.7, NA, NA, 4, 4.6, 3.8)
my_rating

#practice3
TA_rating <- c(4.3, NA, 3.8, 3.0, 2.8, NA, 1.6)
TA_rating

#practice4
member1_rating <- c(3.8, 4.8, 4.5, 2.1, 4.5, 3.1, NA)
member2_rating <- c(4.7, 4.4, 3.8, 4.4, 3.4, NA, NA)
member3_rating <- c(4.5, 5, 3.8, 3.5, 4, NA, 3.7)

team_matrix <- matrix(c(my_rating, TA_rating, member1_rating, member2_rating, member3_rating), byrow = TRUE, nrow = 5)
team_matrix

#practice5
#mean(my_rating, na.rm=TRUE)
stu_mean <- rowSums(team_matrix, na.rm =TRUE) / 5
stu_mean

#practice6
movie_sum <- colSums(team_matrix, na.rm = TRUE)
movie_sum

#practice7
team_matrix <-cbind(team_matrix, stu_mean)
team_matrix

#practice8
team_matrix <- rbind(team_matrix, movie_sum)
team_matrix

#practice9
team_matrix[6,8] <- NA
team_matrix

#practice10
member_name <-c("Me", "TA", "1", "2", "3", "movie_sum")
titles <- c("The Avengers", "Harry Potter", "Home Alone", "Toy Story", "Frozen", "The Notebook", "Intersteller", "stu_mean")
colnames(team_matrix) <- titles
rownames(team_matrix) <- member_name
team_matrix

#practice11
# centering_matrix <- team_matrix[1:5, 1:7]
# centering_matrix[1,] <-centering_matrix[1,] - stu_mean[1]
# centering_matrix[2,] <-centering_matrix[2,] - stu_mean[2]
# centering_matrix[3,] <-centering_matrix[3,] - stu_mean[3]
# centering_matrix[4,] <-centering_matrix[4,] - stu_mean[4]
# centering_matrix[5,] <-centering_matrix[5,] - stu_mean[5]
# centering_matrix

require(stats)
x<-centering_matrix
(centered.x<-scale(x,scale=FALSE))
cov(centered.scaled.x<-scale(x))
centering_matrix

# center_scale <- function(x) {
#   scale(x, scale = FALSE)
# }
# center_scale(centering_matrix)

#practice12
print("before centering")
movie_mean <- team_matrix[6,]/5
movie_mean

print("After centering")
centering_sum <- colSums(centering_matrix, na.rm = TRUE)
centering_mean <-centering_sum/5
centering_mean