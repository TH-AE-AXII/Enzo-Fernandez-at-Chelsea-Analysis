enzo_fernandez_data <- as.table(matrix(c(16, 3, 21, 44), ncol=2, byrow=T))
colnames(enzo_fernandez_data) = c("win", "Lose+Draw")
row.names(enzo_fernandez_data) = c("Without Enzo","With Enzo")
enzo_fernandez_data

prop.test(enzo_fernandez_data, correct=T, alternative="greater")
#?prop.test
p_win_without_enzo = enzo_fernandez_data[1]/(enzo_fernandez_data[1]+enzo_fernandez_data[3])
p_win_with_enzo = enzo_fernandez_data[2]/(enzo_fernandez_data[2]+enzo_fernandez_data[4])

p_win_without_enzo
p_win_with_enzo

obs_diff = p_win_without_enzo - p_win_with_enzo
obs_diff

n_sim =500 
diffs = numeric(n_sim)

total_wins = sum(enzo_fernandez_data[, 1])
total_games = sum(enzo_fernandez_data)
labels = c(rep(1, total_wins), rep(0, total_games - total_wins))

for (i in 1:n_sim) {
  shuffled = sample(labels)  
  without_enzo_win_rate = sum(shuffled[1:(enzo_fernandez_data[1] + enzo_fernandez_data[2])]) / 
    (enzo_fernandez_data[1] + enzo_fernandez_data[2])
  with_enzo_win_rate = sum(shuffled[(enzo_fernandez_data[1] + enzo_fernandez_data[2] + 1):total_games]) /
    (enzo_fernandez_data[3] + enzo_fernandez_data[4])
  
  diffs[i] = without_enzo_win_rate - with_enzo_win_rate
}

hist(diffs, 
     main="Distribution of Simulated Win Rate Differences",
     xlab="Win Rate Difference (Without Enzo - With Enzo)", 
     col="lightblue")

abline(v=obs_diff, col="red", lwd=2)
p_value = mean(diffs >= obs_diff)
p_value
prop.test(enzo_fernandez_data, correct=T, alternative="greater")
?binom.test

binom.test(9,12,0.5,"greater")


