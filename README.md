README for Enzo Fernandez Win Rate Analysis

This R script conducts an analysis on the win rate of a team with and without Enzo Fernandez, comparing the observed win rate difference using a permutation test and statistical tests like prop.test and binom.test.

Requirements

	•	R environment with the following libraries (standard functions only, so no additional libraries are required).

Data Structure

The dataset (enzo_fernandez_data) is a 2x2 contingency table representing the win and “lose/draw” results for a team with and without Enzo Fernandez.

	•	Columns:
	•	"win" — Number of games won.
	•	"Lose+Draw" — Number of games that were either lost or drawn.
	•	Rows:
	•	"Without Enzo" — Results for games without Enzo Fernandez.
	•	"With Enzo" — Results for games with Enzo Fernandez.

Data Creation

enzo_fernandez_data <- as.table(matrix(c(16, 3, 21, 44), ncol=2, byrow=T))
colnames(enzo_fernandez_data) = c("win", "Lose+Draw")
row.names(enzo_fernandez_data) = c("Without Enzo", "With Enzo")

Statistical Analysis Steps

	1.	Proportion Test (prop.test)
The prop.test function is used to test if there is a statistically significant difference in win rates when Enzo Fernandez is in the team, assuming a “greater” alternative hypothesis (testing if the win rate is higher with Enzo).

prop.test(enzo_fernandez_data, correct=T, alternative="greater")


	2.	Observed Win Rate Calculation
The win rates for games with and without Enzo are calculated and the observed difference is stored.

p_win_without_enzo = enzo_fernandez_data[1] / (enzo_fernandez_data[1] + enzo_fernandez_data[3])
p_win_with_enzo = enzo_fernandez_data[2] / (enzo_fernandez_data[2] + enzo_fernandez_data[4])
obs_diff = p_win_without_enzo - p_win_with_enzo


	3.	Permutation Test
A permutation test is performed to simulate the win rate differences under the null hypothesis, where there is no actual difference in performance with or without Enzo.
	•	n_sim is set to 500, the number of simulations.
	•	The observed difference is compared to the simulated distribution to compute a p_value.

n_sim = 500 
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


	4.	Binomial Test (binom.test)
An additional binom.test is performed to validate the win rate difference, specifically testing if the proportion of wins exceeds 50% in a subset of games.

binom.test(9, 12, 0.5, "greater")



Summary Output

The script outputs:

	•	The observed win rates with and without Enzo.
	•	The observed win rate difference.
	•	A histogram showing the distribution of simulated win rate differences.
	•	A p_value indicating the likelihood of observing the difference if there is no true difference.
	•	Results from prop.test and binom.test for statistical inference.

This analysis can help infer whether Enzo Fernandez’s presence in the team significantly affects the win rate.
