# Training data: First 70% of 2025/26 Premier League season
football2026 <- read.csv(file.path("Prem2526data.csv"))
football2026[1:3, ]
Prem2526data <- read_csv("PremPredictionModel/Prem2526data.csv")
Prem2526data[1:3, ]



Prem2526data$By <- relevel(factor(Prem2526data$By), "Leeds")
Prem2526data$Against <- relevel(factor(Prem2526data$Against), "Leeds")

LogLinMod26 <- glm(GoalsScored ~ HomeAway + By + Against, family = poisson, data=Prem2526data)
summary(LogLinMod26)

attack_strength26 <- exp(sort(coef(LogLinMod26)[3:21], decreasing = TRUE))
barplot(rev(attack_strength26), las=2, horiz=TRUE, cex.names=0.75)
# las=2 and cex.names=0.75 rotate the labels

# Can we predict the outcome of the 2026 premier league?
fixtures_remaining2526 <- read_csv("PremPredictionModel/fixtures_remaining2526.csv")
fixtures_remaining2526[20,2] <- 'Liverpool'
Pred26 <- predict(LogLinMod26, newdata=fixtures_remaining2526, type="response")
cbind(fixtures_remaining2526, Pred26)

# number of simulations
B26 <- 3000
n_rem_fix26 <- length(Pred26)/2
# create an empty matrix to store the simulation results
sim_points26 <- matrix(nrow=2*n_rem_fix26, ncol=B26)
for (b in 1:B26) {
  # The simulated difference in the score between the Home and Away teams
  sim_score_diff26 <- rpois(n=n_rem_fix26, lambda=Pred26[1:n_rem_fix26]) -
    rpois(n=n_rem_fix26, lambda=Pred26[(n_rem_fix26+1):(2*n_rem_fix26)])
  # Calculate the points scored
  points_scored26 <- 2*sign(sim_score_diff26)
  points_scored26 <- c(pmax(points_scored26+1, 0), pmax(1-points_scored26, 0))
  sim_points26[, b] <- points_scored26
}

# Transform the points of each team to ranking
sim_table26 <- aggregate(sim_points26, by=list(fixtures_remaining2526$By), FUN=sum)
sim_table26[, 1:10]
rownames(sim_table26) <- sim_table26[, 1]
sim_table26 <- sim_table26[, -1]

PremTable2526soFar <- read_csv("PremPredictionModel/PremTable2526soFar.csv")
sim_table26 <- sim_table26 + PremTable2526soFar[, 2]

sim_ranks26 <- apply(-sim_table26, 2, function(x) rank(x, ties.method="random"))
final_standings26 <- apply(sim_ranks26, 1, function(x) tabulate(x, 20)) / B
heatmap(t(final_standings26), Rowv = NA, Colv=NA)
