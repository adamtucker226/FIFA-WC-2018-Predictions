# Load required packages
library(tidyverse)
library(lubridate)
library(nnet)

# Load data
results <- read_csv("results.csv")
rankings <- read_csv("fifa_ranking.csv")

# Remove results before rankings data and create month and year columns
# for matching with rankings
results <- 
  results %>% 
  filter(year(date) >= 1993, tournament != "Friendly") %>%
  mutate(year = year(date), month = month(date))

# Create month and year columns for matching with results
rankings <-
  rankings %>% mutate(year = year(rank_date), month = month(rank_date))

# Remove countries that don't exist in rankings
for (i in 1:nrow(results)) {
  if (results[i, 2] %in% unique(rankings$country_full) == FALSE) {
    results = results[-i, ]
  }
}

# Remove more data from results that aren't in rankings data
results <- results[-c(which(results$year == 1993 & results$month < 8)), ]

# Change some names for better reference
colnames(results)[2] <- "team_one"
colnames(results)[3] <- "team_two"
colnames(results)[4] <- "team_one_score"
colnames(results)[5] <- "team_two_score"

# Create outcome variable
outcome <- c()
for (i in 1:nrow(results)) {
  if (results$team_one_score[i] == results$team_two_score[i]) {
    outcome = c(outcome, "tie")
  }
  if (results$team_one_score[i] > results$team_two_score[i]) {
    outcome = c(outcome, "win")
  }
  if (results$team_one_score[i] < results$team_two_score[i]) {
    outcome = c(outcome, "loss")
  }
}
results <- cbind(results, outcome)

# Change home team advantage to binary variable
for (i in 1:nrow(results)) {
  if (results[i, 9] == TRUE) {
    results[i, 9] = 1
  } else {
    results[i, 9] = 0
  }
}

# Remove unnecessary columns
results <- results[, -c(6, 7, 8)]

# Sync rankings data
# For unranked opponents, I put an NA value in to easily drop observation later
team_one_rank <- c()
team_two_rank <- c()
team_one_form <- c()
team_two_form <- c()

# Should all be one loop probably, but oh well I got lazy
for (i in 1:nrow(results)) {
  dummy_df = filter(rankings, year == results$year[i], month == results$month[i])
  if (length(which(results$team_one[i] == dummy_df$country_full)) != 1) {
    team_one_rank = c(team_one_rank, NA)
  } else {
    team_one_rank = 
      c(team_one_rank, 
        dummy_df$rank[which(results$team_one[i] == dummy_df$country_full)])
  }
}

for (i in 1:nrow(results)) {
  dummy_df = filter(rankings, year == results$year[i], month == results$month[i])
  if (length(which(results$team_two[i] == dummy_df$country_full)) != 1) {
    team_two_rank = c(team_two_rank, NA)
  } else {
    team_two_rank = 
      c(team_two_rank, 
        dummy_df$rank[which(results$team_two[i] == dummy_df$country_full)])
  }
}

for (i in 1:nrow(results)) {
  dummy_df = filter(rankings, year == results$year[i], month == results$month[i])
  if (length(which(results$team_one[i] == dummy_df$country_full)) != 1) {
    team_one_form = c(team_one_form, NA)
  } else {
    team_one_form = 
      c(team_one_form, 
        dummy_df$rank_change[which(results$team_one[i] == dummy_df$country_full)])
  }
  if (length(which(results$team_two[i] == dummy_df$country_full)) != 1) {
    team_two_form = c(team_two_form, NA)
  } else {
    team_two_form = 
      c(team_two_form, 
        dummy_df$rank_change[which(results$team_two[i] == dummy_df$country_full)])
  }
}

results <- cbind(results, team_one_rank, team_one_form, team_two_rank, team_two_form)

# Remove NA values from results for our model's dataframe
results <- results[complete.cases(results), ]

# Fit the model, but first set tie to be reference point for multinomial model
results$outcome2 <- relevel(results$outcome, ref = "tie")
model <- 
  multinom(outcome2 ~ team_one_rank + team_two_rank + team_one_form + team_two_form + neutral,
           data = results)

# Wald test p-values
z <- summary(model)$coefficients/summary(model)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1)) * 2


# Build group stage df
wc_data <- read_csv("World Cup 2018 Dataset.csv")
wc_data <- wc_data[-c(1, 34), ]
wc_rankings <- filter(rankings, year == 2018, month == 6)

for (j in c(1, 8, 12, 16)) {
  for (i in 1:32) {
    if (wc_data[i , j] == "Porugal") {
      wc_data[i, j] = "Portugal"
    }
    if (wc_data[i , j] == "Columbia") {
      wc_data[i, j] = "Colombia"
    }
    if (wc_data[i , j] == "Costarica") {
      wc_data[i, j] = "Costa Rica"
    }
    if (wc_data[i , j] == "Korea") {
      wc_data[i, j] = "Korea Republic"
    }
    if (wc_data[i, j] == "IRAN") {
      wc_data[i, j] = "IR Iran"
    }
    if (wc_data[i, j] == "Iran") {
      wc_data[i, j] = "IR Iran"
    }
  }
}

team_one <- c()
team_two <- c()
team_one_rank <- c()
team_two_rank <- c()
team_one_form <- c()
team_two_form <- c()

for (i in 1:32) {
  for (j in c(8, 12, 16)) {
    team_one = c(team_one, as.character(wc_data$Team[i]))
    team_two = c(team_two, as.character(wc_data[i, j]))
    team_one_rank = c(team_one_rank,
                      as.integer(wc_rankings$rank[which(wc_rankings$country_full == wc_data$Team[i])]))
    team_two_rank = c(team_two_rank,
                      wc_rankings$rank[which(wc_rankings$country_full == as.character(wc_data[i, j]))])
    team_one_form = c(team_one_form,
                      as.integer(wc_rankings$rank_change[which(wc_rankings$country_full == wc_data$Team[i])]))
    team_two_form = c(team_two_form,
                      wc_rankings$rank_change[which(wc_rankings$country_full == as.character(wc_data[i, j]))])
  }
}

neutral <- c(rep.int(1, 3), rep.int(0, length(team_one) - 3))
group_stage_df <- data_frame(team_one, team_two, team_one_rank, team_two_rank, team_one_form, team_two_form, neutral)
group_stage_preds <- predict(model, group_stage_df, type = "prob")
group_stage_df <- cbind(group_stage_df, group_stage_preds)

# Make expected points predictions
ep_df <- data_frame(Country = unique(group_stage_df$team_one), Group = wc_data$Group)
ep <- c()

for (i in 1:nrow(ep_df)) {
  ep_loop_df = filter(group_stage_df, team_one == ep_df$Country[i])
  ep_loop_df = mutate(ep_loop_df, exp_pts = 0 * loss + 1 * tie + 3 * win)
  ep = c(ep, sum(ep_loop_df$exp_pts))
}

ep_df <- cbind(ep_df, ep)
ep_df$Group <- factor(ep_df$Group)
split.data.frame(ep_df, factor(ep_df$Group))

# Bootstrap for finding uncertainty in expected points
B <- 1000
se_boot <- c()
bootstrap_ep <- function(x) {
  ep_per_game = (0*x[1] + 1*x[2] + 3*x[3])
  return(ep_per_game)
}

for (i in 1:nrow(ep_df)) {
  bootstrap_df = filter(group_stage_df, team_one == ep_df$Country[i])
  bootstrap_ep_country = c()
  for (j in 1:B) {
    b_samp = sample(1:3, size = 3, replace = TRUE)
    b_df = bootstrap_df[b_samp, ]
    b_preds = predict(model, newdata = b_df, type = "probs")
    b_ep_loop = sum(unname(apply(b_preds, 1, bootstrap_ep)))
    bootstrap_ep_country = c(bootstrap_ep_country, b_ep_loop)
  }
  se_boot_loop = sd(bootstrap_ep_country)
  se_boot = c(se_boot, se_boot_loop)
}

ep_df = cbind(ep_df, se_boot)
ep_df = mutate(ep_df, errbar_min = ep - se_boot, errbar_max = ep + se_boot)

# Visualize
ggplot(ep_df, aes(x = Country, y = ep, fill = Group)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = errbar_min, ymax = errbar_max), width = 0.5) +
  coord_flip() +
  scale_x_discrete(limits = rev(ep_df$Country),
                   labels = c("Egypt" = expression(bold(Egypt)),
                              "Uruguay" = expression(bold(Uruguay)),
                              "Portugal" = expression(bold(Portugal)),
                              "Spain" = expression(bold(Spain)),
                              "France" = expression(bold(France)),
                              "Peru" = expression(bold(Peru)),
                              "Argentina" = expression(bold(Argentina)),
                              "Iceland" = expression(bold(Iceland)),
                              "Brazil" = expression(bold(Brazil)),
                              "Switzerland" = expression(bold(Switzerland)),
                              "Germany" = expression(bold(Germany)),
                              "Mexico" = expression(bold(Mexico)),
                              "Belgium" = expression(bold(Belgium)),
                              "England" = expression(bold(England)),
                              "Poland" = expression(bold(Poland)),
                              "Colombia" = expression(bold(Colombia)))) +
  labs(title = "Expected Points - World Cup 2018 Group Stage",
       y = "Expected Points") +
  theme_minimal() +
  theme(axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        plot.title = element_text(size = 18))
ggsave(filename = "ep_plot1.png", device = "png", width = 7, height = 7)
