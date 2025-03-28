library(MatchIt)
library(rdlocrand)
setwd("/Users/ainefernandez/documents/Github/Footballrdlocrand")

# LEAGUES
leagues <- read_csv("compligasfull.csv")
complete_leagues <- leagues[leagues$Advantage_HT >= -1 & leagues$Advantage_HT <= 1, ]
tie_losing_leagues <- leagues[leagues$Advantage_HT >= -1 & leagues$Advantage_HT <= 0, ]
tie_winning_leagues <- leagues[leagues$Advantage_HT >= 0 & leagues$Advantage_HT <= 1, ]
tie_winning_leagues$Winning_HT <- ifelse(tie_winning_leagues$Advantage_HT == 1, 1, 0)

# INTERNATIONAL
international <- read_csv("compintfull.csv")
covariates <- c("goal_difference_groupstage", "points_groupstage")  
international <- international[complete.cases(international[, covariates]), ]
complete_international <- international[international$Advantage_HT >= -1 & international$Advantage_HT <= 1, ]
tie_losing_international <- international[international$Advantage_HT >= -1 & international$Advantage_HT <= 0, ]
tie_winning_international <- international[international$Advantage_HT >= 0 & international$Advantage_HT <= 1, ]
tie_winning_international$Winning_HT <- ifelse(tie_winning_international$Advantage_HT == 1, 1, 0)

covariates_leagues <- c("shots_15", "shots_30", "shots_45", "shots_stoppage_HT1",  
                        "passes_15", "passes_30", "passes_45", "passes_stoppage_HT1",  
                        "subs_15", "subs_30", "subs_45", "subs_stoppage_HT1",  
                        "tactical_15", "tactical_30", "tactical_45", "tactical_stoppage_HT1",  
                        "fouls_won_15", "fouls_won_30", "fouls_won_45", "fouls_won_stoppage_HT1",  
                        "fouls_committed_15", "fouls_committed_30", "fouls_committed_45",  
                        "fouls_committed_stoppage_HT1", "yellow_cards_15", "yellow_cards_30", "yellow_cards_45",  
                        "yellow_cards_stoppage_HT1", "yellow_cards_2_15", "yellow_cards_2_30",  
                        "yellow_cards_2_45", "yellow_cards_2_stoppage_HT1",  
                        "red_cards_15", "red_cards_30", "red_cards_45", "red_cards_stoppage_HT1",  
                        "shots_total_HT1", "passes_total_HT1", "subs_total_HT1", "tactical_total_HT1",  
                        "fouls_won_total_HT1", "fouls_committed_total_HT1",    
                        "yellow_cards_total_HT1", "yellow_cards_2_total_HT1", "red_cards_total_HT1",  
                        "winning_percentage", "loss_percentage", "draw_percentage", "points_season",  
                        "goal_difference_season", "last_match_points", "win_last_match", "draw_last_match",  
                        "loss_last_match", "avg_points_last_2", "avg_goals_scored_last_2", "avg_goals_conceded_last_2")

covariates_international <- c("shots_15", "shots_30", "shots_45", "shots_stoppage_HT1",  
                              "passes_15", "passes_30", "passes_45", "passes_stoppage_HT1",  
                              "subs_15", "subs_30", "subs_45", "subs_stoppage_HT1",  
                              "tactical_15", "tactical_30", "tactical_45", "tactical_stoppage_HT1",  
                              "fouls_won_15", "fouls_won_30", "fouls_won_45", "fouls_won_stoppage_HT1",  
                              "fouls_committed_15", "fouls_committed_30", "fouls_committed_45",  
                              "fouls_committed_stoppage_HT1", "yellow_cards_15", "yellow_cards_30", "yellow_cards_45",  
                              "yellow_cards_stoppage_HT1", "yellow_cards_2_15", "yellow_cards_2_30",  
                              "yellow_cards_2_45", "yellow_cards_2_stoppage_HT1",  
                              "red_cards_15", "red_cards_30", "red_cards_45", "red_cards_stoppage_HT1",  
                              "shots_total_HT1", "passes_total_HT1", "subs_total_HT1", "tactical_total_HT1",  
                              "fouls_won_total_HT1", "fouls_committed_total_HT1",    
                              "yellow_cards_total_HT1", "yellow_cards_2_total_HT1", "red_cards_total_HT1", "goal_difference_groupstage",
                              "points_groupstage", "win_last_match", "loss_last_match", "draw_last_match", "last_match_points")

matching_leagues <- function(data, treatment) {
  model <- matchit(
    treatment ~ shots_15 + shots_30 + shots_45 + shots_stoppage_HT1 +
      points_season + goal_difference_season + winning_percentage +
      loss_percentage + draw_percentage,
    data = data,
    method = "optimal",
    distance = "glm"
  )
  print(summary(model))
  match.data(model)
}

matching_international <- function(data, treatment) {
  model <- matchit(
    treatment ~ shots_15 + shots_30 + shots_45 + shots_stoppage_HT1 +
      goal_difference_groupstage + points_groupstage,
    data = data,
    method = "optimal",
    distance = "glm"
  )
  print(summary(model))
  match.data(model)
}

calculate_balance <- function(matched_data, covariates, running_var, cutoff = 0, wl = -1, wr = 1) {
  # Initialize balance table
  balance_table <- data.frame(Covariate = character(), Diff_in_Means = numeric(), P_Value = numeric())
  
  # Loop through each covariate
  for (var in covariates) {
    result <- rdrandinf(matched_data[[var]], running_var, cutoff = cutoff, wl = wl, wr = wr)
    
    # Add result to balance table
    balance_table <- rbind(balance_table, data.frame(
      Covariate = var,
      Diff_in_Means = result$obs.stat,
      P_Value = result$p.value
    ))
  }
  
  # Return the balance table
  return(balance_table)
}

# NATIONAL LEAGUES

# Complete leagues [-1,1] (Advantage_HT)
matched_complete_leagues <- matching_leagues(complete_leagues, complete_leagues$Losing_HT)
balance_table_complete_leagues <- calculate_balance(matched_complete_leagues, covariates_leagues, matched_complete_leagues$Advantage_HT, cutoff = 0, wl = -1, wr = 1)

# Tie-losing leagues [-1,0] (Advantage_HT)
matched_tie_losing_leagues <- matching_leagues(tie_losing_leagues, tie_losing_leagues$Losing_HT)
balance_table_tie_losing_leagues <- calculate_balance(matched_tie_losing_leagues, covariates_leagues, matched_tie_losing_leagues$Advantage_HT, cutoff = -0.99, wl = -1, wr = 0)

# Tie-winning leagues [0,1] (Advantage_HT)
matched_tie_winning_leagues <- matching_leagues(tie_winning_leagues, tie_winning_leagues$Winning_HT)
balance_table_tie_winning_leagues <- calculate_balance(matched_tie_winning_leagues, covariates_leagues, matched_tie_winning_leagues$Advantage_HT, cutoff = .1, wl = 0, wr = 1)

# INTERNATIONAL

# Complete international [-1,1] (Advantage_HT)
matched_complete_international <- matching_international(complete_international, complete_international$Losing_HT)
balance_table_complete_international <- calculate_balance(matched_complete_international, covariates_international, matched_complete_international$Advantage_HT, cutoff = 0, wl = -1, wr = 1)

# Tie-losing international [-1,0] (Advantage_HT)
matched_tie_losing_international <- matching_international(tie_losing_international, tie_losing_international$Losing_HT)
balance_table_tie_losing_international <- calculate_balance(matched_tie_losing_international, covariates_international, matched_tie_losing_international$Advantage_HT, cutoff = -0.99, wl = -1, wr = 0)

# Tie-winning international [0,1] (Advantage_HT)
matched_tie_winning_international <- matching_international(tie_winning_international, tie_winning_international$Winning_HT)
balance_table_tie_winning_international <- calculate_balance(matched_tie_winning_international, covariates_international, matched_tie_winning_international$Advantage_HT, cutoff = .1, wl = 0, wr = 1)
