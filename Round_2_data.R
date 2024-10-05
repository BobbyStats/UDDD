# Load necessary libraries
library(dplyr)
library(readr)

# Step 1: Merge DataFrames on player name
merged_data_r2 <- deduplicated_data %>%
  left_join(weekly_summary20242, by = c("player_name" = "athlete_display_name"))

# Step 2: Filter for playoff teams (made_playoffs == 1)
playoff_teams <- merged_data_r2 %>%
  filter(made_playoffs == 1)

# Step 3: Filter for weeks 18-23
playoff_weeks <- playoff_teams %>%
  filter(udweek >= 18 & udweek <= 23)

# Step 4: Group by tournament_entry_id and udweek, then calculate the weekly playoff score
playoff_scores <- playoff_weeks %>%
  group_by(tournament_entry_id, udweek) %>%
  # Step 5: Separate by position (Slot Name) and get the best two Gs, best two Ws, best one B, and the next best player
  summarise(
    best_two_guards = sum(total_ud[slotName == "G"] %>% sort(decreasing = TRUE) %>% head(2)),
    best_two_wings = sum(total_ud[slotName == "W"] %>% sort(decreasing = TRUE) %>% head(2)),
    best_big = total_ud[slotName == "B"] %>% sort(decreasing = TRUE) %>% head(1) %>% sum(),
    next_best = total_ud %>% sort(decreasing = TRUE) %>% head(6) %>% tail(1) %>% sum(),
    weekly_score = best_two_guards + best_two_wings + best_big + next_best
  )

# Step 6: Summarize relevant columns from playoff_scores, excluding udweek
playoff_summary <- playoff_scores %>%
  group_by(tournament_entry_id) %>%
  summarize(
    playoff_sprint_best_two_guards = sum(best_two_guards, na.rm = TRUE),
    playoff_sprint_best_two_wings = sum(best_two_wings, na.rm = TRUE),
    playoff_sprint_best_big = sum(best_big, na.rm = TRUE),
    playoff_sprint_next_best = sum(next_best, na.rm = TRUE),
    playoff_sprint_weekly_score = sum(weekly_score, na.rm = TRUE)
  )

# Step 7: Combine this summarized data back into the deduplicated_data
UD_sprint_sim <- deduplicated_data %>%
  left_join(playoff_summary, by = "tournament_entry_id")

#Step 8: Combine this summarized
sprint_summary_df <- UD_sprint_sim %>%
  filter(playoff_sprint_weekly_score>0) %>%
  group_by(tournament_entry_id) %>%
  summarize(
    username = first(username),
    draft_time = first(draft_time),
    clock_avg = mean(clock, na.rm = TRUE),
    pick_order_avg = mean(pick_order, na.rm = TRUE),
    
    # Summary for 'through 6'
    guards_through_6_avg = mean(guards_through_6, na.rm = TRUE),
    wings_through_6_avg = mean(wings_through_6, na.rm = TRUE),
    bigs_through_6_avg = mean(bigs_through_6, na.rm = TRUE),
    roster_build_through_6_avg = first(roster_build_through_6),
    
    # Summary for 'through 10'
    guards_through_10_avg = mean(guards_through_10, na.rm = TRUE),
    wings_through_10_avg = mean(wings_through_10, na.rm = TRUE),
    bigs_through_10_avg = mean(bigs_through_10, na.rm = TRUE),
    roster_build_through_10_avg = first(roster_build_through_10),
    
    # Summary for 'through 16'
    guards_through_16_avg = mean(guards_through_16, na.rm = TRUE),
    wings_through_16_avg = mean(wings_through_16, na.rm = TRUE),
    bigs_through_16_avg = mean(bigs_through_16, na.rm = TRUE),
    roster_build_through_16_avg = first(roster_build_through_16),
    
    # Playoff sprint score average
    playoff_sprint_score_avg = mean(playoff_sprint_weekly_score, na.rm = TRUE)
  )

# Step 1: Sort by playoff_sprint_weekly_score (descending) and draft_time (ascending for tie-breaking)
sprint_summary_df <- sprint_summary_df %>%
  arrange(desc(playoff_sprint_score_avg), draft_time)

# Step 2: Assign ranks
sprint_summary_df <- sprint_summary_df %>%
  mutate(rank = row_number())

# Step 3: Assign prize_value based on rank
sprint_summary_df <- sprint_summary_df %>%
  mutate(
    prize_value = case_when(
      rank == 1 ~ 100000,
      rank == 2 ~ 50000,
      rank == 3 ~ 25000,
      rank == 4 ~ 20000,
      rank == 5 ~ 16000,
      rank == 6 ~ 15000,
      rank == 7 ~ 14000,
      rank == 8 ~ 12000,
      rank == 9 ~ 10750,
      rank == 10 ~ 10000,
      rank >= 11 & rank <= 20 ~ 3000,
      rank >= 21 & rank <= 30 ~ 2000,
      rank >= 31 & rank <= 50 ~ 1000,
      rank >= 51 & rank <= 75 ~ 500,
      rank >= 76 & rank <= 100 ~ 200,
      rank >= 101 & rank <= 200 ~ 100,
      rank >= 201 & rank <= 300 ~ 75,
      rank >= 301 & rank <= 400 ~ 60,
      rank >= 401 & rank <= 500 ~ 50,
      rank >= 501 & rank <= 750 ~ 40,
      rank >= 751 & rank <= 1000 ~ 30,
      rank >= 1001 & rank <= 2000 ~ 25,
      rank >= 2001 & rank <= 3000 ~ 20,
      rank >= 3001 & rank <= 6250 ~ 15,
      TRUE ~ 0  # Default for ranks > 6250
    )
  )

# View the resulting dataframe
print(sprint_summary_df)

write_csv(sprint_summary_df, "sprint_summary_df 23-24.csv")
