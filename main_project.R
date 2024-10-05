install.packages("hoopR")
install.packages("tictoc")
install.packages("dplyr")
install.packages("tidyverse")
install.packages("lubridate")
install.packages("glmnet")


library(dplyr)
library(tidyverse)
library(hoopR)
library(lubridate)
library(readr)


#load nba box score data and add UD scoring for 2023, 2022, and 2024 data and turn team_winner,did_not_play,ejected,starter binary
regular_season_player_box_scores_2022<-load_nba_player_box(seasons = 2022) %>% 
  filter(season_type==2)%>%
  filter(game_id!=401410564) %>% 
  mutate(ud_scoring=((blocks*3)+(steals*3)+(assists*1.5)+(points)+(rebounds*1.2)-(turnovers)),
         minutes = ifelse(minutes == 0, 1, minutes),
         ud_pts_pg=(ud_scoring/minutes))
regular_season_player_box_scores_2022$starter <- as.integer(as.logical(regular_season_player_box_scores_2022$starter))
regular_season_player_box_scores_2022$ejected <- as.integer(as.logical(regular_season_player_box_scores_2022$ejected))
regular_season_player_box_scores_2022$did_not_play <- as.integer(as.logical(regular_season_player_box_scores_2022$did_not_play))
regular_season_player_box_scores_2022$team_winner <- as.integer(as.logical(regular_season_player_box_scores_2022$team_winner))
regular_season_player_box_scores_2022$ud_scoring <- as.numeric(regular_season_player_box_scores_2022$ud_scoring)

regular_season_player_box_scores_2023<-load_nba_player_box(seasons = 2023) %>% 
  filter(season_type==2) %>% 
  filter(game_id!=401524696) %>% 
  mutate(ud_scoring=((blocks*3)+(steals*3)+(assists*1.5)+(points)+(rebounds*1.2)-(turnovers)),
         minutes = ifelse(minutes == 0, 1, minutes),
         ud_pts_pg=(ud_scoring/minutes))

regular_season_player_box_scores_2023$starter <- as.integer(as.logical(regular_season_player_box_scores_2023$starter))
regular_season_player_box_scores_2023$ejected <- as.integer(as.logical(regular_season_player_box_scores_2023$ejected))
regular_season_player_box_scores_2023$did_not_play <- as.integer(as.logical(regular_season_player_box_scores_2023$did_not_play))
regular_season_player_box_scores_2023$team_winner <- as.integer(as.logical(regular_season_player_box_scores_2023$team_winner))
regular_season_player_box_scores_2023$ud_scoring <- as.numeric(regular_season_player_box_scores_2023$ud_scoring)


regular_season_player_box_scores_2024<-load_nba_player_box(seasons = 2024) %>% 
  filter(season_type==2) %>% 
  filter(game_id!=401623259) %>% 
  mutate(ud_scoring=((blocks*3)+(steals*3)+(assists*1.5)+(points)+(rebounds*1.2)-(turnovers)))
regular_season_player_box_scores_2024$starter <- as.integer(as.logical(regular_season_player_box_scores_2024$starter))
regular_season_player_box_scores_2024$ejected <- as.integer(as.logical(regular_season_player_box_scores_2024$ejected))
regular_season_player_box_scores_2024$did_not_play <- as.integer(as.logical(regular_season_player_box_scores_2024$did_not_play))
regular_season_player_box_scores_2024$team_winner <- as.integer(as.logical(regular_season_player_box_scores_2024$team_winner))
regular_season_player_box_scores_2024$ud_scoring <- as.integer(regular_season_player_box_scores_2024$ud_scoring)

#Create a season average for all three seasons and add UD points metric
season_avg_2022<-regular_season_player_box_scores_2022 %>%
  filter(did_not_play==0) %>% 
  group_by(athlete_id,athlete_display_name) %>% 
  summarize(season=mean(season),
            udpts_pg=mean(ud_scoring),
            games_played=n(),
            games_started=sum(starter),
            udpts_pm=mean(ud_scoring/minutes),
            min_pg=mean(minutes),
            fg_made_pg=mean(field_goals_made),
            fg_att_pg=mean(field_goals_attempted),
            three_pfg_made_pg=mean(three_point_field_goals_made),
            three_att_pg=mean(three_point_field_goals_attempted),
            ft_made_pg=mean(free_throws_made),
            ft_att_pg=mean(free_throws_attempted),
            def_reb_pg=mean(defensive_rebounds),
            off_reb_pg=mean(offensive_rebounds),
            tot_reb_pg=mean(rebounds),
            ast_pg=mean(assists),
            stl_pg=mean(steals),
            blk_pg=mean(blocks),
            dp_pg=sum(blocks+steals),
            to_pg=mean(turnovers),
            fls_pg=mean(fouls),
            pts_pg=mean(points),
            team_pts_pg=mean(team_score),
            opp_pts_pg=mean(opponent_team_score))

season_avg_2023<-regular_season_player_box_scores_2023 %>%
  filter(did_not_play==0) %>% 
  group_by(athlete_id,athlete_display_name) %>% 
  summarize(season=mean(season),
            udpts_pg=mean(ud_scoring),
            games_played=n(),
            games_started=sum(starter),
            udpts_pm=mean(ud_scoring/minutes),
            min_pg=mean(minutes),
            fg_made_pg=mean(field_goals_made),
            fg_att_pg=mean(field_goals_attempted),
            three_pfg_made_pg=mean(three_point_field_goals_made),
            three_att_pg=mean(three_point_field_goals_attempted),
            ft_made_pg=mean(free_throws_made),
            ft_att_pg=mean(free_throws_attempted),
            def_reb_pg=mean(defensive_rebounds),
            off_reb_pg=mean(offensive_rebounds),
            tot_reb_pg=mean(rebounds),
            ast_pg=mean(assists),
            stl_pg=mean(steals),
            blk_pg=mean(blocks),
            dp_pg=sum(blocks+steals),
            to_pg=mean(turnovers),
            fls_pg=mean(fouls),
            pts_pg=mean(points),
            team_pts_pg=mean(team_score),
            opp_pts_pg=mean(opponent_team_score))


season_avg_2024<-regular_season_player_box_scores_2024 %>%
  filter(did_not_play==0) %>% 
  group_by(athlete_id,athlete_display_name) %>% 
  summarize(season=mean(season),
            udpts_pg=mean(ud_scoring),
            games_played=n(),
            games_started=sum(starter),
            udpts_pm=mean(ud_scoring/minutes),
            min_pg=mean(minutes),
            fg_made_pg=mean(field_goals_made),
            fg_att_pg=mean(field_goals_attempted),
            three_pfg_made_pg=mean(three_point_field_goals_made),
            three_att_pg=mean(three_point_field_goals_attempted),
            ft_made_pg=mean(free_throws_made),
            ft_att_pg=mean(free_throws_attempted),
            def_reb_pg=mean(defensive_rebounds),
            off_reb_pg=mean(offensive_rebounds),
            tot_reb_pg=mean(rebounds),
            ast_pg=mean(assists),
            stl_pg=mean(steals),
            blk_pg=mean(blocks),
            dp_pg=sum(blocks+steals),
            to_pg=mean(turnovers),
            fls_pg=mean(fouls),
            pts_pg=mean(points),
            team_pts_pg=mean(team_score),
            opp_pts_pg=mean(opponent_team_score))



#Load Underdog Positions
underdog_positions<-read_csv("underdog_positions.csv")
underdog_positions$athlete_display_name<-paste(underdog_positions$firstName,underdog_positions$lastName)


#create roster name changes for compatibility with hoopr
underdog_positions<-underdog_positions %>% 
  mutate(athlete_display_name=case_when(
    athlete_display_name=="Michael Foster"~"Michael Foster Jr.",
    athlete_display_name=="Josh Primo"~"Joshua Primo",
    athlete_display_name=="TyTy Washington"~"TyTy Washington Jr.",
    athlete_display_name=="Wendell Moore"~"Wendell Moore Jr.",
    athlete_display_name=="John Butler"~"John Butler Jr.",
    athlete_display_name=="Greg Brown"~"Greg Brown III",
    athlete_display_name=="Scotty Pippen"~"Scotty Pippen Jr.",
    athlete_display_name=="Vernon Carey"~"Vernon Carey Jr.",
    athlete_display_name=="Trey Murphy"~"Trey Murphy III",
    athlete_display_name=="A.J. Green"~"AJ Green",
    athlete_display_name=="Duane Washington"~"Duane Washington Jr.",
    athlete_display_name=="Vince Williams"~"Vince Williams Jr.",
    athlete_display_name=="Lu Dort"~"Luguentz Dort",
    athlete_display_name=="Kira Lewis"~"Kira Lewis Jr.",
    athlete_display_name=="McKinley Wright"~"McKinley Wright IV",
    athlete_display_name=="Kevin Knox"~"Kevin Knox II",
    athlete_display_name=="Lonnie Walker"~"Lonnie Walker IV",
    athlete_display_name=="Marvin Bagley"~"Marvin Bagley III",
    athlete_display_name=="Herb Jones"~"Herbert Jones",
    athlete_display_name=="Jeff Dowtin"~"Jeff Dowtin Jr.",
    athlete_display_name=="Robert Williams"~"Robert Williams III",
    athlete_display_name=="Moe Wagner"~"Moritz Wagner",
    athlete_display_name=="Cam Johnson"~"Cameron Johnson",
    athlete_display_name=="Kelly Oubre"~"Kelly Oubre Jr.",
    athlete_display_name=="Cam Payne"~"Cameron Payne",
    athlete_display_name=="Danuel House"~"Danuel House Jr.",
    athlete_display_name=="Otto Porter"~"Otto Porter Jr.",
    athlete_display_name=="Marcus Morris"~"Marcus Morris Sr.",
    athlete_display_name=="AJ Lawson"~"A.J. Lawson",
    athlete_display_name=="Jermaine Samuels"~"Jermaine Samuels Jr.",
    athlete_display_name=="Reggie Bullock"~"Reggie Bullock Jr.",
    athlete_display_name=="Craig Porter Jr."~"Craig Porter",
    athlete_display_name=="Ricky Council"~"Ricky Council IV",
    athlete_display_name=="PJ Washington"~"P.J. Washington",
    athlete_display_name=="Charlie Brown"~"Charlie Brown Jr.",
    athlete_display_name=="RJ Hampton"~"R.J. Hampton",
    athlete_display_name=="Kenyon Martin Jr."~"KJ Martin",
    TRUE~ athlete_display_name
  ))


#unhash below for checker of compatibility

underdog_pos <- underdog_positions %>% 
  select(athlete_display_name,slotName,teamName)
#check to make sure all names are correct for joining
reg_test<-regular_season_player_box_scores_2024 %>% 
  select(athlete_display_name,athlete_id)
udcheck<-reg_test %>% 
  left_join(underdog_pos, by=c("athlete_display_name"))



# Define a function to determine the regular season weeek
get_week2223 <- function(game_date) {
  # Convert date range strings to Date objects
  week1_start <- as.Date("2022-10-18")
  week1_end <- as.Date("2022-10-30")
  week2_start <- as.Date("2022-10-31")
  week2_end <- as.Date("2022-11-06")
  week3_start <- as.Date("2022-11-07")
  week3_end <- as.Date("2022-11-13")
  week4_start <- as.Date("2022-11-14")
  week4_end <- as.Date("2022-11-20")
  week5_start <- as.Date("2022-11-21")
  week5_end <- as.Date("2022-11-27")
  week6_start <- as.Date("2022-11-28")
  week6_end <- as.Date("2022-12-05")
  week7_start <- as.Date("2022-12-06")
  week7_end <- as.Date("2022-12-11")
  week8_start <- as.Date("2022-12-12")
  week8_end <- as.Date("2022-12-18")
  week9_start <- as.Date("2022-12-19")
  week9_end <- as.Date("2022-12-25")
  week10_start<- as.Date("2022-12-26")
  week10_end <- as.Date("2023-01-01")
  week11_start <- as.Date("2023-01-02")
  week11_end <- as.Date("2023-01-08")
  week12_start <- as.Date("2023-01-09")
  week12_end <- as.Date("2023-01-15")
  week13_start <- as.Date("2023-01-16")
  week13_end <- as.Date("2023-01-22")
  week14_start <- as.Date("2023-01-23")
  week14_end <- as.Date("2023-01-29")
  week15_start <- as.Date("2023-01-30")
  week15_end <- as.Date("2023-02-05")
  week16_start <- as.Date("2023-02-06")
  week16_end <- as.Date("2023-02-19")
  week17_start <- as.Date("2023-02-20")
  week17_end <- as.Date("2023-02-26")
  week18_start <- as.Date("2023-02-27")
  week18_end <- as.Date("2023-03-05")
  week19_start <- as.Date("2023-03-06")
  week19_end <- as.Date("2023-03-12")
  week20_start <- as.Date("2023-03-13")
  week20_end <- as.Date("2023-03-19")
  week21_start <- as.Date("2023-03-20")
  week21_end <- as.Date("2023-03-26")
  week22_start <- as.Date("2023-03-27")
  week22_end <- as.Date("2023-04-02")
  
  
  # Check the date against the specified date ranges
  if (game_date >= week1_start & game_date <= week1_end) {
    return("1")
  } else if (game_date >= week2_start & game_date <= week2_end) {
    return("2")
  } else if (game_date >= week3_start & game_date <= week3_end) {
    return("3")
  } else if (game_date >= week4_start & game_date <= week4_end) {
    return("4")
  } else if (game_date >= week5_start & game_date <= week5_end) {
    return("5")
  } else if (game_date >= week6_start & game_date <= week6_end) {
    return("6")
  } else if (game_date >= week7_start & game_date <= week7_end) {
    return("7")
  } else if (game_date >= week8_start & game_date <= week8_end) {
    return("8")
  } else if (game_date >= week9_start & game_date <= week9_end) {
    return("9")
  } else if (game_date >= week10_start & game_date <= week10_end) {
    return("10")
  } else if (game_date >= week11_start & game_date <= week11_end) {
    return("11")
  } else if (game_date >= week12_start & game_date <= week12_end) {
    return("12")
  } else if (game_date >= week13_start & game_date <= week13_end) {
    return("13")
  } else if (game_date >= week14_start & game_date <= week14_end) {
    return("14")
  } else if (game_date >= week15_start & game_date <= week15_end) {
    return("15")
  } else if (game_date >= week16_start & game_date <= week16_end) {
    return("16")
  } else if (game_date >= week17_start & game_date <= week17_end) {
    return("17")
  } else if (game_date >= week18_start & game_date <= week18_end) {
    return("18")
  } else if (game_date >= week19_start & game_date <= week19_end) {
    return("19")
  } else if (game_date >= week20_start & game_date <= week20_end) {
    return("20")
  } else if (game_date >= week21_start & game_date <= week21_end) {
    return("21")
  } else if (game_date >= week22_start & game_date <= week22_end) {
    return("22")      
    
    
  } else {
    return(NA)
  }
}

# Define a function to determine the regular season weeek
get_week2324 <- function(game_date) {
  # Convert date range strings to Date objects
  week1_start <- as.Date("2023-10-24")
  week1_end <- as.Date("2023-10-29")
  week2_start <- as.Date("2023-10-30")
  week2_end <- as.Date("2023-11-05")
  week3_start <- as.Date("2023-11-06")
  week3_end <- as.Date("2023-11-12")
  week4_start <- as.Date("2023-11-13")
  week4_end <- as.Date("2023-11-19")
  week5_start <- as.Date("2023-11-20")
  week5_end <- as.Date("2023-11-26")
  week6_start <- as.Date("2023-11-27")
  week6_end <- as.Date("2023-12-03")
  week7_start <- as.Date("2023-12-04")
  week7_end <- as.Date("2023-12-10")
  week8_start <- as.Date("2023-12-11")
  week8_end <- as.Date("2023-12-17")
  week9_start <- as.Date("2023-12-18")
  week9_end <- as.Date("2023-12-24")
  week10_start<- as.Date("2023-12-25")
  week10_end <- as.Date("2023-12-31")
  week11_start <- as.Date("2024-01-01")
  week11_end <- as.Date("2024-01-07")
  week12_start <- as.Date("2024-01-08")
  week12_end <- as.Date("2024-01-14")
  week13_start <- as.Date("2024-01-15")
  week13_end <- as.Date("2024-01-21")
  week14_start <- as.Date("2024-01-22")
  week14_end <- as.Date("2024-01-28")
  week15_start <- as.Date("2024-01-29")
  week15_end <- as.Date("2024-02-04")
  week16_start <- as.Date("2024-02-05")
  week16_end <- as.Date("2024-02-11")
  week17_start <- as.Date("2024-02-12")
  week17_end <- as.Date("2024-02-25")
  week18_start <- as.Date("2024-02-26")
  week18_end <- as.Date("2024-03-03")
  week19_start <- as.Date("2024-03-04")
  week19_end <- as.Date("2024-03-10")
  week20_start <- as.Date("2024-03-11")
  week20_end <- as.Date("2024-03-17")
  week21_start <- as.Date("2024-03-18")
  week21_end <- as.Date("2024-03-24")
  week22_start <- as.Date("2024-03-25")
  week22_end <- as.Date("2024-03-31")
  week23_start <- as.Date("2024-04-01")
  week23_end <- as.Date("2024-04-07")
  
  # Check the date against the specified date ranges
  if (game_date >= week1_start & game_date <= week1_end) {
    return("1")
  } else if (game_date >= week2_start & game_date <= week2_end) {
    return("2")
  } else if (game_date >= week3_start & game_date <= week3_end) {
    return("3")
  } else if (game_date >= week4_start & game_date <= week4_end) {
    return("4")
  } else if (game_date >= week5_start & game_date <= week5_end) {
    return("5")
  } else if (game_date >= week6_start & game_date <= week6_end) {
    return("6")
  } else if (game_date >= week7_start & game_date <= week7_end) {
    return("7")
  } else if (game_date >= week8_start & game_date <= week8_end) {
    return("8")
  } else if (game_date >= week9_start & game_date <= week9_end) {
    return("9")
  } else if (game_date >= week10_start & game_date <= week10_end) {
    return("10")
  } else if (game_date >= week11_start & game_date <= week11_end) {
    return("11")
  } else if (game_date >= week12_start & game_date <= week12_end) {
    return("12")
  } else if (game_date >= week13_start & game_date <= week13_end) {
    return("13")
  } else if (game_date >= week14_start & game_date <= week14_end) {
    return("14")
  } else if (game_date >= week15_start & game_date <= week15_end) {
    return("15")
  } else if (game_date >= week16_start & game_date <= week16_end) {
    return("16")
  } else if (game_date >= week17_start & game_date <= week17_end) {
    return("17")
  } else if (game_date >= week18_start & game_date <= week18_end) {
    return("18")
  } else if (game_date >= week19_start & game_date <= week19_end) {
    return("19")
  } else if (game_date >= week20_start & game_date <= week20_end) {
    return("20")
  } else if (game_date >= week21_start & game_date <= week21_end) {
    return("21")
  } else if (game_date >= week22_start & game_date <= week22_end) {
    return("22")      
  } else if (game_date >= week23_start & game_date <= week23_end) {
    return("23")      
    
  } else {
    return(NA)
  }
}
# Assuming your data frame is called regular_season_player_box_scores_2023 and has a column game_date with the date in format YYYY-MM-DD
regular_season_player_box_scores_2023 <- regular_season_player_box_scores_2023 %>%
  mutate(game_date = as.Date(game_date, format = "%Y-%m-%d")) %>%
  mutate(udweek = sapply(game_date, get_week2223))

regular_season_player_box_scores_2023$plus_minus<-as.numeric(regular_season_player_box_scores_2023$plus_minus)
regular_season_player_box_scores_2023$starter<-as.numeric(regular_season_player_box_scores_2023$starter)
regular_season_player_box_scores_2023$team_winner<-as.numeric(regular_season_player_box_scores_2023$team_winner)
regular_season_player_box_scores_2023$udweek<-as.numeric(regular_season_player_box_scores_2023$udweek)

weekly_summary2023<-regular_season_player_box_scores_2023 %>% 
  filter(udweek<23) %>%  
  group_by(udweek,athlete_id,athlete_display_name) %>% 
  summarize(
    games_played=n(),
    games_won=sum(team_winner),
    total_ud=sum(ud_scoring),
    total_min=sum(minutes),
    avg_min=mean(minutes),
    minimum_min=min(minutes),
    maximum_min=max(minutes),
    pts_average=mean(points),
    pts_minimum=min(points),
    pts_maximum=max(points),
    rbs_assts_minimum=min(rebounds+assists-turnovers),
    defense_minimum=min(steals+blocks),
    rbs_assts_maximum=max(rebounds+assists-turnovers),
    defense_maximum=max(steals+blocks),
    rbs_avg=mean(rebounds),
    assts_avg=mean(assists),
    stl_avg=mean(steals),
    blk_avg=mean(blocks),
    to_avg=mean(turnovers),
    plus_minus_tot=sum(plus_minus),
    tot_team_points=sum(team_score),
    avg_team_points=mean(team_score),
    total_opp_points=sum(opponent_team_score),
    avg_opp_points=mean(opponent_team_score),
  )
weekly_summary2023 <- weekly_summary2023 %>% filter(!is.na(total_ud))


weekly_summary2023<-weekly_summary2023 %>% 
  mutate(udptspg=total_ud/games_played,
         udptspm=total_ud/total_min) %>% 
  select(athlete_id,athlete_display_name,games_played,total_ud,udweek,udptspg,udptspm,total_min,avg_min,minimum_min,maximum_min,pts_average,pts_minimum,pts_maximum,
         rbs_avg,rbs_assts_minimum,rbs_assts_maximum,defense_minimum,defense_maximum,stl_avg,blk_avg,to_avg,plus_minus_tot,tot_team_points,games_won,
         avg_team_points,total_opp_points,avg_opp_points)

weekly_summary20232<-weekly_summary2023 %>% 
  left_join(underdog_pos,by=c("athlete_display_name")) %>% 
  mutate(position=slotName) %>% 
  select(-slotName)

# Assuming your data frame is called regular_season_player_box_scores_2023 and has a column game_date with the date in format YYYY-MM-DD
regular_season_player_box_scores_2024 <- regular_season_player_box_scores_2024 %>%
  mutate(game_date = as.Date(game_date, format = "%Y-%m-%d")) %>%
  mutate(udweek = sapply(game_date, get_week2324))

regular_season_player_box_scores_2024$plus_minus<-as.numeric(regular_season_player_box_scores_2024$plus_minus)
regular_season_player_box_scores_2024$starter<-as.numeric(regular_season_player_box_scores_2024$starter)
regular_season_player_box_scores_2024$team_winner<-as.numeric(regular_season_player_box_scores_2024$team_winner)
regular_season_player_box_scores_2024$udweek<-as.numeric(regular_season_player_box_scores_2024$udweek)

weekly_summary2024<-regular_season_player_box_scores_2024 %>% 
  filter(udweek<24) %>%  
  group_by(udweek,athlete_id,athlete_display_name) %>% 
  summarize(
    games_played=n(),
    games_won=sum(team_winner),
    total_ud=sum(ud_scoring),
    total_min=sum(minutes),
    avg_min=mean(minutes),
    minimum_min=min(minutes),
    maximum_min=max(minutes),
    pts_average=mean(points),
    pts_minimum=min(points),
    pts_maximum=max(points),
    rbs_assts_minimum=min(rebounds+assists-turnovers),
    defense_minimum=min(steals+blocks),
    rbs_assts_maximum=max(rebounds+assists-turnovers),
    defense_maximum=max(steals+blocks),
    rbs_avg=mean(rebounds),
    assts_avg=mean(assists),
    stl_avg=mean(steals),
    blk_avg=mean(blocks),
    to_avg=mean(turnovers),
    plus_minus_tot=sum(plus_minus),
    tot_team_points=sum(team_score),
    avg_team_points=mean(team_score),
    total_opp_points=sum(opponent_team_score),
    avg_opp_points=mean(opponent_team_score),
  )
weekly_summary2024 <- weekly_summary2024 %>% filter(!is.na(total_ud))


weekly_summary2024<-weekly_summary2024 %>% 
  mutate(udptspg=total_ud/games_played,
         udptspm=total_ud/total_min) %>% 
  select(athlete_id,athlete_display_name,games_played,total_ud,udweek,udptspg,udptspm,total_min,avg_min,minimum_min,maximum_min,pts_average,pts_minimum,pts_maximum,
         rbs_avg,rbs_assts_minimum,rbs_assts_maximum,defense_minimum,defense_maximum,stl_avg,blk_avg,to_avg,plus_minus_tot,tot_team_points,games_won,
         avg_team_points,total_opp_points,avg_opp_points)

weekly_summary20242<-weekly_summary2024 %>% 
  left_join(underdog_pos,by=c("athlete_display_name")) %>% 
  mutate(position=slotName) %>% 
  select(-slotName)

write_csv(weekly_summary20232,"weekly_summary_nba_22_23.csv")
write_csv(weekly_summary20242,"weekly_summary_nba_23_24.csv")

underdog_pos2 <- underdog_positions %>% 
  select(athlete_display_name,adp,slotName,teamName)
season_avg_2023_adp<-season_avg_2023 %>% 
  left_join(underdog_pos2,by=c("athlete_display_name"))
season_avg_2024_adp<-season_avg_2024 %>% 
  left_join(underdog_pos2,by=c("athlete_display_name"))
write_csv(season_avg_2024_adp,"season average 2023-24.csv")



# Calculate the baseline score for each position each week for 2023
baseline_scores2023 <- weekly_summary20232 %>%
  group_by(position,udweek) %>%
  arrange(desc(total_ud)) %>% 
  mutate(rank = row_number()) %>%
  select(position, rank, udweek,total_ud)

threshold_replacement2023<-baseline_scores2023 %>% 
  filter(
    (position=="B"&rank==17)|(position=="W"&rank==29)|(position=="G"&rank==29)
  ) %>% 
  mutate(replacement_thresh=total_ud) %>% 
  select(-total_ud,-rank)

threshold_good2023<-baseline_scores2023 %>% 
  filter(
    (position=="B"&rank==7)|(position=="W"&rank==13)|(position=="G"&rank==13)
  ) %>% 
  mutate(good_thresh=total_ud) %>% 
  select(-total_ud,-rank)

threshold_elite2023<-baseline_scores2023 %>% 
  filter(
    (position=="B"&rank==4)|(position=="W"&rank==7)|(position=="G"&rank==7)
  ) %>% 
  mutate(elite_thresh=total_ud) %>% 
  select(-total_ud,-rank)

weekly_summary20233 <- weekly_summary20232 %>%
  left_join(threshold_elite2023, by =c("position","udweek")) %>%
  left_join(threshold_good2023, by =c("position","udweek")) %>% 
  left_join(threshold_replacement2023, by =c("position","udweek")) %>% 
  mutate(
    BiB_elite = pmax(total_ud - elite_thresh, 0),
    BiB_good =pmax(total_ud - good_thresh, 0),
    BiB_replace =pmax(total_ud - replacement_thresh, 0))

weekly_summary20233 <- weekly_summary20233 %>%
  mutate(Elite_week = ifelse(BiB_elite > 0, 1, 0),
         Good_week = ifelse(BiB_good > 0, 1, 0),
         Replace_week = ifelse(BiB_replace > 0, 1, 0))

BiB_2023<-weekly_summary20233 %>% 
  group_by(athlete_id,athlete_display_name) %>%
  summarise(
    games_played=sum(games_played),
    elite_BiB_points=sum(BiB_elite),
    good_BiB_points=sum(BiB_good),
    replacement_BiB_points=sum(BiB_replace),
    elite_BiB_points_pg=sum(BiB_elite/games_played),
    good_BiB_points_pg=sum(BiB_good/games_played),
    replacement_BiB_points_pg=sum(BiB_replace/games_played),
    elite_BiB_weeks=sum(Elite_week),
    good_BiB_weeks=sum(Good_week),
    replacement_BiB_weeks=sum(Replace_week))
# Calculate the baseline score for each position each week for 2024
baseline_scores2024 <- weekly_summary20242 %>%
  group_by(position,udweek) %>%
  arrange(desc(total_ud)) %>% 
  mutate(rank = row_number()) %>%
  select(position, rank, udweek,total_ud)

threshold_replacement2024<-baseline_scores2024 %>% 
  filter(
    (position=="B"&rank==17)|(position=="W"&rank==29)|(position=="G"&rank==29)
  ) %>% 
  mutate(replacement_thresh=total_ud) %>% 
  select(-total_ud,-rank)

threshold_good2024<-baseline_scores2024 %>% 
  filter(
    (position=="B"&rank==7)|(position=="W"&rank==13)|(position=="G"&rank==13)
  ) %>% 
  mutate(good_thresh=total_ud) %>% 
  select(-total_ud,-rank)

threshold_elite2024<-baseline_scores2024 %>% 
  filter(
    (position=="B"&rank==4)|(position=="W"&rank==7)|(position=="G"&rank==7)
  ) %>% 
  mutate(elite_thresh=total_ud) %>% 
  select(-total_ud,-rank)

weekly_summary20243 <- weekly_summary20242 %>%
  left_join(threshold_elite2024, by =c("position","udweek")) %>%
  left_join(threshold_good2024, by =c("position","udweek")) %>% 
  left_join(threshold_replacement2024, by =c("position","udweek")) %>% 
  mutate(
    BiB_elite = pmax(total_ud - elite_thresh, 0),
    BiB_good =pmax(total_ud - good_thresh, 0),
    BiB_replace =pmax(total_ud - replacement_thresh, 0))

weekly_summary20243 <- weekly_summary20243 %>%
  mutate(Elite_week = ifelse(BiB_elite > 0, 1, 0),
         Good_week = ifelse(BiB_good > 0, 1, 0),
         Replace_week = ifelse(BiB_replace > 0, 1, 0))

BiB_2024<-weekly_summary20243 %>% 
  group_by(athlete_id,athlete_display_name) %>%
  summarise(
    games_played=sum(games_played),
    elite_BiB_points=sum(BiB_elite),
    good_BiB_points=sum(BiB_good),
    replacement_BiB_points=sum(BiB_replace),
    elite_BiB_points_pg=sum(BiB_elite/games_played),
    good_BiB_points_pg=sum(BiB_good/games_played),
    replacement_BiB_points_pg=sum(BiB_replace/games_played),
    elite_BiB_weeks=sum(Elite_week),
    good_BiB_weeks=sum(Good_week),
    replacement_BiB_weeks=sum(Replace_week))



BiB_2023_adp<-BiB_2023 %>% 
  left_join(underdog_pos2,by=c("athlete_display_name"))
BiB_2024_adp<-BiB_2024 %>% 
  left_join(underdog_pos2,by=c("athlete_display_name"))
#reorder
BiB_2023_adp<-BiB_2023_adp %>% 
  select(athlete_display_name,adp,slotName,teamName,games_played,elite_BiB_points,elite_BiB_points_pg,elite_BiB_weeks,
         good_BiB_points,good_BiB_points_pg,good_BiB_weeks,replacement_BiB_points,replacement_BiB_points_pg,replacement_BiB_weeks)
BiB_2024_adp<-BiB_2024_adp %>% 
  select(athlete_display_name,adp,slotName,teamName,games_played,elite_BiB_points,elite_BiB_points_pg,elite_BiB_weeks,
         good_BiB_points,good_BiB_points_pg,good_BiB_weeks,replacement_BiB_points,replacement_BiB_points_pg,replacement_BiB_weeks)


write_csv(BiB_2023_adp,"BiB points 2022-23.csv")

write_csv(BiB_2024_adp,"BiB points 2023-24.csv")

#college info
ncaab_box_raw<-load_mbb_player_box(most_recent_mbb_season())
  # Define the list of athlete names to filter by
  athlete_names <- c(
    "Reed Sheppard", "Stephon Castle", "Donovan Clingan", "Dalton Knecht", "Rob Dillingham",
    "Devin Carter", "Cody Williams", "Tristan da Silva", "Jared McCain", "Ja'Kobe Walter",
    "Isaiah Collier", "Johnny Furphy", "Kyle Filipowski", "Kel'el Ware", "Zach Edey",
    "Yves Missi", "Kyshawn George", "Justin Edwards", "Terrence Shannon Jr.", "Tyler Kolek",
    "DaRon Holmes II", "Jaylon Tyson", "Carlton Carrington", "KJ Simpson", "Ryan Dunn",
    "Baylor Scheierman", "Kevin McCullar Jr.", "Ajay Mitchell", "Oso Ighodaro", "Cam Christie",
    "Dillon Jones", "Adem Bona", "Pelle Larsson", "Jamal Shead", "Harrison Ingram",
    "Keshad Johnson", "Jalen Bridges"
  )

# Filter the dataset by athlete names and remove the athlete with athlete_id 5182651
ncaab_box_prospects <- ncaab_box_raw %>%
  filter(athlete_display_name %in% athlete_names) %>%
  filter(athlete_id != 5182651) %>% 
  mutate(ud_scoring=((blocks*3)+(steals*3)+(assists*1.5)+(points)+(rebounds*1.2)-(turnovers)),
         minutes = ifelse(minutes == 0, 1, minutes)) %>% 
  select(game_id,season,game_date,game_date_time,athlete_id,athlete_display_name,team_id,team_display_name,ud_scoring,minutes,field_goals_made,field_goals_attempted,
         three_point_field_goals_made,three_point_field_goals_attempted,free_throws_made,free_throws_attempted,offensive_rebounds,defensive_rebounds,rebounds,assists,
         steals,blocks,turnovers,fouls,points,starter,opponent_team_display_name)
#create an averages table
ncaab_box_prospects_averages<-ncaab_box_prospects %>% 
  filter(minutes>0) %>% 
  group_by(athlete_display_name) %>% 
  summarise(
    udfpts=mean(ud_scoring),
    minutes_per_game=mean(minutes),
    points_per_game=mean(points),
    assists_per_game=mean(assists),
    rebounds_per_game=mean(rebounds),
    steals_per_game=mean(steals),
    blocks_per_game=mean(blocks),
    turnovers_per_game=mean(turnovers),
    fpts_per_min=udfpts/minutes_per_game)
#upload int and gleague

#combine prospects

#upload UD Data for 23-24 season rounds 1 and 2
DD_Data_r1 <- read_csv("double_dribble_2024_r1_results_pick_by_pick.csv")
DD_Data_r2 <- read_csv("double_dribble_2024_r2_results_pick_by_pick.csv")
positions_23 <- read_csv("underdog.csv")
# Combine first and last names to create athlete_display_name in positions_23
positions_23 <- positions_23 %>%
  mutate(athlete_display_name = paste(firstName, lastName))

# Create a name correction mapping
name_corrections <- c(
  "Jabari Smith Jr." = "Jabari Smith",
  "Tim Hardaway Jr." = "Tim Hardaway",
  "Luguentz Dort" = "Lu Dort",
  "KJ Martin" = "Kenyon Martin",
  "Marvin Bagley III" = "Marvin Bagley",
  "PJ Washington" = "P.J. Washington",
  "Trey Murphy III" = "Trey Murphy",
  "OG Anunoby" = "O.G. Anunoby",
  "Michael Porter Jr." = "Michael Porter",
  "Dereck Lively II" = "Dereck Lively",
  "Kelly Oubre Jr." = "Kelly Oubre",
  "Kenneth Lofton Jr." = "Kenneth Lofton",
  "Sasha Vezenkov" = "Aleksandar Vezenkov",
  "Jaime Jaquez Jr." = "Jaime Jaquez",
  "Moritz Wagner" = "Moe Wagner",
  "Nick Smith Jr." = "Nick Smith",
  "RJ Hampton" = "R.J. Hampton",
  "Lonnie Walker IV" = "Lonnie Walker",
  "Kevin Knox II" = "Kevin Knox",
  "Patrick Baldwin Jr." = "Patrick Baldwin",
  "Marcus Morris Sr." = "Marcus Morris",
  "GG Jackson" = "Gregory Jackson",
  "Danuel House Jr." = "Danuel House"
)

# Apply name corrections to DD_Data_r1
DD_Data_r1 <- DD_Data_r1 %>%
  mutate(player_name = ifelse(player_name %in% names(name_corrections), name_corrections[player_name], player_name))

# Apply name corrections to DD_Data_r2
DD_Data_r2 <- DD_Data_r2 %>%
  mutate(player_name = ifelse(player_name %in% names(name_corrections), name_corrections[player_name], player_name))

# Merge the dataframes on player_name and athlete_display_name
DD_Data_r1_combined <- DD_Data_r1 %>%
  left_join(positions_23, by = c("player_name" = "athlete_display_name"))

# Find rows where the merge did not find a match and get unique names
unmatched_names_r1 <- DD_Data_r1_combined %>%
  filter(is.na(firstName)) %>%
  distinct(player_name)

# Display the unique unmatched names for Round 1
print(unmatched_names_r1) #should be na

#clean df
DD_Data_r1_combined<-DD_Data_r1_combined %>% 
  select(-draft_created_time,-draft_filled_time,draft_completed_time,-firstName,-lastName,position_name)

calculate_roster_and_stacking <- function(data, rounds) {
  # Filter data for the specified number of rounds
  data_filtered <- data %>%
    group_by(tournament_entry_id) %>%
    slice_head(n = rounds) %>%  # Select the first 'rounds' picks per team
    ungroup()
  
  # Calculate the number of guards, wings, and bigs
  roster_counts <- data_filtered %>%
    group_by(tournament_entry_id) %>%
    summarize(
      guards = sum(slotName == "G"),
      wings = sum(slotName == "W"),
      bigs = sum(slotName == "B")
    )
  
  # Determine the roster build type
  roster_counts <- roster_counts %>%
    mutate(
      roster_build = paste(guards, wings, bigs, sep = "-")
    )
  
  # Calculate stacking indicators
  stacking_indicators <- data_filtered %>%
    group_by(tournament_entry_id, teamName) %>%
    summarize(player_count = n(), .groups = 'drop') %>%
    group_by(tournament_entry_id) %>%
    summarize(
      pairs = sum(player_count == 2),
      triples = sum(player_count == 3),
      quads_or_more = sum(player_count >= 4),
    )
  
  # Combine roster counts and stacking indicators
  combined_data <- roster_counts %>%
    left_join(stacking_indicators, by = "tournament_entry_id")
  
  # Rename columns to reflect the number of rounds
  combined_data <- combined_data %>%
    rename_with(~paste0(.x, "_through_", rounds), -tournament_entry_id)
  
  return(combined_data)
}

# Calculate roster build and stacking indicators for 6, 10, and 16 rounds
roster_and_stacking_6 <- calculate_roster_and_stacking(DD_Data_r1_combined, 6)
roster_and_stacking_10 <- calculate_roster_and_stacking(DD_Data_r1_combined, 10)
roster_and_stacking_16v2 <- calculate_roster_and_stacking(DD_Data_r1_combined, 16)

#DETOUR FOR STACKING SUCCESS
# Identify tournament_entry_ids that made the playoffs from DD_Data_r2
playoff_entry_ids <- DD_Data_r2 %>%
  distinct(tournament_entry_id) %>%
  pull(tournament_entry_id)

roster_and_stacking_16v2 <- roster_and_stacking_16v2 %>% 
  mutate(made_playoffs = if_else(tournament_entry_id %in% playoff_entry_ids, 1, 0))

summary_roster_build <- roster_and_stacking_16v2 %>%
  group_by(roster_build_through_16) %>%
  summarise(
    times_drafted = n(),
    percent_drafted = n() / 54756,
    times_made_playoffs = sum(made_playoffs),
    percent_made_playoffs= (times_made_playoffs/times_drafted),
    percent_of_playoffs_builds = sum(made_playoffs) / 9400,
    .groups = 'drop'
  )
# Display the summary dataframe
print(summary_roster_build)

# Optionally, save the summary dataframe to a CSV file
write_csv(summary_roster_build, "summary_roster_build_draft_playoff_percentages.csv")

roster_and_stacking_16<-roster_and_stacking_16v2 %>% 
  select(-made_playoffs)


# Combine the results
combined_roster_and_stacking <- DD_Data_r1_combined %>%
  left_join(roster_and_stacking_6, by = "tournament_entry_id") %>%
  left_join(roster_and_stacking_10, by = "tournament_entry_id") %>%
  left_join(roster_and_stacking_16, by = "tournament_entry_id")


# Mark those entries as made_playoffs in the combined_roster_and_stacking dataframe
combined_roster_and_stacking <- combined_roster_and_stacking %>%
  mutate(made_playoffs = if_else(tournament_entry_id %in% playoff_entry_ids, 1, 0))

# Remove duplicates within a tournament entry id by keeping rows with non-NA teamName
deduplicated_data <- combined_roster_and_stacking %>%
  group_by(tournament_entry_id, player_name) %>%
  filter(!(is.na(teamName) & n() > 1)) %>%
  ungroup()

# Calculate the total number of drafted teams
total_teams <- deduplicated_data %>%
  distinct(tournament_entry_id) %>%
  nrow()

# Function to calculate the percentage of drafted teams and percentage of teams that made the playoffs
calculate_percentages <- function(data, group_var) {
  drafted_percentage <- data %>%
    group_by(across(all_of(group_var))) %>%
    summarize(
      teams_drafted = n_distinct(tournament_entry_id),
      percentage_drafted = (teams_drafted / total_teams) * 100,
      .groups = 'drop'
    ) %>%
    rename_with(~ paste0("teams_drafted_", group_var), teams_drafted) %>%
    rename_with(~ paste0("percentage_drafted_", group_var), percentage_drafted)
  
  playoffs_percentage <- data %>%
    filter(made_playoffs == 1) %>%
    group_by(across(all_of(group_var))) %>%
    summarize(
      teams_made_playoffs = n_distinct(tournament_entry_id),
      .groups = 'drop'
    ) %>%
    left_join(drafted_percentage, by = group_var) %>%
    mutate(
      percentage_made_playoffs = (teams_made_playoffs / get(paste0("teams_drafted_", group_var))) * 100
    ) %>%
    select(all_of(group_var), percentage_made_playoffs) %>%
    rename_with(~ paste0("percentage_made_playoffs_", group_var), percentage_made_playoffs)
  
  return(list(drafted_percentage, playoffs_percentage))
}

# Calculate percentages for player_name
player_percentages <- calculate_percentages(deduplicated_data, "player_name")

# Initialize combined_final with deduplicated_data
combined_final <- deduplicated_data

# Merge player_name percentages
combined_final <- combined_final %>%
  left_join(player_percentages[[1]], by = "player_name") %>%
  left_join(player_percentages[[2]], by = "player_name")

# List of other variables to calculate percentages for
variables <- colnames(deduplicated_data)[!colnames(deduplicated_data) %in% c("tournament_entry_id", "teamName", "player_name", "made_playoffs")]

# Loop through each variable and calculate percentages
for (var in variables) {
  var_percentages <- calculate_percentages(deduplicated_data, var)
  combined_final <- combined_final %>%
    left_join(var_percentages[[1]], by = var) %>%
    left_join(var_percentages[[2]], by = var)
}
combined_final_v2 <- combined_final %>%
  select(
    draft_id, user_id, username, percentage_drafted_user_id, percentage_made_playoffs_user_id, teams_drafted_user_id, draft_time, clock, draft_entry_id, tournament_entry_id, player_name, player_id, slotName, teams_drafted_player_name, percentage_drafted_player_name,percentage_made_playoffs_player_name, teamName,
    projection_adp, source, pick_order, percentage_made_playoffs_pick_order, overall_pick_number, team_pick_number, pick_created_time, pick_points, roster_points,
    made_playoffs, guards_through_6, wings_through_6, bigs_through_6, roster_build_through_6, pairs_through_6, triples_through_6, quads_or_more_through_6,
    guards_through_10, wings_through_10, bigs_through_10, roster_build_through_10, pairs_through_10, triples_through_10, quads_or_more_through_10,
    guards_through_16, wings_through_16, bigs_through_16, roster_build_through_16, pairs_through_16, triples_through_16, quads_or_more_through_16,
    teams_drafted_guards_through_6, percentage_drafted_guards_through_6, percentage_made_playoffs_guards_through_6,
    teams_drafted_wings_through_6, percentage_drafted_wings_through_6, percentage_made_playoffs_wings_through_6,
    teams_drafted_bigs_through_6, percentage_drafted_bigs_through_6, percentage_made_playoffs_bigs_through_6,
    teams_drafted_roster_build_through_6, percentage_drafted_roster_build_through_6, percentage_made_playoffs_roster_build_through_6,
    teams_drafted_pairs_through_6, percentage_drafted_pairs_through_6, percentage_made_playoffs_pairs_through_6,
    teams_drafted_triples_through_6, percentage_drafted_triples_through_6, percentage_made_playoffs_triples_through_6,
    teams_drafted_quads_or_more_through_6, percentage_drafted_quads_or_more_through_6, percentage_made_playoffs_quads_or_more_through_6,
    teams_drafted_guards_through_10, percentage_drafted_guards_through_10, percentage_made_playoffs_guards_through_10,
    teams_drafted_wings_through_10, percentage_drafted_wings_through_10, percentage_made_playoffs_wings_through_10,
    teams_drafted_bigs_through_10, percentage_drafted_bigs_through_10, percentage_made_playoffs_bigs_through_10,
    teams_drafted_roster_build_through_10, percentage_drafted_roster_build_through_10, percentage_made_playoffs_roster_build_through_10,
    teams_drafted_pairs_through_10, percentage_drafted_pairs_through_10, percentage_made_playoffs_pairs_through_10,
    teams_drafted_triples_through_10, percentage_drafted_triples_through_10, percentage_made_playoffs_triples_through_10,
    teams_drafted_quads_or_more_through_10, percentage_drafted_quads_or_more_through_10, percentage_made_playoffs_quads_or_more_through_10,
    teams_drafted_guards_through_16, percentage_drafted_guards_through_16, percentage_made_playoffs_guards_through_16,
    teams_drafted_wings_through_16, percentage_drafted_wings_through_16, percentage_made_playoffs_wings_through_16,
    teams_drafted_bigs_through_16, percentage_drafted_bigs_through_16, percentage_made_playoffs_bigs_through_16,
    teams_drafted_roster_build_through_16, percentage_drafted_roster_build_through_16, percentage_made_playoffs_roster_build_through_16,
    teams_drafted_pairs_through_16, percentage_drafted_pairs_through_16, percentage_made_playoffs_pairs_through_16,
    teams_drafted_triples_through_16, percentage_drafted_triples_through_16, percentage_made_playoffs_triples_through_16,
    teams_drafted_quads_or_more_through_16, percentage_drafted_quads_or_more_through_16, percentage_made_playoffs_quads_or_more_through_16
  )
calculate_success_index <- function(data, prefix) {
  data %>%
    mutate(!!paste0("success_index_", prefix) := 
             get(paste0("percentage_made_playoffs_", prefix)) / get(paste0("percentage_drafted_", prefix)))
}

# Apply the success index calculation to each relevant variable
combined_final_v2 <- combined_final_v2 %>%
  calculate_success_index("user_id") %>%
  calculate_success_index("player_name") %>%
  calculate_success_index("guards_through_6") %>%
  calculate_success_index("wings_through_6") %>%
  calculate_success_index("bigs_through_6") %>%
  calculate_success_index("roster_build_through_6") %>%
  calculate_success_index("pairs_through_6") %>%
  calculate_success_index("triples_through_6") %>%
  calculate_success_index("quads_or_more_through_6") %>%
  calculate_success_index("guards_through_10") %>%
  calculate_success_index("wings_through_10") %>%
  calculate_success_index("bigs_through_10") %>%
  calculate_success_index("roster_build_through_10") %>%
  calculate_success_index("pairs_through_10") %>%
  calculate_success_index("triples_through_10") %>%
  calculate_success_index("quads_or_more_through_10") %>%
  calculate_success_index("guards_through_16") %>%
  calculate_success_index("wings_through_16") %>%
  calculate_success_index("bigs_through_16") %>%
  calculate_success_index("roster_build_through_16") %>%
  calculate_success_index("pairs_through_16") %>%
  calculate_success_index("triples_through_16") %>%
  calculate_success_index("quads_or_more_through_16")

# Optionally, save the updated dataframe to a CSV file
write_csv(combined_final_v2, "combined_final_with_success_index.csv")

# Display the updated dataframe with success index
print(combined_final_v2)



# Group by tournament_entry_id and summarize each variable
summarized_data <- combined_final_v2 %>%
  group_by(tournament_entry_id) %>%
  summarise(
    draft_id = first(draft_id),
    user_id = first(user_id),
    username = first(username),
    percentage_drafted_user_id = first(percentage_drafted_user_id),
    percentage_made_playoffs_user_id = first(percentage_made_playoffs_user_id),
    teams_drafted_user_id = first(teams_drafted_user_id),
    draft_time = first(draft_time),
    clock = first(clock),
    draft_entry_id = first(draft_entry_id),
    teams_drafted_player_name = first(teams_drafted_player_name),
    percentage_drafted_player_name = first(percentage_drafted_player_name),
    projection_adp = first(projection_adp),
    pick_order = first(pick_order),
    percentage_made_playoffs_pick_order = first(percentage_made_playoffs_pick_order),
    overall_pick_number = first(overall_pick_number),
    team_pick_number = first(team_pick_number),
    pick_created_time = first(pick_created_time),
    pick_points = first(pick_points),
    roster_points = first(roster_points),
    made_playoffs = first(made_playoffs),
    guards_through_6 = first(guards_through_6),
    wings_through_6 = first(wings_through_6),
    bigs_through_6 = first(bigs_through_6),
    roster_build_through_6 = paste(unique(roster_build_through_6), collapse = ", "),
    pairs_through_6 = first(pairs_through_6),
    triples_through_6 = first(triples_through_6),
    quads_or_more_through_6 = first(quads_or_more_through_6),
    guards_through_10 = first(guards_through_10),
    wings_through_10 = first(wings_through_10),
    bigs_through_10 = first(bigs_through_10),
    roster_build_through_10 = paste(unique(roster_build_through_10), collapse = ", "),
    pairs_through_10 = first(pairs_through_10),
    triples_through_10 = first(triples_through_10),
    quads_or_more_through_10 = first(quads_or_more_through_10),
    guards_through_16 = first(guards_through_16),
    wings_through_16 = first(wings_through_16),
    bigs_through_16 = first(bigs_through_16),
    roster_build_through_16 = paste(unique(roster_build_through_16), collapse = ", "),
    pairs_through_16 = first(pairs_through_16),
    triples_through_16 = first(triples_through_16),
    quads_or_more_through_16 = first(quads_or_more_through_16),
    teams_drafted_guards_through_6 = first(teams_drafted_guards_through_6),
    teams_drafted_wings_through_6 = first(teams_drafted_wings_through_6),
    teams_drafted_bigs_through_6 = first(teams_drafted_bigs_through_6),
    teams_drafted_roster_build_through_6 = first(teams_drafted_roster_build_through_6),
    teams_drafted_pairs_through_6 = first(teams_drafted_pairs_through_6),
    teams_drafted_triples_through_6 = first(teams_drafted_triples_through_6),
    teams_drafted_quads_or_more_through_6 = first(teams_drafted_quads_or_more_through_6),
    teams_drafted_guards_through_10 = first(teams_drafted_guards_through_10),
    teams_drafted_wings_through_10 = first(teams_drafted_wings_through_10),
    teams_drafted_bigs_through_10 = first(teams_drafted_bigs_through_10),
    teams_drafted_roster_build_through_10 = first(teams_drafted_roster_build_through_10),
    teams_drafted_pairs_through_10 = first(teams_drafted_pairs_through_10),
    teams_drafted_triples_through_10 = first(teams_drafted_triples_through_10),
    teams_drafted_quads_or_more_through_10 = first(teams_drafted_quads_or_more_through_10),
    teams_drafted_guards_through_16 = first(teams_drafted_guards_through_16),
    teams_drafted_wings_through_16 = first(teams_drafted_wings_through_16),
    teams_drafted_bigs_through_16 = first(teams_drafted_bigs_through_16),
    teams_drafted_roster_build_through_16 = first(teams_drafted_roster_build_through_16),
    teams_drafted_pairs_through_16 = first(teams_drafted_pairs_through_16),
    teams_drafted_triples_through_16 = first(teams_drafted_triples_through_16),
    teams_drafted_quads_or_more_through_16 = first(teams_drafted_quads_or_more_through_16),
    success_index_guards_through_6 = first(success_index_guards_through_6),
    success_index_wings_through_6 = first(success_index_wings_through_6),
    success_index_bigs_through_6 = first(success_index_bigs_through_6),
    success_index_roster_build_through_6 = first(success_index_roster_build_through_6),
    success_index_pairs_through_6 = first(success_index_pairs_through_6),
    success_index_triples_through_6 = first(success_index_triples_through_6),
    success_index_quads_or_more_through_6 = first(success_index_quads_or_more_through_6),
    success_index_guards_through_10 = first(success_index_guards_through_10),
    success_index_wings_through_10 = first(success_index_wings_through_10),
    success_index_bigs_through_10 = first(success_index_bigs_through_10),
    success_index_roster_build_through_10 = first(success_index_roster_build_through_10),
    success_index_pairs_through_10 = first(success_index_pairs_through_10),
    success_index_triples_through_10 = first(success_index_triples_through_10),
    success_index_quads_or_more_through_10 = first(success_index_quads_or_more_through_10),
    success_index_guards_through_16 = first(success_index_guards_through_16),
    success_index_wings_through_16 = first(success_index_wings_through_16),
    success_index_bigs_through_16 = first(success_index_bigs_through_16),
    success_index_roster_build_through_16 = first(success_index_roster_build_through_16),
    success_index_pairs_through_16 = first(success_index_pairs_through_16),
    success_index_triples_through_16 = first(success_index_triples_through_16),
    success_index_quads_or_more_through_16 = first(success_index_quads_or_more_through_16)
  )

# Display the summarized data
print(summarized_data)

write_csv(summarized_data, "summarized_combined_final_v2.csv") #delete duplicates of TEID to get each build

#use summarized_data to slice team data

# Create the player_advancement_df
player_advancement_df <- combined_roster_and_stacking %>%
  group_by(player_name) %>%
  summarise(
    times_drafted = n(),
    times_made_playoffs = sum(made_playoffs),
    percent_drafted = n() / 4563,
    percent_made_playoffs = times_made_playoffs/times_drafted,
    .groups = 'drop'
  )

# Display the player_advancement_df
print(player_advancement_df)

# Optionally, save the player_advancement_df to a CSV file
write_csv(player_advancement_df, "player_advancement_df.csv")

# Define a function to get the top drafted player and their percentage
get_top_player_info <- function(df, position, total_drafts) {
  top_player <- df %>% 
    filter(slotName == position) %>% 
    count(player_name) %>% 
    arrange(desc(n)) %>% 
    slice(1)
  
  if (nrow(top_player) == 0) {
    return(c(NA, 0))
  } else {
    top_player_name <- top_player$player_name[1]
    top_player_percent <- top_player$n[1] / total_drafts * 100
    return(c(top_player_name, top_player_percent))
  }
}

# Create a summary dataframe for each user
user_summary_list <- combined_roster_and_stacking %>%
  group_by(username) %>%
  group_map(~{
    user_data <- .x
    total_drafts <- n_distinct(user_data$tournament_entry_id)
    most_common_pick_slot <- as.numeric(names(sort(table(user_data$pick_order), decreasing = TRUE)[1]))
    max_roster_points <- max(user_data$roster_points, na.rm = TRUE)
    avg_roster_points <- mean(user_data$roster_points, na.rm = TRUE)
    made_playoff_percent <- mean(user_data$made_playoffs) * 100
    most_common_build_through_6 <- names(sort(table(user_data$roster_build_through_6), decreasing = TRUE)[1])
    most_common_build_through_10 <- names(sort(table(user_data$roster_build_through_10), decreasing = TRUE)[1])
    most_common_build_through_16 <- names(sort(table(user_data$roster_build_through_16), decreasing = TRUE)[1])
    
    top_G_info <- get_top_player_info(user_data, "G", total_drafts)
    top_W_info <- get_top_player_info(user_data, "W", total_drafts)
    top_B_info <- get_top_player_info(user_data, "B", total_drafts)
    
    tibble(
      username = .y$username,
      total_drafts = total_drafts,
      most_common_pick_slot = most_common_pick_slot,
      max_roster_points = max_roster_points,
      avg_roster_points = avg_roster_points,
      made_playoff_percent = made_playoff_percent,
      most_common_build_through_6 = most_common_build_through_6,
      most_common_build_through_10 = most_common_build_through_10,
      most_common_build_through_16 = most_common_build_through_16,
      top_G_drafted = top_G_info[1],
      top_G_percent = as.numeric(top_G_info[2]),
      top_W_drafted = top_W_info[1],
      top_W_percent = as.numeric(top_W_info[2]),
      top_B_drafted = top_B_info[1],
      top_B_percent = as.numeric(top_B_info[2])
    )
  })

# Combine the list of tibbles into a single dataframe
user_summary_df <- bind_rows(user_summary_list)

# Display the user summary dataframe
print(user_summary_df)

# Optionally, save the user summary dataframe to a CSV file
write_csv(user_summary_df, "user_summary_df.csv")

#combinitorial ownership
generate_combinations <- function(players) {
  player_combinations <- unlist(lapply(1:length(players), function(m) combn(players, m, simplify = FALSE)), recursive = FALSE)
  return(player_combinations)
}

# Generate combinations for each draft and summarize
#combinatorial_ownership <- DD_Data_r1_combined %>%
  group_by(tournament_entry_id) %>%
  summarise(player_combinations = list(generate_combinations(player_name))) %>%
  unnest(player_combinations) %>%
  mutate(player_combination_str = sapply(player_combinations, paste, collapse = ", ")) %>%
  count(player_combination_str, sort = TRUE) %>%
  rename(combination = player_combination_str, times_drafted = n)

# Display the combinatorial ownership dataframe
#print(combinatorial_ownership)
  
# Create playoff_info dataframe
  playoff_info <- roster_and_stacking_16v2 %>%
    select(tournament_entry_id, made_playoffs) %>%
    distinct()

  
  # Ensure combined_data is available
  combined_data <- DD_Data_r1_combined %>%
    select(-contains("made_playoffs")) %>%  # Remove old made_playoffs column if it exists
    left_join(playoff_info, by = "tournament_entry_id") %>%
    left_join(BiB_2024_adp, by = c("player_name" = "athlete_display_name")) %>%
    filter(tournament_entry_id != "f92cfd2d-a2f0-4e89-8157-66709426a5fe")
  
  # Select relevant columns for BiB points and summarize top 10 within each tournament_entry_id
  top_10_summary_by_team <- combined_data %>%
    group_by(tournament_entry_id) %>%
    arrange(desc(elite_BiB_points)) %>%
    slice(1:10) %>%
    summarize(
      total_elite_BiB_points = sum(elite_BiB_points, na.rm = TRUE),
      total_good_BiB_points = sum(good_BiB_points, na.rm = TRUE),
      total_replacement_BiB_points = sum(replacement_BiB_points, na.rm = TRUE),
      total_elite_BiB_points_pg = sum(elite_BiB_points_pg, na.rm = TRUE),
      total_good_BiB_points_pg = sum(good_BiB_points_pg, na.rm = TRUE),
      total_replacement_BiB_points_pg = sum(replacement_BiB_points_pg, na.rm = TRUE),
      total_elite_BiB_weeks = sum(elite_BiB_weeks, na.rm = TRUE),
      total_good_BiB_weeks = sum(good_BiB_weeks, na.rm = TRUE),
      total_replacement_BiB_weeks = sum(replacement_BiB_weeks, na.rm = TRUE),
      total_roster_points = sum(roster_points, na.rm = TRUE),
      total_made_playoffs = sum(made_playoffs, na.rm = TRUE),
      .groups = 'drop'  # Ensure ungrouping after summarizing
    )
  
  # Standardize the data
  top_10_summary_standardized <- top_10_summary_by_team %>%
    mutate(across(starts_with("total"), scale))  

