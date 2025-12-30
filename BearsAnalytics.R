library(tidyverse)

comeback_wins <- c(4, 6, 9, 10, 11, 16) # weeks that the Bears had comeback wins (Raiders, Commanders, Bengals, Giants, Vikings--Wk11, Packers--Wk16)
one_pos_losses <- c(1, 14, 17) # weeks that the Bears lost one-possession games (failed comebacks: Vikings--Wk1, Packers--Wk14, 49ers--Wk17)

# Load data
gamefiles <- list.files("PlayByPlay", full.names = TRUE) # Play-by-play raw data files are in PlayByPlay/ folder  
gamelogs <- lapply(gamefiles, read_csv, col_select=c(qtr, fixed_drive, posteam, yrdln, down, ydstogo, play_type, yards_gained, touchdown, fumble_lost, interception, penalty, fd, complete_pass, cp, cpoe, ep, epa, success, air_yards, return_yards, passer_player_name, rusher_player_name, receiver_player_name, time, total_home_score, total_away_score, fixed_drive_result, series, series_result, series_success, vegas_home_wp, vegas_home_wpa, home_wp, desc, home_team, away_team, week)); rm(gamefiles)
seasonpbp <- bind_rows(gamelogs) %>% arrange(week); rm(gamelogs) # Data frame of every play in every Bears game for Weeks 1-17 (16 games)
# split_offense <- lapply(gamelogs, filter, posteam=="CHI", !is.na(down), play_type %in% c("pass","run", "qb_kneel", "qb_spike")) # Used for reconciliation

# Create separate play-by-play data frames for offense, defense, dropbacks, special teams
CHI_offense <- seasonpbp %>% filter(posteam=="CHI", !is.na(down), play_type %in% c("pass", "run", "qb_kneel", "qb_spike")) # !is.na(down) to exclude two-point attempts
CHI_defense <- seasonpbp %>% filter(posteam!="CHI", !is.na(posteam), !is.na(down), play_type %in% c("pass", "run", "qb_kneel", "qb_spike"))
CHI_dropbacks <- filter(CHI_offense, play_type %in% c("pass", "qb_spike") | str_detect(desc, "scrambles")) # Includes passes, sacks, spikes, scrambles
CHI_special <- seasonpbp %>% filter(play_type %in% c("kickoff","punt","field_goal","extra_point")) %>% 
  mutate(adj_epa = if_else(posteam=="CHI", epa, -1*epa)) # adj_epa is so positive EPA is good in all scenarios

# Function for finding the advanced stats for a given set of non-special teams plays. Argument: play-by-play data frame
offense_stats <- function(offense_pbp) {
  c(epa=mean(offense_pbp$epa),
       epa_pass=mean(filter(offense_pbp, play_type %in% c("pass", "qb_spike"))$epa),
       epa_run=mean(filter(offense_pbp, play_type %in% c("run", "qb_kneel"))$epa),
       epa_dropback=mean(filter(offense_pbp, play_type %in% c("pass", "qb_spike") | str_detect(desc, "scrambles"))$epa),
       success_rate=mean(offense_pbp$success),
       big_plays=nrow(filter(offense_pbp, epa > 0.90)),
       bad_plays=nrow(filter(offense_pbp, epa < -0.75)),
       big_play_rate=nrow(filter(offense_pbp, epa > 0.90))/nrow(offense_pbp),
       bad_play_rate=nrow(filter(offense_pbp, epa < -0.75))/nrow(offense_pbp),
       explosive_plays=nrow(filter(offense_pbp, (play_type=="pass" & yards_gained>=20) | (play_type=="run" & yards_gained>=10))),
       explosive_play_rate=nrow(filter(offense_pbp, (play_type=="pass" & yards_gained>=20) | (play_type=="run" & yards_gained>=10)))/nrow(offense_pbp),
       fumbles=sum(offense_pbp$fumble_lost),
       interceptions=sum(offense_pbp$interception),
       turnovers=sum(offense_pbp$fumble_lost) + sum(offense_pbp$interception),
       to_rate=mean(offense_pbp$fumble_lost)+mean(offense_pbp$interception),
       completion_percent=mean(filter(offense_pbp, play_type %in% c("pass", "qb_spike"), !str_detect(desc, "sack"))$complete_pass),
       completion_percent_oe=mean(offense_pbp$cpoe, na.rm=TRUE)/100,
       yac=mean(filter(offense_pbp, complete_pass==1)$yards_gained-filter(offense_pbp, complete_pass==1)$air_yards),
       air_yards_completion=mean(filter(offense_pbp, complete_pass==1)$air_yards))
}

# Function for finding advanced stats for a given set of special teams plays. Arguments: play-by-play data frame, EPA threshold for Big Play
special_teams_stats <- function(st_pbp, epa_thresh) {
  c(epa=mean(st_pbp$adj_epa),
      big_plays=nrow(filter(st_pbp, adj_epa > epa_thresh)),
      bad_plays=nrow(filter(st_pbp, adj_epa < -0.75)),
      big_play_rate=nrow(filter(st_pbp, adj_epa > epa_thresh))/nrow(st_pbp),
      bad_play_rate=nrow(filter(st_pbp, adj_epa < -0.75))/nrow(st_pbp))
}

# Season stats
offense <- offense_stats(CHI_offense)
defense <- offense_stats(CHI_defense) # Stats of opposing team offenses against CHI
dropbacks <- offense_stats(CHI_dropbacks)
special_teams <- special_teams_stats(CHI_special, 0.90)

# Stats for Bears comebacks fourth quarters (+ Wk 16 overtime, hereafter not mentioned)
cbq4_offense_pbp <- CHI_offense %>% filter(week %in% comeback_wins & qtr>=4)
cbq4_defense_pbp <- CHI_defense %>% filter(week %in% comeback_wins & qtr>=4)
cbq4_dropbacks_pbp <- CHI_dropbacks %>% filter(week %in% comeback_wins & qtr>=4)
cbq4_special_pbp <- CHI_special %>% filter(week %in% comeback_wins & qtr>=4)

cbq4_offense <- offense_stats(cbq4_offense_pbp)
cbq4_defense <- offense_stats(cbq4_defense_pbp)
cbq4_dropbacks <- offense_stats(cbq4_dropbacks_pbp)
cbq4_special_teams <- special_teams_stats(cbq4_special_pbp, epa_thresh=0.90)

# Stats for Bears fourth quarters not in comebacks
otherq4_offense_pbp <- CHI_offense %>% filter(!(week %in% comeback_wins) & qtr>=4)
otherq4_defense_pbp <- CHI_defense %>% filter(!(week %in% comeback_wins) & qtr>=4)
otherq4_dropbacks_pbp <- CHI_dropbacks %>% filter(!(week %in% comeback_wins) & qtr>=4)
otherq4_special_pbp <- CHI_special %>% filter(!(week %in% comeback_wins) & qtr>=4)

otherq4_offense <- offense_stats(otherq4_offense_pbp)
otherq4_defense <- offense_stats(otherq4_defense_pbp)
otherq4_dropbacks <- offense_stats(otherq4_dropbacks_pbp)
otherq4_special_teams <- special_teams_stats(otherq4_special_pbp, epa_thresh=0.90)

# Stats for Bears comebacks first three quarters
cbq13_offense_pbp <- CHI_offense %>% filter(week %in% comeback_wins & qtr<4)
cbq13_defense_pbp <- CHI_defense %>% filter(week %in% comeback_wins & qtr<4)
cbq13_dropbacks_pbp <- CHI_dropbacks %>% filter(week %in% comeback_wins & qtr<4)
cbq13_special_pbp <- CHI_special %>% filter(week %in% comeback_wins & qtr<4)

cbq13_offense <- offense_stats(cbq13_offense_pbp)
cbq13_defense <- offense_stats(cbq13_defense_pbp)
cbq13_dropbacks <- offense_stats(cbq13_dropbacks_pbp)
cbq13_special_teams <- special_teams_stats(cbq13_special_pbp, epa_thresh=0.90)

# Stats for Bears not comebacks first three quarters
otherq13_offense_pbp <- CHI_offense %>% filter(!(week %in% comeback_wins) & qtr<4)
otherq13_defense_pbp <- CHI_defense %>% filter(!(week %in% comeback_wins) & qtr<4)
otherq13_dropbacks_pbp <- CHI_dropbacks %>% filter(!(week %in% comeback_wins) & qtr<4)
otherq13_special_pbp <- CHI_special %>% filter(!(week %in% comeback_wins) & qtr<4)

otherq13_offense <- offense_stats(otherq13_offense_pbp)
otherq13_defense <- offense_stats(otherq13_defense_pbp)
otherq13_dropbacks <- offense_stats(otherq13_dropbacks_pbp)
otherq13_special_teams <- special_teams_stats(otherq13_special_pbp, epa_thresh=0.90)

# Stats for Bears season quarters one through three
q13_offense_pbp <- CHI_offense %>% filter(qtr<4)
q13_defense_pbp <- CHI_defense %>% filter(qtr<4)
q13_dropbacks_pbp <- CHI_dropbacks %>% filter(qtr<4)
q13_special_pbp <- CHI_special %>% filter(qtr<4)

q13_offense <- offense_stats(q13_offense_pbp)
q13_defense <- offense_stats(q13_defense_pbp)
q13_dropbacks <- offense_stats(q13_dropbacks_pbp)
q13_special_teams <- special_teams_stats(q13_special_pbp, epa_thresh=0.90)

# Stats for Bears season quarter four and overtime
q4_offense_pbp <- CHI_offense %>% filter(qtr>=4)
q4_defense_pbp <- CHI_defense %>% filter(qtr>=4)
q4_dropbacks_pbp <- CHI_dropbacks %>% filter(qtr>=4)
q4_special_pbp <- CHI_special %>% filter(qtr>=4)

q4_offense <- offense_stats(q4_offense_pbp)
q4_defense <- offense_stats(q4_defense_pbp)
q4_dropbacks <- offense_stats(q4_dropbacks_pbp)
q4_special_teams <- special_teams_stats(q4_special_pbp, epa_thresh=0.90)

# Stats for Bears one possession loss fourth quarters
oplq4_offense_pbp <- CHI_offense %>% filter(week %in% one_pos_losses & qtr>=4)
oplq4_defense_pbp <- CHI_defense %>% filter(week %in% one_pos_losses & qtr>=4)
oplq4_dropbacks_pbp <- CHI_dropbacks %>% filter(week %in% one_pos_losses & qtr>=4)
oplq4_special_pbp <- CHI_special %>% filter(week %in% one_pos_losses & qtr>=4)

oplq4_offense <- offense_stats(oplq4_offense_pbp)
oplq4_defense <- offense_stats(oplq4_defense_pbp)
oplq4_dropbacks <- offense_stats(oplq4_dropbacks_pbp)
oplq4_special_teams <- special_teams_stats(oplq4_special_pbp, epa_thresh = 0.90)

# Stats for Bears first three quarters successful and failed comebacks
opq13_offense_pbp <- CHI_offense %>% filter(week %in% c(one_pos_losses, comeback_wins), qtr<4)
opq13_defense_pbp <- CHI_defense %>% filter(week %in% c(one_pos_losses, comeback_wins), qtr<4)
opq13_dropbacks_pbp <- CHI_dropbacks %>% filter(week %in% c(one_pos_losses, comeback_wins), qtr<4)
opq13_special_pbp <- CHI_special %>% filter(week %in% c(one_pos_losses, comeback_wins), qtr<4)

opq13_offense <- offense_stats(opq13_offense_pbp)
opq13_defense <- offense_stats(opq13_defense_pbp)
opq13_dropbacks <- offense_stats(opq13_dropbacks_pbp)
opq13_special_teams <- special_teams_stats(opq13_special_pbp, epa_thresh = 0.90)

# Stats for Bears season excluding comeback fourth quarters
control_offense_pbp <- anti_join(CHI_offense, cbq4_offense_pbp)
control_defense_pbp <- anti_join(CHI_defense, cbq4_defense_pbp)
control_dropbacks_pbp <- anti_join(CHI_dropbacks, cbq4_dropbacks_pbp)
control_special_pbp <- anti_join(CHI_special, cbq4_special_pbp)

control_offense <- offense_stats(control_offense_pbp)
control_defense <- offense_stats(control_defense_pbp)
control_dropbacks <- offense_stats(control_dropbacks_pbp)
control_special_teams <- special_teams_stats(control_special_pbp, epa_thresh=0.90)

# Combining Results
offense_compare <- rbind(cbq4_offense, cbq13_offense, otherq4_offense, otherq13_offense, oplq4_offense, opq13_offense, q4_offense, q13_offense, offense, control_offense)
defense_compare <- rbind(cbq4_defense, cbq13_defense, otherq4_defense, otherq13_defense, oplq4_defense, opq13_defense, q4_defense, q13_defense, defense, control_defense)
dropbacks_compare <- rbind(cbq4_dropbacks, cbq13_dropbacks, otherq4_dropbacks, otherq13_dropbacks, oplq4_dropbacks, opq13_dropbacks, q4_dropbacks, q13_dropbacks, dropbacks, control_dropbacks)
special_teams_compare <- rbind(cbq4_special_teams, cbq13_special_teams, otherq4_special_teams, otherq13_special_teams, oplq4_special_teams, opq13_special_teams, q4_special_teams, q13_special_teams, special_teams, control_special_teams)

save(offense_compare, defense_compare, dropbacks_compare, special_teams_compare, file=("BearsCompare_Subset_Wk17.RData"))
