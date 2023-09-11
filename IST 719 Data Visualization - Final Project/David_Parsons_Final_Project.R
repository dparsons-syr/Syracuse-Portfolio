#
# Author David Parsons
# Purpose: IST 719 Final Project
# FIFA World Cup Analysis
#
#
# Data Sources
#
# https://www.kaggle.com/datasets/jahaidulislam/fifa-world-cup-1930-2022-all-match-dataset
#
# https://www.kaggle.com/datasets/jahaidulislam/fifa-world-cup-all-goals-1930-2022-dataset
#

library(tidyverse)
library(ggplot2)
library(treemap)
library(rnaturalearth)
library(sp)
library(rgdal)
library(RColorBrewer)
library(dplyr)

setwd("/Users/david/Documents/Grad School/IST 719/Final_Project")

world_cup_matches <- read.csv(file = 'FIFA World Cup 1930-2022 All Match Dataset.csv'
                  , header = TRUE
                  , stringsAsFactors = FALSE
                  , fileEncoding = "latin1")

world_cup_goals <- read.csv(file = 'FIFA World Cup All Goals 1930-2022.csv'
                  , header = TRUE
                  , stringsAsFactors = FALSE
                  , fileEncoding = 'latin1')

#view(world_cup_matches)
#view(world_cup_goals)



# Main Plot
# Map of world colored by amount of goals scored




# Plot 1
# Goals scored by World Cup

goals_1 <- world_cup_goals

goals_1$goal_count <- 1

goals_by_world_cup <- aggregate(goals_1$goal_count, list(goals_1$tournament_name), sum)

plot_1_colors <- colorRampPalette(c('darkseagreen1', 'darkgreen'))

plot_1_colors <- plot_1_colors(length(goals_by_world_cup$Group.1))

ggplot(goals_by_world_cup) +
  aes(x = Group.1, y = x, fill = Group.1) +
  geom_bar(stat = 'identity') +
  theme_minimal() +
  theme(legend.position = 'none') +
  coord_flip() +
  xlab('World Cup') +
  ylab('Goals') +
  ggtitle('Total Goals Scored by World Cup Tournament') +
  scale_fill_manual(values = plot_1_colors) 
  


# Plot 2
# Goals by minute of game

goals_2 <- world_cup_goals

ggplot(goals_2) + 
  aes(x = minute_regulation) +
  geom_histogram(bins = 120, color = 'darkgreen', fill = 'darkseagreen1') +
  xlim(0, 120) +
  theme_minimal() +
  xlab('Minute of Game') +
  ylab('Goals') +
  ggtitle('Time of Goals Scored') 




# Plot 3
# Wins by Country

match_3 <- world_cup_matches

match_3$winner <- ifelse(match_3$Home.Team.Score > match_3$Away.Team.Score, match_3$Home.Team.Name, match_3$Away.Team.Name)
match_3$winner <- ifelse(match_3$Home.Team.Score == match_3$Away.Team.Score, 'Draw', match_3$winner)

match_3$win_count <- 1

match_3$winner[which(match_3$winner == 'United States')] <- 'USA'
match_3$winner[which(match_3$winner == "West Germany")] <- 'Germany'
match_3$winner[which(match_3$winner == "East Germany")] <- 'Germany'
match_3$winner[which(match_3$winner == "Soviet Union")] <- 'Russia'
match_3$winner[which(match_3$winner == "Czechoslovakia")] <- 'Czech Republic'

#view(match_3)

treemap(match_3, index = c("winner")
        , vSize = "win_count"
        , vColor = ""
        , fontsize.labels =  12
        , palette = "YlGn")




# Plot 4
# Stage scoring breakdown

match_4 <- world_cup_matches

match_4$total_goals_in_game <- match_4$Home.Team.Score + match_4$Away.Team.Score

#view(match_4)

stage_scoring2 <- tapply(match_4$total_goals_in_game, list(match_4$tournament.Name, match_4$Stage.Name), mean)

drops <- c('final round', 'second group stage')

stage_scoring2 <- stage_scoring2[ , colnames(stage_scoring2) != 'final round']
stage_scoring2 <- stage_scoring2[ , colnames(stage_scoring2) != 'second group stage']

#view(stage_scoring2)

col_order <- c('final', 'semi-finals', 'third-place match', 'quarter-finals', 'round of 16', 'group stage')  

stage_scoring2 <- stage_scoring2[, col_order]

colnames(stage_scoring2) <- c('Final', 'Semi-finals', 'Third-place Match', 'Quarter-Finals', 'Round of 16', 'Group Stage')

boxplot(stage_scoring2
        , main = 'Scoring by Round'
        , xlab = 'Round'
        , ylab = 'Avg. Goals'
        , col = c('gold', 'indianred', 'antiquewhite2', 'cadetblue', 'snow3', 'darkgoldenrod'))




# Plot 5
# Top Scoring Players

goals_5 <- world_cup_goals

goals_5$full_name <- if_else(goals_5$given_name != "not applicable", paste(goals_5$given_name, goals_5$family_name, sep = " "), paste(goals_5$family_name))

goals_5$goal_count <- 1

goals_by_player <- aggregate(goals_5$goal_count, list(goals_5$player_id, goals_5$full_name, goals_5$team_code), sum)

goals_by_player <- goals_by_player[order(goals_by_player$x, decreasing = T), ]

#view(goals_by_player)

goals_by_player_top_20 <- head(goals_by_player, 20)



colnames(goals_by_player_top_20) <- c('PlayedID', 'Name', 'Nation', 'Goals')

plot_5_col_vec <- rep(rgb(30, 144, 255, maxColorValue = 255), nrow(goals_by_player_top_20))

plot_5_col_vec[goals_by_player_top_20$Nation == 'BRA'] <- rgb(0, 151, 57, maxColorValue = 255)
plot_5_col_vec[goals_by_player_top_20$Nation == 'DEU'] <- rgb(0, 0, 0, maxColorValue = 255)
plot_5_col_vec[goals_by_player_top_20$Nation == 'ARG'] <- rgb(8, 172, 228, maxColorValue = 255)
plot_5_col_vec[goals_by_player_top_20$Nation == 'FRA'] <- rgb(0, 38, 84, maxColorValue = 255)
plot_5_col_vec[goals_by_player_top_20$Nation == 'HUN'] <- rgb(71, 112, 80, maxColorValue = 255)
plot_5_col_vec[goals_by_player_top_20$Nation == 'ENG'] <- rgb(206, 17, 36, maxColorValue = 255)
plot_5_col_vec[goals_by_player_top_20$Nation == 'PER'] <- rgb(200, 16, 46, maxColorValue = 255)
plot_5_col_vec[goals_by_player_top_20$Nation == 'POL'] <- rgb(220, 20, 60, maxColorValue = 255)

#view(goals_by_player_top_20)

goals_by_player_top_20 <- goals_by_player_top_20[order(-goals_by_player_top_20$Goals), ]

ggplot(goals_by_player_top_20) +
  aes(x = reorder(Name, Goals), y = Goals) +
  geom_bar(stat = 'identity', fill = plot_5_col_vec) +
  #theme(legend.position = 'none') +
  coord_flip() +
  xlab('Player') +
  ylab('Goals') +
  ggtitle('Top World Cup Scorers') +
  scale_fill_manual(values = plot_5_col_vec) +
  theme_minimal()


# Map Plot
# Plot of Map colored by goals scored

world_coord <- map_data('world')

matches_map <- world_cup_matches

matches_map$winner <- ifelse(matches_map$Home.Team.Score > matches_map$Away.Team.Score, matches_map$Home.Team.Name, matches_map$Away.Team.Name)
matches_map$winner <- ifelse(matches_map$Home.Team.Score == matches_map$Away.Team.Score, 'Draw', matches_map$winner)

matches_map$win_count <- 1

matches_map$winner[which(matches_map$winner == 'United States')] <- 'USA'
matches_map$winner[which(matches_map$winner == "West Germany")] <- 'Germany'
matches_map$winner[which(matches_map$winner == "East Germany")] <- 'Germany'
matches_map$winner[which(matches_map$winner == "Soviet Union")] <- 'Russia'
matches_map$winner[which(matches_map$winner == "Czechoslovakia")] <- 'Czech Republic'

#view(matches_map)

wins_map <- fortify(world_coord, region = "region")

country_wins <- aggregate(matches_map$win_count, list(matches_map$winner), sum)
colnames(country_wins) <- c("region", "wins")

country_wins[order(country_wins$wins),]

wins_map_merged <- left_join(x = wins_map, y = country_wins, by = "region")

#view(wins_map_merged)
#view(country_wins)

ggplot() + 
  geom_map(data = wins_map_merged, map = wins_map_merged, colour = 'black') +
  aes(x = long, y = lat, map_id = region, group = region, fill = wins) +
  ggtitle("Wins by Nation") +
  scale_fill_distiller(palette = 'Greens', direction = 1, na.value = 'lightgrey') +
  theme_classic()


# Games Played Hist

country_matches <- world_cup_matches

country_matches$Home.Team.Name[which(country_matches$Home.Team.Name == 'United States')] <- 'USA'
country_matches$Home.Team.Name[which(country_matches$Home.Team.Name == "West Germany")] <- 'Germany'
country_matches$Home.Team.Name[which(country_matches$Home.Team.Name == "East Germany")] <- 'Germany'
country_matches$Home.Team.Name[which(country_matches$Home.Team.Name == "Soviet Union")] <- 'Russia'
country_matches$Home.Team.Name[which(country_matches$Home.Team.Name == "Czechoslovakia")] <- 'Czech Republic'

country_matches$Away.Team.Name[which(country_matches$Away.Team.Name == 'United States')] <- 'USA'
country_matches$Away.Team.Name[which(country_matches$Away.Team.Name == "West Germany")] <- 'Germany'
country_matches$Away.Team.Name[which(country_matches$Away.Team.Name == "East Germany")] <- 'Germany'
country_matches$Away.Team.Name[which(country_matches$Away.Team.Name == "Soviet Union")] <- 'Russia'
country_matches$Away.Team.Name[which(country_matches$Away.Team.Name == "Czechoslovakia")] <- 'Czech Republic'

home_team_matches <- country_matches
away_team_matches <- country_matches

matches_home <- home_team_matches %>% count(Home.Team.Name)
matches_away <- away_team_matches %>% count(Away.Team.Name)

matches_total <- merge(matches_home, matches_away, by.x = 'Home.Team.Name', by.y = 'Away.Team.Name')
matches_total$total_matches <- matches_total$n.x + matches_total$n.y

ggplot(matches_total) +
  aes(x = total_matches) +
  geom_histogram(bins = 15, color = 'darkseagreen1', fill = 'darkgreen') +
  theme_minimal() +
  ggtitle('Distribution of Matches Played by Country')

# Country Wins and Goals Scatter

wins_goals <- world_cup_matches

wins_goals$Home.Team.Name[which(wins_goals$Home.Team.Name == 'United States')] <- 'USA'
wins_goals$Home.Team.Name[which(wins_goals$Home.Team.Name == "West Germany")] <- 'Germany'
wins_goals$Home.Team.Name[which(wins_goals$Home.Team.Name == "East Germany")] <- 'Germany'
wins_goals$Home.Team.Name[which(wins_goals$Home.Team.Name == "Soviet Union")] <- 'Russia'
wins_goals$Home.Team.Name[which(wins_goals$Home.Team.Name == "Czechoslovakia")] <- 'Czech Republic'

wins_goals$Away.Team.Name[which(wins_goals$Away.Team.Name == 'United States')] <- 'USA'
wins_goals$Away.Team.Name[which(wins_goals$Away.Team.Name == "West Germany")] <- 'Germany'
wins_goals$Away.Team.Name[which(wins_goals$Away.Team.Name == "East Germany")] <- 'Germany'
wins_goals$Away.Team.Name[which(wins_goals$Away.Team.Name == "Soviet Union")] <- 'Russia'
wins_goals$Away.Team.Name[which(wins_goals$Away.Team.Name == "Czechoslovakia")] <- 'Czech Republic'

home_goals <- aggregate(wins_goals$Home.Team.Score, list(wins_goals$Home.Team.Name), sum)
away_goals <- aggregate(wins_goals$Away.Team.Score, list(wins_goals$Away.Team.Name), sum)

goals_totals <- merge(home_goals, away_goals, by = 'Group.1')

goals_totals$total_goals <- goals_totals$x.x + goals_totals$x.y

wins_goals_merged <- merge(goals_totals, country_wins, by.x = 'Group.1', by.y = 'region')

wins_goals_merged <- merge(wins_goals_merged, matches_total, by.x = 'Group.1', by.y = 'Home.Team.Name')

wins_goals_merged$champion <- 0

wins_goals_merged$champion[which(wins_goals_merged$Group.1 == 'Uruguay')] <- 1
wins_goals_merged$champion[which(wins_goals_merged$Group.1 == 'Italy')] <- 1
wins_goals_merged$champion[which(wins_goals_merged$Group.1 == 'Germany')] <- 1
wins_goals_merged$champion[which(wins_goals_merged$Group.1 == 'Brazil')] <- 1
wins_goals_merged$champion[which(wins_goals_merged$Group.1 == 'England')] <- 1
wins_goals_merged$champion[which(wins_goals_merged$Group.1 == 'Argentina')] <- 1
wins_goals_merged$champion[which(wins_goals_merged$Group.1 == 'France')] <- 1
wins_goals_merged$champion[which(wins_goals_merged$Group.1 == 'Spain')] <- 1

wins_col_vec <- rep(rgb(115, 181, 91, maxColorValue = 255), nrow(wins_goals_merged))

wins_col_vec[wins_goals_merged$Group.1 == 'Uruguay'] <- rgb(14, 82, 67, maxColorValue = 255)
wins_col_vec[wins_goals_merged$Group.1 == 'Italy'] <- rgb(14, 82, 67, maxColorValue = 255)
wins_col_vec[wins_goals_merged$Group.1 == 'Germany'] <- rgb(14, 82, 67, maxColorValue = 255)
wins_col_vec[wins_goals_merged$Group.1 == 'Brazil'] <- rgb(14, 82, 67, maxColorValue = 255)
wins_col_vec[wins_goals_merged$Group.1 == 'England'] <- rgb(14, 82, 67, maxColorValue = 255)
wins_col_vec[wins_goals_merged$Group.1 == 'Argentina'] <- rgb(14, 82, 67, maxColorValue = 255)
wins_col_vec[wins_goals_merged$Group.1 == 'France'] <- rgb(14, 82, 67, maxColorValue = 255)
wins_col_vec[wins_goals_merged$Group.1 == 'Spain'] <- rgb(14, 82, 67, maxColorValue = 255)

ggplot(wins_goals_merged) +
  aes(x = total_goals, y = total_matches, size = wins, label = Group.1) +
  geom_point(color = wins_col_vec) +
  #geom_label(data = wins_goals_merged %>% filter(champion == 1), aes(label = Group.1)) +
  #geom_text(label = wins_goals_merged$Group.1, nudge_x = 0.25, nudge_y = 0.25, check_overlap = T)
  geom_text(aes(label=ifelse(champion == 1,as.character(Group.1),'')),hjust=1,vjust=0) +
  theme_minimal() +
  xlab('Total Goals') +
  ylab('Total Matches') +
  scale_size_continuous(range = c(1,12))

