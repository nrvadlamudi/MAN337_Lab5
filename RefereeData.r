library(dplyr);
library(tidyr);
library(ggplot2);

NFLGameData <- read.csv("https://raw.githubusercontent.com/UTSportsAnalytics/data/main/NFLGameData.csv");
# Let's just work with the 2021 season
NFLGameData <- NFLGameData %>% 
  filter(game_type=="REG");

RefData <- NFLGameData %>%
    # referee has over 10 games
    select(referee, home_score, away_score) %>%
    mutate(home_win = ifelse(home_score > away_score, 1, 0)) %>%
    mutate(away_win = ifelse(home_score < away_score, 1, 0)) %>%
    group_by(referee) %>%
    summarise(home_wins = sum(home_win), away_wins = sum(away_win), games = n()) %>%
    mutate(home_win_pct = (home_wins / (home_wins + away_wins))*100) %>%
    filter(games >= 10) %>%
    # get non NA or "" referee names
    filter(!is.na(referee)) %>%
    filter(referee != "") %>%
    arrange(desc(home_win_pct));

print(RefData, n=Inf);

# plot ref data as a bar chart with their names on the x axis and home win percentage on the y axis
ggp <- ggplot(RefData, aes(x = reorder(referee, home_win_pct), y = home_win_pct)) + 
  geom_bar(stat = "identity") +
  labs(x = "Referee", y = "Home Win Percentage") +
  # rotate chart 90 degrees
    coord_flip() +
    # increase the x axis tick spacing
    scale_x_discrete(expand = c(0, 1)) +
    # add a title
    labs(title = "Home Win Percentage by Referee") +
    # add a horizontal line at 50%
    geom_hline(yintercept = 50, linetype = "dashed", color = "red");


plot(ggp);