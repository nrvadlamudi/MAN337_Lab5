# How does field surface and stadium type affect the score of a game?

library(dplyr);
library(tidyr);
library(ggplot2);

NFLGameData <- read.csv("https://raw.githubusercontent.com/UTSportsAnalytics/data/main/NFLGameData.csv");
# Let's just work with the regular season
NFLGameData <- NFLGameData %>% 
  filter(game_type=="REG");

# field total points scored into four categories, dome roof and grass, dome roof and astroturf, open roof and grass, open roof and astroturf
# group by field type and stadium type and count the number of games
# then calculate the average score for each field type and stadium type
# then plot the results
StadiumData <- NFLGameData %>%
    # filter out games with no stadium no roof or no score
    filter(!is.na(stadium)) %>%
    filter(!is.na(roof)) %>%
    filter(!is.na(home_score)) %>%
    filter(!is.na(away_score)) %>%
    select(surface, roof, home_score, away_score) %>%
    mutate(surface = ifelse(surface == "grass", "grass", "astroturf")) %>%
    mutate(roof = ifelse(roof == "dome", "dome", "outdoors")) %>%
    group_by(surface, roof) %>%
    summarise(games = n(), avg_home_score = mean(home_score),avg_away_score = mean(away_score)) %>%
    arrange(desc(avg_home_score));

print(StadiumData, n=Inf);

