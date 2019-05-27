library(pacman)
source("temp_funcs.R")
p_load(baseballr,tidyverse,RCurl,XML,rvest,lubridate)
options(timeout= 4000000)
#szn_19 = ncaa_get_season_schedule(2019,div=1)
#saveRDS(szn_19,"data/szn_19.RDS")
szn_19 = readRDS("data/szn_19.RDS")

games=szn_19 %>%
  distinct(GameId, .keep_all = TRUE) %>%
  mutate(home_team=ifelse(Loc %in% c('H', 'N'), Team, Opp),
         away_team=ifelse(Loc=='H', Opp, Team ),
         temp_date = gsub("\\(.*\\)", "", Date),
         month = month(mdy(temp_date))) %>% 
  select(Year, Date, GameId, home_team, away_team,month) %>% 
  drop_na(GameId)

games_lst = split(games, games$month)

feb = games_lst$`2`
mar = games_lst$`3`
apr = games_lst$`4`
may = games_lst$`5`

feb_pbp = feb %>% mutate(
  pbp_raw = purrr::map2(GameId,Year,clean_games))

write_csv(feb_pbp,'EPA/data/NCAA_baseball_pbp_feb_2019.csv')