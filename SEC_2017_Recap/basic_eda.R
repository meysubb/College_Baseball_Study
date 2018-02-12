library(tidyverse)
library(rvest)
library(lubridate)
library(zoo)
base_url <- "http://stats.ncaa.org"
sec_urls <- "http://stats.ncaa.org/team/inst_team_list?academic_year=2018&conf_id=911&division=1&sport_code=MBB"

team_names <- read_html(sec_urls) %>% html_nodes(".css-panes a") %>% html_text()
team_names <- trimws(team_names)


game_dat2 <- read_tsv("game_data.tsv")

game_dat3 <- game_dat2 %>% replace(is.na(.), 0) %>% 
  mutate(
  home_win = ifelse(home_r > away_r,1,0)
)


