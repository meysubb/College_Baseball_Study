library(dplyr)
library(baseballr)
library(tidyr)
library(lubridate)

dat_18 <- read_tsv("2018_game_data.tsv")
dat_19  <- read_tsv("2019_game_data.tsv")
all_games <- bind_rows(dat_18,dat_19)

all_games <- replace_na(all_games, list(home_r=0, away_r=0))

team = master_ncaa_team_lu %>% filter(year == 2019 & division == 1) %>% select(school,conference)

extract_team_df <- function(team_name,df){
  df_raw <- df %>% filter(home_team_name == team_name | away_team_name == team_name)
  ind1 <- which(df_raw$away_team_name == team_name)
  ind2 <- which(!(df_raw$away_team_name == team_name))
  df_raw[ind1,c(23:42,3:22,43)] <- df_raw[ind1,c(3:ncol(df_raw))]
  df_raw[ind1,"location"] <- "A"
  df_raw[ind2,"location"] <- "H"
  df_raw <- df_raw %>% 
    mutate(
      location = ifelse(neutral==1,"n",location),
    RD = home_r - away_r) %>% select(game_date,game_id,away_team_name,location,RD)
  return(df_raw)
}


raw_dat <- team %>% mutate(
  sched = purrr::map(school,extract_team_df,df=all_games)
)

full_dat <- raw_dat %>% unnest()
colnames(full_dat)[5] <- "opponent"

# Adjust for 10 run (7 inning) rule. Otherwise huge scores will 
# throw things off
full_dat <- full_dat %>% 
  mutate(
    RD = ifelse(RD>=10,10,RD),
    RD = ifelse(RD<=-10,-10,RD),
    game_date = mdy(game_date)
    )

full_18 <- full_dat %>% filter(year(game_date)==2018) %>% 
  mutate(
    days = difftime(max(game_date),game_date,units="days"),
    ffactor = 0.98 ^ (as.numeric(days)+1))


full_19 <- full_dat %>% filter(year(game_date)==2019) %>% 
  mutate(
    days = difftime(max(game_date),game_date,units="days"),
    ffactor = (0.94 ^ as.numeric(days))*3)

full_dat_back <- bind_rows(full_18,full_19) 

full_dat_conf = full_dat_back %>% 
  left_join(team,by=c("opponent"="school")) %>% 
  mutate(
    conf_play = ifelse(conference.x==conference.y,T,F),
    conf_play = ifelse(is.na(conf_play),F,conf_play)
  ) 

