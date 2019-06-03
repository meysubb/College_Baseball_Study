library(tidyr)

hitting <- read_tsv("data/player_data.tsv")
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
hitting <- hitting %>% mutate(
  player_name = trim(player_name)
)

tu <- hitting %>% filter(team_name %in% c("Texas"))

###
game <- read_tsv("data/game_data.tsv")

team_hit <- read_tsv("data/summary_team_hitting_data.tsv")

### extract_team_df
extract_team_df <- function(team_name,df=game){
  df_raw <- df %>% filter(home_team_name == team_name | away_team_name == team_name)
  ind1 <- which(df_raw$home_team_name == team_name)
  df_raw[ind1,c(3:ncol(df_raw))] <- df_raw[ind1,c(23:42,3:22,43)]
  return(df_raw)
}

team_overall <- team_hit %>% select(team_name) %>% rename(team=team_name) %>% distinct(team)

team_overall = team_overall %>% 
  mutate(
    schedule = purrr::map(team,.f=extract_team_df))

#team_overall2 <- team_overall %>% select(schedule) %>% unnest()
team_o3 <- bind_rows(team_overall$schedule)

team4 <- team_o3 %>% replace_na(list(away_r=0,home_r=0)) %>%  mutate(
  res = ifelse(away_r>home_r,'W','L'),
  res = ifelse(away_r ==home_r,'T',res),
  RD = abs(away_r - home_r)
) %>% select(game_date,away_team_name,home_team_name,res,RD) %>% 
  group_by(away_team_name) %>% arrange(game_date,.by_group=TRUE) %>% ungroup()

conf_ties = master_ncaa_team_lu %>% filter(year == 2019 & division==1) %>% select(school,conference)

# team4 = team4 %>% 
#   left_join(conf_ties,by=c("away_team_name"="school")) %>% 
#   rename(away_conference=conference) %>% 
#   left_join(conf_ties,by=c("home_team_name"="school")) %>% 
#   rename(home_conference=conference) %>% 
#   mutate(conf_game = away_conference == home_conference) %>% 
#   drop_na(conf_game) %>% filter(conf_game==T)

team3 <- team4 %>% 
  group_by(away_team_name) %>% arrange(game_date,.by_group=TRUE) %>% 
  summarize(
    W = sum(ifelse(res=='W',1,0)),
    L = sum(ifelse(res=='L',1,0)),
    T = sum(ifelse(res=='T',1,0)),
  ) %>% mutate(
    w_pct = W/(W+L)
  ) %>% rename(team_name = away_team_name)


team_hit = team_o3 %>% group_by(home_team_name) %>% 
  summarise(
    team_R = sum(home_r,na.rm=T),
    opp_team_R = sum(away_r,na.rm=T)
  ) %>% 
  rename(team_name = home_team_name)


overall_teams <- team_hit %>% select(team_name,team_R,opp_team_R) %>% 
  mutate(
    pyth_w_pct = (team_R)^1.83 / (opp_team_R^1.83 + team_R^1.83)
  ) %>% inner_join(.,team3) %>% arrange(team_name)

overall_teams[overall_teams$team_name == "NC State","team_name"] <- "North Carolina St."
overall_teams[overall_teams$team_name == "A&M-Corpus Christi","team_name"] <- "A&M-Corpus Chris"
overall_teams[overall_teams$team_name == "Saint Mary's (CA)","team_name"] <- "St. Mary's (CA)"
overall_teams[overall_teams$team_name == "South Alabama","team_name"] <- "South Ala."
overall_teams[overall_teams$team_name == "DBU","team_name"] <- "Dallas Baptist"



yr_2019 <- master_ncaa_team_lu %>% filter(year==2019)
### Final Team/Conference and Win vs Pyth win Record Data-Frame. Make sure to save
team_name_conf <- overall_teams %>% inner_join(.,yr_2019,by=c("team_name"="school"))

team_name_conf_final <- team_name_conf %>% select(team_name,W,L,w_pct,pyth_w_pct,team_R,opp_team_R,conference) %>% rename(RS=team_R,RA=opp_team_R)

write.csv(team_name_conf_final,"final_data/pyth_win_loss.csv")
write.csv(team4,"final_data/run_diff.csv")


### ALL you need for the shiny conference plot
pyth_df <- read_csv("final_data/pyth_win_loss.csv")
raw_rd <- read_csv("final_data/run_diff.csv")
plot_confrence <- function(conf){
  require(cowplot)
  require(baseballr)
  require(ggrepel)
  ### ensure team_name_conf_final is loaded
  conf_df <- pyth_df %>% filter(conference==conf)
  a <- ggplot(data=conf_df,aes(x=w_pct,y=pyth_w_pct,label=team_name)) + 
    geom_point() + geom_text_repel() + 
    geom_abline(slope=1,intercept=0,linetype='dashed') + 
    theme_fivethirtyeight(base_size = 16) +
    labs(x="Actual W%",y="Pyth W%",title='Actual vs. Predicted # of wins',
         subtitle=paste0(conf,' 2018 Season')) + theme(axis.title = element_text()) 
  ### Find conference teams 
  data("master_ncaa_team_lu") 
  teams <- master_ncaa_team_lu %>% filter(year==2017,conference==conf) %>% select(school) %>% pull()
  ### Now filter out by team
  run_diff <- raw_rd %>% filter(away_team_name %in% teams)
  ## bin widths
  breaks <- pretty(range(run_diff$RD), n = nclass.FD(run_diff$RD), min.n = 1)
  bwidth <- breaks[2]-breaks[1]
  ## render run plot
  b <- ggplot(run_diff) + 
    geom_histogram(aes(x=RD,fill=away_team_name),binwidth=bwidth,alpha=0.8) +
    facet_wrap(~away_team_name) + theme_fivethirtyeight(base_size = 16) + 
    labs(x="Run Differential",y='',title="Run Differential Distribution",
         subtitle=paste0(conf,' 2018 Season'),
         caption='@msubbaiah1') + 
    guides(fill=FALSE) + theme(axis.title = element_text()) 
  plot_grid(a,b)
}
