library(dplyr)
library(readr)
library(baseballr)
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

confs = master_ncaa_team_lu %>% filter(division==1) %>% distinct(conference)


hitting = read_tsv("data/player_data.tsv")

hitting <- hitting %>% mutate(
  player_name = trim(player_name))

top_hit = hitting %>% select(-team_id,-game_date,-Pos) %>% group_by(player_id,player_name,team_name) %>% 
  summarise_all(sum,na.rm=T) %>% 
  ungroup() %>% 
  filter(AB>0) %>% 
  mutate(BA = H/AB)


rolling_plots <- function(type="conf",team="Texas A&M",conference_name="SEC"){
  require(baseballr)
  require(zoo)
  teams <- master_ncaa_team_lu %>% filter(year==2019,conference==conference_name) %>% select(school) %>% pull()
  if(type=="conf"){
    top_hit2 <- top_hit %>% filter(team_name %in% teams)
    AB_cutoff <- quantile(top_hit2$AB,prob=seq(0,1,by=0.05))[19]
    BA_cutoff <- quantile(top_hit2$BA,prob=seq(0,1,by=0.05))[19]
  }else{
    top_hit2 <- top_hit %>% filter(team_name == team)
    AB_cutoff <- quantile(top_hit2$AB,prob=seq(0,1,by=0.05))[15]
    BA_cutoff <- quantile(top_hit2$BA,prob=seq(0,1,by=0.05))[15]
  }
  ### Dynamic way to identify AB and BA requirements 
  
  
  players_df <- top_hit2 %>% filter(AB>AB_cutoff & BA>BA_cutoff) %>% mutate(
    `1B` = TB - (2*`2B` +3*`3B`),
    wOBA = (.69 * BB + .72 * HBP + .89 * `1B` + 1.27 * `2B` + 1.62 * `3B` + 2.10 * HR)/(AB+BB+SF+HBP)
  )
  players <- players_df %>% select(player_name) %>% pull()
  hitting2 <- hitting %>% 
    filter(player_name %in% players & team_name %in% teams) 
  hitting2[is.na(hitting2)] <- 0
  hitting3 <- hitting2 %>% mutate(
    `1B` = TB - (2*`2B` +3*`3B`),
    wOBA = (.69 * BB + .72 * HBP + .89 * `1B` + 1.27 * `2B` + 1.62 * `3B` + 2.10 * HR)/(AB+BB+SF+HBP)
  ) %>% group_by(player_name) %>%  
    arrange(game_date,.by_group=TRUE) %>% 
    mutate(
      game_num = row_number()
    ) %>% ungroup() 
  
  hitting4 <- hitting3 %>% select(player_name,game_num,wOBA) %>% mutate(
    roll_wOBA = rollapply(wOBA,3,FUN=mean,align='right',fill=NA)
  )
  
  hitting5 <- hitting3 %>% select(player_name,game_num,H) %>% filter(H==1)
  hitting6 <- hitting3 %>% select(player_name,H)
  
  players_df$player_name <- factor(players_df$player_name)
  
  subtitle_text = paste0('Returning ',conference_name,' Top hitters (AB > ', AB_cutoff,' & BA > ',BA_cutoff,')\nVertical lines show # of 1-hit games')
  
  roll_plot <- ggplot() + 
    geom_line(data=hitting4,aes(x=game_num,y=roll_wOBA),color='blue') + 
    facet_wrap(~player_name) +
    geom_hline(data=players_df,aes(yintercept=wOBA),linetype='dashed',color='black',alpha=0.75) + 
    geom_rug(data=hitting5,aes(x=game_num,y=H),sides="b",color='maroon') + 
    theme_fivethirtyeight(base_size = 16) + 
    theme(axis.title = element_text()) + 
    labs(x = "Game Numbers", y = "wOBA",
         title = "wOBA in 3-Game Rolling Intervals",
         subtitle = subtitle_text) +  
    theme(strip.text.x = element_text(size = 12, colour = "blue",face="bold"),
          axis.title.x = element_text(size=18),
          plot.subtitle = element_text(size=16),
          plot.caption = element_text(color="#999999")) + 
    guides(color= FALSE)
  
  d <- ggplot() +
    geom_histogram(data=hitting6,aes(x=H),fill='blue',alpha=0.6) + 
    facet_wrap(~player_name) +
    theme_fivethirtyeight(base_size = 16) + 
    theme(axis.title = element_text()) +
    labs(x="Hits",y='',
         title='Hit Distribution',
         caption = '@msubbaiah1')
  require(cowplot)
  plot_grid(roll_plot,d)
}
