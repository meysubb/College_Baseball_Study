library(tidyverse)
library(ggrepel)
library(ggimage)
library(ggthemes)

names <- list.files("images/")
names <- paste0("images/",names)

game <- read_tsv("game_data.tsv")
team_hit <- read_tsv("summary_team_hitting_data.tsv")
team_pit <- read_tsv("summary_team_pitching_data.tsv")


### extract_team_df

extract_team_df <- function(df,team_name){
  df_raw <- df %>% filter(home_team_name == team_name | away_team_name == team_name)
  ind1 <- which(df_raw$home_team_name == team_name)
  df_raw[ind1,c(3:ncol(df_raw))] <- df_raw[ind1,c(23:42,3:22,43)]
  return(df_raw)
}

team_overall <- team_hit %>% select(team_name) %>% rename(team=team_name) 
team_overall$schedule <- 0
for(i in 1:nrow(team_overall)){
  t_n <- team_overall$team[i]
  team_overall$schedule[i] <- nest(extract_team_df(game,t_n))
}
team_overall2 <- team_overall %>% select(schedule) %>% unnest()
team_o3 <- bind_rows(team_overall2$schedule)

team4 <- team_o3 %>% replace_na(list(away_r=0,home_r=0)) %>%  mutate(
  res = ifelse(away_r>home_r,'W','L'),
  RD = abs(away_r - home_r)
) %>% select(game_date,away_team_name,home_team_name,res,RD) %>% 
  group_by(away_team_name) %>% arrange(game_date,.by_group=TRUE) %>% ungroup()

team3 <- team4 %>% 
  group_by(away_team_name) %>% arrange(game_date,.by_group=TRUE) %>% 
  summarize(
    W = sum(ifelse(res=='W',1,0)),
    L = sum(ifelse(res=='L',1,0))
  ) %>% mutate(
    w_pct = W/(W+L)
  ) %>% rename(team_name = away_team_name)



overall_teams <- team_hit %>% select(team_name,team_R,opp_team_R) %>% 
  mutate(
    pyth_w_pct = (team_R)^1.83 / (opp_team_R^1.83 + team_R^1.83)
  ) %>% inner_join(.,team3) %>% arrange(team_name)

overall_teams$images <- names
overall_teams$team_name <- as.factor(overall_teams$team_name)
overall_teams$images[c(8,9)] <- overall_teams$images[c(9,8)]

a <- ggplot(data=overall_teams,aes(x=w_pct,y=pyth_w_pct)) + 
  geom_image(aes(image=images),size=0.05) +
  geom_abline(slope=1,intercept=0,linetype='dashed') + 
  ylim(0.3518519, 0.7323944) + 
  theme_bw(base_size = 16) +
  labs(x="Actual W%",y="Pyth W%",title='Actual vs. Predicted # of wins',
       subtitle='SEC 2017 Season') 

library(teamcolors)

breaks <- pretty(range(team4$RD), n = nclass.FD(team4$RD), min.n = 1)
bwidth <- breaks[2]-breaks[1]
b <- ggplot(team4) + 
  geom_histogram(aes(x=RD,fill=away_team_name),binwidth=bwidth,alpha=0.8) +
  facet_wrap(~away_team_name) + theme_bw(base_size = 16) + 
  labs(x="Run Differential",y='',title="Run Differential Distribution",
       subtitle='SEC 2017 Season',
       caption='@msubbaiah1') + 
  guides(fill=FALSE)

library(cowplot)
### Team Record vs Run Differential overview
#plot_grid(a,b)
## background histograms
## https://drsimonj.svbtle.com/plotting-background-data-for-groups-with-ggplot2


## Team Hitting
## Run Production
team_hit2 <- team_hit %>% mutate(
  RunProd = team_SF + team_SH + team_SB 
)


e <- ggplot(team_hit2,aes(x=team_HR,y=RunProd,label=team_name)) + 
  geom_point() + 
  geom_text_repel() + 
  theme_fivethirtyeight(base_size = 16) + 
  theme(axis.title = element_text()) + 
  labs(x='HR',y='Sac Fly/Hit + Stolen Bases',
       title='Run Production')

### Distribution of hits

h_dist <- team_hit %>% mutate(
  team_1B = team_H - (team_2B + team_3B + team_HR)
) %>%  select(team_name,team_1B,team_2B,team_3B,team_HR,team_BB,team_HBP) 


h_dist <- h_dist %>%
  gather(variable, value, -team_name) %>% 
  group_by(team_name) %>% 
  mutate(percentage = value/sum(value)) %>% 
  select(-value)
 
h_dist$variable <- as.factor(h_dist$variable)
levels(h_dist$variable) <- c("Single","Double","Triple","Walks","Hit By Pitch","Home Run")

h_dist$team_name <- factor(h_dist$team_name, levels = team_hit$team_name[order(team_hit$team_R)])




f <- ggplot(h_dist, aes(team_name, variable, fill = percentage)) + 
  geom_tile(colour = "white") + 
  geom_text(aes(x = team_name, y = variable, label = sprintf("%1.2f%%", round(percentage*100, digits = 2))),color='white') + 
  scale_fill_gradient(low = "blue", high = "firebrick3") + 
  coord_flip() + 
  labs(x="",y="",title='Hit Distribution',
       subtitle='Teams ordered by most runs scored',
       caption='@msubbaiah1') + 
  theme_fivethirtyeight() +
  guides(fill=FALSE)
## Run Prod + Hit Distribution - Team Overlook
#plot_grid(e,f)

### Team Pitching

play_pitch <- read_tsv("summary_pitching_data.tsv") %>% filter(Pitches>0) %>% select(-player_name) %>% 
  rename(team_name=jersey,team_id=team_name,player_name=team_id) 

pitch_depth <- play_pitch %>%  group_by(team_name) %>% mutate(
  BF_pct = BF/sum(BF,na.rm=T),
  pitch_pct = Pitches/sum(Pitches,na.rm=T),
  IP_pct = IP/sum(IP,na.rm=T)
) %>% ungroup()

pitch_back <- pitch_depth %>% filter(!(year=="Sr")) %>% group_by(team_name) %>% 
  summarize(
    sum_BF = sum(BF_pct,na.rm=T),
    sum_pitch = sum(pitch_pct,na.rm=T),
    sum_IP = sum(IP_pct,na.rm=T)
  )


team_ip_yr <- play_pitch %>% group_by(team_name,year) %>% 
  summarise(
    IP = sum(IP,na.rm=T)
  ) %>% ungroup() %>% group_by(team_name) %>% mutate(
    IP_pct = IP/sum(IP)
  )
team_ip_yr$year <- factor(team_ip_yr$year,levels = c("Fr","So","Jr","Sr"))


int_breaks <- function(x, n = 5) pretty(x, n)[pretty(x, n) %% 1 == 0] 

pit_dist <- ggplot(team_ip_yr,aes(x=year,y=IP_pct)) + 
  geom_histogram(stat='identity') + 
  facet_wrap(~team_name) + 
  theme_fivethirtyeight(base_size = 16) + 
  theme(axis.title = element_text()) +
  labs(x='Year',y='IP (%)',title='Pitching Distribution by Class') + 
  scale_y_continuous(labels = scales::percent)


### Individual Players Analysis 
### 1) Streaks vs Hit Distribution per player
top_hit <- read_tsv("summary_hitting_data.tsv") 
top_hit2 <- top_hit %>% filter(AB >200 & BA > .300 & !(year %in% c('Sr','Jr')))



hitting <- read_tsv("player_data.tsv")
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
hitting <- hitting %>% mutate(
  player_name = trim(player_name)
)

hitting2 <- hitting %>% 
  filter(player_name %in% top_hit2$player_name) %>% 
  replace_na(list(H=0,AB=0)) %>% group_by(player_name) %>%  
  arrange(game_date,.by_group=TRUE) %>% 
  mutate(
    game_num = row_number()
  ) %>% 
  ungroup() %>% mutate(
    BA = H/AB
  )

library(zoo)

hitting3 <- hitting2 %>% select(player_name,game_num,BA) %>% mutate(
  roll_BA = rollapply(BA,3,FUN=mean,align='right',fill=NA)
)

hitting4 <- hitting2 %>% select(player_name,game_num,H) %>% filter(H==1)

### Streak-yness plot
c <- ggplot() + 
  geom_line(data=hitting3,aes(x=game_num,y=roll_BA),color='blue') + 
  geom_hline(data=top_hit2,aes(yintercept=BA),linetype='dashed',color='black',alpha=0.75) + 
  geom_rug(data=hitting4,aes(x=game_num,y=H),sides="b",color='maroon') + 
  facet_wrap(~player_name) + 
  theme_fivethirtyeight(base_size = 16) + 
  theme(axis.title = element_text()) + 
  labs(x = "Game Numbers", y = "Average",
       title = "Average in 3-Game Rolling Intervals",
       subtitle = 'Returning SEC Top hitters (AB > 250 & BA > .300) \nVertical lines show # of 1-hit games') +  
  theme(strip.text.x = element_text(size = 12, colour = "blue",face="bold"),
        axis.title.x = element_text(size=18),
        plot.subtitle = element_text(size=16),
        plot.caption = element_text(color="#999999")) + 
  guides(color= FALSE)


hitting5 <- hitting2 %>% select(player_name,H)

d <- ggplot() +
  geom_histogram(data=hitting5,aes(x=H),fill='blue',alpha=0.6) + 
  facet_wrap(~player_name) +
  theme_fivethirtyeight(base_size = 16) + 
  theme(axis.title = element_text()) +
  labs(x="Hits",y='',
       title='Hit Distribution',
       subtitle = 'Returning SEC Top hitters (AB > 250 & BA > .300)',
       caption = '@msubbaiah1')

#plot_grid(c,d)


### K - BB ratio (hitters vs. pitchers)
play_hit <- read_tsv("summary_hitting_data.tsv") 
play_hit[is.na(play_hit)] <- 0


hit_plate_disc <- play_hit %>% filter(AB>50) %>% mutate(
    PA = AB + BB + HBP + SF + SH,
    K_PCT = K/AB,
    BB_PCT = BB/PA
  ) %>% select(player_name,team_name,AB,K,BB,PA,K_PCT,BB_PCT)



k_excellent = quantile(hit_plate_disc$K_PCT,probs=seq(0,1,by=0.05))[6]
bb_excellent = quantile(hit_plate_disc$BB_PCT,probs=seq(0,1,by=0.05))[17]

hit_name_play <- hit_plate_disc %>% filter(K_PCT <= k_excellent & BB_PCT >= bb_excellent)

mycolors = c(RColorBrewer::brewer.pal(name="Dark2", n = 8), RColorBrewer::brewer.pal(name="Paired", n = 6))

hit_pd <- ggplot(hit_plate_disc,aes(x=BB_PCT,y=K_PCT)) + 
  geom_point(aes(color=team_name)) + 
  geom_label_repel(data=hit_name_play,aes(label=player_name,hjust=1),point.padding = 0.5) + 
  guides(fill=FALSE) + 
  theme_bw(base_size = 16) + 
  theme(axis.title = element_text()) + 
  labs(x="BB%",y="K%",title="Plate Discpline",
       subtitle='SEC Hitters, AB > 30',
       color='Team') + 
  geom_hline(yintercept = k_excellent,color="blue",linetype='dashed') + 
  geom_vline(xintercept = bb_excellent, color="blue",linetype='dashed') +
  scale_color_manual(values = mycolors)

pit_plate_disc <- play_pitch %>% filter(IP>10) %>% mutate(
  K_PCT = SO/BF,
  BB_PCT = BB/BF
) %>% select(player_name,team_name,BF,SO,BB,BF,K_PCT,BB_PCT)

so_excellent = quantile(pit_plate_disc$K_PCT,probs=seq(0,1,by=0.05))[17]
w_excellent = quantile(pit_plate_disc$BB_PCT,probs=seq(0,1,by=0.05))[6]

pit_name_play <- pit_plate_disc %>% filter(K_PCT >= so_excellent & BB_PCT <= w_excellent)

pit_pd <- ggplot(pit_plate_disc,aes(x=BB_PCT,y=K_PCT)) + 
  geom_point(aes(color=team_name)) + 
  geom_label_repel(data=pit_name_play,aes(label=player_name),point.padding = 0.5) + 
  guides(fill=FALSE) + 
  theme_bw(base_size = 16) + 
  theme(axis.title = element_text()) + 
  labs(x="BB%",y="K%",title="Plate Discpline",
       subtitle='SEC Pitchers, IP > 10',
       caption='@msubbaiah1',color='Team') + 
  geom_hline(yintercept = so_excellent,color="blue",linetype='dashed') + 
  geom_vline(xintercept = w_excellent, color="blue",linetype='dashed') +
  scale_color_manual(values = mycolors) + 
  guides(color=FALSE)


legend_b <- get_legend(hit_pd + 
                         theme(legend.direction = "vertical",
                               legend.justification="right" ))

p_row <- plot_grid(hit_pd+guides(color=FALSE),pit_pd)

p_fin <- plot_grid(p_row,NULL,legend_b,rel_widths = c(3,0.02,.3),nrow=1)
