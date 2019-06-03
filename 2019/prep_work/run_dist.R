team_hit <- read_tsv("data/summary_team_hitting_data.tsv")

run_dist <- function(conf){
  require(cowplot)
  require(baseballr)
  data("master_ncaa_team_lu") 
  teams <- master_ncaa_team_lu %>% filter(year==2017,conference==conf) %>% select(school) %>% pull()
  
  plot_hit <- team_hit %>% filter(team_name %in% teams) %>%  mutate(
    RunProd = team_SF + team_SH + team_SB) %>% select(team_HR,RunProd,team_name) 
    
  ## LEFT PLOT
  e <- ggplot(plot_hit,aes(x=team_HR,y=RunProd,label=team_name)) + 
    geom_point() + 
    geom_text_repel() + 
    theme_fivethirtyeight(base_size = 16) + 
    theme(axis.title = element_text()) + 
    labs(x='HR',y='Sac Fly/Hit + Stolen Bases',
         title='Run Production')
  
  ### Hit Dist by Conference
  h_dist <- team_hit %>% filter(team_name %in% teams) %>% mutate(
    team_1B = team_H - (team_2B + team_3B + team_HR)
  ) %>%  select(team_name,team_1B,team_2B,team_3B,team_HR,team_BB,team_HBP) 
  
  
  h_dist <- h_dist %>%
    gather(variable, value, -team_name) %>% 
    group_by(team_name) %>% 
    mutate(percentage = value/sum(value)) %>% 
    group_by(variable) %>% 
    mutate(
      scale_value = scale(value),
      fill_pct = scale_value/sum(scale_value)) %>% select(-value,-scale_value)
  
  h_dist$variable <- as.factor(h_dist$variable)
  levels(h_dist$variable) <- c("Single","Double","Triple","Walks","Hit By Pitch","Home Run")
  
  h_dist$team_name <- factor(h_dist$team_name, levels = team_hit$team_name[order(team_hit$team_R)])
  
  f <- ggplot(h_dist, aes(team_name, variable, fill = fill_pct)) + 
    geom_tile(colour = "white") + 
    geom_text(aes(x = team_name, y = variable, label = sprintf("%1.2f%%", round(percentage*100, digits = 2))),color='white') + 
    scale_fill_gradient(low = "blue", high = "firebrick3") + 
    coord_flip() + 
    labs(x="",y="",title='Hit Distribution',
         subtitle='Teams ordered by most runs scored',
         caption='@msubbaiah1') + 
    theme_fivethirtyeight(base_size = 16) +
    guides(fill=FALSE) + scale_y_discrete(position = "top") 
 plot_grid(e,f)
}
