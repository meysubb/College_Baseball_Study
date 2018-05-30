### helper funcs
### Confer Pyth Wins
plot_confrence <- function(conf){
  require(cowplot)
  require(baseballr)
  require(ggrepel)
  ### ensure team_name_conf_final is loaded
  conf_df <- pyth_df %>% filter(conference==conf)
  w_min <- min(conf_df$w_pct)
  p_min <- min(conf_df$pyth_w_pct)
  min_axis_limit <- min(w_min,p_min)
  
  a <- ggplot(data=conf_df,aes(x=w_pct,y=pyth_w_pct,label=team_name)) + 
    geom_point() + geom_text_repel() + 
    geom_abline(slope=1,intercept=0,linetype='dashed') + 
    theme_fivethirtyeight(base_size = 16) +
    labs(x="Actual W%",y="Pyth W%",title='Actual vs. Predicted # of wins',
         subtitle=paste0(conf,' 2018 Season')) + theme(axis.title = element_text()) +
    xlim(min_axis_limit,1) + 
    ylim(min_axis_limit,1)
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

### Conference Run Distribution
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

### Filter by team or conference based
rolling_plots <- function(type="conf",team="Texas A&M",conference_name="Southeastern"){
  require(baseballr)
  require(zoo)
  teams <- master_ncaa_team_lu %>% filter(year==2017,conference==conference_name) %>% select(school) %>% pull()
  if(type=="conf"){
    top_hit2 <- top_hit %>% filter(team_name %in% teams)
    AB_cutoff <- quantile(top_hit2$AB,prob=seq(0,1,by=0.05))[19]
    BA_cutoff <- quantile(top_hit2$BA,prob=seq(0,1,by=0.05))[19]
    subtitle_text = paste0('Returning ',conference_name,' Top hitters (AB > ', round(AB_cutoff,2),' & BA > ',round(BA_cutoff,4),')\nVertical lines show # of 1-hit games')
  }else{
    top_hit2 <- top_hit %>% filter(team_name == team)
    AB_cutoff <- quantile(top_hit2$AB,prob=seq(0,1,by=0.05))[15]
    BA_cutoff <- quantile(top_hit2$BA,prob=seq(0,1,by=0.05))[15]
    subtitle_text = paste0('Returning ',team,' Top hitters (AB > ', round(AB_cutoff,2),' & BA > ',round(BA_cutoff,4),')\nVertical lines show 1-hit games')
    
  }
  ### Dynamic way to identify AB and BA requirements 
  
  
  players_df <- top_hit2 %>% filter(AB>AB_cutoff & BA>BA_cutoff) %>% mutate(
    `X1B` = TB - (2*`X2B` +3*`X3B`),
    wOBA = (.69 * BB + .72 * HBP + .89 * `X1B` + 1.27 * `X2B` + 1.62 * `X3B` + 2.10 * HR)/(AB+BB+SF+HBP)
  )
  players <- players_df %>% select(Player) %>% pull()
  hitting2 <- hitting %>% 
    filter(player_name %in% players) 
  hitting2[is.na(hitting2)] <- 0
  hitting3 <- hitting2 %>% mutate(
    `X1B` = TB - (2*`X2B` +3*`X3B`),
    wOBA = (.69 * BB + .72 * HBP + .89 * `X1B` + 1.27 * `X2B` + 1.62 * `X3B` + 2.10 * HR)/(AB+BB+SF+HBP)
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
  
  players_df$player_name <- factor(players_df$Player)
  
    
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


pca_func <- function(conf="Southeastern"){
  dat <- final_adv_metrics %>% filter(conference==conf) %>% select(-conference,-AB,-X)
  team_names <- dat %>% select(team_name) %>% pull
  dat <- dat %>% select(-team_name)
  dat_matrix <- as.matrix(dat)
  dat_matrix <- dat_matrix[,-1]
  dat_matrix <- apply(dat_matrix, 2, as.numeric)
  rownames(dat_matrix) <- dat$Player
  pca1 <- prcomp(dat_matrix, scale. = TRUE)
  
  prinsmat <- pca1$x
  pos_b <- sum(sign(pca1$rotation[,1])) == 10
  ## Use joliffe rule (eigenvalues >0.7) 
  if(pos_b){
    prins_df <- data.frame(prinsmat) %>% rownames_to_column(var = 'Player') %>% select(Player,PC1,PC2,PC3) %>% mutate(OPG=PC1) %>% select(Player,OPG,PC2,PC3)
  }else{
    prins_df <- data.frame(prinsmat) %>% rownames_to_column(var = 'Player') %>% select(Player,PC1,PC2,PC3) %>% mutate(OPG=-PC1) %>% select(Player,OPG,PC2,PC3) 
  }
  final_df <- dat %>% inner_join(.,prins_df)
  final_df$team_name <- team_names
  return(final_df)
  ## supply final_df for clustering
}

k_means_cluster <- function(pc_df){
  ## use NbClust and minimize WSS to identify optimal clusters
  require(NbClust)
  wss_test_df <- pc_df %>% select(OPG,PC2,PC3)
  nb <-  NbClust(wss_test_df, distance = "euclidean", min.nc = 2,max.nc = 10, method = "kmeans")
  opt_clus_char <- names(which.max(table(nb$Best.nc[1,])))
  opt_clus_num <- as.numeric(opt_clus_char)
  set.seed(1234)
  k_mod <- kmeans(wss_test_df,opt_clus_num,nstart=10)
  cluster <- k_mod$cluster
  final_df <- cbind(pc_df,cluster)
  ab_df <- final_adv_metrics %>% select(Player,AB)
  
  plot_df <- final_df %>% inner_join(.,ab_df) %>% mutate(cluster=factor(cluster))
  return(plot_df)
}

opg_hc_plot <- function(df_plot){
  library(highcharter)
  
  x <- c("Player Name: ","Team Name: ", "OPG","AB","BA","OBP","SLG","OPS")
  y <-c ("{point.Player}","{point.team_name}", 
         sprintf("{point.%s:.3f}", c("OPG", "AB","BA","OBPct","SlgPct","OPS")))
  tltip <- tooltip_table(x,y)
  
  final_n <- length(unique(df_plot$cluster))
  mycolors = c(RColorBrewer::brewer.pal(name="Dark2", n = final_n))
  
  hc_pt <- highchart() %>% 
    hc_add_series(df_plot,hcaes(x=AB,y=OPG,group=cluster),type='scatter',marker=list(symbol="circle")) %>%
    hc_colors(mycolors) %>% 
    hc_legend(title=list(text="Cluster"),layout = "vertical", verticalAlign = "top",
              align = "right", valueDecimals = 0) %>% 
    hc_yAxis(title = list(text = "Offensive Player Grade (OPG")) %>% 
    hc_xAxis(title=list(text="At-Bats (AB)")) %>% 
    hc_title(text = "Offensive Player Similarity",align = "left") %>% 
    hc_subtitle(text="Clusters indicate similar players; OPG statistic derived by Tung [2012]",align="left") %>% 
    hc_credits(
      enabled = TRUE,
      text = "@msubbaiah1",
      href = "https://twitter.com/msubbaiah1"
    ) %>% hc_add_theme(hc_theme_flat()) %>%
    hc_tooltip(useHTML = TRUE, headerFormat = "", pointFormat = tltip)
  return(hc_pt)
}




