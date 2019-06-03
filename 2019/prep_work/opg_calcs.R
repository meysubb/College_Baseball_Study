### Replicate Tung's work from 
### Data Mining Career Batting Performances in Baseball

player_hit <- read_tsv("data/summary_hitting_data.tsv")

adv_metrics <- player_hit %>% mutate(
  OPS = OBPct + SlgPct,
  # total average
  TA = (TB + BB + HBP + SB - CS)/(AB-H+CS+DP),
  # Isolated power 
  ISO = SlgPct - BA,
  # Secondary Average
  SECA = (TB - H + BB + SB - CS)/(AB),
  ## Runs Created
  RC = ((H + BB + HBP - CS - DP) * (TB + 0.26*(BB + HBP)+ 0.52*(SH + SF + SB)))/(AB + BB + HBP + SH + SF),
  ## Runs Created per game (27 outs in a 9 game inning)
  RC27 = RC/((AB-H+SH+SF-CS+DP)/27)
) %>% filter(AB>50) %>% select(Player,team_name,AB,TB,BA,OBPct,SlgPct,OPS,TA,ISO,SECA,RC,RC27)

data("master_ncaa_team_lu")
conf_2019 <- master_ncaa_team_lu %>% filter(year==2019) %>% select(school,conference)

adv_metrics[adv_metrics$team_name == "NC State","team_name"] <- "North Carolina St."
adv_metrics[adv_metrics$team_name == "A&M-Corpus Christi","team_name"] <- "A&M-Corpus Chris"
adv_metrics[adv_metrics$team_name == "Saint Mary's (CA)","team_name"] <- "St. Mary's (CA)"
adv_metrics[adv_metrics$team_name == "South Alabama","team_name"] <- "South Ala."
adv_metrics[adv_metrics$team_name == "DBU","team_name"] <- "Dallas Baptist"


final_adv_metrics <- adv_metrics %>% inner_join(.,conf_2019,by=c("team_name"="school"))
write.csv(final_adv_metrics,"final_data/clust_basic_df.csv")

### Run PCA after filtering by conference
### create a function

pca_func <- function(conf="SEC"){
  dat <- final_adv_metrics %>% filter(conference==conf) %>% select(-conference,-AB)
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
  ggplot(plot_df,aes(x=AB,y=OPG,color=cluster)) + 
    geom_point() + 
    scale_color_discrete()
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


