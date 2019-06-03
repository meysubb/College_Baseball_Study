
summary_pitching = read_csv("final_data/summary_team_pitching.csv")


team_pitching <- function(conference_name="SEC"){
  sec = summary_pitching %>% filter(conference == conference_name) %>%
    mutate(balls_in_play = team_GO + team_FO)
  
  so_quant = quantile(sec$team_SO,prob=seq(0,1,by=0.05))
  good_minx = so_quant[17]
  bb_quant =  quantile(sec$team_BB,prob=seq(0,1,by=0.05))
  good_maxy = bb_quant[5]
  
  bad_maxx = so_quant[2]
  bad_miny = bb_quant[13]
  
  p1 = ggplot(sec,aes(team_SO,team_BB,label=team_name)) + 
    geom_rect(
      aes(
        xmin = good_minx,xmax = max(sec$team_SO),
        ymin = min(sec$team_BB),ymax = good_maxy),
      fill = "palegreen", 
      alpha = 0.2, 
      size = 2
    ) +
    geom_rect(
      aes(
        xmin = min(sec$team_SO)-15,xmax = bad_maxx,
        ymin = bad_miny,ymax = max(sec$team_BB)),
      fill = "#FF9999", 
      alpha = 0.2, 
      size = 2
    ) + 
    annotate("text", x = good_minx+20, y = good_maxy, 
             label = 'atop(bold("Great Spot\nMore Ks Less Walks"))',
             parse = TRUE)+ 
    annotate("text", x = min(sec$team_SO)+2, y = bad_miny-5, 
             label = 'atop(bold("Bad Spot\nMore Walks Less Ks"))',
             parse = TRUE)+ 
    geom_point() + geom_text_repel() +
    theme_fivethirtyeight(base_size = 16) + 
    labs(x="K (Total)",y="BB (Total)",
         title = "K/BB Ratio") + 
    theme(axis.title = element_text()) 
  
  
  good_so <- paste( "More K's",sprintf('\u2192'))
  good_in_outs <- paste( sprintf('\u2191'),"In-Play Out's")
  
  p2 = ggplot(sec, aes(team_SO, balls_in_play, label = team_name)) +
    geom_point() + geom_text_repel() +
    theme_fivethirtyeight(base_size = 16) +
    annotate(
      "text",
      x = mean(sec$team_SO),
      y = min(sec$balls_in_play),
      label = good_so,
      color = 'darkgreen',
      size = 7
    ) +
    annotate(
      "text",
      x = min(sec$team_SO) + 20,
      y = mean(sec$balls_in_play),
      label = good_in_outs,
      color = 'darkgreen',
      size = 7
    ) +
    labs(x = "Strikeouts (K)", y = "Outs from Balls in Play (GO+FO)",
         title = "Where do they get their outs?") +
    theme(axis.title = element_text())
  
  cowplot::plot_grid(p1,p2)
  
  
}





