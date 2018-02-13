library(highcharter)
library(htmlwidgets)
library(tidyverse)

play_hit <- read_tsv("summary_hitting_data.tsv") 
play_hit[is.na(play_hit)] <- 0


hit_plate_disc <- play_hit %>% filter(AB>50) %>% mutate(
  PA = AB + BB + HBP + SF + SH,
  K_PCT = K/AB,
  BB_PCT = BB/PA
) %>% select(player_name,team_name,AB,PA,SLGpct,OBPpct,K,BB,K_PCT,BB_PCT,BA)


x <- c("Player","Team Name","At Bats", "Batting Average","OBP","SLG","K %","BB %")
y <-c ("{point.player_name}","{point.team_name}", "{point.BA}",
       sprintf("{point.%s:.3f}", c("BA", "OBPpct","SLGpct","K_PCT","BB_PCT")))
tltip <- tooltip_table(x,y)

mycolors = c(RColorBrewer::brewer.pal(name="Dark2", n = 8), RColorBrewer::brewer.pal(name="Paired", n = 6))

hc_hit <- hchart(hit_plate_disc, "scatter", hcaes(x = BB_PCT, y = K_PCT, group = team_name)) %>% 
  hc_colors(mycolors) %>% 
  hc_xAxis(title = list(text = "BB%")) %>% 
  hc_yAxis(title = list(text = "K%")) %>% 
  hc_title(text = "Plate Discipline",align = "left") %>% 
  hc_subtitle(text = "SEC Hitters, AB > 30",align = "left") %>% 
  hc_credits(
    enabled = TRUE,
    text = "@msubbaiah1",
    href = "https://twitter.com/msubbaiah1"
  ) %>% 
  hc_tooltip(useHTML = TRUE, headerFormat = "", pointFormat = tltip) %>% 
  hc_legend(layout = "vertical", verticalAlign = "top",
            align = "right", valueDecimals = 0) %>% 
  hc_add_theme(hc_theme_db())

saveWidget(hc_hit, file="hc_hit.html")



play_pitch <- read_tsv("summary_pitching_data.tsv") %>% filter(Pitches>0) %>% select(-player_name) %>% 
  rename(team_name=jersey,team_id=team_name,player_name=team_id) 

pit_plate_disc <- play_pitch %>% filter(IP>10) %>% mutate(
  K_PCT = SO/BF,
  BB_PCT = BB/BF
) %>% select(player_name,team_name,W,L,SV,IP,ERA,BF,SO,BB,BF,K_PCT,BB_PCT)

x <- c("Player","Team Name","W","L","SV","IP","Batters Faced","ERA","K %","BB %")
y <-c ("{point.player_name}","{point.team_name}", "{point.W}",
       "{point.L}","{point.SV}","{point.IP}","{point.BF}",
       sprintf("{point.%s:.3f}", c("ERA", "K_PCT","BB_PCT")))
tltip <- tooltip_table(x,y)

hc_pit <- hchart(pit_plate_disc, "scatter", hcaes(x = BB_PCT, y = K_PCT, group = team_name)) %>% 
  hc_colors(mycolors) %>% 
  hc_xAxis(title = list(text = "BB%")) %>% 
  hc_yAxis(title = list(text = "K%")) %>% 
  hc_title(text = "Plate Discipline",align = "left") %>% 
  hc_subtitle(text = "SEC Pitchers, IP > 10",align = "left") %>% 
  hc_credits(
    enabled = TRUE,
    text = "@msubbaiah1",
    href = "https://twitter.com/msubbaiah1"
  ) %>% 
  hc_tooltip(useHTML = TRUE, headerFormat = "", pointFormat = tltip) %>% 
  hc_legend(layout = "vertical", verticalAlign = "top",
            align = "right", valueDecimals = 0) %>% 
  hc_add_theme(hc_theme_db())

saveWidget(hc_pit, file="hc_pit.html")