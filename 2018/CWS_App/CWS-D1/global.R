### Global.R Script
## Conference List
library(cowplot)
library(baseballr)
library(ggrepel)
library(tidyverse)
library(highcharter)
library(ggthemes)
library(DT)

library(rdrop2)
token <- readRDS("my-token.rds")
drop_acc(dtoken = token)


### For Python Plot (Add this data to dropbox, and read from there. If possible.)
pyth_raw <- drop_read_csv("NCAA_D1_BShiny/pyth_win_loss.csv") %>% select(-X)
pyth_df <- pyth_raw %>% select(team_name,pyth_w_pct,w_pct,conference)
raw_rd <- drop_read_csv("NCAA_D1_BShiny/run_diff.csv") %>% select(-X)
team_hit <- drop_read_csv("NCAA_D1_BShiny/summary_team_hitting_data.tsv",sep="\t")


data("master_ncaa_team_lu") 
conf_df <- master_ncaa_team_lu %>% filter(year==2017) %>% select(school,conference) 
team_names <- team_hit %>% select(team_name) %>% distinct()

conf_list <- conf_df %>% inner_join(.,team_names,by=c("school"="team_name")) %>% select(conference) %>% distinct() %>% pull() %>% sort()
team_list <- team_names %>% pull() %>% sort()

top_hit <- drop_read_csv("NCAA_D1_BShiny/summary_hitting_data.tsv",sep="\t")
hitting <- drop_read_csv("NCAA_D1_BShiny/player_hitting.csv")
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
hitting <- hitting %>% mutate(
  player_name = trim(player_name)
)

final_adv_metrics <- drop_read_csv("NCAA_D1_BShiny/clust_basic_df.csv")

source("helper_functions.R")
