# Player Data
setwd("~/OneDrive/Mac_Desktop/Baseball_Scrapper_2016-master/data")
player_pitching <- read.csv('summary_pitching_data.tsv',header=TRUE,sep = "\t",quote="",stringsAsFactors = FALSE)
player_hitting <- read.csv('summary_hitting_data.tsv',header=TRUE,sep = "\t",quote="",stringsAsFactors = FALSE)

player_hitting[,c("R","H","X2B","X3B", "HR","RBI","BB","HBP","SF","SH","K","DP","SB","CS")] <- 
  apply(player_hitting[,c("R","H","X2B","X3B","HR","RBI","BB","HBP","SF","SH","K","DP","SB","CS")], 2, function(x){replace(x, is.na(x), 0)})

player_hitting$K_PCT <- with(player_hitting,K/AB)
player_hitting$BB_PCt <- with(player_hitting,BB/AB)
#player_hitting$X3B[is.na(player_hitting$X3B)] <- 0
#player_hitting$HR[is.na(player_hitting$HR)] <- 0

player_hitting$X1B <- with(player_hitting, H - (X2B+X3B+HR))

# Team Data
hitting_table <- read.table('summary_team_hitting_data.tsv',header=TRUE,sep="\t",quote="", stringsAsFactors = FALSE)
pitching_table <- read.table('summary_team_pitching_data.tsv',header=TRUE,sep="\t",quote="", stringsAsFactors = FALSE)

for(i in 1:nrow(hitting_table)){
  sub_data <- player_hitting %>% filter(team_name %in% hitting_table$team_name[i])
  games <- max(sub_data$played)
  hitting_table$team_games[i] <- games
}

rm(sub_data)
