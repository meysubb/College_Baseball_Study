library(rvest)
### Get Division Team names
# url <- 'http://stats.ncaa.org/team/inst_team_list?academic_year=2016&conf_id=911&division=1&sport_code=MBA'
url <- 'http://stats.ncaa.org/team/inst_team_list?academic_year=2016&conf_id=-1&division=1&sport_code=MBA'
read_url <- read_html(url)
division <- read_url %>% 
  html_nodes('li a') %>%
  html_text() 
division_id <- read_url %>% 
  html_nodes('li a') %>%
  html_attr("href")

division_df <- data_frame(division[35:65],division_id[35:65])
colnames(division_df) <- c("Division","Href_ID")
# Extract text in HREF
division_df$Href_ID <- as.numeric(gsub(".*\\((.*)\\).*", "\\1", division_df$Href_ID))

team_div <- list()
for(i in 1:length(division_df$Href_ID)){
  div_url <- paste0('http://stats.ncaa.org/team/inst_team_list?academic_year=2016&conf_id=',division_df$Href_ID[i],'&division=1&sport_code=MBA')
  read_div <- read_html(div_url)
  names <- read_div %>%
    html_nodes('td a') %>%
    html_text()
  df_div <- data_frame(names,rep(division_df$Division[i],length(names)))
  team_div[[i]] <- df_div
}

library(reshape2)
team_div_df <- melt(team_div)
colnames(team_div_df) <- c("Team_Names","Division","L1")
team_div_df <- team_div_df %>% select(Team_Names,Division)


### Grab Top 25 teams

top_25_url <- "http://www.ncaa.com/rankings/baseball/d1/d1baseballcom-top-25"
read_top_25 <- read_html(top_25_url)
top_25_names <- read_top_25 %>%
  html_table()

top_25_names <- as.data.frame(top_25_names)
top_25_names$Conference <- 0
top_25_names$SCHOOL[2] <- "Miami (FL)"
# Replace State to St. 
top_25_names$SCHOOL <- gsub("State","St.",top_25_names$SCHOOL)
top_25_names$SCHOOL[13] <- "North Carolina St."
top_25_names$SCHOOL[17] <- "Coastal Caro."
top_25_names$SCHOOL[18] <- "Fla. Atlantic"
top_25_names$SCHOOL[20] <- "UL Lafayette"
top_25_names$SCHOOL[23] <- "Southern Miss."

for(i in 1:nrow(top_25_names)){
  index = which(top_25_names$SCHOOL[i] == team_div_df$Team_Names)
  top_25_names$Conference[i] = team_div_df$Division[index]
}

SEC <- subset(team_div_df,Division == "Southeastern")

#### Scrape Data for MLB Draftees
mlb_draft_url <- 'http://www.baseball-reference.com/draft/?query_type=year_round&year_ID=2016&draft_round=1&draft_type=junreg&'
read_draft <- read_html(mlb_draft_url)
mlb_names <- read_draft %>% 
  html_nodes('#div_draft_stats') %>%
  html_text()

mlb_names <- read_draft %>% html_table()
test <- as.data.frame(mlb_names[[2]])

for(l in 2:10){
  mlb_d_url <- paste0('http://www.baseball-reference.com/draft/?query_type=year_round&year_ID=2016&draft_round=',l,'&draft_type=junreg&')
  read_draft <- read_html(mlb_d_url)
  mlb_names <- read_draft %>% 
    html_nodes('#div_draft_stats') %>%
    html_text()
  mlb_names <- read_draft %>% html_table()
  test <- rbind(test,mlb_names[[2]])
}
colnames(test)[23] <- "School"
test <- test[colSums(!is.na(test)) > 0]
mlb_draft <- test %>% select(Rnd,OvPck,Tm,Name,Pos,School)

mlb_draft$Name <- gsub("\\s*\\([^\\)]+\\)","",mlb_draft$Name)
mlb_draft$School <- gsub("\\s*\\([^\\)]+\\)","",mlb_draft$School)
mlb_draft <- mlb_draft[-grep("HS", mlb_draft$School),]
mlb_draft <- mlb_draft %>% separate(Name,c("First","Last")," ")
mlb_draft$Name <- paste(mlb_draft$Last,", ",mlb_draft$First,sep="")

rm(test,division_df,df_div)
