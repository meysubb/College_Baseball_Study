### Helper Functions from here on out. 
library(curl)
options(timeout= 4000000)
stripwhite <- function(x) gsub("\\s*$", "", gsub("^\\s*", "", x))

score_fill=function(score_in){
  m=length(score_in)
  score_in[1]=0
  for(i in 2:m){
    if (is.na(score_in[i])){
      score_in[i]=score_in[i-1]
    }
  }
  return(score_in)
}

clean_games = function(game_id,year){
  print(paste0('Processing pbp for ',game_id))
  first_url='https://stats.ncaa.org/contests/'
  first_x = paste(first_url,game_id,'box_score',sep='/')
  s = curl(first_x, handle = curl::new_handle("useragent" = "Mozilla/5.0"))
  read_url = try(read_html(s))
  if(class(read_url)=='try-error'){
    print(paste('Cannot connect to server for', game_id, 'in', year))
    return(NULL)
  }
  adv_game_id = read_url %>%  html_nodes("#root li:nth-child(3) a") %>% html_attr("href")
  
  base_url='http://stats.ncaa.org'
  x= paste(base_url, adv_game_id, sep='/')
  # random sleep 
  val = sample(c(T,F),size = 1,prob=c(0.1,0.9))
  if(val==T){
    print('sleep')
    Sys.sleep(1)
  }
  
  x_read <- try(getURL(x))
  if (class(x_read)=='try-error'){
    print(paste('Cannot connect to server for', game_id, 'in', year))
    return(NULL)
  }
  y=readHTMLTable(x_read)
  # Play by play is in table form
  y = y[which(!sapply(y,is.null))]
  
  if (length(y) == 0) {
    print(paste("Play by Play data not available for game", game_id, sep=' '))
    return(NULL)
  }
  else if((length(y) < ncol(y[[3]])) ){
    print(paste("Play by Play data not available for game", game_id, sep=' '))
    return(NULL)
  }
  else{
    j=1
    for (i in 1:length(y)){
      # Disgard NULL tables
      if (is.null(y[[i]])==FALSE){
        # Only take pbp tables (3 cols)
        if (ncol(y[[i]])==3){
          inn=as.data.frame(y[[i]])%>%
            mutate(inning=j,
                   game_id=game_id,
                   year=year)%>%
            select(year,game_id,inning,everything())
          j=j+1
          if(j==2){
            pbp = inn
          }else{
            pbp = rbind(pbp,inn)
          }
        }
      }
    }
  }
  if(!exists('pbp')){
    return(NULL)
  }
  pbp = pbp %>% mutate(away_team = colnames(pbp)[4],
                       home_team = colnames(pbp)[6],
                       away_score = as.integer(gsub('-.*', '', Score)),
                       home_score = as.integer(gsub('.*-', '', Score)),
                       away_score=score_fill(away_score),
                       home_score=score_fill(home_score))%>%
    rename(away_text = 4,
           home_text = 6)%>%
    filter(substr(away_text,1,3)!='R: ')%>%
    select(year, game_id, inning, away_team, home_team, away_score, home_score, away_text, home_text)
  return(pbp)
}