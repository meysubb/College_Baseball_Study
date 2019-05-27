library(pacman)
p_load(baseballr,tidyverse)

pbp_17 = read_csv('EPA/data/NCAA_baseball_pbp_2017.csv')
erm_17 = ncaa_get_expected_runs_matrix(pbp_17)

erm_17_df = as.data.frame(erm_17)
erm_17_df$situation = c("no base runners","first base","second base",
                        "first/second base","third base","first/third base",
                        "second/third base","bases loaded")
rownames(erm_17_df) <- NULL
erm_17_df = reshape2::melt(erm_17_df)
colnames(erm_17_df)[2] <- "outs_before"
erm_17_df$outs_before = as.numeric(erm_17_df$outs_before) - 1
colnames(erm_17_df)[3] <- "expected_runs_added"

pbp_17 = pbp_17 %>% mutate(
  situation = case_when(
    is.na(r1_name) & is.na(r2_name) & is.na(r3_name) ~ "no base runners",
    !is.na(r1_name) & is.na(r2_name) & is.na(r3_name) ~ "first base",
    is.na(r1_name) & !is.na(r2_name) & is.na(r3_name) ~ "second base",
    !is.na(r1_name) & !is.na(r2_name) & is.na(r3_name) ~ "first/second base",
    is.na(r1_name) & is.na(r2_name) & !is.na(r3_name) ~ "third base",
    !is.na(r1_name) & is.na(r2_name) & !is.na(r3_name) ~ "first/third base",
    is.na(r1_name) & !is.na(r2_name) & !is.na(r3_name) ~ "second/third base",
    !is.na(r1_name) & !is.na(r2_name) & !is.na(r3_name) ~ "bases loaded"
  ),
  outs_before = as.numeric(outs_before)
)

pbp_17_era = pbp_17 %>% left_join(erm_17_df)

## expected score difference
pbp_17_era = pbp_17_era %>% mutate(
  esd = case_when(
    top_inning == 1 ~ expected_runs_added + away_score - home_score,
    top_inning == 0 ~ expected_runs_added + home_score - away_score
  ))

results = pbp_17_era %>% filter(inning==9) %>% 
  mutate(
    result = case_when(
      home_score > away_score ~ 1,
      home_score < away_score ~ 0,
      home_score == away_score ~ 0.5)) %>% select(date,game_id,result) %>% distinct(date,game_id,.keep_all = T)

pbp_17_final = pbp_17_era %>% inner_join(results)


library(mgcv)

t = bam(as.factor(result)~s(esd)+s(inning),
        data=pbp_17_final,
        family=binomial(link = "logit"))

save(t,file="EPA/models/wp_17_model.RData")

temp_dat = pbp_17_final %>% select(game_id,esd,inning)

new_res = predict.bam(t,newdata = pbp_17_final %>% select(-game_id),type="response")

pbp_17_final$home_wpa = new_res

davidson_game = pbp_17_final %>% filter(game_id == 4376802) %>% 
  mutate(away_wpa = 1 - home_wpa,
         in_time = case_when(
          top_inning == 1 ~ inning + (outs_before/10),
          top_inning == 0 ~ inning + (outs_before/10) + 0.3,
         )) 

plot_ready = davidson_game %>% select(home_wpa,away_wpa,in_time) %>% 
  reshape2::melt(id="in_time")


ggplot(plot_ready ,aes(x=in_time,y=value,color=variable)) + 
  geom_line() + 
  theme_bw(base_size = 16) + 
  labs(x='Inning',y="Win Probablity",
       title = "Texas A&M vs Davidson, June 9th, 2017") + 
  scale_color_manual(values=c("firebrick3","lightblue"),labels = c("Texas A&M","Davidson")) +
  scale_y_continuous(labels = scales::percent)


#pbp_18 = read_csv('EPA/data/NCAA_baseball_pbp_2018.csv')
#erm_18 = ncaa_get_expected_runs_matrix(pbp_18)
