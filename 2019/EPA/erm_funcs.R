library(pacman)
p_load(baseballr,tidyverse)

erm_pbp <- function(pbp) {
  erm_mat = ncaa_get_expected_runs_matrix(pbp)
  erm_df = as.data.frame(erm_mat)
  erm_df$situation = c(
    "no base runners",
    "first base",
    "second base",
    "first/second base",
    "third base",
    "first/third base",
    "second/third base",
    "bases loaded"
  )
  rownames(erm_df) <- NULL
  erm_df = reshape2::melt(erm_df)
  colnames(erm_df)[2] <- "outs_before"
  erm_df$outs_before = as.numeric(erm_df$outs_before) - 1
  colnames(erm_df)[3] <- "expected_runs_added"
  
  pbp = pbp %>% mutate(
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
  
  pbp_era = pbp %>% left_join(erm_df) %>% mutate(
    esd = case_when(
      top_inning == 1 ~ expected_runs_added + away_score - home_score,
      top_inning == 0 ~ expected_runs_added + home_score - away_score
    )
  )
  
  results = pbp_era %>% filter(inning == 9) %>%
    mutate(
      result = case_when(
        home_score > away_score ~ 1,
        home_score < away_score ~ 0,
        home_score == away_score ~ 0.5
      )
    ) %>% select(game_id, result) %>% distinct(game_id, .keep_all = T)
  
  pbp_final = pbp_era %>% inner_join(results)
  
  return(pbp_final)
  
}

pbp_17 = read_csv("2019/EPA/data/NCAA_baseball_pbp_2017.csv")
pbp_18 = read_csv("2019/EPA/data/NCAA_baseball_pbp_2018.csv")
pbp_19 = read_csv("2019/EPA/data/NCAA_baseball_pbp_2019.csv")

erm_17 = erm_pbp(pbp_17)
erm_18 = erm_pbp(pbp_18)
erm_19 = erm_pbp(pbp_19)