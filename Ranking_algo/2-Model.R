library(tibble)
lm.bb <- lm(RD ~ school + opponent + location + conf_play, data = full_dat_conf,weights = ffactor)
scale_factor <- median(lm.bb$coefficients[2:297])

power_df2 = data.frame(
  coeff = lm.bb$coefficients[2:299] - scale_factor
) %>% rownames_to_column(var='school') %>% 
  mutate(
    school = gsub("school","",school),
    rank = dense_rank(-coeff)) 

write_csv(power_df2,"power_ranks.csv")
