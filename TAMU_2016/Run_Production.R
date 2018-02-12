## HR vs. SH + SF + SB
## Look at whole team 
## This team extreme outlier, playing small ball, power threat
## TAMU 

Runs_data <- hitting_table %>% select(team_name,team_AB,team_BA,team_HR,team_SF,team_SH,team_SB) %>%
  na.omit() 

Runs_data$total <- with(Runs_data,team_SH+team_SF+team_SB)
Runs_data$TAMU <- ifelse(Runs_data$team_name == "Texas A&M",T,F)

avg_HR <- round(mean(Runs_data$team_HR))
avg_total <- round(mean(Runs_data$total))

RD <- Runs_data %>% filter(team_name %in% SEC$Team_Names | team_name %in% top_25_names$SCHOOL)
SEC_HR <- round(mean(RD$team_HR))
SEC_total <- round(mean(RD$total))

T25 <- Runs_data %>% filter(team_name %in% top_25_names$SCHOOL)
T25_HR <- round(mean(T25$team_HR))
T25_total <- round(mean(T25$total))


ggplot(data=Runs_data,aes(x=team_HR,y=total,color=TAMU,label=team_name)) + 
  geom_point(alpha=0.5) + 
  theme(legend.position="none")  + 
  labs(x="HR",y="Sac Fly/Hit + Stolen Bases") + 
  geom_vline(xintercept = avg_HR) + 
  geom_vline(xintercept = SEC_HR,color="blue") + 
  geom_vline(xintercept = T25_HR, color="orange") + 
  geom_hline(yintercept = avg_total) + 
  geom_hline(yintercept = SEC_total, color="blue") + 
  geom_hline(yintercept = T25_total, color="orange") + 
  scale_colour_manual(values = setNames(c('maroon','grey'),c(T, F))) + 
  geom_text(data=subset(Runs_data,team_name == "Texas A&M"),vjust=-1,hjust=0.25) + 
  #geom_text(data=subset(Runs_data,team_HR>75 & total>175),vjust=-1,hjust=1) + 
  geom_text(data=subset(Runs_data,team_HR<25 & total>200,vjust=-5,hjust=0.25)) + 
  geom_text(data=subset(Runs_data,team_HR>75 & total >150),vjust=-1,hjust=0.8)

ggplot(data=RD,aes(x=team_HR,y=total,color=TAMU,label=team_name)) + 
  geom_point() + 
  theme(legend.position="none")  + 
  labs(x="HR",y="Sac Fly/Hit + Stolen Bases") + 
  geom_vline(xintercept = SEC_HR,color="blue") + 
  geom_vline(xintercept = T25_HR, color="orange") + 
  geom_hline(yintercept = SEC_total,color="blue") + 
  geom_hline(yintercept = T25_total, color="orange") + 
  scale_colour_manual(values = setNames(c('maroon','grey'),c(T, F))) + 
  geom_text(data=subset(RD,team_name == "Texas A&M"),vjust=-1,hjust=1) + 
  geom_text(data=subset(RD,team_HR>=70 & team_name != "Texas A&M"),vjust=-1,hjust=1) +
  geom_text(data=subset(RD,total>175 & team_name != "Texas A&M"),vjust=-1,hjust=1) 

rm(Runs_data,RD,T25)