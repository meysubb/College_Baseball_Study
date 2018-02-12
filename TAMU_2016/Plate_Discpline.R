## Rename for Plate Discpline

plate_discipline <- player_hitting %>% select(player_name,team_name,AB,K,BB,K_PCT,BB_PCt) %>%
  na.omit() 
plot <- plate_discipline %>% filter(team_name %in% SEC$Team_Names)
plot$TAMU <- ifelse(plot$team_name == "Texas A&M",T,F)
plot$K_PCT <- plot$K_PCT * 100
plot$BB_PCt <- plot$BB_PCt * 100
plot <- plot %>% filter(AB>25)

## Plate Discipline within SEC
## K% vs BB%
k_excellent = 10
k_poor = 27.5
bb_excellent = 15
bb_poor = 4

ggplot(data=plot, aes(x=BB_PCt,y=K_PCT,color=TAMU,label=player_name)) + 
  geom_point(alpha=0.5) + 
  xlim(0,60) + ylim(0,65) + 
  labs(x="BB%",y="K%") + 
  theme(legend.position="none") + 
  geom_text(data = subset(plot,team_name == "Texas A&M" & (K_PCT > 40 | BB_PCt > 30)),vjust=-1,hjust=0.25) + 
  geom_text(data = subset(plot,team_name == "Texas A&M" & (K_PCT < 10 & BB_PCt < 15)),vjust=1.2,hjust=0.25) + 
  scale_colour_manual(values = setNames(c('maroon','grey'),c(T, F))) +
  geom_hline(yintercept = k_excellent,color="blue") + 
  #geom_hline(yintercept = k_poor, color = "red") + 
  geom_vline(xintercept = bb_excellent, color="blue") 
#+ geom_vline(xintercept = bb_poor, color = "red")

  

## Plate Discpline within Top 25
Top_25 <- plate_discipline %>% filter(team_name %in% top_25_names$SCHOOL)
Top_25$TAMU <- ifelse(Top_25$team_name == "Texas A&M",T,F)
Top_25$K_PCT <- Top_25$K_PCT * 100
Top_25$BB_PCt <- Top_25$BB_PCt * 100
Top_25 <- Top_25 %>% filter(AB>25)

ggplot(data=Top_25, aes(x=BB_PCt,y=K_PCT,color=TAMU,label=player_name)) + 
  geom_point(alpha=0.5) + 
  xlim(0,60) + ylim(0,65) + 
  labs(x="BB%",y="K%") + 
  theme(legend.position="none") + 
  geom_text(data = subset(Top_25,team_name == "Texas A&M" & (K_PCT > 40 | BB_PCt > 30)),vjust=-1,hjust=0.25) + 
  geom_text(data = subset(Top_25,team_name == "Texas A&M" & (K_PCT < 10 & BB_PCt < 15)),vjust=1.2,hjust=0.25) + 
  geom_text(data = subset(Top_25,BB_PCt > 40),vjust=1.2,hjust=0.25) + 
  scale_colour_manual(values = setNames(c('maroon','grey'),c(T, F))) +
  geom_hline(yintercept = k_excellent,color="blue") + 
  #geom_hline(yintercept = k_poor, color = "red") + 
  geom_vline(xintercept = bb_excellent, color="blue") 
  #+ geom_vline(xintercept = bb_poor, color = "red")

## Ronnie Gideon only had 67 at bats. 

rm(plate_discipline,plot,Top_25)
